-module(benchmark_final).

-export([
    mupi/1,
    timeline_worker/2,
    threadpool/0,
    followers/1
]).

followers(FollowersAtomList) ->
    Times = 30,
    FollowersAtom = hd(FollowersAtomList),
    Followers = list_to_integer(atom_to_list(FollowersAtom)),
    FileName = "benchmarks/followers/followers_" ++ integer_to_list(Followers) ++ ".csv",
    {ok, Fd} = file:open(FileName, [append]),
    SetupStartTime = os:timestamp(),
    [VubUsers, UlbUsers] = experiment_setup(10000, Followers, Followers, 50, 250),
    Users = VubUsers ++ UlbUsers,
    SetupWallClockTime = timer:now_diff(os:timestamp(), SetupStartTime),
    io:fwrite(Fd, "~f~n",[SetupWallClockTime/1000.000]),
    ThisPid = self(),
    %Timeline Benchmarks
    lists:foreach(
        fun(_) ->
            spawn_link(
                fun() -> 
                    timeline_benchmark(Users, Fd),
                    ThisPid ! done
                end),
            receive
                done -> ok
            end 
        end, lists:seq(1, Times)).

threadpool() ->
    Times = 30,
    FileName = "benchmarks/threadpool/threadpool_" ++ integer_to_list(erlang:system_info(schedulers)) ++ ".csv",
    {ok, Fd} = file:open(FileName, [append]),
    SetupStartTime = os:timestamp(),
    [VubUsers, UlbUsers] = experiment_setup(10000, 50, 50, 50, 250),
    Users = VubUsers ++ UlbUsers,
    SetupWallClockTime = timer:now_diff(os:timestamp(), SetupStartTime),
    io:fwrite(Fd, "~f~n",[SetupWallClockTime/1000.000]),
    ThisPid = self(),
    %Timeline Benchmarks
    lists:foreach(
        fun(_) ->
            spawn_link(
                fun() -> 
                    timeline_benchmark(Users, Fd),
                    ThisPid ! done
                end),
            receive
                done -> ok
            end 
        end, lists:seq(1, Times)).

mupi(MupiAtomList) ->
    %lists:foreach(
        %fun(MUPI) ->
        MupiAtom = hd(MupiAtomList),
            MUPI = list_to_integer(atom_to_list(MupiAtom)),
            Times = 30,
            FileName = "benchmarks/mupi/mupi_" ++ integer_to_list(MUPI) ++ ".csv",
            {ok, Fd} = file:open(FileName, [append]),
            SetupStartTime = os:timestamp(),
            [VubUsers, UlbUsers] = experiment_setup(10000, 50, 50, 50, MUPI),
            Users = VubUsers ++ UlbUsers,
            SetupWallClockTime = timer:now_diff(os:timestamp(), SetupStartTime),
            io:fwrite(Fd, "~f~n",[SetupWallClockTime/1000.000]),
            ThisPid = self(),
            %Timeline Benchmarks
            lists:foreach(
                fun(_) ->
                    spawn_link(
                        fun() -> 
                            timeline_benchmark(Users, Fd),
                            ThisPid ! done
                        end),
                    receive
                        done -> ok
                    end 
                end, lists:seq(1, Times)).

timeline_benchmark(Users, Fd) ->
    StartTime = os:timestamp(),
    RequestAmount = 10,
    Workers = [spawn(?MODULE, timeline_worker, [Users, self()]) || _ <- lists:seq(1, RequestAmount)],
    collect_replies(Workers),
    WallClockTime = timer:now_diff(os:timestamp(), StartTime),
    io:fwrite(Fd, "~f~n",[WallClockTime/1000.000]).

collect_replies(Workers) ->
    %receive if there are no more workers to wait for
    if Workers == [] -> %io:format("All workers finished"),
                ok;
    %if there are still workers to wait for
    true ->
        receive
            {From, _} ->
                %io:format("Received reply from ~p: ~p~n", [From, Reply]),
                % Remove the worker from the list of pending workers
                RemainingWorkers = lists:delete(From, Workers),
                % Continue collecting replies from the remaining workers
                collect_replies(RemainingWorkers)
        end
    end.
    
timeline_worker(Users, ParentPid) ->
    [UserPid, UserName] = pick_random(Users),
    %io:format("getting timeline for ~p at pid~p ~n", [UserName, UserPid]),
    %server:get_timeline(UserPid2, UserName2),
    UserPid ! {self(), get_timeline, UserName},
    receive
        {_ResponsePid, timeline, _, Timeline} ->
            Timeline;
        _ ->
            []
    end,
    %io:format("got timeline for ~p at pid~p ~n", [UserName, UserPid]),
    ParentPid ! {self(), done}.

pick_random(List) ->
    [Pid, UserName] = lists:nth(rand:uniform(length(List)), List),
    [Pid, UserName].

experiment_setup(UserCount, LocalSubscriptionsCount, RemoteSubscriptionsCount, MessageCount, MaxUsersPerServer) ->
    Vub = server_para:init_instance('vub.be', MaxUsersPerServer),
    Ulb = server_para:init_instance('ulb.be', MaxUsersPerServer),
    VubUsers = foreach(UserCount, fun(Count, VubUsers) -> 
            %io:format("Iteration: ~p~n", [Count]),
            %Make user on vub.be
            UserName = "vub_user" ++ integer_to_list(Count),
            User = server:register_user(Vub, UserName),
            %send MessageCount messages
            lists:foreach(
                fun(I) ->
                    server:send_message(User, UserName, "vub_" ++ integer_to_list(Count) ++ "_message_" ++ integer_to_list(I))
                end,
                lists:seq(1, MessageCount)
            ),
            %follow LocalSubscriptionsCount random users on vub.be
            lists:foreach(
                fun(_) ->
                    server:follow(User, UserName, "vub_user" ++ integer_to_list(rand:uniform(UserCount))  ++ "@vub.be")
                end,
                lists:seq(1, LocalSubscriptionsCount)
            ),
            %follow RemoteSubscriptionsCount random users on ulb.be
            lists:foreach(
                fun(_) ->
                    server:follow(User, UserName, "ulb_user" ++ integer_to_list(rand:uniform(UserCount)) ++ "@ulb.be")
                end,
                lists:seq(1, RemoteSubscriptionsCount)
            ),

            VubUsers ++ [[User, UserName]]
        end, []),
    UlbUsers = foreach(UserCount, fun(Count, UlbUsers) -> 
            %io:format("Iteration: ~p~n", [Count]),
            %Make user on ulb.be
            UserName = "ulb_user" ++ integer_to_list(Count),
            User = server:register_user(Ulb, UserName),
            %send MessageCount messages
            lists:foreach(
                fun(I) ->
                    server:send_message(User, UserName, "ulb_" ++ integer_to_list(Count) ++ "_message_" ++ integer_to_list(I))
                end,
                lists:seq(1, MessageCount)
            ),
            %follow LocalSubscriptionsCount random users on ulb.be
            lists:foreach(
                fun(_) ->
                    server:follow(User, UserName, "ulb_user" ++ integer_to_list(rand:uniform(UserCount))  ++ "@ulb.be")
                end,
                lists:seq(1, LocalSubscriptionsCount)
            ),
            %follow RemoteSubscriptionsCount random users on ulb.be
            lists:foreach(
                fun(_) ->
                    server:follow(User, UserName, "vub_user" ++ integer_to_list(rand:uniform(UserCount)) ++ "@vub.be")
                end,
                lists:seq(1, RemoteSubscriptionsCount)
            ),

            UlbUsers ++ [[User, UserName]]

        end, []),

    [VubUsers, UlbUsers].

foreach(0, _, Res) -> Res;
foreach(Count, Fun, Res) ->
    NewRes = Fun(Count, Res),
    foreach(Count - 1, Fun, NewRes).