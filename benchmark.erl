-module(benchmark).

-export([
    test_fib/0,
    test_timeline/0,
    test_timeline_para_users/0,
    test_timeline_para_instances/0,
    test_send_message/0,
    test_send_message_para_users/0,
    test_send_message_para_instances/0,
    test_test_para_inst/0,
    test_bench/0,
    experiment_setup/0
]).

%% Fibonacci
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

%% Benchmark helpers

test_bench() ->
    [VubUsers, UlbUsers] = experiment_setup(),
    %io:fwrite("Users: ~p~n", [Users]).
    lists:foreach(
        fun(N) ->
            %select random user
            [UserPid, UserName] = pick_random(VubUsers),
            Timeline = server:get_timeline(UserPid, UserName),
            io:fwrite("username: ~p~n", [list_to_atom(UserName)]),
            io:fwrite("Timeline: ~p~n", [Timeline]),
            
            Timeline
        end,
            lists:seq(1, 1)
        ),
        lists:foreach(
        fun(N) ->
            %select random user
            [UserPid, UserName] = pick_random(UlbUsers),
            Timeline = server:get_timeline(UserPid, UserName),
            io:fwrite("username: ~p~n", [list_to_atom(UserName)]),
            io:fwrite("Timeline: ~p~n", [Timeline]),
            Timeline
        end,
            lists:seq(1, 1)
        ).


% Recommendation: run each test at least 30 times to get statistically relevant
% results.
run_benchmark(Name, Fun, Times) ->
    ThisPid = self(),
    lists:foreach(
        fun(N) ->
            % Recommendation: to make the test fair, each run executes in its own,
            % newly created Erlang process. Otherwise, if all tests run in the same
            % process, the later tests start out with larger heap sizes and
            % therefore probably do fewer garbage collections. Also consider
            % restarting the Erlang emulator between each test.
            % Source: http://erlang.org/doc/efficiency_guide/profiling.html
            spawn_link(fun() ->
                run_benchmark_once(Name, Fun, N),
                ThisPid ! done
            end),
            receive
                done ->
                    ok
            end
        end,
        lists:seq(1, Times)
    ).

run_benchmark_once(Name, Fun, N) ->
    io:format("Starting benchmark ~s: ~p~n", [Name, N]),

    % Start timers
    % Tips:
    % * Wall clock time measures the actual time spent on the benchmark.
    %   I/O, swapping, and other activities in the operating system kernel are
    %   included in the measurements. This can lead to larger variations.
    %   os:timestamp() is more precise (microseconds) than
    %   statistics(wall_clock) (milliseconds)
    % * CPU time measures the actual time spent on this program, summed for all
    %   threads. Time spent in the operating system kernel (such as swapping and
    %   I/O) is not included. This leads to smaller variations but is
    %   misleading.

    % Wall clock time
    StartTime = os:timestamp(),
    %statistics(runtime),       % CPU time, summed for all threads

    % Run
    Fun(),

    % Get and print statistics
    % Recommendation [1]:
    % The granularity of both measurement types can be high. Therefore, ensure
    % that each individual measurement lasts for at least several seconds.
    % [1] http://erlang.org/doc/efficiency_guide/profiling.html
    WallClockTime = timer:now_diff(os:timestamp(), StartTime),
    %{_, CpuTime} = statistics(runtime),
    io:format("Wall clock time = ~p ms~n", [WallClockTime / 1000.0]),
    %io:format("CPU time = ~p ms~n", [CpuTime]),
    io:format("~s done~n", [Name]).

%% Benchmarks
% Below are some example benchmarks. Extend these to test the best and worst
% case of your implementation, some typical scenarios you imagine, or some
% extreme scenarios.

test_fib() ->
    io:format("Parameters:~n"),
    io:format("~n"),
    run_benchmark("fib", fun test_fib_benchmark/0, 30).

test_fib_benchmark() ->
    % Spawn 64 processes that each compute the 30th Fibonacci number.
    BenchmarkPid = self(),
    Pids = [
        spawn(fun() ->
            fib(30),
            BenchmarkPid ! done
        end)
     || _ <- lists:seq(1, 64)
    ],
    lists:foreach(
        fun(_) ->
            receive
                done ->
                    ok
            end
        end,
        Pids
    ).

foreach(0, _, Res) -> Res;

foreach(Count, Fun, Res) ->
    NewRes = Fun(Count, Res),
    foreach(Count - 1, Fun, NewRes).


experiment_setup() ->
    io:format("Experiment 1~n"),
    io:format("Parameters:~n"),
    io:format("Number of users: ~p~n", [5000]),
    io:format("Number of subscriptions: ~p~n", [30]),
    io:format("Number of messages: ~p~n", [10]),
    io:format("~n"),
    UserCount = 2500,
    LocalSubscriptionsCount = 30,
    RemoteSubscriptionsCount = 30,
    MessageCount = 10,
    MaxUsersPerServer = 50,
    Vub = server_para:init_instance('vub.be', MaxUsersPerServer),
    Ulb = server_para:init_instance('ulb.be', MaxUsersPerServer),
    %VubUsers = [],
    %UlbUsers = [],
    VubUsers = foreach(2500, fun(Count, VubUsers) -> 
            io:format("Iteration: ~p~n", [Count]),
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
                fun(I) ->
                    server:follow(User, UserName, "vub_user" ++ integer_to_list(rand:uniform(UserCount))  ++ "@vub.be")
                end,
                lists:seq(1, LocalSubscriptionsCount)
            ),
            %follow RemoteSubscriptionsCount random users on ulb.be
            lists:foreach(
                fun(I) ->
                    server:follow(User, UserName, "ulb_user" ++ integer_to_list(rand:uniform(UserCount)) ++ "@ulb.be")
                end,
                lists:seq(1, RemoteSubscriptionsCount)
            ),

            VubUsers ++ [[User, UserName]]
        end, []),

    %lists:foreach(
    %    fun(Count, VubUsers) -> 
    %        io:format("Iteration: ~p~n", [Count]),
    %        %Make user on vub.be
    %        UserName = "vub_user" ++ integer_to_list(Count),
    %        User = server:register_user(Vub, UserName),
    %        %send MessageCount messages
    %        lists:foreach(
    %            fun(I) ->
    %                server:send_message(User, UserName, "vub_" ++ integer_to_list(Count) ++ "_message_" ++ integer_to_list(I))
    %            end,
    %            lists:seq(1, MessageCount)
    %        ),
    %        %follow LocalSubscriptionsCount random users on vub.be
    %        lists:foreach(
    %            fun(I) ->
    %                server:follow(User, UserName, "vub_user" ++ integer_to_list(rand:uniform(UserCount))  ++ "@vub.be")
    %            end,
    %            lists:seq(1, LocalSubscriptionsCount)
    %        ),
    %        %follow RemoteSubscriptionsCount random users on ulb.be
    %        lists:foreach(
    %            fun(I) ->
    %                server:follow(User, UserName, "ulb_user" ++ integer_to_list(rand:uniform(UserCount)) ++ "@ulb.be")
    %            end,
    %            lists:seq(1, RemoteSubscriptionsCount)
    %        ),
%
    %        VubUsers = VubUsers ++ [User, UserName]
    %    end,
    %    lists:seq(1, 2500), [[User, UserName] | VubUsers]],
    %),
    UlbUsers = foreach(2500, fun(Count, UlbUsers) -> 
            io:format("Iteration: ~p~n", [Count]),
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
                fun(I) ->
                    server:follow(User, UserName, "ulb_user" ++ integer_to_list(rand:uniform(UserCount))  ++ "@ulb.be")
                end,
                lists:seq(1, LocalSubscriptionsCount)
            ),
            %follow RemoteSubscriptionsCount random users on ulb.be
            lists:foreach(
                fun(I) ->
                    server:follow(User, UserName, "vub_user" ++ integer_to_list(rand:uniform(UserCount)) ++ "@vub.be")
                end,
                lists:seq(1, RemoteSubscriptionsCount)
            ),

            UlbUsers ++ [[User, UserName]]

        end, []),

    %lists:foreach(
    %    fun(Count) -> 
    %        io:format("Iteration: ~p~n", [Count]),
    %        %Make user on ulb.be
    %        UserName = "ulb_user" ++ integer_to_list(Count),
    %        User = server:register_user(Ulb, UserName),
    %        %send MessageCount messages
    %        lists:foreach(
    %            fun(I) ->
    %                server:send_message(User, UserName, "ulb_" ++ integer_to_list(Count) ++ "_message_" ++ integer_to_list(I))
    %            end,
    %            lists:seq(1, MessageCount)
    %        ),
    %        %follow LocalSubscriptionsCount random users on ulb.be
    %        lists:foreach(
    %            fun(I) ->
    %                server:follow(User, UserName, "ulb_user" ++ integer_to_list(rand:uniform(UserCount))  ++ "@ulb.be")
    %            end,
    %            lists:seq(1, LocalSubscriptionsCount)
    %        ),
    %        %follow RemoteSubscriptionsCount random users on ulb.be
    %        lists:foreach(
    %            fun(I) ->
    %                server:follow(User, UserName, "vub_user" ++ integer_to_list(rand:uniform(UserCount)) ++ "@vub.be")
    %            end,
    %            lists:seq(1, RemoteSubscriptionsCount)
    %        ),
%
    %        UlbUsers = UlbUsers ++ [User, UserName]
%
    %    end,
    %    lists:seq(1, 2500)
    %),
    [VubUsers, UlbUsers].

% Creates a server with 5000 users following 25 others and sending 10 messages.
%
% Note that this code depends on the implementation of the server. You will need to
% change it if you change the representation of the data in the server.
initialize_server() ->
    % Seed random number generator to get reproducible results.
    rand:seed_s(exsplus, {0, 0, 0}),
    % Parameters
    NumberOfUsers = 5000,
    NumberOfSubscriptions = 30,
    NumberOfMessages = 10,
    io:format("Parameters:~n"),
    io:format("Number of users: ~p~n", [NumberOfUsers]),
    io:format("Number of subscriptions: ~p~n", [NumberOfSubscriptions]),
    io:format("Number of messages: ~p~n", [NumberOfMessages]),
    io:format("~n"),
    % Generate user names: just the numbers from 1 to NumberOfUsers, as strings.
    % Note: integer_to_list convert an integer to a string, e.g. 123 to "123".
    % Note: the syntax [F(X) || X <- L] is a list comprehension. It generates a list
    % by applying F to each element of L. It is equivalent to
    % lists:map(fun (X) -> F(X) end, L).
    UserNames = [integer_to_list(I) || I <- lists:seq(1, NumberOfUsers)],
    % Generate users dict.
    Users = dict:from_list(
        lists:map(
            fun(Name) ->
                % Random subscriptions.
                Subscriptions = [
                    pick_random(UserNames)
                 || _ <- lists:seq(1, NumberOfSubscriptions)
                ],
                % Random messages.
                Messages = [generate_message(Name, I) || I <- lists:seq(1, NumberOfMessages)],
                User = {user, Name, sets:from_list(Subscriptions), Messages},
                % {key, value} for dict.
                {Name, User}
            end,
            UserNames
        )
    ),
    ServerPid = server_centralized:initialize_with(Users),
    {ServerPid, UserNames}.

% Pick a random element from a list.
pick_random(List) ->
    [Pid, UserName] = lists:nth(rand:uniform(length(List)), List),
    [Pid, UserName].
pick_random_remote_instance(NumberOfInstances, LocalInstance) ->
    Instances = [
        integer_to_list(I)
     || I <- lists:delete(LocalInstance, lists:seq(1, NumberOfInstances))
    ],
    RemoteInstances = lists:delete(LocalInstance, Instances),

    pick_random(RemoteInstances).

generate_user_dict(
    NumberOfUsers,
    NumberOfSubscriptions,
    NumberOfMessages,
    LocalSubscriptionsAmount,
    RemoteSubscriptionsAmount,
    NumberOfInstances,
    LocalInstance
) ->
    UserNames = [integer_to_list(I) || I <- lists:seq(1, NumberOfUsers)],
    LocalUserNames = [integer_to_list(I) || I <- lists:seq(1, NumberOfUsers)],
    RemoteUserNames = [
        integer_to_list(I) ++ "@" ++
            pick_random_remote_instance(NumberOfInstances, LocalInstance)
     || I <- lists:seq(1, NumberOfUsers)
    ],

    Users = dict:from_list(
        lists:map(
            fun(Name) ->
                % Random subscriptions.
                LocalSubscriptions = [
                    pick_random(LocalUserNames)
                 || %pick_random(LocalUserNames)
                    _ <- lists:seq(1, LocalSubscriptionsAmount)
                ],
                RemoteSubscriptions = [
                    pick_random(RemoteUserNames)
                 || _ <- lists:seq(1, RemoteSubscriptionsAmount)
                ],
                Subscriptions = LocalSubscriptions ++ RemoteSubscriptions,
                %io:format(Subscriptions),
                % Random messages.
                Messages = [generate_message(Name, I) || I <- lists:seq(1, NumberOfMessages)],
                User = {user, Name, sets:from_list(Subscriptions), Messages},
                % {key, value} for dict.
                {Name, User}
            end,
            UserNames
        )
    ),
    %io:format("Users: ~p~n", [Users]),
    Users.

initialize_instances(
    NumberOfUsers,
    NumberOfSubscriptions,
    LocalSubscriptions,
    RemoteSubscriptions,
    NumberOfMessages,
    NumberOfInstances
) ->
    rand:seed_s(exsplus, {0, 0, 0}),
    % Parameters
    io:format("Parameters:~n"),
    io:format("Number of users: ~p~n", [NumberOfUsers]),
    io:format("Number of subscriptions: ~p~n", [NumberOfSubscriptions]),
    io:format("Number of messages: ~p~n", [NumberOfMessages]),
    io:format("Number of instances: ~p~n", [NumberOfInstances]),
    io:format("~n"),

    InstanceNames = [integer_to_list(I) || I <- lists:seq(1, NumberOfInstances)],
    InstanceDict = dict:from_list(
        lists:map(
            fun(Name) ->
                UserDict = generate_user_dict(
                    NumberOfUsers,
                    NumberOfSubscriptions,
                    NumberOfMessages,
                    LocalSubscriptions,
                    RemoteSubscriptions,

                    NumberOfInstances,
                    Name
                ),
                io:format("Instance: ~p~n", [list_to_atom(Name)]),
                server_para:initialize_with(list_to_atom(Name), UserDict),
                {Name, UserDict}
            end,
            InstanceNames
        )
    ),

    %io:format("InstanceDict: ~p~n", [InstanceDict]),
    InstanceDict.

% Generate a random message `I` for `UserName`.
generate_message(UserName, I) ->
    Text = "Message " ++ integer_to_list(I) ++ " from " ++ UserName,
    {message, UserName, Text, os:system_time()}.

test_test_para_inst() ->
    io:format("Testing new solutions~n"),
    A = server_para:init_instance('vub.be'),
    B = server_para:init_instance('ulb.be'),
    lists:foreach(
        fun(Count) ->
            
            A_User = server:register_user(A, "A_user" ++ integer_to_list(Count)),
            B_User = server:register_user(B, "B_user" ++ integer_to_list(Count)),
            server:send_message(A_User, "A_user" ++ integer_to_list(Count), "A_Hello_" ++ integer_to_list(Count)),
            server:send_message(B_User, "B_user" ++ integer_to_list(Count), "B_Hello_" ++ integer_to_list(Count)),
            server:follow(A_User, "A_user" ++ integer_to_list(Count), "B_user" ++ integer_to_list(Count) ++ "@ulb.be"),
            server:follow(B_User, "B_user" ++ integer_to_list(Count), "A_user" ++ integer_to_list(Count) ++ "@vub.be"),
            M1 = server:get_timeline(A_User, "A_user" ++ integer_to_list(Count)),
            M2 = server:get_timeline(B_User, "B_user" ++ integer_to_list(Count)),
            M1,
            io:fwrite("M1: ~p~n", [M1]),
            io:fwrite("M2: ~p~n", [M2])
            %M2
        end,
        lists:seq(1, 10)
    ).

% Get timeline of 10000 users (repeated 30 times).
test_timeline_para_users() ->
    io:format("Timeline test with varying user count~n"),
    lists:foreach(
        fun(Count) ->
            io:format("User count: ~p~n", [Count * 1000]),
            Instances = initialize_instances(Count * 1000, 30, 15, 15, 20, 4),
            InstanceNames = dict:fetch_keys(Instances),

            run_benchmark(
                "timeline_para_users",
                fun() ->
                    lists:foreach(
                        fun(_) ->
                            Instance = pick_random(InstanceNames),
                            UserDict = dict:fetch(Instance, Instances),
                            Users = dict:fetch_keys(UserDict),
                            User = pick_random(Users),
                            InstanceAtom = list_to_atom(Instance),
                            server:get_timeline(
                                InstanceAtom, User
                            )
                        end,
                        lists:seq(1, 500)
                    )
                end,
                30
            ),
            lists:foreach(
                fun(Instance) ->
                    InstanceAtom = list_to_atom(Instance),
                    unregister(InstanceAtom)
                end,
                InstanceNames
            )
        end,
        lists:seq(1, 10)
    ).

test_timeline_para_instances() ->
    io:format("Timeline test with varying instance count~n"),
    lists:foreach(
        fun(Count) ->
            io:format("Instance count: ~p~n", [Count * 2]),
            InstanceCount = Count * 2,
            Instances = initialize_instances(5000, 30, 15, 15, 20, InstanceCount),
            InstanceNames = dict:fetch_keys(Instances),

            run_benchmark(
                "timeline_para_instances",
                fun() ->
                    lists:foreach(
                        fun(_) ->
                            Instance = pick_random(InstanceNames),
                            UserDict = dict:fetch(Instance, Instances),
                            Users = dict:fetch_keys(UserDict),
                            User = pick_random(Users),
                            InstanceAtom = list_to_atom(Instance),
                            server:get_timeline(
                                InstanceAtom, User
                            )
                        end,
                        lists:seq(1, 500)
                    )
                end,
                30
            ),
            lists:foreach(
                fun(Instance) ->
                    InstanceAtom = list_to_atom(Instance),
                    unregister(InstanceAtom)
                end,
                InstanceNames
            )
        end,
        lists:seq(1, 5)
    ).

test_send_message_para_users() ->
    io:format("Send message test with varying user count~n"),
    lists:foreach(
        fun(Count) ->
            io:format("User count~p~n", [Count * 1000]),
            Instances = initialize_instances(Count * 1000, 30, 15, 15, 20, 4),
            InstanceNames = dict:fetch_keys(Instances),

            run_benchmark(
                "send_message_para_users",
                fun() ->
                    lists:foreach(
                        fun(_) ->
                            Instance = pick_random(InstanceNames),
                            UserDict = dict:fetch(Instance, Instances),
                            Users = dict:fetch_keys(UserDict),
                            User = pick_random(Users),
                            InstanceAtom = list_to_atom(Instance),
                            server:send_message(InstanceAtom, User, "Test")
                        end,
                        lists:seq(1, 500)
                    )
                end,
                30
            ),
            lists:foreach(
                fun(Instance) ->
                    InstanceAtom = list_to_atom(Instance),
                    unregister(InstanceAtom)
                end,
                InstanceNames
            )
        end,
        lists:seq(1, 10)
    ).

test_send_message_para_instances() ->
    io:format("Send message test with varying isntance count~n"),
    lists:foreach(
        fun(Count) ->
            io:format("Instance count~p~n", [Count * 2]),
            InstanceCount = Count * 2,
            Instances = initialize_instances(5000, 30, 15, 15, 20, InstanceCount),
            InstanceNames = dict:fetch_keys(Instances),

            run_benchmark(
                "send_message_para_instances",
                fun() ->
                    lists:foreach(
                        fun(_) ->
                            Instance = pick_random(InstanceNames),
                            UserDict = dict:fetch(Instance, Instances),
                            Users = dict:fetch_keys(UserDict),
                            User = pick_random(Users),
                            InstanceAtom = list_to_atom(Instance),
                            server:send_message(InstanceAtom, User, "Test")
                        end,
                        lists:seq(1, 500)
                    )
                end,
                30
            ),
            lists:foreach(
                fun(Instance) ->
                    InstanceAtom = list_to_atom(Instance),
                    unregister(InstanceAtom)
                end,
                InstanceNames
            )
        end,
        lists:seq(1, 5)
    ).

% Get timeline of 10000 users (repeated 30 times).
test_timeline() ->
    {ServerPid, UserName} = initialize_server(),
    run_benchmark(
        "timeline",
        fun() ->
            lists:foreach(
                fun(_) ->
                    server:get_timeline(ServerPid, pick_random(UserName))
                end,
                lists:seq(1, 1000)
            )
        end,
        30
    ).

% Send message for 10000 users.
test_send_message() ->
    {ServerPid, UserName} = initialize_server(),
    run_benchmark(
        "send_message",
        fun() ->
            lists:foreach(
                fun(_) ->
                    server:send_message(ServerPid, pick_random(UserName), "Test")
                end,
                lists:seq(1, 10000)
            )
        end,
        30
    ).
