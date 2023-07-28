%% This is a simple implementation of the project, using one server instance.
%%
%% It will create one "server" actor that contains all internal state (users,
%% their subscriptions, and their messages).
%%
%% This implementation is provided with unit tests, however, these tests are
%% neither complete nor implementation independent, so be careful when reusing
%% them.
-module(server_para).

-include_lib("eunit/include/eunit.hrl").

-export([
    init_instance/1,
    initialize_with/2,
    server_actor/2,
    instance_actor/7,
    typical_session_1/1,
    typical_session_2/1,
    initialize/0
]).

initialize() ->
    io:fwrite("Hello World!~n").

%%
%% Additional API Functions
%%

% Start server.
init_instance(Address) ->
    %initialize_with(Address, dict:new()).
    FirstServer = spawn_link(?MODULE, server_actor, [Address, dict:new()]),
    catch unregister(server_actor),
    io:format("Server starting at ~p~n", [FirstServer]),
    InstancePid = spawn_link(?MODULE, instance_actor, [Address, 0, FirstServer, 1, 2, [FirstServer], dict:new()]),
    catch unregister(instance_actor),
    register(Address, InstancePid),
    io:format("Instance starting at ~p~n", [InstancePid]),
    %io:format("Instance PID: ~p~n", [InstancePid]),
    InstancePid.

% Start server with an initial state.
% Useful for benchmarking.
initialize_with(Address, Users) ->
    ServerPid = spawn_link(?MODULE, server_actor, [Users]),
    catch unregister(server_actor),
    io:format("Server starting at ~p~n", [Address]),
    io:format("Server PID: ~p~n", [ServerPid]),
    register(Address, ServerPid),
    ServerPid.

% The server actor works like a small database and encapsulates all state of
% this simple implementation.
%
% Users is a dictionary of user names to tuples of the form:
%     {user, Name, Subscriptions, Messages}
% where Subscriptions is a set of usernames that the user follows, and
% Messages is a list of messages, of the form:
%     {message, UserName, MessageText, SendTime}



instance_actor(Address, CurrentUserCount, CurrentServer, CurrentServerCount, MaxUsersPerInstance, ServerPids, Users) ->
    receive
        {Sender, register_user, UserName} ->
            case dict:find(UserName, Users) of
                {ok, Value}->
                    io:format("User ~p already exists~n", [Value]),
                    Sender ! {self(), user_already_exists},
                    instance_actor(Address, CurrentUserCount, CurrentServer, CurrentServerCount, MaxUsersPerInstance, ServerPids, Users);
                _Else ->
                    if
                        CurrentUserCount >= MaxUsersPerInstance ->
                            NewServerPid = spawn_link(?MODULE, server_actor, [Address, dict:new()]),
                            io:format("Too many users, new server starting at ~p~n", [NewServerPid]),
                            NewCurrentServer = NewServerPid,
                            NewCurrentServerCount = CurrentServerCount + 1,
                            NewCurrentUserCount = 0,
                            NewServerPid ! {Sender, register_user, UserName},
                            NewServerPids = ServerPids ++ [NewCurrentServer],
                            NewUsers = dict:store(UserName, NewServerPid, Users);
                        true ->
                            CurrentServer ! {Sender, register_user, UserName},
                            NewCurrentServer = CurrentServer,
                            NewCurrentServerCount = CurrentServerCount,
                            NewCurrentUserCount = CurrentUserCount + 1,
                            NewServerPids = ServerPids,
                            NewUsers = dict:store(UserName, CurrentServer, Users)
                    end,
                    instance_actor(Address, NewCurrentUserCount, NewCurrentServer, NewCurrentServerCount, MaxUsersPerInstance, NewServerPids, NewUsers)
            end;
        {Sender, get_profile, Username} ->
            
            ContainsAt = string:str(Username, "@"),
                if
                    ContainsAt > 0 ->
                        [_Name, _] = string:tokens(Username, "@"),
                        {_, UserServerPid} = dict:find(_Name, Users),
                        UserServerPid ! {Sender, get_profile, Username};
                    true ->
                        {_, UserServerPid} = dict:find(Username, Users),
                        UserServerPid ! {Sender, get_profile, Username}
                end,
            instance_actor(Address, CurrentUserCount, CurrentServer, CurrentServerCount, MaxUsersPerInstance, ServerPids, Users)
    end.



server_actor(HostAddress, Users) ->
    receive
        {Sender, register_user, UserName} ->
            NewUsers = dict:store(UserName, create_user(UserName), Users),
            io:fwrite("registering user ~p~n", [UserName]),
            Sender ! {self(), user_registered},
            server_actor(HostAddress, NewUsers);
        {Sender, log_in, _UserName} ->
            % This doesn't do anything, but you could use this operation if needed.
            Sender ! {self(), logged_in},
            server_actor(HostAddress, Users);
        {Sender, follow, UserName, UserNameToFollow} ->
            NewUsers = follow(Users, UserName, UserNameToFollow),
            Sender ! {self(), followed},
            server_actor(HostAddress, NewUsers);
        {Sender, send_message, UserName, MessageText, Timestamp} ->
            NewUsers = store_message(Users, {message, UserName, MessageText, Timestamp}),
            Sender ! {self(), message_sent},
            server_actor(HostAddress, NewUsers);
        {Sender, get_timeline, UserName} ->
            Sender ! {self(), timeline, UserName, timeline(Users, UserName)},
            server_actor(HostAddress, Users);
        {Sender, get_profile, UserName} ->
            io:fwrite("---------------get_profile----------------~n"),
            % split address from name
            [Name, Domain] = string:tokens(UserName, "@"),
            %get Pid of domain with Address
            DomainPid = whereis(list_to_atom(Domain)),
            %the address is the current server
            io:fwrite("Self:~w~n", [HostAddress]),
            io:fwrite("DomainPid:~w~n", [list_to_atom(Domain)]),
            DomainAtom = list_to_atom(Domain),
            if
                DomainAtom == HostAddress ->
                    io:fwrite("Address is current server~n"),
                    %check if we have user
                    case get_user(Name, Users) of
                        {user, _, _, _} ->
                            io:fwrite("User is here~n"),
                            Sender !
                                {
                                    self(),
                                    profile,
                                    UserName,
                                    sort_messages(get_messages(Users, Name))
                                };
                        error ->
                            io:fwrite("User is not here~n"),
                            HostAddress ! {Sender, get_profile, UserName}
                    end;
                    
                %the address is remote
                DomainAtom =/= HostAddress->
                    io:fwrite("Address is remote~n"),
                    DomainPid ! {Sender, get_profile, UserName}
            end,
            server_actor(HostAddress, Users);
        {Sender, is_profile_here, UserName} ->
            io:fwrite("is_profile_here~n"),
            [Name, _] = string:tokens(UserName, "@"),
            case get_user(Name, Users) of
                {user, _, _, _} ->
                    io:fwrite("found_profile~n"),
                    Sender ! {self(), profile, UserName, sort_messages(get_messages(Users, Name))};
                error ->
                    Sender ! {self(), profile_not_here, UserName}
            end,
            server_actor(HostAddress, Users)
    end.

%%
%% Internal Functions
%%

% Create a new user with `UserName`.
create_user(UserName) ->
    {user, UserName, sets:new(), []}.

% Get user with `UserName` in `Users`.
% Throws an exception if user does not exist (to help in debugging).
% In your project, you do not need specific error handling for users that do not exist;
% you can assume that all users that use the system exist.
get_user(UserName, Users) ->
    case dict:find(UserName, Users) of
        {ok, User} -> User;
        _ -> error
    end.

% Update `Users` so `UserName` follows `UserNameToFollow`.
follow(Users, UserName, UserNameToFollow) ->
    {user, Name, Subscriptions, Messages} = get_user(UserName, Users),
    NewUser = {user, Name, sets:add_element(UserNameToFollow, Subscriptions), Messages},
    dict:store(UserName, NewUser, Users).

% Modify `Users` to store `Message`.
store_message(Users, Message) ->
    {message, UserName, _MessageText, _Timestamp} = Message,
    {user, Name, Subscriptions, Messages} = get_user(UserName, Users),
    NewUser = {user, Name, Subscriptions, Messages ++ [Message]},
    dict:store(UserName, NewUser, Users).

% Get all messages by `UserName`.
get_messages(Users, UserName) ->
    {user, _, _, Messages} = get_user(UserName, Users),
    Messages.

% Generate timeline for `UserName`.
timeline(Users, UserName) ->
    {user, _, Subscriptions, _} = get_user(UserName, Users),
    UnsortedMessagesForTimeLine =
        lists:foldl(
            fun(FollowedUserName, AllMessages) ->
                ContainsAt = string:str(FollowedUserName, "@"),
                if
                    ContainsAt > 0 ->
                        [_Name, Domain] = string:tokens(FollowedUserName, "@"),
                        DomainPid = whereis(list_to_atom(Domain)),
                        DomainPid ! {self(), get_profile, FollowedUserName},
                        receive
                            {_ResponsePid, profile, FollowedUserName, Messages} ->
                                Messages
                        end,
                        AllMessages ++ Messages;
                    true ->
                        AllMessages ++ get_messages(Users, FollowedUserName)
                end
            end,
            [],
            sets:to_list(Subscriptions)
        ),
    sort_messages(UnsortedMessagesForTimeLine).

% Sort `Messages` from most recent to oldest.
sort_messages(Messages) ->
    % Sort on the 4th element of the message tuple (= timestamp, this uses 1-based
    % indexing), and then reverse to put most recent first.
    lists:reverse(lists:keysort(4, Messages)).

%%
%% Tests
%%
% These tests are for this specific implementation. They are a partial
% definition of the semantics of the provided interface but also make certain
% assumptions of the implementation. You can re-use them, but you might need to
% modify them.

% Test initialize function.
initialize_test() ->
    catch unregister(server_actor),
    initialize().

% Initialize server and test user registration of 4 users.
% Returns list of user names to be used in subsequent tests.
register_user_test() ->
    initialize_test(),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "A")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "B")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "C")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "D")),
    ["A", "B", "C", "D"].

% Test log in.
log_in_test() ->
    [UserName1, UserName2 | _] = register_user_test(),
    ?assertMatch({_Server1, logged_in}, server:log_in(server_actor, UserName1)),
    ?assertMatch({_Server2, logged_in}, server:log_in(server_actor, UserName2)).
% Note: returned pids _Server1 and _Server2 do not necessarily need to be
% the same.

% Test follow: user 1 will follow 2 and 3.
follow_test() ->
    [UserName1, UserName2, UserName3 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(server_actor, UserName1),
    ?assertMatch(followed, server:follow(Server1, UserName1, UserName2)),
    ?assertMatch(followed, server:follow(Server1, UserName1, UserName3)),
    {UserName1, Server1, [UserName2, UserName3]}.

% Test sending a message.
send_message_test() ->
    {UserName1, Server1, Subscriptions} = follow_test(),
    ?assertMatch(
        message_sent,
        server:send_message(Server1, UserName1, "Hello!")
    ),
    ?assertMatch(
        message_sent,
        server:send_message(Server1, UserName1, "How is everyone?")
    ),
    {UserName1, Server1, Subscriptions}.

% Test getting a timeline.
get_timeline_test() ->
    {UserName1, Server1, [UserName2, UserName3]} = follow_test(),

    % When nothing has been sent, the timeline is empty.
    ?assertMatch([], server:get_timeline(Server1, UserName1)),

    ?assertMatch(
        message_sent,
        server:send_message(Server1, UserName2, "Hello I'm B!")
    ),

    % One message in the timeline.
    ?assertMatch(
        [
            {message, UserName2, "Hello I'm B!", _TimeB1}
        ],
        server:get_timeline(Server1, UserName1)
    ),

    ?assertMatch(
        message_sent,
        server:send_message(Server1, UserName2, "How is everyone?")
    ),
    ?assertMatch(
        message_sent,
        server:send_message(Server1, UserName3, "Hello I'm C!")
    ),

    % All three messages in the timeline, newest first.
    ?assertMatch(
        [
            {message, UserName3, "Hello I'm C!", _TimeC1},
            {message, UserName2, "How is everyone?", _TimeB2},
            {message, UserName2, "Hello I'm B!", _TimeB1}
        ],
        server:get_timeline(Server1, UserName1)
    ),

    % User 2 does not follow any so gets an empty timeline.
    ?assertMatch([], server:get_timeline(Server1, UserName2)).

% Test getting the profile.
get_profile_test() ->
    {UserName1, Server1, [UserName2 | _]} = send_message_test(),
    % Most recent message is returned first.
    ?assertMatch(
        [
            {message, UserName1, "How is everyone?", _TimeA2},
            {message, UserName1, "Hello!", _TimeA1}
        ],
        server:get_profile(Server1, UserName1)
    ),
    % User 2 hasn't sent any messages.
    ?assertMatch([], server:get_profile(Server1, UserName2)).

% A "typical" session.
typical_session_test() ->
    initialize_test(),
    Session1 = spawn_link(?MODULE, typical_session_1, [self()]),
    Session2 = spawn_link(?MODULE, typical_session_2, [self()]),
    receive
        {Session1, ok} ->
            receive
                {Session2, ok} ->
                    done
            end
    end.

typical_session_1(TesterPid) ->
    {_, user_registered} = server:register_user(server_actor, "Alice@vub.be"),
    {Server, logged_in} = server:log_in(server_actor, "Alice@vub.be"),
    message_sent = server:send_message(Server, "Alice@vub.be", "Hello!"),
    message_sent = server:send_message(Server, "Alice@vub.be", "How is everyone?"),
    % Check own profile
    [
        {message, "Alice@vub.be", "How is everyone?", Time2},
        {message, "Alice@vub.be", "Hello!", Time1}
    ] =
        server:get_profile(Server, "Alice@vub.be"),
    ?assert(Time1 =< Time2),
    TesterPid ! {self(), ok}.

typical_session_2(TesterPid) ->
    {_, user_registered} = server:register_user(server_actor, "Bob@vub.be"),
    {Server, logged_in} = server:log_in(server_actor, "Bob@vub.be"),

    % Sleep one second, while Alice sends messages.
    timer:sleep(1000),

    [] = server:get_timeline(Server, "Bob@vub.be"),
    followed = server:follow(Server, "Bob@vub.be", "Alice@vub.be"),
    [
        {message, "Alice@vub.be", "How is everyone?", Time2},
        {message, "Alice@vub.be", "Hello!", Time1}
    ] =
        server:get_timeline(Server, "Bob@vub.be"),
    ?assert(Time1 =< Time2),

    TesterPid ! {self(), ok}.
