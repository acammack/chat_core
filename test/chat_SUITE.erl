-module(chat_SUITE).

%%% For common_test
-export([all/0, groups/0, init_per_suite/1, init_per_group/2, end_per_group/2]).

%%% Tests
-export([room_broadcast/1]).

%%% Util
-export([chat/1]).

all() ->
  %[{group, joining}, {group, one_room_chat}].
  [{group, one_room_chat}].

groups() ->
  [{one_room_chat, [], [room_broadcast]}].

init_per_suite(Config) ->
  application:ensure_all_started(chat_core),
  lager:set_loglevel(lager_console_backend, error),
  Users = [ spawn(?MODULE, chat, [User]) || User <- chat_users() ],
  [{users, Users} | Config].

init_per_group(one_room_chat, Config) ->
  Room = <<"room1">>,
  [ chat_join(U, Room) || U <- keyfind(users, Config) ],
  [{room, Room} | Config].

end_per_group(one_room_chat, Config) ->
  [ begin chat_part(U, <<"room1">>), chat_flush(U) end || U <- keyfind(users, Config) ],
  Config.

room_broadcast(Config) ->
  Users = [A | _T] = keyfind(users, Config),
  Room = keyfind(room, Config),
  ok = chat_send(A, <<"Hello, world!">>),
  [ {chat, Room, {<<"alice">>, <<"Hello, world!">>}}
    = chat_receive(U, chat) || U <- Users ].

%%% Functions for using our fake users

chat_join(U, Room) ->
  chat_call(U, {do_join, Room}).

chat_part(U, Room) ->
  chat_call(U, {do_part, Room}).

chat_send(U, Data) ->
  chat_call(U, {do_send, Data}).

chat_receive(U, Event) ->
  chat_call(U, {do_receive, Event}).

chat_flush(U) ->
  chat_call(U, flush).

chat_call(Pid, Data) ->
  Ref = make_ref(),
  Pid ! {Data, Ref, self()},
  receive
    {Ref, What} -> What
  after 1000 ->
    call_timeout
  end.

%%% Functions for our fake users (maybe turn them into gen_servers?)

chat({Nick, Password}) ->
  {ok, Me} = chat_core:log_on(Nick, Password),
  chat_loop(Me).

chat_loop(Me) ->
  receive
    {{do_join, Room}, Ref, Q} -> Q ! {Ref, chat_core:join(Me, Room)}, chat_loop(Me);
    {{do_part, Room}, Ref, Q} -> Q ! {Ref, chat_core:part(Me, Room)}, chat_loop(Me);
    {{do_send, Data}, Ref, Q} -> Q ! {Ref, chat_core:send(Me, Data)}, chat_loop(Me);
    {do_rooms, Ref, Q} -> Q ! {Ref, chat_corer:rooms(Me)}, chat_loop(Me);
    {flush, Ref, Q} -> Q ! {Ref, chat_flush()}, chat_loop(Me);
    {{do_receive, Event}, Ref, Q} ->
      receive
        {Event, Room, What} ->
          Q ! {Ref, {Event, Room, What}}
      after 500 ->
          Q ! {Ref, chat_timeout}
      end,
      chat_loop(Me)
  end.

chat_flush() ->
  receive
    _Msg ->
      chat_flush()
  after 0 ->
      ok
  end.

chat_users() ->
  [{<<"alice">>, <<"abcd">>},
    {<<"bob">>, <<"efgh">>},
    {<<"carol">>, <<"ijkl">>},
    {<<"dave">>, <<"mnop">>}].

%%% Util functions

keyfind(Key, List) ->
  {Key, Val} = lists:keyfind(Key, 1, List),
  Val.
