% Copyright (c) 2015, Adam Cammack <adam@acammack.com>
% 
% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
% 
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
% PERFORMANCE OF THIS SOFTWARE.

-module(chat_SUITE).

%%% For common_test
-export([all/0, groups/0, init_per_suite/1, init_per_group/2, end_per_group/2]).

%%% Tests
-export([room_broadcast/1]).

all() ->
  %[{group, joining}, {group, one_room_chat}].
  [{group, one_room_chat}].

groups() ->
  [{one_room_chat, [], [room_broadcast]}].

init_per_suite(Config) ->
  application:ensure_all_started(chat_core),
  lager:set_loglevel(lager_console_backend, error),
  Users = [ begin {ok, U} = chat_harness:start(User), U end || User <- chat_users() ],
  [{users, Users} | Config].

init_per_group(one_room_chat, Config) ->
  Room = <<"room1">>,
  [ chat_harness:join(U, Room) || U <- keyfind(users, Config) ],
  [{room, Room} | Config].

end_per_group(one_room_chat, Config) ->
  [ begin chat_harness:part(U, <<"room1">>), chat_harness:flush(U) end || U <- keyfind(users, Config) ],
  Config.

%%% Tests

room_broadcast(Config) ->
  % All users in a room should receive messages sent to the room
  Users = [A | _T] = keyfind(users, Config),
  Room = keyfind(room, Config),
  ok = chat_harness:send(A, <<"Hello, world!">>),
  [ {chat, Room, {<<"alice">>, <<"Hello, world!">>}}
    = hd(chat_harness:recv(U, chat)) || U <- Users ].

%%% Functions for using our fake users

chat_users() ->
  [{<<"alice">>, <<"abcd">>},
    {<<"bob">>, <<"efgh">>},
    {<<"carol">>, <<"ijkl">>},
    {<<"dave">>, <<"mnop">>}].

%%% Util functions

keyfind(Key, List) ->
  {Key, Val} = lists:keyfind(Key, 1, List),
  Val.
