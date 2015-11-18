%% Copyright (c) 2015, Adam Cammack <adam@acammack.com>
%% 
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
%% OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(chat_core).

-export([log_on/2, join/2, part/2, send/2, rooms/1]).

-spec log_on(binary(), binary()) -> {ok, pid()} | {error, badauth}.
log_on(Nickname, Password) ->
  chat_core_user:log_on(Nickname, Password, []).

-spec join(pid(), binary()) -> ok.
join(User, Room) ->
  chat_core_user:join(User, Room).

-spec part(pid(), binary()) -> ok.
part(User, Room) ->
  chat_core_user:part(User, Room).

-spec send(pid(), iodata()) -> ok | error.
send(User, Data) ->
  chat_core_user:send(User, Data).

-spec rooms(pid()) -> [binary()].
rooms(User) ->
  chat_core_user:rooms(User).

