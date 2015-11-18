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

-module(chat_core_room).

-export([join/3, part/3, send/3]).

-compile([{parse_transform, lager_transform}]).

-spec join(pid(), binary(), binary()) -> ok.
join(User, Nickname, Room) ->
  gproc:reg({r, l, Room}, User),
  lager:info("Joins ~s: ~s", [Room, Nickname]),
  do_send(join, Room, Nickname).

-spec part(pid(), binary(), binary()) -> ok.
part(_User, Nickname, Room) ->
  % Gproc uses the PID of the calling process implicitly
  gproc:unreg({r, l, Room}),
  lager:info("Departs from ~s: ~s", [Room, Nickname]),
  gproc:send({n, l, Nickname}, {part, Room, Nickname}), % Ensure that all the connections know we left
  do_send(part, Room, Nickname).

-spec send(binary(), binary(), iodata()) -> ok.
send(Nickname, Room, Data) ->
  lager:info("~w bytes sent to ~s by ~s", [iolist_size(Data), Room, Nickname]),
  lager:debug("Sends ~s to ~s: ~p", [Nickname, Room, Data]),
  do_send(chat, Room, {Nickname, Data}).

-spec do_send(chat | join | part, binary(), binary() | {binary(), iodata()}) -> ok.
do_send(Event, Room, What) ->
  Msg = {Event, Room, What},
  Msg = gproc:send({r, l, Room}, Msg),
  ok.
