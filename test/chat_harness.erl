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

-module(chat_harness).
-behaviour(gen_server).

-export([join/2, part/2, send/2, rooms/1, recv/2, flush/1]).

-export([start/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec join(pid(), binary()) -> ok.
join(User, Room) ->
  gen_server:call(User, {join, Room}).

-spec part(pid(), binary()) -> ok.
part(User, Room) ->
  gen_server:call(User, {part, Room}).

-spec send(pid(), iodata()) -> ok | error.
send(User, Data) ->
  gen_server:call(User, {send, Data}).

-spec rooms(pid()) -> [binary()].
rooms(User) ->
  gen_server:call(User, rooms).

-spec recv(pid(), atom()) -> [tuple()].
recv(User, Type) ->
  gen_server:call(User, {recv, Type}).

-spec flush(pid()) -> ok.
flush(User) ->
  gen_server:call(User, flush).

-spec start({binary(), binary()}) -> {ok, pid()} | {error, term()}.
start({Nickname, Password}) ->
  gen_server:start(?MODULE, {Nickname, Password}, []).

-spec init({binary(), binary()}) -> {ok, pid()}.
init({Nickname, Password}) ->
  {ok, Me} = chat_core:log_on(Nickname, Password),
  {ok, {Me, queue:new()}}.

-spec handle_call(term(), term(), pid()) -> {reply, term(), pid()}.
handle_call({join, Room}, _From, {Me, _Q} = S) -> {reply, chat_core:join(Me, Room), S};
handle_call({part, Room}, _From, {Me, _Q} = S) -> {reply, chat_core:part(Me, Room), S};
handle_call({send, Data}, _From, {Me, _Q} = S) -> {reply, chat_core:send(Me, Data), S};
handle_call(rooms, _From, {Me, _Q} = S) -> {reply, chat_core:rooms(Me), S};
handle_call({recv, Type}, _From, {_Me, Q} = S) -> {reply, lists:filter(filter_type(Type), queue:to_list(Q)), S};
handle_call(flush, _From, {Me, _Q}) -> {reply, ok, {Me, queue:new()}};
handle_call(Reqest, _From, State) ->
  lager:error("Invalid request to gen_server ~w: ~p", [?MODULE, Reqest]),
  {reply, {error, unexpected_call}, State}.

-spec handle_cast(term(), pid()) -> {noreply, pid()}.
handle_cast(Reqest,  State) ->
  lager:error("Invalid cast to gen_server ~w: ~p", [?MODULE, Reqest]),
  {noreply, State}.

-spec handle_info(term(), pid()) -> {noreply, pid()}.
handle_info({_Event, _Room, _What} = E, {Me, Q}) ->
  Q2 = queue:in(E, Q),
  {noreply, {Me, Q2}};
handle_info(Info,  State) ->
  lager:error("Unexpected message to gen_server ~w: ~p", [?MODULE, Info]),
  {noreply, State}.

-spec terminate(term(), pid()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), pid(), term()) -> {ok, pid()}.
code_change(_Old, State, _Extra) ->
  {ok, State}.

-spec filter_type(atom()) -> function().
filter_type(Type) ->
  fun
    ({T, _R, _W}) when T =:= Type -> true;
    (_E) -> false
  end.
