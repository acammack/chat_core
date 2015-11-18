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

-module(chat_core_user).
-behaviour(gen_server).

-export([log_on/3, join/2, part/2, send/2, rooms/1]).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  nick = <<>> :: binary(),
  password_salt = <<>> :: binary(),
  password_hash = <<>> :: binary(),
  rooms = [] :: [binary()]
}).

-compile([{parse_transform, lager_transform}]).

-spec log_on(binary(), binary(), pid()) -> {ok, pid()} | {error, badauth}.
log_on(Nickname, Password, _Connection) ->
  case auth(Nickname, Password) of
    {ok, User} ->
      gproc:reg({r, l, Nickname}),
      %ok = gen_server:call(User, {connect, Connection}),
      lager:info("Nick ~s connected to process ~w", [Nickname, self()]),
      {ok, User};
    Error ->
      lager:info("Failed login for ~s", [Nickname]),
      Error
  end.

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

-spec start_link(binary(), binary()) ->
  {ok, pid()} | {error, already_started, pid()} | {error, term()}.
start_link(Nickname, Password) ->
  gen_server:start_link({via, gproc, {n, l, Nickname}}, ?MODULE, {Nickname, Password}, []).

-spec init({binary(), binary()}) -> {ok, #state{}}.
init({Nickname, Password}) ->
  Salt = crypto:strong_rand_bytes(64),
  Hash = crypto:hash(sha512, [Salt, Password]),
  {ok, #state{nick = Nickname, password_salt = Salt, password_hash = Hash}}.

-spec handle_call(term(), term(), #state{}) -> {reply, term(), #state{}}.
handle_call({connect, _Pid}, _From, State) ->
  {reply, ok, State};
handle_call({join, Room}, _From, State = #state{rooms = Rooms}) ->
  R2 = case lists:member(Room, Rooms) of
    false ->
      chat_core_room:join(self(), State#state.nick, Room),
      Rooms;
    true ->
      lists:delete(Room, Rooms)
  end,
  {reply, ok, State#state{rooms = [Room | R2]}};
handle_call({part, <<>>}, _From, State = #state{rooms = [Room | Rooms]}) ->
  chat_core_room:part(self(), State#state.nick, Room),
  {reply, ok, State#state{rooms = Rooms}};
handle_call({part, Room}, _From, State = #state{rooms = Rooms}) ->
  case lists:member(Room, Rooms) of
    false -> ok;
    true ->
      chat_core_room:part(self(), State#state.nick, Room)
  end,
  R2 = lists:delete(Room, Rooms),
  {reply, ok, State#state{rooms = R2}};
handle_call({send, Data}, _From, State = #state{rooms = [Room | _]}) ->
  chat_core_room:send(State#state.nick, Room, Data),
  {reply, ok, State};
handle_call({send, _Data}, _From, State) ->
  {reply, error, State};
handle_call({auth, Password}, _From, State = #state{password_hash = Hash, password_salt = Salt}) ->
  Maybe_Hash = crypto:hash(sha512, [Salt, Password]),
  R = case constant_compare(Hash, Maybe_Hash) of
    equal -> ok;
    notequal -> badauth
  end,
  {reply, R, State};
handle_call(rooms, _From, State = #state{rooms = Rooms}) ->
  {reply, Rooms, State};
handle_call(Reqest, _From, State) ->
  lager:error("Invalid request to gen_server ~w: ~p", [?MODULE, Reqest]),
  {reply, {error, unexpected_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(Reqest,  State) ->
  lager:error("Invalid cast to gen_server ~w: ~p", [?MODULE, Reqest]),
  {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({Event, _Room, _What} = E,  State) when Event == chat; Event == join; Event == part ->
  gproc:send({r, l, State#state.nick}, E),
  {noreply, State};
handle_info(Info,  State) ->
  lager:error("Unexpected message to gen_server ~w: ~p", [?MODULE, Info]),
  {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_Old, State, _Extra) ->
  {ok, State}.

-spec auth(binary(), binary()) -> {ok, pid()} | {error, badauth}.
auth(Nickname, Password) ->
  case gproc:where({n, l, Nickname}) of
    undefined ->
      case supervisor:start_child(chat_core_users_sup, [Nickname, Password]) of
        {ok, Child} -> {ok, Child};
        _Error -> {error, badauth}
      end;
    Pid ->
      case gen_server:call(Pid, {auth, Password}) of
        ok -> {ok, Pid};
        badauth -> {error, badauth}
      end
  end.

-spec constant_compare(binary(), binary()) -> equal | notequal.
constant_compare(One, Two) ->
  constant_compare(One, Two, 0).

-spec constant_compare(binary(), binary(), integer()) -> equal | notequal.
constant_compare(<<>>, <<>>, 0) ->
  equal;
constant_compare(<<One, RestOne/binary>>, <<Two, RestTwo/binary>>, Acc) ->
  constant_compare(RestOne, RestTwo, (One bxor Two) bor Acc);
constant_compare(_One, _Two, _Acc) ->
  notequal.
