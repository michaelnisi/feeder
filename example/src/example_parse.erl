
%% example_parse - stream parse feed over HTTP

-module(example_parse).

-export([start_link/1]).
-export([resume/1]).

-behaviour(gen_fsm).

-export([code_change/4]).
-export([handle_event/3]).
-export([handle_info/3]).
-export([handle_sync_event/4]).
-export([init/1]).
-export([ready/3]).
-export([request/1]).
-export([terminate/3]).

-record(state, {
  entries=[],
  feed,
  httpcPid,
  parsing=false,
  reqId,
  url
}).

init(Url) ->
  {ok, ready, #state{url=Url}}.

%% Create gen_fsm as part of a supervision tree.
start_link(Url) ->
  gen_fsm:start_link(?MODULE, Url, []).

%% Begin stream parsing over HTTP.
resume(FsmRef) ->
  gen_fsm:sync_send_event(FsmRef, executing).

opts(http) -> [
  {autoredirect, true}];
opts(req) -> [
  {body_format, binary},
  {stream, {self, once}},
  {sync, false}].

-define(TIMEOUT, 3000).

request(State=#state{url=Url}) ->
  {ok, ReqId} = httpc:request(get, {Url, []}, opts(http), opts(req)),
  receive
    {http, {ReqId, stream_start, _Headers, Pid}} ->
      stream(State#state{reqId=ReqId, httpcPid=Pid});
    {http, {error, Reason}} ->
      {error, Reason}
  after
    ?TIMEOUT ->
      {error, timeout}
  end.

result({ok, State, _Rest}) ->
  Entries = lists:reverse(State#state.entries),
  {stop, normal, {ok, State#state.feed, Entries}, State};
result({error, Reason}) ->
  {stop, error, Reason};
result({fatal_error, _, Reason,_ ,_State}) ->
  {stop, error, Reason}.

ready(executing, _, State) ->
  R = request(State),
  result(R).

terminate(_Reason, _StateName, #state{reqId=ReqId}) ->
  httpc:cancel_request(ReqId);
terminate(_Reason, _StateName, _StateData) ->
  ok.

event_fun({entry, Entry}, State) ->
  State#state{entries=[Entry|State#state.entries]};
event_fun({feed, Feed}, State) ->
  State#state{feed=Feed};
event_fun(endFeed, State) ->
  State.

parser_opts(State) ->
  [{event_state, State}, {event_fun, fun event_fun/2},
    {continuation_state, State}, {continuation_fun, fun stream/1}].

parse(Chunk, State) when State#state.parsing ->
  {Chunk, State};
parse(Chunk, State) ->
  feeder:stream(Chunk, parser_opts(State#state{parsing=true})).

stream(State=#state{reqId=ReqId, httpcPid=Pid}) ->
  httpc:stream_next(Pid),
  receive
    {http, {ReqId, stream, Chunk}} ->
      parse(Chunk, State);
    {http, {error, Reason}} ->
      {error, Reason};
    {http, {ReqId, stream_end, _Headers}} ->
      {<<>>, State}
  end.

handle_sync_event(_Event, _From, _StateName, StateData) ->
  {stop, error, StateData}.

handle_event(_Event, _StateName, StateData) ->
  {stop, error, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

handle_info({'EXIT', _Pid, _Reason}, _StateName, StateData) ->
  {stop, normal, StateData}.
