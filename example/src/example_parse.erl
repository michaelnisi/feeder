
%% example_parse - stream parse feed over HTTP

-module(example_parse).

-export([start_link/1]).
-export([resume/1]).

-behaviour(gen_statem).

-export([terminate/3]).
-export([code_change/4]).
-export([init/1]).
-export([callback_mode/0]).

-export([ready/3]).
-export([request/1]).

-record(state, {
  entries=[],
  feed,
  httpcPid,
  reqId,
  url
}).

-define(TIMEOUT, 5000).

%% API

resume(FsmRef) ->
  gen_statem:call(FsmRef, executing, ?TIMEOUT).

start_link(Url) ->
  gen_statem:start_link(?MODULE, Url, []).

%% Mandatory callback functions

terminate(_Reason, _StateName, #state{reqId=ReqId}) ->
  httpc:cancel_request(ReqId);
terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

init(Url) ->
  {ok, ready, #state{url=Url}}.

callback_mode() -> state_functions.

%% Internals

event_fun({entry, Entry}, State) ->
  State#state{entries=[Entry|State#state.entries]};
event_fun({feed, Feed}, State) ->
  State#state{feed=Feed};
event_fun(endFeed, State) ->
  State.

parser_opts(State) ->
  [{event_state, State}, {event_fun, fun event_fun/2},
    {continuation_state, State}, {continuation_fun, fun stream/1}].

stream(State=#state{reqId=ReqId, httpcPid=Pid}) ->
  httpc:stream_next(Pid),
  receive
    {http, {ReqId, stream, Chunk}} ->
      {Chunk, State};
    {http, {error, Reason}} ->
      {error, Reason};
    {http, {ReqId, stream_end, _Headers}} ->
      {<<>>, State}
  end.

opts(http) -> [
  {autoredirect, true}];
opts(req) -> [
  {body_format, binary},
  {stream, {self, once}},
  {sync, false}].

request(State=#state{url=Url}) ->
  {ok, ReqId} = httpc:request(get, {Url, []}, opts(http), opts(req)),
  receive
    {http, {ReqId, stream_start, _Headers, Pid}} ->
      feeder:stream(<<>>, parser_opts(State#state{reqId=ReqId, httpcPid=Pid}));
    {http, {error, Reason}} ->
      {error, Reason}
  after
    ?TIMEOUT ->
      {error, timeout}
  end.

result(From, {ok, State, _Rest}) ->
  Entries = lists:reverse(State#state.entries),
  {stop_and_reply, normal, [{reply, From, {ok, State#state.feed, Entries}}]};
result(_From, {error, Reason}) ->
  {stop, {error, Reason}};
result(_From, {fatal_error, _, Reason,_ ,_State}) ->
  {stop, {error, Reason}}.

%% State callbacks

ready({call, From}, executing, State) ->
  R = request(State),
  result(From, R).
