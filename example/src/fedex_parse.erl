%% example_parse - stream parse feed over HTTP

-module(fedex_parse).

-export([start_link/1]).
-export([resume/1]).

-behaviour(gen_statem).

-export([terminate/3]).
-export([code_change/4]).
-export([init/1]).
-export([callback_mode/0]).

-export([ready/3]).
-export([executing/3]).

-record(state, {
  httpcPid,
  reqId,
  url
}).

-define(TIMEOUT, 5000).

%% API

resume(FsmRef) ->
  gen_statem:cast(FsmRef, request).

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

event_fun({entry, Entry}, State=#state{url=Url}) ->
  gen_event:notify(fedex_event_man, {entry, Entry, Url}),
  State;
event_fun({feed, Feed}, State) ->
  gen_event:notify(fedex_event_man, {feed, Feed}),
  State;
event_fun(endFeed, State) ->
  State.

parser_opts(State) ->
  [{event_state, State}, {event_fun, fun event_fun/2},
    {continuation_state, State}, {continuation_fun, fun stream/1}].

opts(http) -> [
  {autoredirect, true}];
opts(req) -> [
  {body_format, binary},
  {stream, {self, once}},
  {sync, false}].

%% State callbacks

ready(cast, request, Data=#state{url=Url}) ->
  {ok, ReqId} = httpc:request(get, {Url, []}, opts(http), opts(req)),
  NewData = Data#state{reqId=ReqId},
  {next_state, executing, NewData}.

executing(info, {http, {ReqId, stream_start, _Headers, Pid}}, Data) ->
  ReqId = Data#state.reqId,
  NewData = Data#state{httpcPid=Pid},
  {ok, _EventState, _Rest} = feeder:stream(<<>>, parser_opts(NewData)),
  {stop, normal}.
