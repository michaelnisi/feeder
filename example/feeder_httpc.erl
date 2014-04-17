
%% feeder_httpc - stream parse document from URL

%% This is meant as an example. Don't call this from your code, but
%% implement it in your own process. Using this directly adds a level of
%% indirection and unnecessary message passing.

-module(feeder_httpc).
-export([request/2]).

-include("../include/feeder.hrl").

-record(state, {
    from,
    reqId,
    httpcPid,
    started=false
  }).

-define(TIMEOUT, 3000).

event_fun({entry, Entry}, State) ->
  State#state.from ! {feeder, {State#state.reqId, entry, Entry}},
  State;
event_fun({feed, Feed}, State) ->
  State#state.from ! {feeder, {State#state.reqId, feed, Feed}},
  State;
event_fun(endFeed, State) ->
  State#state.from ! {feeder, {State#state.reqId, stream_end}},
  State.

parser_opts(State) ->
  [{event_state, State}, {event_fun, fun event_fun/2},
    {continuation_state, State}, {continuation_fun, fun resume/1}].

parse(Chunk, State) when State#state.started ->
  {Chunk, State};
parse(Chunk, State) ->
  feeder:stream(Chunk, parser_opts(State#state{started=true})).

resume(State=#state{reqId=ReqId, httpcPid=Pid}) ->
  httpc:stream_next(Pid),
  receive
    {http, {ReqId, stream, Chunk}} ->
      parse(Chunk, State);
    {http, {error, Reason}} ->
      {error, Reason};
    {http, {ReqId, stream_end, _Headers}} ->
      {<<>>, State}
  end.

req_opts() ->
  [{sync, false}, {stream, {self, once}}, {body_format, binary}].

request(Url, From) ->
  {ok, ReqId} = httpc:request(get, {Url, []}, [], req_opts()),
  receive
    {http, {ReqId, stream_start, _Headers, Pid}} ->
      State = #state{from=From, reqId=ReqId, httpcPid=Pid},
      resume(State);
    {http, {error, Reason}} ->
      {error, Reason}
  after
    ?TIMEOUT ->
      {error, timeout}
  end.
