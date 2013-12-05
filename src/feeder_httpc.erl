
%% feeder_httpc - stream parse document at URL

-module(feeder_httpc).
-export([request/2]).

-include("../include/feeder.hrl").

-record(state, {
    from,
    reqId,
    httpcPid,
    started=false
  }).

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
    3000 ->
      {error, timeout}
  end.

parser_opts(State) ->
  [{event_state, State}, {event_fun, fun event_fun/2},
    {continuation_state, State}, {continuation_fun, fun resume/1}].

resume(State) ->
  RequestId = State#state.reqId,
  Pid = State#state.httpcPid,
  Started = State#state.started,
  httpc:stream_next(Pid),
  receive
    {http, {RequestId, stream, Chunk}} ->
      if
        not Started ->
          NState = State#state{started=true},
          feeder_parser:stream(Chunk, parser_opts(NState));
        true ->
          {Chunk, State}
      end;
    {http, {error, Reason}} ->
      {error, Reason};
    {http, {RequestId, stream_end, _Headers}} ->
      {<<>>, State}
  end.

from(State) ->
  State#state.from.

reqId(State) ->
  State#state.reqId.

event_fun({entry, Entry}, State) ->
  from(State) ! {feeder, {reqId(State), entry, Entry}},
  State;
event_fun({feed, Feed}, State) ->
  from(State) ! {feeder, {reqId(State), feed, Feed}},
  State;
event_fun(endFeed, State) ->
  from(State) ! {feeder, {reqId(State), stream_end}},
  State.
