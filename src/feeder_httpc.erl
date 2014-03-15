
%% feeder_httpc - stream parse document from URL

-module(feeder_httpc).
-export([request/2]).

-include("../include/feeder.hrl").

-record(state, {
    from,
    reqId,
    httpcPid,
    started=false
  }).

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

resume(State) ->
  httpc:stream_next(Pid),
  receive
    {http, {State#state.reqId, stream, Chunk}} ->
      if
        not State#state.started ->
          NState = State#state{started=true},
          feeder_parser:stream(Chunk, parser_opts(NState));
        true ->
          {Chunk, State}
      end;
    {http, {error, Reason}} ->
      {error, Reason};
    {http, {State#state.reqId, stream_end, _Headers}} ->
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
    3000 ->
      {error, timeout}
  end.
