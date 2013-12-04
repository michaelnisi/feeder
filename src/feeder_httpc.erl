
%% feeder_httpc - stream parse document at URL

-module(feeder_httpc).
-export([request/2]).

-include("../include/feeder.hrl").

-record(state, {from, reqId, httpcPid, started=false}).

req_opts() ->
  [{sync, false}, {stream, {self, once}}, {body_format, binary}].

request(Url, From) ->
  {ok, ReqId} = httpc:request(get, {Url, []}, [], req_opts()),
  receive
    {http, {ReqId, stream_start, _Headers, Pid}} ->
      State = #state{from=From, reqId=ReqId, httpcPid=Pid},
      resume(State)
    % TODO: {http, {error, etc.
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
    {http, {RequestId, stream_end, _Headers}} ->
      {<<>>, State}
  end.

event_fun({entry, Entry}, State) ->
  From = State#state.from,
  ReqId = State#state.reqId,
  From ! {feeder, {ReqId, entry, Entry}},
  State;
event_fun({feed, Feed}, State) ->
  From = State#state.from,
  ReqId = State#state.reqId,
  From ! {feeder, {ReqId, feed, Feed}},
  State;
event_fun(endFeed, State) ->
  From = State#state.from,
  ReqId = State#state.reqId,
  From ! {feeder, {ReqId, stream_end}},
  State.
