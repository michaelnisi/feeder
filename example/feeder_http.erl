-module(feeder_http).
-export([url/1]).

req_opts() ->
  [{sync, false}, {stream, {self, once}}, {body_format, binary}].

url(Url) ->
  httpc:request(get, {Url, []}, [], req_opts()),
  receive
    {http, {_RequestId, stream_start, _Headers, Pid}} ->
      resume(Pid)
  after
    3000 ->
      {error, timeout}
  end,
  ok.

resume(Pid) ->
  httpc:stream_next(Pid),
  receive
    {http, {_RequestId, stream, BinBodyPart}} ->
      feeder:stream(BinBodyPart, feeder_opts(Pid)),
      {BinBodyPart, Pid};
    {http, {_RequestId, stream_end, _Headers}} ->
      {<<"">>, Pid}
  end.

feeder_opts(Pid) ->
  [{event_state, {}}, {event_fun, fun event/2},
   {continuation_state, Pid}, {continuation_fun, fun resume/1}].

event({entry, Entry}, S) ->
  S;
event({feed, _Feed}, S) ->
  S;
event(endFeed, S) ->
  S;
event(_, S) ->
  S.
