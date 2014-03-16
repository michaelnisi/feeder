# feeder - parse RSS/Atom

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.png)](http://travis-ci.org/michaelnisi/feeder)

Feeder is an Erlang library to parse RSS and Atom formatted XML feeds. It is a stream based parser that sends the events through a callback interface.

## Usage

### Stream parse document from URL

```Erlang
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

parse(Chunk, State) when State#state.started ->
  {Chunk, State};
parse(Chunk, State) ->
  feeder_parser:stream(Chunk, parser_opts(State#state{started=true})).

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
    3000 ->
      {error, timeout}
  end.
```
You can try this in the shell with the feeder module:

```
make
erl -pa ebin
```
```Erlang
feeder:start().
feeder:request("http://5by5.tv/rss", self()).
flush().
```

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
