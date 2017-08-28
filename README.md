[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.svg)](http://travis-ci.org/michaelnisi/feeder)

# feeder - parse RSS and Atom

The **feeder** [Erlang](http://www.erlang.org/) library parses RSS and Atom formatted XML feeds. It is a stream based parser that sends its events through a callback interface.

## Usage

Parse a file and accumulate parser events:

```erlang
-module(acc).
-export([file/1]).

event({entry, Entry}, {Feed, Entries}) ->
  {Feed, [Entry|Entries]};
event({feed, Feed}, {[], Entries}) ->
  {Feed, Entries};
event(endFeed, {Feed, Entries}) ->
  {Feed, lists:reverse(Entries)}.

opts() ->
  [{event_state, {[],[]}}, {event_fun, fun event/2}].

file(Filename) ->
  {ok, EventState, _Rest} = feeder:file(Filename, opts()),
  EventState.
```

## Example

To try HTTP streaming do:

```
cd example
make
erl -pa ebin deps/*/ebin
```

```erlang
fedex:start().
fedex:fetch("http://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml").
```

## Types

### feed()

The `channel` or `feed` tuple.

### enclosure()

The `enclosure` tuple for an `item` or `entry`.

### entry()

An `item` or `entry` tuple.

### option()

Options to setup the parser.

`{continuation_fun, ContinuationFun}`
[`ContinuationFun`](http://www.erlang.org/doc/man/xmerl_sax_parser.html#ContinuationFun-1) is a call back function to decide what to do if the parser runs into EOF before the document is complete.

`{continuation_state, term()}`
State that is accessible in the continuation call back function.

`{event_fun, EventFun}`
[`EventFun`](http://www.erlang.org/doc/man/xmerl_sax_parser.html#EventFun-3) is the call back function for parser events.

`{event_state, term()}`
State that is accessible in the event call back function.

### event()

The events that are sent to the user via the callback.

```erlang
{feed, Feed}
```

- `Feed = feed()`

Receive notification when the meta information of the feed or channel has been parsed.

```erlang
{entry, Entry}
```

- `Entry = entry()`

Receive notification for each entry or article in the feed.

```erlang
endFeed
```

Receive notification of the end of a document. **feeder** will send this event only once, and it will be the last event during the parse.

## Exports

### Parsing feeds

```erlang
feeder:file(Filename, Opts) -> Result
```
- `Filename = string()`
- `Opts = [option()]`
- `Result = {ok, EventState, Rest}`
- `Rest = unicode_binary() | latin1_binary()`
- `EventState = term()`

```erlang
feeder:stream(Xml, Opts) -> Result
```
- `Xml = unicode_binary() | latin1_binary() | [unicode_char()]`
- `Opts = [option()]`
- `Result = {ok, EventState, Rest}`
- `Rest = unicode_binary() | latin1_binary()`
- `EventState = term()`

### Accessing values

```erlang
feeder_feeds:get(Key, Feed) -> Value
```
- `Key = author | id | image | language | link | subtitle | summary | title | updated | url`
- `Feed = feed()`
- `Value = binary() | undefined`

```erlang
feeder_enclosures:get(Key, Enclosure) -> Value
```
- `Key = url | length | type`
- `Enclosure = enclosure()`
- `Value = binary() | undefined`

```erlang
feeder_entries:get(Key, Entry) -> Value
```
- `Key = author | categories | duration | enclosure | id | image | link | subtitle | summary | title | updated`
- `Entry = entry()`
- `Value = binary() | enclosure() | undefined`

## Tests

```bash
$ make tests
```

## Install

You can install **feeder** with [Rebar3](https://github.com/erlang/rebar3), of course, and there are [hex](https://hex.pm/packages/feeder) and [erlang.mk](https://github.com/ninenines/erlang.mk/tree/master/index) packages.

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
