# feeder - parse RSS and Atom

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.svg)](http://travis-ci.org/michaelnisi/feeder)

The **feeder** [Erlang](http://www.erlang.org/) module parses RSS and Atom formatted XML feeds. It is a stream based parser that sends its events through a callback interface. The parser implements an opinionated (incomplete) data transformation, but should be easy to extend (PRs welcome).

## Usage

Parse a file and accumulate parser events:

```erlang
-module(acc).
-export([file/1]).

event({entry, Entry}, {Feed, Entries}) ->
  {Feed, [Entry|Entries]};
event({feed, Feed}, {[], Entries}) ->
  {Feed, Entries};
event(endFeed, S) ->
  S.

opts() ->
  [{event_state, {[],[]}}, {event_fun, fun event/2}].

file(Filename) ->
  {ok, EventState, _Rest} = feeder:file(Filename, opts()),
  EventState.
```

## Example

To run a simple example application do:

```
cd example
make
erl -pa ebin deps/*/ebin
```

```erlang
example:start().
example:print_titles("http://5by5.tv/rss").
```

## types

### feed()

Feed is a [Map](http://www.erlang.org/doc/man/maps.html) with following keys and value types:

```erlang
author :: nil | binary()
id :: nil | binary()
image :: nil | binary()
language :: nil | binary()
link :: nil | binary()
subtitle :: nil | binary()
summary :: nil | binary()
title :: nil | binary()
updated :: nil | binary()
```

### enclosure()

Enclosure is a [Map](http://www.erlang.org/doc/man/maps.html) with following keys and value types:

```erlang
url :: nil | binary()
length :: nil | binary()
type :: nil | binary()
```

### entry()

Entry is a [Map](http://www.erlang.org/doc/man/maps.html) with following keys and value types:

```erlang
author :: nil | binary()
duration :: nil | binary()
enclosure :: nil | enclosure
id :: nil | binary()
image :: nil | binary()
link :: nil | binary()
subtitle :: nil | binary()
summary :: nil | binary()
title :: nil | binary()
updated :: nil | binary()
```

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

`{feed, Feed}`

- `Feed = feed()`

Receive notification when the meta information of the feed or channel has been parsed.

`{entry, Entry}`

- `Entry = entry()`

Receive notification for each entry or article in the feed.

`endFeed`

Receive notification of the end of a document. **Feeder** will send this event only once, and it will be the last event during the parse.

## exports

### file(Filename, Opts) -> Result

- `Filename = string()`
- `Opts = [option()]`

### stream(Xml, Opts) -> Result

- `Xml = unicode_binary() | latin1_binary() | [unicode_char()]`
- `Opts = [option()]`

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
