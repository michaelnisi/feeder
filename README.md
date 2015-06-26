# feeder - parse RSS and Atom

The **feeder** [Erlang](http://www.erlang.org/) library parses RSS and Atom formatted XML feeds. It is a stream based parser that sends its events through a callback interface.

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.svg)](http://travis-ci.org/michaelnisi/feeder)

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
example:start().
example:print_titles("http://5by5.tv/rss").
```

## Types

### feed()

The `channel` or `feed` element of the feed.

```erlang
{feed, Author, Id, Image, Language, Link, Subtitle, Summary, Title, Updated}
```
- `Author = undefined | binary()`
- `Id = undefined | binary()`
- `Image = undefined | binary()`
- `Language = undefined | binary()`
- `Link = undefined | binary()`
- `Subtitle = undefined | binary()`
- `Summary = undefined | binary()`
- `Title = undefined | binary()`
- `Updated = undefined | binary()`

### enclosure()

The `enclosure` element of an `item` or `entry` of the feed.

```erlang
{enclosure, Url, Length, Type}
```

- `Url = undefined | binary()`
- `Length = undefined | binary()`
- `Type = undefined | binary()`

### entry()

An `item` or `entry` element of the feed.

```erlang
{entry, Author, Duration, Enclosure, Id, Image, Link, Subtitle, Summary, Title, Updated}
```

- `Author = undefined | binary()`
- `Duration = undefined | binary()`
- `Enclosure = undefined | enclosure()`
- `Id = undefined | binary()`
- `Image = undefined | binary()`
- `Link = undefined | binary()`
- `Subtitle = undefined | binary()`
- `Summary = undefined | binary()`
- `Title = undefined | binary()`
- `Updated = undefined | binary()`

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

### file(Filename, Opts) -> Result

- `Filename = string()`
- `Opts = [option()]`

### stream(Xml, Opts) -> Result

- `Xml = unicode_binary() | latin1_binary() | [unicode_char()]`
- `Opts = [option()]`

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
