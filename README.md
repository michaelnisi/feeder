# feeder - parse feeds (well, eventually)

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.png)](http://travis-ci.org/michaelnisi/feeder)

Word on the street has it that Erlang is terrible at parsing strings. A fair reason for me to write an XML parser for RSS and Atom feeds in it. Let's see how this goes.

## Usage

### HTTP

```Erlang
-module(example).

-export([start/0, request/1]).

start() ->
  inets:start(),
  feeder_httpc:start_link().

request(Url) ->
  feeder_httpc:request(Url),
  loop().

loop() ->
  receive
    {feed, Feed} ->
      io:format("feed: ~p~n", [Feed]),
      loop();
    {entry, Entry} ->
      io:format("entry: ~p~n", [Entry]),
      loop();
    endFeed ->
      io:format("done~n")
  end.
```

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
