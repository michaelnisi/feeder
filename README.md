# feeder - parse RSS/Atom

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.png)](http://travis-ci.org/michaelnisi/feeder)

Word on the street has it that Erlang is terrible at parsing strings. While I'm learning the language—why not write an XML parser for RSS and Atom feeds.

## Usage

```Erlang
feeder:start().
```

```Erlang
feeder:file(Filename, Opts) ->
```

```Erlang
feeder:stream(Xml, Opts) ->
```

```Erlang
feeder:url("http://5by5.tv/rss", self()).
```

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
