# feeder - parse feeds (well, eventually)

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.png)](http://travis-ci.org/michaelnisi/feeder)

Word on the street has it that Erlang is terrible at parsing strings. A fair reason for me to write an XML parser for RSS and Atom feeds in it. Let's see how this goes.

## Usage

```Erlang
{ok, EventState, Rest} = feeder:file(Filename, opts())
```

```Erlang
{ok, EventState, Rest} = feeder:stream(Chunk, opts())
```

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
