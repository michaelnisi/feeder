# feeder - parse RSS and Atom

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.png)](http://travis-ci.org/michaelnisi/feeder)

The Feeder [Erlang](http://www.erlang.org/) module parses RSS and Atom formatted XML feeds. It is a stream based parser that sends its events through a callback interface.

## types

### `option()`

## exports

### `file(Filename, Opts) -> Result`

- `Filename = string()`
- `Opts = [option()]`

### `stream(Xml, Opts) -> Result`

- `Xml = unicode_binary() | latin1_binary() | [unicode_char()]`
- `Opts = [option()]`

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
