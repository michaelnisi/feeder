# feeder - parse RSS and Atom

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.svg)](http://travis-ci.org/michaelnisi/feeder)

The Feeder [Erlang](http://www.erlang.org/) module parses RSS and Atom formatted XML feeds. It is a stream based parser that sends its events through a callback interface.

## types

### option()

Options used to customize the behaviour of the parser. Possible options are:

`{continuation_fun, ContinuationFun}`
[`ContinuationFun`](http://www.erlang.org/doc/man/xmerl_sax_parser.html#ContinuationFun-1) is a call back function to decide what to do if the parser runs into EOF before the document is complete.

`{continuation_state, term()}`
State that is accessible in the continuation call back function.

`{event_fun, EventFun}`
[`EventFun`](http://www.erlang.org/doc/man/xmerl_sax_parser.html#EventFun-3) is the call back function for parser events.

`{event_state, term()}`
State that is accessible in the event call back function.

## exports

### file(Filename, Opts) -> Result

- `Filename = string()`
- `Opts = [option()]`

### stream(Xml, Opts) -> Result

- `Xml = unicode_binary() | latin1_binary() | [unicode_char()]`
- `Opts = [option()]`

## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
