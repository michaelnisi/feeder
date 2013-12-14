# feeder - parse RSS/Atom

[![Build Status](https://secure.travis-ci.org/michaelnisi/feeder.png)](http://travis-ci.org/michaelnisi/feeder)

Feeder is an Erlang library to parse RSS and Atom formatted XML feeds. It is a stream based parser that sends the events through a callback interface.

## Usage

```Erlang
feeder:start().
feeder:url("http://5by5.tv/rss", self()).
```
## License

[MIT License](https://raw.github.com/michaelnisi/feeder/master/LICENSE)
