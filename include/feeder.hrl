
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(feed, {
    author :: binary(),
    id :: binary(),
    image :: binary(),
    link :: binary(),
    subtitle :: binary(),
    summary :: binary(),
    title :: binary(),
    updated :: integer()
  }).

-record(enclosure, {
    url :: binary(),
    length :: binary(),
    type :: binary()
  }).

-record(entry, {
    author :: binary(),
    enclosure :: #enclosure{},
    id :: binary(),
    image :: binary(),
    link :: binary(),
    subtitle :: binary(),
    summary :: binary(),
    title :: binary(),
    updated :: integer()
  }).
