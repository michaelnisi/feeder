-record(enclosure, {
  url :: undefined | binary(),
  length :: undefined | binary(),
  type :: undefined | binary()
}).

-record(feed, {
  author :: undefined | binary(),
  id :: undefined | binary(),
  image :: undefined | binary(),
  language :: undefined | binary(),
  link :: undefined | binary(),
  subtitle :: undefined | binary(),
  summary :: undefined | binary(),
  title :: undefined | binary(),
  updated :: undefined | binary(),
  url :: undefined | binary()
}).

-record(entry, {
  author :: undefined | binary(),
  categories :: undefined | [binary()],
  duration :: undefined | binary(),
  enclosure :: undefined | enclosure(),
  id :: undefined | binary(),
  image :: undefined | binary(),
  link :: undefined | binary(),
  subtitle :: undefined | binary(),
  summary :: undefined | binary(),
  title :: undefined | binary(),
  updated :: undefined | binary()
}).

-type entry() :: #entry{}.
-type enclosure() :: #enclosure{}.
-type feed() :: #feed{}.
