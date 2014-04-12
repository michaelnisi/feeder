
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(feed, {
    author,
    id,
    image,
    link,
    subtitle,
    summary,
    title,
    updated
  }).

-record(entry, {
    author,
    enclosure,
    id,
    image,
    link,
    subtitle,
    summary,
    title,
    updated
  }).

-record(enclosure, {
    url,
    length,
    type
  }).
