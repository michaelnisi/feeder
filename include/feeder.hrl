
-define(STATE, {_Chars, _Feed, _Entry, _User}).

-define(IS_FEED, _Feed =/= undefined, _Entry =:= undefined).
-define(IS_ENTRY, _Entry =/= undefined).

-define(RSS,
  E =:= title orelse
  E =:= link orelse
  E =:= description orelse
  E =:= language orelse
  E =:= pubDate orelse
  E =:= guid
).

-define(ATOM,
  E =:= updated orelse
  E =:= name orelse
  E =:= summary orelse
  E =:= id orelse
  E =:= author % TODO: <author><name>John Doe</name></author>
).

-record(feed, {
    author,
    copyright,
    id,
    image,
    language,
    link,
    payment,
    subtitle,
    summary,
    title,
    ttl,
    updated
  }).

-record(entry, {
    author,
    enclosure,
    duration,
    id,
    link,
    subtitle,
    summary,
    title,
    updated
  }).
