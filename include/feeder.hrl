
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
