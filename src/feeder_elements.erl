%%
%% feeder_elements - Qualify elements by name
%%

-module(feeder_elements).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([qname/1]).

qname({"atom", "link"}) -> url;
qname({_, "author"}) -> author;
qname({_, "category"}) -> category;
qname({_, "channel"}) -> feed;
qname({_, "contributor"}) -> author;
qname({_, "creator"}) -> author;
qname({_, "description"}) -> summary;
qname({_, "duration"}) -> duration;
qname({_, "enclosure"}) -> enclosure;
qname({_, "entry"}) -> entry;
qname({_, "feed"}) -> feed;
qname({_, "guid"}) -> id;
qname({_, "id"}) -> id;
qname({_, "image"}) -> image;
qname({_, "item"}) -> entry;
qname({_, "language"}) -> language;
qname({_, "link"}) -> link;
qname({_, "name"}) -> name;
qname({_, "pubDate"}) -> updated;
qname({_, "subtitle"}) -> subtitle;
qname({_, "summary"}) -> summary;
qname({_, "title"}) -> title;
qname({_, "updated"}) -> updated;
qname({_, "url"}) -> url;
qname({_, _}) -> undefined.

-ifdef(TEST).

q([{Wanted, QualifiedNames}|T], Tests) ->
  F = fun (QualifiedName) -> ?_assertMatch(Wanted, qname(QualifiedName)) end,
  q(T, [Tests|lists:map(F, QualifiedNames)]);
q([], Tests) ->
  Tests.
qname_test_() -> q([
  {author, [{"", "author"}]},
  {category, [{"", "category"}]},
  {duration, [{"", "duration"}]},
  {enclosure, [{"", "enclosure"}]},
  {entry, [{"", "entry"}, {"", "item"}]},
  {feed, [{"", "feed"}, {"", "channel"}]},
  {id, [{"", "id"}, {"", "guid"}]},
  {image, [{"", "image"}]},
  {language, [{"", "language"}]},
  {link, [{"", "link"}]},
  {name, [{"", "name"}]},
  {subtitle, [{"", "subtitle"}]},
  {summary, [{"", "summary"}, {"", "description"}]},
  {title, [{"", "title"}]},
  {undefined, [{"", "wtf"}, {"", ""}, {"", "!"}]},
  {updated, [{"", "updated"}, {"", "pubDate"}]},
  {url, [{"atom", "link"}]}
], []).

-endif.
