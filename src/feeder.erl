
%% feeder - parse RSS and Atom formatted XML documents

-module(feeder).

-export([file/2]).
-export([stream/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

-type user_state() :: term().
-export_type([user_state/0]).

-type user_fun() :: function().
-export_type([user_fun/0]).

-type feed() :: tuple().
-export_type([feed/0]).

-type enclosure() :: tuple().
-export_type([enclosure/0]).

-type entry() :: tuple().
-export_type([entry/0]).

-record(feed, {
  author :: undefined | binary(),
  id :: undefined | binary(),
  image :: undefined | binary(),
  language :: undefined | binary(),
  link :: undefined | binary(),
  subtitle :: undefined | binary(),
  summary :: undefined | binary(),
  title :: undefined | binary(),
  updated :: undefined | binary()
}).

-record(enclosure, {
  url :: undefined | binary(),
  length :: undefined | binary(),
  type :: undefined | binary()
}).

-record(entry, {
  author :: undefined | binary(),
  duration :: undefined | binary(),
  enclosure :: undefined | #enclosure{},
  id :: undefined | binary(),
  image :: undefined | binary(),
  link :: undefined | binary(),
  subtitle :: undefined | binary(),
  summary :: undefined | binary(),
  title :: undefined | binary(),
  updated :: undefined | binary()
}).

-record(state, {
  chars :: undefined | [binary()],
  feed :: #feed{},
  entry :: #entry{},
  user :: {user_state(), user_fun()}
}).

trim(S) ->
  Bin = unicode:characters_to_binary(S, utf8),
  RE = "^[ \t\n\r]+|[ \t\n\r]+$",
  re:replace(Bin, RE, "", [global, {return, binary}]).

update(T, F, L) when L =/= [] ->
  setelement(F, T, trim(L));
update(T, _, _) ->
  T.

feed(F, title, L) -> update(F, #feed.title, L);
feed(F, subtitle, L) -> update(F, #feed.subtitle, L);
feed(F, language, L) -> update(F, #feed.language, L);
feed(F, link, L) -> update(F, #feed.link, L);
feed(F, summary, L) -> update(F, #feed.summary, L);
feed(F, author, L) -> update(F, #feed.author, L);
feed(F, updated, L) -> update(F, #feed.updated, L);
feed(F, image, L) -> update(F, #feed.image, L);
feed(F, id, L) -> update(F, #feed.id, L).

entry(E, author, L) -> update(E, #feed.author, L);
entry(E, id, L) -> update(E, #entry.id, L);
entry(E, link, L) -> update(E, #entry.link, L);
entry(E, subtitle, L) -> update(E, #entry.subtitle, L);
entry(E, summary, L) -> update(E, #entry.summary, L);
entry(E, title, L) -> update(E, #entry.title, L);
entry(E, updated, L) -> update(E, #entry.updated, L);
entry(E, duration, L) -> update(E, #entry.duration, L);
entry(E, image, L) -> update(E, #entry.image, L);
entry(E, enclosure, _L) -> E.

-define(isFeed,
  State#state.feed =/= undefined,
  State#state.entry =:= undefined).

-define(isEntry,
  State#state.entry =/= undefined).

enc(E, [H|T]) ->
  {_, _, K, V} = H,
  enc(enc(E, list_to_atom(K), list_to_binary(V)), T);
enc(E, []) ->
  E.

enc(E, url, V) -> E#enclosure{url=V};
enc(E, length, V) -> E#enclosure{length=V};
enc(E, type, V) -> E#enclosure{type=V}.

enclosure(Attrs) ->
  enc(#enclosure{}, Attrs).

attribute(feed, F, link, [{_, _, "href", L}]) ->
  feed(F, link, L);
attribute(feed, F, image, [{_, _, "href", L}]) ->
  feed(F, image, L);
attribute(feed, F, _, _) ->
  F;
attribute(entry, E, link, [{_, _, "href", L}]) ->
  entry(E, link, L);
attribute(entry, Entry, enclosure, Attrs) ->
  Entry#entry{enclosure=enclosure(Attrs)};
attribute(entry, E, image, [{_, _, "href", L}]) ->
  entry(E, image, L);
attribute(entry, E, _, _) ->
  E.

end_element(undefined, State) ->
  State;
end_element(document, State) ->
  {UserState, UserFun} = State#state.user,
  UserFun(endFeed, UserState);
end_element(feed, State) ->
  {UserState, UserFun} = State#state.user,
  NewUserState = UserFun({feed, State#state.feed}, UserState),
  State#state{feed=undefined, user={NewUserState, UserFun}};
end_element(entry, State) ->
  {UserState, UserFun} = State#state.user,
  NewUserState = UserFun({entry, State#state.entry}, UserState),
  State#state{entry=undefined, user={NewUserState, UserFun}};
end_element(E, State) when ?isFeed ->
  Feed = feed(State#state.feed, E, State#state.chars),
  State#state{feed=Feed};
end_element(E, State) when ?isEntry ->
  Entry = entry(State#state.entry, E, State#state.chars),
  State#state{entry=Entry}.

start_element(undefined, _, State) ->
  State;
start_element(feed, _Attrs, State) ->
  State#state{feed=#feed{}};
start_element(entry, _Attrs, State) ->
  State#state{entry=#entry{}};
start_element(E, Attrs, State) when ?isFeed ->
  Feed = attribute(feed, State#state.feed, E, Attrs),
  State#state{chars=[], feed=Feed};
start_element(E, Attrs, State) when ?isEntry ->
  Entry = attribute(entry, State#state.entry, E, Attrs),
  State#state{chars=[], entry=Entry}.

%% Normalize qualified names
qname({_, "author"}) -> author;
qname({_, "channel"}) -> feed;
qname({_, "description"}) -> summary;
qname({_, "enclosure"}) -> enclosure;
qname({_, "entry"}) -> entry;
qname({_, "feed"}) -> feed;
qname({_, "guid"}) -> id;
qname({_, "id"}) -> id;
qname({_, "image"}) -> image;
qname({_, "item"}) -> entry;
qname({_, "language"}) -> language;
qname({_, "duration"}) -> duration;
qname({_, "link"}) -> link;
qname({_, "pubDate"}) -> updated;
qname({_, "subtitle"}) -> subtitle;
qname({_, "summary"}) -> summary;
qname({_, "title"}) -> title;
qname({_, "updated"}) -> updated;
qname({_, _}) -> undefined.

event(startDocument, _, S) ->
  S;
event({startElement, _, _LocalName, QName, Attrs}, _, S) ->
  start_element(qname(QName), Attrs, S);
event({endElement, _, _LocalName, QName}, _, S) ->
  end_element(qname(QName), S);
event({characters, C}, _, S) ->
  S#state{chars=[S#state.chars|C]};
event(endDocument, _, S) ->
  end_element(document, S);
event(_, _, S) ->
  S.

opts(file, Opts) ->
  UserState = proplists:get_value(event_state, Opts),
  UserFun = proplists:get_value(event_fun, Opts),
  User = {UserState, UserFun},
  [{event_state, #state{user=User}}, {event_fun, fun event/3}];
opts(stream, Opts) ->
  CS = proplists:get_value(continuation_state, Opts),
  CF = proplists:get_value(continuation_fun, Opts),
  [{continuation_state, CS}, {continuation_fun, CF}] ++ opts(file, Opts).

-spec file(binary() | maybe_improper_list(), [any()]) -> any().
file(Filename, Opts) ->
  xmerl_sax_parser:file(Filename, opts(file, Opts)).

-spec stream(binary() | maybe_improper_list(), [any()]) -> any().
stream(Xml, Opts) ->
  xmerl_sax_parser:stream(Xml, opts(stream, Opts)).
