%% feeder - parse RSS and Atom formatted XML documents

-module(feeder).

-export([file/2]).
-export([stream/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("feeder_records.hrl").

-record(state, {
  author :: boolean(),
  chars :: undefined | [binary()],
  entry :: entry(),
  feed :: feed(),
  image :: boolean(),
  user :: {user_state(), user_fun()}
}).

-type state() :: #state{}.
-type user_fun() :: function().
-type user_state() :: term().
-export_type([user_fun/0, user_state/0]).

%% Third pass

trim(S) ->
  Bin = unicode:characters_to_binary(S, utf8),
  RE = "^[ \t\n\r]+|[ \t\n\r]+$",
  re:replace(Bin, RE, "", [global, {return, binary}]).

%% Once we deliberately set something, we do not override it.
update(T, F, L) when L =/= [] ->
  case element(F, T) of
    undefined -> setelement(F, T, trim(L));
    _ -> T
  end;
update(T, _, _) ->
  T.

-spec feed(feed(), atom(), state()) -> feed().
feed(F, author, State) ->
  update(F, #feed.author, State#state.chars);
feed(F, name, State) when State#state.author =:= true ->
  update(F, #feed.author, State#state.chars);
feed(F, id, State) ->
  update(F, #feed.id, State#state.chars);
feed(F, image, State) ->
  update(F, #feed.image, State#state.chars);
feed(F, url, State) when State#state.image =:= true ->
  update(F, #feed.image, State#state.chars);
feed(F, url, State) ->
  update(F, #feed.url, State#state.chars);
feed(F, language, State) ->
  update(F, #feed.language, State#state.chars);
feed(F, link, State) ->
  update(F, #feed.link, State#state.chars);
feed(F, subtitle, State) ->
  update(F, #feed.subtitle, State#state.chars);
feed(F, summary, State) ->
  update(F, #feed.summary, State#state.chars);
feed(F, title, State) ->
  update(F, #feed.title, State#state.chars);
feed(F, updated, State) ->
  update(F, #feed.updated, State#state.chars);
feed(F, _, _) ->
  F.

-spec entry(entry(), atom(), state()) -> entry().
entry(E, author, State) ->
  update(E, #entry.author, State#state.chars);
entry(E, name, State) when State#state.author =:= true ->
  update(E, #entry.author, State#state.chars);
entry(E, duration, State) ->
  update(E, #entry.duration, State#state.chars);
entry(E, enclosure, _State) -> E;
entry(E, id, State) ->
  update(E, #entry.id, State#state.chars);
entry(E, image, State) ->
  update(E, #entry.image, State#state.chars);
entry(E, link, State) ->
  update(E, #entry.link, State#state.chars);
entry(E, subtitle, State) ->
  update(E, #entry.subtitle, State#state.chars);
entry(E, summary, State) ->
  update(E, #entry.summary, State#state.chars);
entry(E, title, State) ->
  update(E, #entry.title, State#state.chars);
entry(E, updated, State) ->
  update(E, #entry.updated, State#state.chars);
entry(E, _, _) ->
  E.

%% Second pass

-define(IS_FEED,
  State#state.feed =/= undefined,
  State#state.entry =:= undefined).

-define(IS_ENTRY,
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

href(Attrs) ->
  case lists:keyfind("href", 3, Attrs) of
    {_, _, "href", L} ->
      case lists:keyfind("rel", 3, Attrs) of
        {_, _, "rel", "shorturl"} -> [];
        _ -> L
      end;
    false -> []
  end.

-spec attribute(atom(), state(), atom(), list()) -> feed() | entry().
attribute(feed, State, link, Attrs) ->
  feed(State#state.feed, link, State#state{chars=href(Attrs)});
attribute(feed, State, image, Attrs) ->
  feed(State#state.feed, image, State#state{chars=href(Attrs)});
attribute(feed, State, url, Attrs) ->
  feed(State#state.feed, url, State#state{chars=href(Attrs)});
attribute(feed, State, _, _) ->
  State#state.feed;
attribute(entry, State, link, Attrs) ->
  entry(State#state.entry, link, State#state{chars=href(Attrs)});
attribute(entry, State, image, Attrs) ->
  entry(State#state.entry, image, State#state{chars=href(Attrs)});
attribute(entry, State, enclosure, Attrs) ->
  Entry = State#state.entry,
  Entry#entry{enclosure=enclosure(Attrs)};
attribute(entry, State, _, _) ->
  State#state.entry.

flag(author, State, Flag) ->
  State#state{author=Flag};
flag(image, State, Flag) ->
  State#state{image=Flag};
flag(_, State, _) ->
  State.

-spec end_element(atom(), state()) -> state().
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
end_element(E, State) when ?IS_FEED ->
  NewState = flag(E, State, false),
  Feed = feed(NewState#state.feed, E, State),
  NewState#state{feed=Feed};
end_element(E, State) when ?IS_ENTRY ->
  NewState = flag(E, State, false),
  Entry = entry(NewState#state.entry, E, State),
  NewState#state{entry=Entry}.

-spec start_element(atom(), list(), state()) -> state().
start_element(undefined, _, State) ->
  State#state{chars=[]};
start_element(feed, _, State) ->
  State#state{feed=#feed{}};
start_element(entry, _, State) ->
  State#state{entry=#entry{}};
start_element(E, Attrs, State) when ?IS_FEED ->
  NewState = flag(E, State, true),
  Feed = attribute(feed, NewState, E, Attrs),
  NewState#state{chars=[], feed=Feed};
start_element(E, Attrs, State) when ?IS_ENTRY ->
  NewState = flag(E, State, true),
  Entry = attribute(entry, NewState, E, Attrs),
  NewState#state{chars=[], entry=Entry}.

%% First pass

qname({"atom", "link"}) -> url;
qname({_, "author"}) -> author;
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

%% API

event({entry, Entry}, {Feed, Entries}) ->
  {Feed, [Entry|Entries]};
event({feed, Feed}, {[], Entries}) ->
  {Feed, Entries};
event(endFeed, {Feed, Entries}) ->
  {Feed, lists:reverse(Entries)}.

user_state(undefined) -> {[], []};
user_state(S) -> S.

user_fun(undefined) -> fun event/2;
user_fun(F) -> F.

opts(file, Opts) ->
  US = user_state(proplists:get_value(event_state, Opts)),
  UF = user_fun(proplists:get_value(event_fun, Opts)),
  User = {US, UF},
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

-ifdef(TEST).
trim_test_() -> [
  ?_assertMatch(<<"">>, trim("")),
  ?_assertMatch(<<"hello">>, trim(" hello "))
].

q([{Wanted, QualifiedNames}|T], Tests) ->
  F = fun (QualifiedName) -> ?_assertMatch(Wanted, qname(QualifiedName)) end,
  q(T, [Tests|lists:map(F, QualifiedNames)]);
q([], Tests) ->
  Tests.
qname_test_() -> q([
  {author, [{"", "author"}]},
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
