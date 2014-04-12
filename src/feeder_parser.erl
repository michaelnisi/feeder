
%% feeder_parser - parse RSS and Atom formatted XML feeds

-module(feeder_parser).
-export([file/2, stream/2]).

-include("../include/feeder.hrl").

-type user_state() :: term().
-type user_fun() :: term().

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

-define(UF(Atom),
  if
    F#feed.Atom =:= undefined, L =/= [] ->
      F#feed{Atom=trim(L)};
    true ->
      F
  end).

feed(F, title, L) -> ?UF(title);
feed(F, subtitle, L) -> ?UF(subtitle);
feed(F, link, L) -> ?UF(link);
feed(F, summary, L) -> ?UF(summary);
feed(F, name, L) -> ?UF(author);
feed(F, author, L) -> ?UF(author);
feed(F, updated, L) -> ?UF(updated);
feed(F, image, L) -> ?UF(image);
feed(F, id, L) -> ?UF(id).

-define(UE(Atom),
  if
    E#entry.Atom =:= undefined, L =/= [] ->
      E#entry{Atom=trim(L)};
    true ->
      E
  end).

entry(E, author, L) -> ?UE(author);
entry(E, id, L) -> ?UE(id);
entry(E, link, L) -> ?UE(link);
entry(E, subtitle, L) -> ?UE(subtitle);
entry(E, summary, L) -> ?UE(summary);
entry(E, title, L) -> ?UE(title);
entry(E, updated, L) -> ?UE(updated);
entry(E, image, L) -> ?UE(image);
entry(E, enclosure, _L) -> E.

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

-define(FEED,
  State#state.feed =/= undefined,
  State#state.entry =:= undefined).

-define(ENTRY,
  State#state.entry =/= undefined).

start_element(undefined, _, S) ->
  S;
start_element(feed, _Attrs, State) ->
  State#state{feed=#feed{}};
start_element(entry, _Attrs, State) ->
  State#state{entry=#entry{}};
start_element(E, Attrs, State) when ?FEED ->
  Feed = attribute(feed, State#state.feed, E, Attrs),
  State#state{chars=[], feed=Feed};
start_element(E, Attrs, State) when ?ENTRY ->
  Entry = attribute(entry, State#state.entry, E, Attrs),
  State#state{chars=[], entry=Entry};
start_element(_, _, S) ->
  S.

end_element(undefined, S) ->
  S;
end_element(document, S) ->
  {UserState, UserFun} = S#state.user,
  UserFun(endFeed, UserState),
  UserState;
end_element(feed, State) ->
  {UserState, UserFun} = State#state.user,
  NewUserState = UserFun({feed, State#state.feed}, UserState),
  State#state{feed=undefined, user={NewUserState, UserFun}};
end_element(entry, State) ->
  {UserState, UserFun} = State#state.user,
  NewUserState = UserFun({entry, State#state.entry}, UserState),
  State#state{entry=undefined, user={NewUserState, UserFun}};
end_element(E, State) when ?FEED ->
  Feed = feed(State#state.feed, E, State#state.chars),
  State#state{feed=Feed};
end_element(E, State) when ?ENTRY ->
  Entry = entry(State#state.entry, E, State#state.chars),
  State#state{entry=Entry};
end_element(_, S) ->
  S.

qname({_, "channel"}) -> feed;
qname({_, "feed"}) -> feed;
qname({_, "item"}) -> entry;
qname({_, "entry"}) -> entry;
qname({_, "title"}) -> title;
qname({_, "subtitle"}) -> subtitle;
qname({_, "description"}) -> summary;
qname({_, "summary"}) -> summary;
qname({_, "link"}) -> link;
qname({_, "pubDate"}) -> updated;
qname({_, "updated"}) -> updated;
qname({_, "guid"}) -> id;
qname({_, "id"}) -> id;
qname({_, "name"}) -> name;
qname({_, "author"}) -> author;
qname({_, "enclosure"}) -> enclosure;
qname({_, "image"}) -> image;
qname({_, _}) -> undefined.

event(startDocument, _, S) ->
  S;
event({startElement, _, _LocalName, QName, Attrs}, _, S) ->
  start_element(qname(QName), Attrs, S);
event({endElement, _, _LocalName, QName}, _, S) ->
  end_element(qname(QName), S);
event({characters, C}, _, S) ->
  S#state{chars=[S#state.chars, C]};
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

file(Filename, Opts) ->
  xmerl_sax_parser:file(Filename, opts(file, Opts)).

stream(Xml, Opts) ->
  xmerl_sax_parser:stream(Xml, opts(stream, Opts)).
