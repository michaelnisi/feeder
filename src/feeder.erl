
%% feeder - parse RSS and Atom feeds

-module(feeder).
-export([file/2, stream/2]).

-include("../include/feeder.hrl").

%% API

file(Filename, Opts) ->
  xmerl_sax_parser:file(Filename, opts(file, Opts)).

stream(Xml, Opts) ->
  xmerl_sax_parser:stream(Xml, opts(stream, Opts)).

%% State

opts(file, Opts) ->
  UserState = proplists:get_value(event_state, Opts),
  UserFun = proplists:get_value(event_fun, Opts),
  User = {UserState, UserFun},
  [{event_state, state(User)}, {event_fun, fun event/3}];
opts(stream, Opts) ->
  ContinuationState = proplists:get_value(continuation_state, Opts),
  ContinuationFun = proplists:get_value(continuation_fun, Opts),
  [{continuation_state, ContinuationState},
   {continuation_fun, ContinuationFun}] ++ opts(file, Opts).

state(User) ->
  {undefined, undefined, undefined, User}.

%% Event handlers

start_element(E, _Attrs, ?STATE) when E =:= channel; E =:= feed ->
  {_Chars, #feed{}, _Entry, _User};
start_element(E, _Attrs, ?STATE) when E =:= item; E =:= entry->
  {_Chars, _Feed, #entry{}, _User};
start_element(E, Attrs, ?STATE) when ?IS_FEED, ?RSS orelse ?ATOM ->
  {[], attribute(feed, _Feed, E, Attrs), _Entry, _User};
start_element(E, Attrs, ?STATE) when ?IS_ENTRY, ?RSS orelse ?ATOM ->
  {[], _Feed, attribute(entry, _Entry, E, Attrs), _User};
start_element(_, _, S) ->
  S.

end_element(E, ?STATE) when E =:= channel; E =:= feed ->
  {UserState, UserFun} = _User,
  UserFun({feed, _Feed}, UserState),
  {_Chars, undefined, _Entry, _User};
end_element(E, ?STATE) when E =:= item; E =:= entry ->
  {UserState, UserFun} = _User,
  UserFun({entry, _Entry}, UserState),
  {_Chars, _Feed, undefined, _User};
end_element(E, ?STATE) when ?IS_FEED, ?RSS orelse ?ATOM ->
  {_Chars, update_feed(_Feed, E, _Chars), _Entry, _User};
end_element(E, ?STATE) when ?IS_ENTRY, ?RSS orelse ?ATOM ->
  {_Chars, _Feed, update_entry(_Entry, E, _Chars), _User};
end_element(_, S) ->
  S.

%% SAX events

event({startElement, _, _LocalName, QName, Attrs}, _, S) ->
  start_element(qname(QName), Attrs, S);
event({endElement, _, _LocalName, QName}, _, S) ->
  end_element(qname(QName), S);
event({characters, C}, _, ?STATE) ->
  {[_Chars, C], _Feed, _Entry, _User};
event(endDocument, _, S) ->
  {_, _, _, {UserState, UserFun}} = S,
  UserFun(endFeed, UserState),
  S;
event(_, _, S) ->
  S.

%% Internal funs

qname({_, Name}) ->
  list_to_atom(Name).

attribute(feed, F, link, [{_, _, "href", L}]) ->
  update_feed(F, link, L);
attribute(feed, F, _, _) ->
  F;
attribute(entry, E, link, [{_, _, "href", L}]) ->
  update_entry(E, link, L);
attribute(entry, E, _, _) ->
  E.

-define(UF(Atom),
  if
    Feed#feed.Atom =:= undefined ->
      Feed#feed{Atom=iolist_to_binary(Chars)};
    true ->
      Feed
  end).

update_feed(Feed, title, Chars)  ->
  ?UF(title);
update_feed(Feed, link, Chars)  ->
  ?UF(link);
update_feed(Feed, description, Chars)  ->
  ?UF(summary);
update_feed(Feed, name, Chars)  ->
  ?UF(author);
update_feed(Feed, updated, Chars)  ->
  ?UF(updated);
update_feed(Feed, id, Chars)  ->
  ?UF(id);
update_feed(Feed, _, _) ->
  Feed.

-define(UE(Atom),
  if
    Entry#entry.Atom =:= undefined ->
      Entry#entry{Atom=iolist_to_binary(Chars)};
    true ->
      Entry
  end).

update_entry(Entry, author, Chars)  ->
  ?UE(author);
update_entry(Entry, E, Chars) when E =:= guid; E =:= id ->
  ?UE(id);
update_entry(Entry,link, Chars) ->
  ?UE(link);
update_entry(Entry, subtitle, Chars) ->
  ?UE(subtitle);
update_entry(Entry, E, Chars) when E =:= description; E =:= summary ->
  ?UE(summary);
update_entry(Entry, title, Chars) ->
  ?UE(title);
update_entry(Entry, E, Chars) when E =:= pubDate; E =:= updated ->
  ?UE(updated);
update_entry(Entry, _, _) ->
  Entry.

