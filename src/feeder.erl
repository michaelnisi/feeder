
%% feeder - parse RSS and Atom feeds

-module(feeder).
-export([file/2, stream/2]).

-include("../include/feeder.hrl").

%% API

file(Filename, Opts) ->
  xmerl_sax_parser:file(Filename, opts(file, Opts)).

%% Events
%% {feed, Feed}, State
%% {entry, Entry}, State
%% endFeed, State
stream(Chunk, Opts) ->
  xmerl_sax_parser:stream(Chunk, opts(stream, Opts)).

%% Types

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

start_element(item, ?STATE) ->
  {_Chars, _Feed, #entry{}, _User};
start_element(E, ?STATE) when ?IS_ENTRY, ?RSS orelse ?ATOM ->
  {[], _Feed, _Entry, _User};
start_element(_, S) ->
  S.

end_element(E, ?STATE) when ?IS_ENTRY, ?RSS orelse ?ATOM ->
  {_Chars, _Feed, update_entry(_Entry, E, _Chars), _User};
end_element(item, ?STATE) ->
  {UserState, UserFun} = _User,
  UserFun({entry, _Entry}, UserState),
  {_Chars, _Feed, undefined, _User};
end_element(_, S) ->
  S.

%% SAX events

event({startElement, _, _LocalName, QName, _Attrs}, _, S) ->
  start_element(qname(QName), S);
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

update_entry(Entry, author, Chars)  ->
  Entry#entry{author=iolist_to_binary(Chars)};
update_entry(Entry, E, Chars) when E =:= guid; E =:= id ->
  Entry#entry{id=iolist_to_binary(Chars)};
update_entry(Entry,link, Chars) ->
  Entry#entry{link=iolist_to_binary(Chars)};
update_entry(Entry, subtitle, Chars) ->
  Entry#entry{subtitle=iolist_to_binary(Chars)};
update_entry(Entry, E, Chars) when E =:= description; E =:= summary ->
  Entry#entry{summary=iolist_to_binary(Chars)};
update_entry(Entry, title, Chars) ->
  Entry#entry{title=iolist_to_binary(Chars)};
update_entry(Entry, E, Chars) when E =:= pubDate; E =:= updated ->
  Entry#entry{updated=iolist_to_binary(Chars)};
update_entry(Entry, _, _) ->
  Entry.
