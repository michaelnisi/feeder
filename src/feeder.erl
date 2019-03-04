%%
%% feeder - parse RSS and Atom formatted XML documents
%%

-module(feeder).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("feeder_records.hrl").

-export([file/2]).
-export([stream/2]).

-record(state, {
  author :: boolean(),
  category :: boolean(),
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

%% Updating the feed record

-spec feed(feed(), atom(), state()) -> feed().
feed(F, author, State) ->
  feeder_tuples:update(F, #feed.author, State#state.chars);
feed(F, name, State) when State#state.author =:= true ->
  feeder_tuples:update(F, #feed.author, State#state.chars);
feed(F, id, State) ->
  feeder_tuples:update(F, #feed.id, State#state.chars);
feed(F, image, State) ->
  feeder_tuples:update(F, #feed.image, State#state.chars);
feed(F, url, State) when State#state.image =:= true ->
  feeder_tuples:update(F, #feed.image, State#state.chars);
feed(F, url, State) ->
  feeder_tuples:update(F, #feed.url, State#state.chars);
feed(F, language, State) ->
  feeder_tuples:update(F, #feed.language, State#state.chars);
feed(F, link, State) ->
  feeder_tuples:update(F, #feed.link, State#state.chars);
feed(F, subtitle, State) ->
  feeder_tuples:update(F, #feed.subtitle, State#state.chars);
feed(F, summary, State) ->
  feeder_tuples:update(F, #feed.summary, State#state.chars);
feed(F, title, State) ->
  feeder_tuples:update(F, #feed.title, State#state.chars);
feed(F, updated, State) ->
  feeder_tuples:update(F, #feed.updated, State#state.chars);
feed(F, _, _) ->
  F.

%% Updating the current entry record

-spec entry(entry(), atom(), state()) -> entry().
entry(E, author, State) ->
  feeder_tuples:update(E, #entry.author, State#state.chars);
entry(E, name, State) when State#state.author =:= true ->
  feeder_tuples:update(E, #entry.author, State#state.chars);
entry(E, category, State) ->
  feeder_tuples:append(E, #entry.categories, State#state.chars);
entry(E, duration, State) ->
  feeder_tuples:update(E, #entry.duration, State#state.chars);
entry(E, enclosure, _State) -> E;
entry(E, id, State) ->
  feeder_tuples:update(E, #entry.id, State#state.chars);
entry(E, image, State) ->
  feeder_tuples:update(E, #entry.image, State#state.chars);
entry(E, link, State) ->
  feeder_tuples:update(E, #entry.link, State#state.chars);
entry(E, subtitle, State) ->
  feeder_tuples:update(E, #entry.subtitle, State#state.chars);
entry(E, summary, State) ->
  feeder_tuples:update(E, #entry.summary, State#state.chars);
entry(E, title, State) ->
  feeder_tuples:update(E, #entry.title, State#state.chars);
entry(E, updated, State) ->
  feeder_tuples:update(E, #entry.updated, State#state.chars);
entry(E, _, _) ->
  E.

%% Extracting and formatting characters

-define(IS_FEED,
  State#state.feed =/= undefined,
  State#state.entry =:= undefined).

-define(IS_ENTRY,
  State#state.entry =/= undefined).

-spec attribute(atom(), state(), atom(), list()) -> feed() | entry().
attribute(feed, State, link, Attrs) ->
  feed(State#state.feed, link, State#state{chars=feeder_attributes:anchor(Attrs)});
attribute(feed, State, image, Attrs) ->
  feed(State#state.feed, image, State#state{chars=feeder_attributes:anchor(Attrs)});
attribute(feed, State, url, Attrs) ->
  feed(State#state.feed, url, State#state{chars=feeder_attributes:anchor(Attrs)});
attribute(feed, State, _, _) ->
  State#state.feed;
attribute(entry, State, link, Attrs) ->
  entry(State#state.entry, link, State#state{chars=feeder_attributes:anchor(Attrs)});
attribute(entry, State, image, Attrs) ->
  entry(State#state.entry, image, State#state{chars=feeder_attributes:anchor(Attrs)});
attribute(entry, State, category, Attrs) ->
  entry(State#state.entry, category, State#state{chars=feeder_attributes:category(Attrs)});
attribute(entry, State, enclosure, Attrs) ->
  Entry = State#state.entry,
  Entry#entry{enclosure=feeder_attributes:enclosure(Attrs)};
attribute(entry, State, _, _) ->
  State#state.entry.

flag(author, State, Flag) ->
  State#state{author=Flag};
flag(image, State, Flag) ->
  State#state{image=Flag};
flag(category, State, Flag) ->
  State#state{category=Flag};
flag(_, State, _) ->
  State.

%% Handling parser events

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

%% Routing parser events

event(startDocument, _, S) ->
  S;
event({startElement, _, _LocalName, QName, Attrs}, _, S) ->
  start_element(feeder_elements:qname(QName), Attrs, S);
event({endElement, _, _LocalName, QName}, _, S) ->
  end_element(feeder_elements:qname(QName), S);
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
