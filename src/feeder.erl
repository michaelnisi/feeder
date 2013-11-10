
%% feeder - parse RSS and Atom feeds

-module(feeder).
-export([stream/2]).

%% API

%% Events
%% {feed, Feed}, State
%% {entry, Entry}, State
%% endFeed, State
stream(Chunk, UserOpts) ->
  Opts = opts(UserOpts),
  xmerl_sax_parser:stream(Chunk, Opts).

%% Tuples

opts(Opts) -> 
  ContinuationState = proplists:get_value(continuation_state, Opts),
  ContinuationFun = proplists:get_value(continuation_fun, Opts),
  UserState = proplists:get_value(event_state, Opts),
  UserFun = proplists:get_value(event_fun, Opts),
  User = {UserState, UserFun},
  [{continuation_state, ContinuationState}
 , {continuation_fun, ContinuationFun}
 , {event_state, state(User)}
 , {event_fun, fun event/3}].

-define(STATE, {_Chars, _Feed, _Entry, _User}).
state(User) ->
  {nil, nil, nil, User}.

-define(IS_FEED, _Feed =/= null, _Entry =:= nil).
-define(FEED, {_Title, _Summary, _Link}).
feed() ->
  {nil, nil, nil}.

-define(IS_ENTRY, _Entry =/= nil).
-define(ENTRY, {_Title, _Summary, _Link, _PubDate}).
entry() ->
  {nil, nil, nil, nil}.

entry(title, Title, ?ENTRY) ->
  {Title, _Summary, _Link, _PubDate};
entry(summary, Summary, ?ENTRY) ->
  {_Title, Summary, _Link, _PubDate}.

chars() ->
  [].

%% Event handlers

start_element(channel, ?STATE) ->
  {_Chars, feed(), _Entry, _User};
start_element(item, ?STATE) ->
  {_Chars, _Feed, entry(), _User};
start_element(title, ?STATE) ->
  {chars(), _Feed, _Entry, _User};
start_element(_, S) ->
  S.

end_element(title, ?STATE) when ?IS_ENTRY ->
  Title = iolist_to_binary(_Chars),
  Entry = entry(title, Title, _Entry),
  {nil, _Feed, Entry, _User}; 
end_element(item, ?STATE) ->
  {UserState, UserFun} = _User,
  UserFun({entry, _Entry}, UserState),
  {_Chars, _Feed, nil, _User};
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
