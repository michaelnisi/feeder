%%
%% feeder_attributes - extract XML attributes
%%

-module(feeder_attributes).

-include("feeder_records.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([category/1]).
-export([enclosure/1]).
-export([anchor/1]).

enc(E, [H|T]) ->
  {_, _, K, V} = H,
  enc(enc(E, list_to_atom(K), list_to_binary(V)), T);
enc(E, []) ->
  E.

enc(E, url, V) -> E#enclosure{url=V};
enc(E, length, V) -> E#enclosure{length=V};
enc(E, type, V) -> E#enclosure{type=V}.

%% Returns an enclosure record from Attributes.
enclosure(Attributes) ->
  enc(#enclosure{}, Attributes).

href(_, {_, _, "rel", "shorturl"}) -> [];
href({_, _, "href", L}, _) -> L;
href(_, _) -> [].

%% Returns the href URL from Attributes.
anchor(Attributes) ->
  HRef = lists:keyfind("href", 3, Attributes),
  Rel = lists:keyfind("rel", 3, Attributes),
  href(HRef, Rel).

term({_, _, "term", L}) -> L;
term(_) -> [].

%% Returns the first category term found in Attributes.
%%
%% Atom describes categories with attibutes.
%% https://tools.ietf.org/html/rfc4287#section-4.2.2
category(Attributes) ->
  term(lists:keyfind("term", 3, Attributes)).

-ifdef(TEST).

enclosure_test() ->
  #enclosure{} = enclosure([]).

category_test() ->
  [] = category([]),
  [] = category([{}]),
  C = <<"food">>,
  F = {nil, nil, "term", C},
  C = category([nil, {}, F, nil]),
  C = category([nil, {}, F, {nil, nil, "term", <<"second food">>}]).

anchor_test() ->
  [] = anchor([]),
  [] = anchor([{}]),
  URL = binary:list_to_bin("https://troubled.pro"),
  URL = anchor([{nil, nil, "href", URL}]),
  URL = anchor([{nil, nil, "href", URL}, {nil, nil, "rel", "alternate"}]),

  % Blocking short URLs seems arbitrary, why would we do that? Just leaving
  % that in because there is a test for that, test/feederSUITE_data/author.*.
  [] = anchor([{nil, nil, "href", URL}, {nil, nil, "rel", "shorturl"}]),

  [] = anchor([{nil, nil, "rel", "shorturl"}, {nil, nil, "href", URL}]).

-endif.
