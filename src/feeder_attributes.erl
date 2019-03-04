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
-export([href/1]).

enc(E, [H|T]) ->
  {_, _, K, V} = H,
  enc(enc(E, list_to_atom(K), list_to_binary(V)), T);
enc(E, []) ->
  E.

enc(E, url, V) -> E#enclosure{url=V};
enc(E, length, V) -> E#enclosure{length=V};
enc(E, type, V) -> E#enclosure{type=V}.

%% Returns an enclosure record from Attrs.
enclosure(Attrs) ->
  enc(#enclosure{}, Attrs).

%% Returns the href URL from Attrs.
href(Attrs) ->
  case lists:keyfind("href", 3, Attrs) of
    {_, _, "href", L} ->
      case lists:keyfind("rel", 3, Attrs) of
        {_, _, "rel", "shorturl"} -> [];
        _ -> L
      end;
    false -> []
  end.

%% Returns the first category term found in Attrs.
%%
%% Atom describes categories with attibutes.
%% https://tools.ietf.org/html/rfc4287#section-4.2.2
category(Attrs) ->
  case lists:keyfind("term", 3, Attrs) of
    {_, _, "term", L} -> L;
    false -> []
  end.

-ifdef(TEST).

enclosure_test() ->
  #enclosure{} = enclosure([]).

category_test() ->
  [] = category([]),
  [] = category([{}]),
  Food = {nil, nil, "term", <<"food">>},
  <<"food">> = category([nil, {}, Food, nil]),
  <<"food">> = category([nil, {}, Food, {nil, nil, "term", <<"second food">>}]).

href_test() ->
  [] = href([]),
  [] = href([{}]),
  URL = binary:list_to_bin("https://troubled.pro"),
  URL = href([{nil, nil, "href", URL}]),
  % TODO: Understand shorturl filter. Can’t remember the reasoning behind it.
  [] = href([{nil, nil, "href", URL}, {nil, nil, "rel", "shorturl"}]).

-endif.
