-module(feeder_enclosures).

-export([get/2]).

-include("feeder_records.hrl").

get(url, Enc) ->
  Enc#enclosure.url;
get(length, Enc) ->
  Enc#enclosure.length;
get(type, Enc) ->
  Enc#enclosure.type.
