-module(feeder_enclosures).

-include("feeder_records.hrl").

-export([get/2]).

get(url, Enc) ->
  Enc#enclosure.url;
get(length, Enc) ->
  Enc#enclosure.length;
get(type, Enc) ->
  Enc#enclosure.type.
