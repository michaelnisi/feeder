-module(feeder_entries).

-export([get/2]).

-include("feeder_records.hrl").

get(author, E) ->
  E#entry.author;
get(categories, E) ->
  E#entry.categories;
get(duration, E) ->
  E#entry.duration;
get(enclosure, E) ->
  E#entry.enclosure;
get(id, E) ->
  E#entry.id;
get(image, E) ->
  E#entry.image;
get(link, E) ->
  E#entry.link;
get(subtitle, E) ->
  E#entry.subtitle;
get(summary, E) ->
  E#entry.summary;
get(title, E) ->
  E#entry.title;
get(updated, E) ->
  E#entry.updated.
