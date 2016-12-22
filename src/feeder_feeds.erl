-module(feeder_feeds).

-export([get/2]).

-include("feeder_records.hrl").

get(author, F) ->
  F#feed.author;
get(id, F) ->
  F#feed.id;
get(image, F) ->
  F#feed.image;
get(language, F) ->
  F#feed.language;
get(link, F) ->
  F#feed.link;
get(subtitle, F) ->
  F#feed.subtitle;
get(summary, F) ->
  F#feed.summary;
get(title, F) ->
  F#feed.title;
get(updated, F) ->
  F#feed.updated;
get(url, F) ->
  F#feed.url.
