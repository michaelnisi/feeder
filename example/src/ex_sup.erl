-module(ex_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

ex_parser_sup() -> #{
  id => ex_parse_sup,
  start => {ex_parse_sup, start_link, []},
  restart => transient,
  type => supervisor
}.

ex_event_man() -> #{
  id => ex_event_man,
  start => {gen_event, start_link, [{local, ex_event_man}]},
  modules => dynamic
}.

init([]) ->
  {ok, {{one_for_one, 1, 5}, [
    ex_parser_sup(),
    ex_event_man()
  ]}}.
