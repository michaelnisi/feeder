-module(example_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

child_spec() -> {
  example_worker,
  {example_parse, start_link, []},
  transient,
  3000,
  worker,
  [example_parse]
}.

init([]) ->
  {ok, {{simple_one_for_one, 10, 1}, [child_spec()]}}.
