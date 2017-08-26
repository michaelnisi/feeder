-module(fedex_parse_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

parser() -> #{
  id => fedex_parse,
  start => {fedex_parse, start_link, []},
  restart => temporary,
  shutdown => 3000,
  type => worker,
  modules => [fedex_parse]
}.

init([]) ->
  {ok, {{simple_one_for_one, 1, 1}, [parser()]}}.
