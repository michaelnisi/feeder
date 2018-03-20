-module(fedex_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  StartLinkRet = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  % Using gen_event for instant but separated output.
  ok = gen_event:add_handler(fedex_event_man, fedex_logger, []),
  StartLinkRet.

fedex_parser_sup() -> #{
  id => fedex_parse_sup,
  start => {fedex_parse_sup, start_link, []},
  restart => transient,
  type => supervisor
}.

fedex_event_man() -> #{
  id => fedex_event_man,
  start => {gen_event, start_link, [{local, fedex_event_man}]},
  modules => dynamic
}.

init([]) ->
  {ok, {{one_for_one, 1, 5}, [
    fedex_parser_sup(),
    fedex_event_man()
  ]}}.
