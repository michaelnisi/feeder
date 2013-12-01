
%% feeder_httpc - stream feeds over http

-module(feeder_httpc).
-export([start_link/0, stop/0, request/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

% public

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
request(Url) -> gen_server:call(?SERVER, {request, Url}).
stop() -> gen_server:cast(?SERVER, stop).

% callback functions

init([]) ->
  {ok, []}.

handle_call({request, Url}, From, State) ->
  {reply, httpc_request(Url, From), State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% details

req_opts() ->
  [{sync, false}, {stream, {self, once}}, {body_format, binary}].

-record(c_state, {pid, from, started=false}).

httpc_request(Url, {From, _}) ->
  httpc:request(get, {Url, []}, [], req_opts()),
  receive
    {http, {_RequestId, stream_start, _Headers, Pid}} ->
      CS = #c_state{pid=Pid, from=From},
      resume(CS),
      ok
  after
    3000 ->
      {error, timeout}
  end.

event_fun({entry, Entry}, From) ->
  From ! {entry, Entry},
  From;
event_fun({feed, Feed}, From) ->
  From ! {feed, Feed},
  From;
event_fun(endFeed, From) ->
  From ! endFeed,
  From.

parser_opts(CS) ->
  [{event_state, CS#c_state.from}, {event_fun, fun event_fun/2},
   {continuation_state, CS}, {continuation_fun, fun resume/1}].

resume(CS) ->
  httpc:stream_next(CS#c_state.pid),
  receive
    {http, {_RequestId, stream, BinBodyPart}} ->
      if
        CS#c_state.started ->
          {BinBodyPart, CS};
        true ->
          feeder_parser:stream(BinBodyPart, parser_opts(CS)),
          {BinBodyPart, CS#c_state{started=true}}
        end;
    {http, {_RequestId, stream_end, _Headers}} ->
      {<<>>, CS}
  end.
