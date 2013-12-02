
%% feeder_httpc - stream XML feed at URL to tuples

-module(feeder_httpc).
-export([start_link/0, stop/0, request/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% API

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
request(Url) -> gen_server:call(?SERVER, {request, Url}).
stop() -> gen_server:cast(?SERVER, stop).

%% Gen_Server callbacks

init([]) ->
  {ok, []}.

handle_call({request, Url}, {Pid, _}, State) ->
  gen_server:cast(?SERVER, {request, Pid, Url}),
  {reply, ok, State}.

handle_cast({request, From, Url}, State) ->
  request(From, Url),
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-record(c_state, {pid, from, started=false, ended=false, id}).

req_opts() ->
  [{sync, false}, {stream, {self, once}}, {body_format, binary}].

request(From, Url) ->
  httpc:request(get, {Url, []}, [], req_opts()),
  receive
    {http, {RequestId, stream_start, _Headers, Pid}} ->
      CS = #c_state{pid=Pid, from=From, id=RequestId},
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

resume(State) ->
  RequestId = State#c_state.id,
  httpc:stream_next(State#c_state.pid),
  receive
    {http, {RequestId, stream, BinBodyPart}} ->
      if
        not State#c_state.started ->
          NewState = State#c_state{started=true},
          feeder_parser:stream(BinBodyPart, parser_opts(NewState));
        true ->
          {BinBodyPart, State}
      end;
    {http, {RequestId, stream_end, _Headers}} ->
      {<<>>, State}
  end.
