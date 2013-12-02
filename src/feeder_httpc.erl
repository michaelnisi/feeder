
%% feeder_httpc - stream XML feed at URL to tuples

-module(feeder_httpc).
-export([start_link/0, stop/0, request/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(sstate, {pid, from, started=false, ended=false, id}).

-define(SERVER, ?MODULE).

%% API

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
request(Url) -> gen_server:call(?SERVER, {request, Url}).
stop() -> gen_server:cast(?SERVER, stop).

%% Gen_Server callbacks

init([]) ->
  {ok, []}.

handle_call({request, Url}, {Pid, _}, State) ->
  Reply = httpc:request(get, {Url, []}, [], req_opts()),
  gen_server:cast(?SERVER, {request, Pid, Url}),
  {reply, Reply, State}.

handle_cast({request, From, _Url}, State) ->
  receive
    {http, {RequestId, stream_start, _Headers, Pid}} ->
      StreamState = #sstate{pid=Pid, from=From, id=RequestId},
      resume(StreamState),
      {noreply, State}
  after
    3000 ->
      From ! {feeder, {error, timeout}},
      {noreply, State}
  end;

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Details

req_opts() ->
  [{sync, false}, {stream, {self, once}}, {body_format, binary}].

event_fun({entry, Entry}, StreamState) ->
  StreamState#sstate.from ! {feeder, {StreamState#sstate.id, entry, Entry}},
  StreamState;
event_fun({feed, Feed}, StreamState) ->
  StreamState#sstate.from ! {feeder, {StreamState#sstate.id, feed, Feed}},
  StreamState;
event_fun(endFeed, StreamState) ->
  StreamState#sstate.from ! {feeder, {StreamState#sstate.id, stream_end}},
  StreamState.

parser_opts(StreamState) ->
  [{event_state, StreamState}, {event_fun, fun event_fun/2},
   {continuation_state, StreamState}, {continuation_fun, fun resume/1}].

resume(StreamState) ->
  RequestId = StreamState#sstate.id,
  httpc:stream_next(StreamState#sstate.pid),
  receive
    {http, {RequestId, stream, BinBodyPart}} ->
      if
        not StreamState#sstate.started ->
          NewState = StreamState#sstate{started=true},
          feeder_parser:stream(BinBodyPart, parser_opts(NewState));
        true ->
          {BinBodyPart, StreamState}
      end;
    {http, {RequestId, stream_end, _Headers}} ->
      {<<>>, StreamState}
  end.
