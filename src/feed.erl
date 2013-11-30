
%% feed - consume xml feed

-module(feed).
-export([start_link/0, stop/0, parse/1, feed/0, entries/0]).

-export([set_feed/1, add_entry/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {url, feed, entries=[]}).

% public

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
parse(Url) -> gen_server:call(?SERVER, {parse, Url}).
feed() -> gen_server:call(?SERVER, feed).
entries() -> gen_server:call(?SERVER, entries).
stop() -> gen_server:cast(?SERVER, stop).

% private

set_feed(F) -> gen_server:cast(?SERVER, {set_feed, F}).
add_entry(E) -> gen_server:cast(?SERVER, {add_entry, E}).

% callback functions

init([]) ->
  {ok, #state{}}.

handle_call({parse, Url}, From, _State) ->
  request(Url, From),
  {reply, ok, #state{url=Url}}; % new state
handle_call(feed, _From, State) ->
  {reply, State#state.feed, State};
handle_call(entries, _From, State) ->
  {reply, State#state.entries, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({set_feed, Feed}, State) ->
  {noreply, State#state{feed=Feed}};
handle_cast({add_entry, Entry}, State) ->
  Entries = [Entry|State#state.entries],
  {noreply, State#state{entries=Entries}};

handle_cast(_Msg, State) ->
  {noreply, State}.

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

request(Url, {From, _}) ->
  httpc:request(get, {Url, []}, [], req_opts()),
  receive
    {http, {_RequestId, stream_start, _Headers, Pid}} ->
      CS = #c_state{pid=Pid, from=From},
      resume(CS)
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

feeder_opts(CS) ->
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
          feeder:stream(BinBodyPart, feeder_opts(CS)),
          {BinBodyPart, CS#c_state{started=true}}
        end;
    {http, {_RequestId, stream_end, _Headers}} ->
      {<<"">>, CS}
  end.
