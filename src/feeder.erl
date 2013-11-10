
%% feeder - parse RSS and Atom feeds

-module(feeder).
-export([stream/2]).

stream(Chunk, UserOpts) ->
  Opts = opts(UserOpts),
  xmerl_sax_parser:stream(Chunk, Opts).

opts(Opts) -> 
  ContinuationState = proplists:get_value(continuation_state, Opts),
  ContinuationFun = proplists:get_value(continuation_fun, Opts),
  [{continuation_state, ContinuationState}
 , {continuation_fun, ContinuationFun}
 , {event_state, state()}
 , {event_fun, fun event/3}].

state() ->
  {}.

qname({_, Name}) ->
  list_to_atom(Name).

start_element(E, _S) ->
  io:format("start ~p~n", [E]).

end_element(E, _S) ->
  io:format("end ~p~n", [E]).

event({startElement, _, _LocalName, QName, _Attrs}, _, S) ->
  start_element(qname(QName), S);
event({endElement, _, _LocalName, QName}, _, S) ->
  end_element(qname(QName), S);
event({characters, C}, _, S) ->
  S;
event(_, _, S) ->
  S.
