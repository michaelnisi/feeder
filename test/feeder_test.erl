-module(feeder_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

test_stream() ->
  Expected = {ok, ok, []},
  Actual = feeder:stream("<thing></thing>", []),
  ?assert_equal(Expected, Actual). 
