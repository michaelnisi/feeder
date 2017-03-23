
-module(atom).
-export([wanted/0]).

wanted() -> {{
  feed,
  <<"John Doe">>,
  <<"urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6">>,
  undefined,
  undefined,
  <<"http://example.org/">>,
  undefined,
  undefined,
  <<"Example Feed">>,
  <<"Sun, 18 May 2014 16:13:31 GMT">>,
  <<"http://example.org/feed/">>}, [
{
  entry,
  undefined,
  undefined,
  undefined,
  undefined,
  <<"urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a">>,
  undefined,
  <<"http://example.org/2003/12/13/atom03">>,
  undefined,
  <<"Some text.">>,
  <<"Atom-Powered Robots Run Amok">>,
  <<"Sun, 18 May 2014 16:13:31 GMT">>
}]}.
