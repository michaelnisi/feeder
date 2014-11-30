
-module(atom).
-export([wanted/0]).

wanted() -> {#{
  author => <<"John Doe">>,
  id => <<"urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6">>,
  image => nil,
  link => <<"http://example.org/">>,
  language => nil,
  subtitle => nil,
  summary => nil,
  title => <<"Example Feed">>,
  updated => <<"Sun, 18 May 2014 16:13:31 GMT">>}, [
#{
  author => nil,
  enclosure => nil,
  id => <<"urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a">>,
  image => nil,
  link => <<"http://example.org/2003/12/13/atom03">>,
  duration => nil,
  subtitle => nil,
  summary => <<"Some text.">>,
  title => <<"Atom-Powered Robots Run Amok">>
}]}.
