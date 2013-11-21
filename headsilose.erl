-module(headsilose).
%-export([headsilose/2]).
-export([readapikey/0]).

%Supply a direction and location and work out if head wind or not
%Should be relatively easy

%headsilose(Direction, Location) -> 

%Need to read API key from file
readapikey() ->
	{Status, KeyB} = file:read_file(os:getenv("HOME") ++ "/.datapoint"),
	string:strip(erlang:binary_to_list(KeyB),right,$\n).
