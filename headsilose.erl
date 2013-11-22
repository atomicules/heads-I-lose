-module(headsilose).
%-export([headsilose/2]).
-export([readapikey/0, get_locations/0]).

%Supply a direction and location and work out if head wind or not
%Should be relatively easy
%For now "know the location id" upfront, but ideally need to search for it at some point or present a choice.

%headsilose(Direction, Location) -> 

%Based on: http://pragdave.pragprog.com/pragdave/2007/04/a_first_erlang_.html 

-define(BASE_URL,
	"http://datapoint.metoffice.gov.uk/public/data/").

-define(SITELIST,
	   "val/wxfcs/all/datatype/sitelist").

datapoint_url(Location) ->
	?BASE_URL ++ ?SITELIST ++ "?key=" ++ readapikey.


get_locations() ->
	inets:start(),
	Key = readapikey(),
	URL = ?BASE_URL ++ ?SITELIST ++ "?key=" ++ Key,
	{ ok, {_Status, _Headers, Body }} = httpc:request(URL),
	Body.

get_weather(Location) ->
	URL = "g",
	{ ok, {_Status, _Headers, Body }} = httpc:request(URL),
	Body.

%Need to read API key from file
readapikey() ->
	{Status, KeyB} = file:read_file(os:getenv("HOME") ++ "/.datapoint"),
	string:strip(erlang:binary_to_list(KeyB),right,$\n).
