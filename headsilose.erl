-module(headsilose).
%-export([headsilose/2]).
-export([readapikey/0, get_locations/0, get_weather/1]).
-include_lib("xmerl/include/xmerl.hrl").

%Supply a direction and location and work out if head wind or not
%Should be relatively easy
%For now "know the location id" upfront, but ideally need to search for it at some point or present a choice.

%headsilose(Direction, Location) -> 

%Based on: http://pragdave.pragprog.com/pragdave/2007/04/a_first_erlang_.html 

-define(BASE_URL,
	"http://datapoint.metoffice.gov.uk/public/data/").

-define(WXFCS_SITELIST,
	"val/wxfcs/all/xml/sitelist").

-define(WXFCS_LOCATIONID,
	"val/wxfcs/all/xml/").


get_locations() ->
	inets:start(),
	Key = readapikey(),
	URL = ?BASE_URL ++ ?WXFCS_SITELIST ++ "?key=" ++ Key,
	{ ok, {_Status, _Headers, Body }} = httpc:request(URL),
	Body.

get_weather(Location) ->
	inets:start(),
	Key = readapikey(),
	URL = ?BASE_URL ++ ?WXFCS_LOCATIONID ++ Location ++ "?key=" ++ Key ++ "&res=3hourly",
	{ ok, {_Status, _Headers, Body }} = httpc:request(URL),
	{ Xml, _Rest } = xmerl_scan:string(Body),
	[ #xmlAttribute{value=Direction} ]  = xmerl_xpath:string("//Period[@value='2013-11-28Z']/Rep[.='180']/@D", Xml),
	Direction.

%Todo:
% Need to get current time and depnding on before/after 8am 5pm then analyse the evening/morning direction
% Need somethng that understands compass directions
%{{Year, Month, Day}, {Hours, _Minutes, _Seconds}} = erlang:localtime()
%if ( Hours < 8 ) %then that day
%if ( Hours < 19 ) ) %assume want home
%if ( Hours > 19 ) ) %assume want next day weather
% Change xpath filter basically

%Need to read API key from file
readapikey() ->
	{Status, KeyB} = file:read_file(os:getenv("HOME") ++ "/.datapoint"),
	string:strip(erlang:binary_to_list(KeyB),right,$\n).
