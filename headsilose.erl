-module(headsilose).
-export([readapikey/0, get_locations/0, get_weather/1, direction_is_opposite/2, headsilose/2]).
-include_lib("xmerl/include/xmerl.hrl").

%Supply a direction and location and work out if head wind or not
%For now "know the location id" upfront, but ideally need to search for it at some point or present a choice.

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


headsilose(Location, Heading) ->
	Wind = get_weather(Location),
	Direction_is_opposite = direction_is_opposite(Heading, Wind),
	if Direction_is_opposite ->
		heads_you_lose;
	true -> 
		tails_you_win
	end.


get_weather(Location) ->
	inets:start(),
	Key = readapikey(),
	URL = ?BASE_URL ++ ?WXFCS_LOCATIONID ++ Location ++ "?key=" ++ Key ++ "&res=3hourly",
	{ ok, {_Status, _Headers, Body }} = httpc:request(URL),
	{ Xml, _Rest } = xmerl_scan:string(Body),
	Date_today = erlang:localtime(),
	{ Date_formatted, Rep } = date_and_rep(Date_today),
	% D is wind direction
	% G is gust
	% S is speed
	[ #xmlAttribute{value=Direction} ]  = xmerl_xpath:string("//Period[@value='" ++ Date_formatted ++ "']/Rep[.='" ++ Rep ++ "']/@D", Xml),
	Direction.


date_and_rep(Date) ->
	{{_Year, _Month, _Day}, {Hours, _Minutes, _Seconds}} = Date,
	date_and_rep(Date, Hours).
date_and_rep(Date, Hours) when Hours < 8 ->
	Rep = "360",
	{format_date(Date), Rep};
date_and_rep(Date, Hours) when Hours >= 8, Hours < 19 ->
	Rep = "1080",
	{format_date(Date), Rep};
date_and_rep(Date, Hours) when Hours >= 19 ->
	Rep = "360",
	{format_date(find_next_day(Date)), Rep}.


format_date(Date_to_format) ->
	{{Year, Month, Day}, {_Hours, _Minutes, _Seconds}} = Date_to_format,
	Thing = erlang:integer_to_list(Year) ++ "-" ++ erlang:integer_to_list(Month) ++ "-" ++ erlang:integer_to_list(Day) ++ "Z",
	Thing.


nth_wrap(N, List) ->
	Rem = N rem (length(List)),
	if Rem > 0 ->
	   lists:nth(Rem, List);
	Rem =:= 0 ->
		1
	end.


find_next_day(Date_today) ->
	%get in seconds
	Seconds_today = calendar:datetime_to_gregorian_seconds(Date_today),
	Date_tomorrow = calendar:gregorian_seconds_to_datetime(Seconds_today+86400),
	Date_tomorrow.


direction_is_opposite(Heading, Wind) ->
	Compass = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"],
	%Quicker dirtier(?) way to do below would be: http://stackoverflow.com/a/6762191/208793
	Index = length(lists:takewhile(fun(X) -> X  =/= Wind end, Compass))+1,
	%Since heading is to direction and winds are from, opposite is -2 to +2
	%Or to make it easier to wrap, +14 +18
	Opposites = [nth_wrap(Index+14, Compass),
	             nth_wrap(Index+15, Compass),
	             nth_wrap(Index+16, Compass),
	             nth_wrap(Index+17, Compass),
	             nth_wrap(Index+18, Compass)],
	%Yuck! Must be better way to build that
	lists:member(Heading, Opposites).


%Need to read API key from file
readapikey() ->
	{Status, KeyB} = file:read_file(os:getenv("HOME") ++ "/.datapoint"),
	string:strip(erlang:binary_to_list(KeyB),right,$\n).
