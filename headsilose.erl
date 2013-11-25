-module(headsilose).
%-export([headsilose/2]).
-export([readapikey/0, get_locations/0, get_weather/1]).
-include_lib("xmerl/include/xmerl.hrl").

%Supply a direction and location and work out if head wind or not
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


find_next_day(Date_today) ->
	%get in seconds
	Seconds_today = calendar:datetime_to_gregorian_seconds(Date_today),
	Date_tomorrow = calendar:gregorian_seconds_to_datetime(Seconds_today+86400),
	Date_tomorrow.


%Need to read API key from file
readapikey() ->
	{Status, KeyB} = file:read_file(os:getenv("HOME") ++ "/.datapoint"),
	string:strip(erlang:binary_to_list(KeyB),right,$\n).
