-module(headsilose).
-export([get_locations/0, get_locations/1, headsilose/2, headsilose/1]).
-include_lib("xmerl/include/xmerl.hrl").

%Supply a direction and location and work out if head wind or not
%For now "know the location id" upfront, but ideally need to search for it at some point or present a choice.

%Initially based on: http://pragdave.pragprog.com/pragdave/2007/04/a_first_erlang_.html 

-define(BASE_URL,
	"http://datapoint.metoffice.gov.uk/public/data/").

-define(WXFCS_SITELIST,
	"val/wxfcs/all/xml/sitelist").

-define(WXFCS_LOCATIONID,
	"val/wxfcs/all/xml/").


%First one for command line usage
get_locations([Search]) ->
	get_locations(Search);
get_locations(Search) ->
	get_locations_("//Location[contains(@name, '"++Search++"')]").
get_locations() ->
	get_locations_("//Location").
get_locations_(Xpath) ->
	inets:start(),
	Key = readapikey(),
	URL = ?BASE_URL ++ ?WXFCS_SITELIST ++ "?key=" ++ Key,
	try
		%Handling timeouts: http://stackoverflow.com/a/14143762/208793
		{ ok, {_Status, _Headers, Body }} = httpc:request(get, {URL, []}, [{timeout, timer:seconds(10)}], []),
		{ Xml, _Rest } = xmerl_scan:string(Body),
		%Print a list of Locations and IDs
		print_locations(xmerl_xpath:string(Xpath, Xml))
		%Strictly speaking this function shouldn't be here as it's recursive
		%but get_locations is a run once function so I think it's ok
		%See note above http://learnyousomeerlang.com/errors-and-exceptions#theres-more
	catch
        error:Reason ->
			io:format("API Might be down~n"),
			Reason
	end.


%Based on http://intertwingly.net/blog/2007/08/28/Parsing-Atom-with-Erlang
print_locations([]) ->
	end_of_list;
print_locations([Node|Rest]) ->
	%Need to do this recursively
	[ #xmlAttribute{value=Location} ] = xmerl_xpath:string("@name", Node),
	[ #xmlAttribute{value=ID} ] = xmerl_xpath:string("@id", Node),
	io:format(Location++", "++ID++"~n"),
	print_locations(Rest).


get_weather(Location) ->
	inets:start(),
	Key = readapikey(),
	URL = ?BASE_URL ++ ?WXFCS_LOCATIONID ++ Location ++ "?key=" ++ Key ++ "&res=3hourly",
	Date_today = erlang:localtime(),
	{ Date_formatted, Rep } = date_and_rep(Date_today),
	try
		{ ok, {_Status, _Headers, Body }} = httpc:request(get, {URL, []}, [{timeout, timer:seconds(10)}], []),
		{ Xml, _Rest } = xmerl_scan:string(Body),
		[ #xmlAttribute{value=Direction} ]  = xmerl_xpath:string("//Period[@value='" ++ Date_formatted ++ "']/Rep[.='" ++ Rep ++ "']/@D", Xml),
		[ #xmlAttribute{value=Speed} ]  = xmerl_xpath:string("//Period[@value='" ++ Date_formatted ++ "']/Rep[.='" ++ Rep ++ "']/@S", Xml),
		[ #xmlAttribute{value=Gust} ]  = xmerl_xpath:string("//Period[@value='" ++ Date_formatted ++ "']/Rep[.='" ++ Rep ++ "']/@G", Xml),
		{Direction, Speed, Gust}
	catch
        error:Reason ->
			io:format("API Might be down~n"),
			Reason
	end.



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
	%Thanks to: http://stackoverflow.com/a/7599506/208793
	DateAsString = io_lib:format("~4..0w-~2..0w-~2..0wZ", [Year, Month, Day]),
	lists:flatten(DateAsString).


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


%For command line usage, from http://stackoverflow.com/a/8498073/208793
headsilose([Location, Heading]) ->
	headsilose(Location, Heading).
headsilose(Location, Heading) ->
	{Direction, Speed, Gust} = get_weather(Location),
	Compass = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"],
	%Quicker dirtier(?) way to do below would be: http://stackoverflow.com/a/6762191/208793
	Index = length(lists:takewhile(fun(X) -> X  =/= Direction end, Compass))+1,
	%Since heading is to direction and winds are from, opposite is -2 to +2, or to make it easier to wrap, +14 +18
	%Since heading is to direction and winds are from, sidewinds are -5 to -3 and +3 to +5, or to make it easier to wrap, +14 +18
	%And so on
	HeadwindList = lists:seq(14,18),
	SidewindList = lists:seq(3,5)++lists:seq(11,13),
	TailwindList = lists:seq(6,10),
	Headwinds = lists:map(fun(X) -> nth_wrap(Index+X, Compass) end, HeadwindList),
	Sidewinds = lists:map(fun(X) -> nth_wrap(Index+X, Compass) end, SidewindList),
	Tailwinds = lists:map(fun(X) -> nth_wrap(Index+X, Compass) end, TailwindList),
	Headwind = lists:member(Heading, Headwinds),
	Sidewind = lists:member(Heading, Sidewinds),
	Tailwind = lists:member(Heading, Tailwinds),
	if Headwind ->
		io:format("Heads you lose!~n");
	Sidewind ->
		io:format("It's a draw~n");
	Tailwind ->
		io:format("Tails you win!~n")
	end,
	io:format("Direction: ~s, Speed: ~s mph, Gust: ~s mph~n", [Direction, Speed, Gust]).


%Need to read API key from file
readapikey() ->
	{_Status, KeyB} = file:read_file(os:getenv("HOME") ++ "/.datapoint"),
	string:strip(erlang:binary_to_list(KeyB),right,$\n).
