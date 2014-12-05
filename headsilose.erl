-module(headsilose).
-export([get_locations/0, get_locations/1, get_weather/1, headsilose/1]).
-include_lib("xmerl/include/xmerl.hrl").
-import(weather_types, [weather_type/1]).
-import(polyline, [decode/1]).
-import(osrm, [read_route/0]).

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
		_:Reason ->
			io:format("API Might be down~n"),
			Reason
	after
		maybe_quit()
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
		[ [ #xmlAttribute{value=Direction} ],
		  [ #xmlAttribute{value=Speed} ],
		  [ #xmlAttribute{value=Gust} ],
		  [ #xmlAttribute{value=Weather} ],
		  [ #xmlAttribute{value=Temperature} ] ] = lists:map(
			fun(X) ->
				xmerl_xpath:string("//Period[@value='" ++ Date_formatted ++ "']/Rep[.='" ++ Rep ++ "']/@"++X, Xml)
			end,
			["D", "S", "G", "W", "T"]),
		{Direction, Speed, Gust, Weather, Temperature}
	catch
		_:Reason ->
			io:format("API Might be down~n"),
			Reason
	after
		maybe_quit()
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
	Date_as_string = io_lib:format("~4..0w-~2..0w-~2..0wZ", [Year, Month, Day]),
	lists:flatten(Date_as_string).


nth_wrap(N, List) ->
	Rem = N rem (length(List)),
	if Rem > 0 ->
		lists:nth(Rem, List);
	Rem =:= 0 ->
		%Get last of list
		hd(lists:reverse(List))
	end.


find_next_day(Date_today) ->
	%get in seconds
	Seconds_today = calendar:datetime_to_gregorian_seconds(Date_today),
	Date_tomorrow = calendar:gregorian_seconds_to_datetime(Seconds_today+86400),
	Date_tomorrow.


maybe_quit() ->
	Args = init:get_arguments(), 
	Found = lists:keyfind(noshell, 1, Args),
	if Found =:= false ->
		dont_quit;
	true ->
		init:stop()
	end.


build_list_of_wind_directions(Wind_direction) ->
	%There is only one wind direction, but can build groups of directions that will count as head, side and tail winds
	Compass = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"],
	%Quicker dirtier(?) way to do below would be: http://stackoverflow.com/a/6762191/208793
	Index = length(lists:takewhile(fun(X) -> X  =/= Wind_direction end, Compass))+1,
	%Since heading is to direction and winds are from, opposite is -2 to +2, or to make it easier to wrap, +14 +18
	%Since heading is to direction and winds are from, sidewinds are -5 to -3 and +3 to +5, or to make it easier to wrap, +14 +18
	%And so on
	Headwind_list = lists:seq(14,18),
	Sidewind_list = lists:seq(3,5)++lists:seq(11,13),
	Tailwind_list = lists:seq(6,10),
	lists:map(
		fun(Wind_list) ->
			lists:map(
				fun(X) ->
					nth_wrap(Index+X, Compass)
				end,
				Wind_list)
		end,
		[Headwind_list, Sidewind_list, Tailwind_list]).


convert_lats_longs_to_distance_heading([_Head1 | [ _Head2 | Rest]])  -> 
	%All co-ords are diff, so just ignore first two
	convert_lats_longs_to_distance_heading_(Rest, []).
convert_lats_longs_to_distance_heading_([Lat | [Lon | Rest]], Distance_headings_list) ->
	%Want to map through the list convert co-ords to distance and heading
	Distance = math:sqrt(math:pow(Lat,2) + math:pow(Lon,2)),
	Heading_signed = math:atan2(Lon, Lat),
	%Need to convert heading into a 2π value
	Heading = if Heading_signed < 0 ->
				Heading_signed + 2*math:pi();
			true ->
				Heading_signed
			end,
	Compass_direction = get_compass_direction_for(Heading),
	convert_lats_longs_to_distance_heading_(Rest, [{Distance, Compass_direction}]++Distance_headings_list);
convert_lats_longs_to_distance_heading_([], Distance_headings_list) ->
	lists:reverse(Distance_headings_list).


get_compass_direction_for(Heading) ->
	%In a way this is a waste of time as could just do headwind, etc based on angles, but since already have some code, why not?
	Segment = 2*math:pi()/16,
	Segments = erlang:round(Heading/Segment),
	Compass = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"],
	lists:nth(Segments, Compass).


head_side_or_tail_wind(Direction, [Headwinds, Sidewinds, Tailwinds]) ->
	[Headwind, Sidewind, Tailwind] = lists:map(
		fun(Winds) ->
			lists:member(Direction, Winds)
		end,
		[Headwinds, Sidewinds, Tailwinds]),
	if Headwind ->
		headwind;
	Sidewind ->
		sidewind;
	Tailwind ->
		tailwind
	end.
%Something like that?
	

headsilose(Location) ->
	{Direction, Speed, Gust, Weather, Temperature} = get_weather(Location),
	Weather_type = weather_types:weather_type(erlang:list_to_integer(Weather)),
	[Headwinds, Sidewinds, Tailwinds] = build_list_of_wind_directions(Direction),
	%Ok so now map this list over (like previous function) the headings list to get list of head, side, tail and distances?

	{_Checksum, Polyline} = osrm:read_route(),
	Polyline_decoded = polyline:decode(Polyline),
	Distances_and_compass_list = convert_lats_longs_to_distance_heading(Polyline_decoded),
	%If now have a set of co-ords need to figure out distances and directions
	Distances_and_wind_type_list = lists:map(
		fun({Distance, Compass}) ->
			Wind_type = head_side_or_tail_wind(Compass, [Headwinds, Sidewinds, Tailwinds]),
			{Distance, Wind_type}
		end,
		Distances_and_compass_list),
	[Headwind_distances, Sidewind_distances, Tailwind_distances] = lists:map(
		fun(Wind_type_filter) ->
			lists:filter(
				fun({_Distance, Wind_type}) ->
					Wind_type == Wind_type_filter
				end,
				Distances_and_wind_type_list)
		end,
		[headwind, sidewind, tailwind]),
	[Sum_of_headwind_distances, Sum_of_sidewind_distances, Sum_of_tailwind_distances] = lists:map(
		fun(Wind_type) ->
			lists:foldl(
				fun({Distance, _Wind}, Sum) ->
					Distance + Sum
				end,
				0,
				Wind_type)
		end,
		[Headwind_distances, Sidewind_distances, Tailwind_distances]).
	%io:format("Direction: ~s~nSpeed: ~s mph~nGust: ~s mph~nWeather type: ~s~nTemperature: ~s deg C~n", [Direction, Speed, Gust, Weather_type, Temperature]).

%Need to read API key from file
readapikey() ->
	{_Status, Key} = file:read_file(os:getenv("HOME") ++ "/.datapoint"),
	string:strip(erlang:binary_to_list(Key),right,$\n).
