-module(osrm).
-export([get_route/1, get_route/2, read_route/1]).
-import(polyline, [decode/1]).

%https://github.com/Project-OSRM/osrm-backend/wiki/Server-api
%For now, get weather for one location (probably good enough as relatively short distances weather wise; ultimately consider time as well?)
%To get lats and longs could also do a query for here: http://www.uk-postcodes.com/ (json again)

-define(BASE_URL,
	"http://router.project-osrm.org/").

-define(VIAROUTE,
	"viaroute").

%Need hints and checksum
%First one for command line usage
%%These will be strings from the command line
get_route([Start_lat, Start_lon, Finish_lat, Finish_lon]) ->
	get_route([Start_lat, Start_lon], [Finish_lat, Finish_lon]).
get_route([Start_lat, Start_lon], [Finish_lat, Finish_lon]) ->
	inets:start(),
	URL = ?BASE_URL ++ ?VIAROUTE ++ "?loc=" ++ Start_lat ++ "," ++ Start_lon ++ "&loc=" ++ Finish_lat ++ "," ++ Finish_lon,
	%Need UA to work with OSRM API
	UA = "Mozilla/5.0 (X11; NetBSD i386; rv:28.0) Gecko/20100101 Firefox/28.0",
	try
		%Handling timeouts: http://stackoverflow.com/a/14143762/208793
		{ ok, {_Status, _Headers, Body }} = httpc:request(get, {URL, [{"User-Agent", UA}]}, [{timeout, timer:seconds(10)}], []),
		%For development purposes, write this out and keep it
		_Write_status = file:write_file(os:getenv("HOME") ++ "/.headsilose_route", Body),
		%Need to catch file write errors as well
		Body
	catch
		_:Reason ->
			io:format("API Might be down~n"),
			Reason
	after
		maybe_quit()
	end.


read_route(Route_choice) ->
	{_Status, Route} = file:read_file(os:getenv("HOME") ++ "/.headsilose_route"),
	%Use jiffy
	%http://www.snip2code.com/Snippet/51463/how-to-support-chinese-in-http-request-b/
	{ Props } = jiffy:decode(Route),
	Route_geometry = if Route_choice =:= "route_geometry" ->
		proplists:get_value(binary:list_to_bin(Route_choice), Props);
	Route_choice =:= "alternative_geometries" ->
		%hd only if alternative though!
		%for now I think there is only ever one alternative so that is why we pick hd
		hd(proplists:get_value(binary:list_to_bin(Route_choice), Props))
	end,
	%That is all for now? Because...
	%And these don't seem to actually be returned, hence having to go down the route of polyline decoding
	%Route_Instructions = proplists:get_value(<<"route_instructions">>, Props),
	%Total_Distance = proplists:get_value(<<"total_distance">>, proplists:get_value(<<"route_summary">>, Props)),
	%And need to figure out how to get nested values. I.e like xpath. Just nest the queries? Nope that doesn't work
	%Like so:
	{Hint_data} = proplists:get_value(<<"hint_data">>, Props),
	Checksum = proplists:get_value(<<"checksum">>, Hint_data),
	Route_geometry_as_string = binary:bin_to_list(Route_geometry),
	{Checksum, Route_geometry_as_string}.


maybe_quit() ->
	Args = init:get_arguments(),
	Found = lists:keyfind(noshell, 1, Args),
	if Found =:= false ->
		dont_quit;
	true ->
		init:stop()
	end.
