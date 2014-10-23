-module(polyline).
-export([decode/1, eight_bit_chunks/1, six_bit_chunks/1, six_bit_chunk/1, split_up_six_bits/1, five_bit_chunks/1, bin_flip/1]).

%See https://developers.google.com/maps/documentation/utilities/polylinealgorithm


decode(Encoded_polyline) ->
	%Steps 11 back to 8
	Six_bit_chunks = six_bit_chunks(Encoded_polyline),
	%Step 8
	List_of_groups_of_chunks = split_up_six_bits(Six_bit_chunks),
	%Step 8 back to 6
	Five_bit_chunks = five_bit_chunks(List_of_groups_of_chunks),
	%---TODO
	%Maybe some more of the below need splitting out into different functions or nesting in a map?
	%Which option to go for, a function that maps as per five_bit_chunks, or mapping functions as per below?
	%I don't think I can map all functions in one go because ultimately need to change number of members in groups.
	%I.e. following will go from groups of five to eight.
	%---
	%Step 5
	Eight_bit_chunks = lists:map(
		fun(Group_of_chunks) ->
			eight_bit_chunks(Group_of_chunks)
		end,
		Five_bit_chunks),
	Results = lists:map(
		fun(Group_of_chunks) ->
			%TODO These should probably be separate functions, rather than one long mess inside a map
			Last_bit = [hd(lists:reverse(hd(lists:reverse(Group_of_chunks))))],
			Flipped_chunks = lists:map(
				fun(Chunk) ->
					bin_flip_(Chunk, [])
				end,
				Group_of_chunks),
			%Step 5
			Chunks = if Last_bit =:= "1" ->
				Flipped_chunks;
			true ->
				Group_of_chunks
			end,
			{ok, [Flattened_binary], []} = io_lib:fread("~2u", lists:flatten(Chunks)),
			%Step 4
			Shifted_binary = Flattened_binary bsr 1,
			%Since bin_flip returns a string need to then change back to a number
			{ok, [Shifted_binary_], []} = io_lib:fread("~2u", bin_flip(Shifted_binary - 1)),
			%Step 3
			Final_binary = if Last_bit =:= "1" ->
				Shifted_binary_;
			true ->
				Shifted_binary
			end,
			%Step 2 back to 1
			Decoded = if Last_bit =:= "1" ->
				-1 * Final_binary/100000;
			true ->
				Final_binary/100000	
			end,
			Decoded
		end,
		Eight_bit_chunks),
	Results.
	

%Step 8 - Split up six bit chunks, per the 0x20 bit
split_up_six_bits(List_of_bit_chunks) ->
	split_up_six_bits_(List_of_bit_chunks, [], []).
split_up_six_bits_([Head | Tail], Group_of_bit_chunks, List_of_groups_of_bit_chunks) when [hd(Head)] == "1" ->
	split_up_six_bits_(Tail, [Head]++Group_of_bit_chunks, List_of_groups_of_bit_chunks);
split_up_six_bits_([Head | Tail], Group_of_bit_chunks, List_of_groups_of_bit_chunks) when [hd(Head)] == "0" ->
	%Then need to start a new list, but after this 0 one!
	split_up_six_bits_(Tail, [], [lists:reverse([Head]++Group_of_bit_chunks)]++List_of_groups_of_bit_chunks);	
split_up_six_bits_([], Group_of_bit_chunks, List_of_groups_of_bit_chunks) when length(Group_of_bit_chunks) > 0 ->
	split_up_six_bits_([], [], [lists:reverse(Group_of_bit_chunks)]++List_of_groups_of_bit_chunks);
split_up_six_bits_([], [], List_of_groups_of_bit_chunks) ->
	%TODO Might be neater to map lists:reverse over the list instead of doing above and here.
	lists:reverse(List_of_groups_of_bit_chunks).


%Step 5
%TODO See if better way of doing this
eight_bit_chunks(List_of_five_bit_chunks) ->
	Five_bit_chunk_string = lists:reverse(lists:flatten(List_of_five_bit_chunks)),
	eight_bit_chunks_(Five_bit_chunk_string, []).
eight_bit_chunks_(Five_bit_chunk_string, List_of_eight_bit_chunks) when length(Five_bit_chunk_string) > 8 ->  
	Eight_bit_chunk = lists:reverse(lists:sublist(Five_bit_chunk_string,1,8)),
	Rest_of_five_bit_chunk_string = lists:nthtail(8,Five_bit_chunk_string),
	eight_bit_chunks_(Rest_of_five_bit_chunk_string, [Eight_bit_chunk]++List_of_eight_bit_chunks);
eight_bit_chunks_(Five_bit_chunk_string, List_of_eight_bit_chunks) when length(Five_bit_chunk_string) =< 8, Five_bit_chunk_string /= [] ->  
	Padded_bit_string = pad_to(8, lists:reverse(Five_bit_chunk_string)),
	eight_bit_chunks_([], [Padded_bit_string]++List_of_eight_bit_chunks);
eight_bit_chunks_([], List_of_eight_bit_chunks) ->
	List_of_eight_bit_chunks.


six_bit_chunks(Encoded_polyline) ->
	six_bit_chunks_(Encoded_polyline, []).
six_bit_chunks_([Head | Rest], List_of_chunks) ->
	Six_bit_chunk = six_bit_chunk(Head),
	%Add to Reversed_chunks
	six_bit_chunks_(Rest, [Six_bit_chunk]++List_of_chunks);
six_bit_chunks_([], List_of_chunks) ->
	lists:reverse(List_of_chunks).


six_bit_chunk(Ascii_bit) ->
	%Step 10
	Shifted_bit = Ascii_bit - 63,
	%Step 9
	%From http://erlangcentral.org/wiki/index.php/Converting_Between_Binary_and_Decimal
	Binary_chunk = hd(io_lib:format("~.2B", [Shifted_bit])),
	%---TODO
	%What if Binary_chunk is shorter than 6?
	%Well in that case, I guess that means we'd want to split, but for now pad to six and check for 0x20 elsewhere.
	%---
	pad_to(6, Binary_chunk).


five_bit_chunks(List_of_groups_of_chunks) ->
	lists:map(
		fun(Group_of_chunks) ->
			%Step 7 - Un-reverse the five bit chunks
			lists:reverse(lists:map(
				fun(Chunk) ->
					%Step 8 - "Un-or" the 0x20 bit
					lists:sublist(Chunk,2,6)
				end,
				Group_of_chunks))
		end,
		List_of_groups_of_chunks).


%I can't figure out padding with io:format etc when printing binary numbers
pad_to(Length, Binary_string) when length(Binary_string) < Length ->
	Padded_binary_string = "0"++Binary_string,
	pad_to(Length, Padded_binary_string);
pad_to(Length, Binary_string) when length(Binary_string) == Length ->
	Binary_string.
	

%bnot doesn't seem to work as I thought it would so do it very inelegantly by switching each "bit" in a string.
bin_flip(Binary_number) ->
	Binary_string =	hd(io_lib:format("~.2B", [Binary_number])),
	bin_flip_(Binary_string, []).
bin_flip_([Head | Rest], Flipped_string) ->
	Head_bit = hd(io_lib:format("~c",[Head])),
	Flipped_bit = if Head_bit =:= "0" ->
		"1";
	true ->
		"0"
	end,
	bin_flip_(Rest, Flipped_bit++Flipped_string);
bin_flip_([], Flipped_string) ->
	lists:reverse(Flipped_string).


%Notes
	%To enter numbers of a different base 2#101010
	%0x1f is 31, 0x20 is 32
	%Need to ensure functions are tail recursive
	%To convert to integer, just need to do $<char>
	%just do hd(String) will come as a number
	%Last = hd(io_lib:format("~c", [hd(lists:reverse(hd(io_lib:format("~.2B", [Binary_number]))))])),
	%{ok, Almost_flipped, _} = io_lib:fread("~2u",Almost_flipped_string),
