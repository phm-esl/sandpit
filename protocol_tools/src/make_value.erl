-module(make_value).

-compile([export_all]).

-export([test/0]).

-export(
 [ date_time/0
 , date/0
 , time/0
 , year/0
 , year_month/0
 , base64/2
 , boolean/0
 , from_regexp/1
 , pick_random/1]).

-define(log(F,A),io:format("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).

-define(MAX,64).

unicode_range() ->
  % Excluding modifiers & combining,
  % TODO: symbols, other languages, Semitic, Arabic, African, American etc...
  [ {32,128} % ASCII
  , {16#00a0,16#02b0} % Latin, IPA
  , {16#0370,16#0400} % Greek, Coptic
  , {16#0400,16#0500} % Cyrillic
  , {16#1e00,16#1f00} % Latin extended
  , {16#3000,16#3040} % Japanese punctuation
  , {16#3040,16#30a0} % Hiragana
  , {16#30a0,16#3100} % Katakana
  , {16#4e00,16#9fb0} % CJK ideographs
  , {16#ff00,16#fff0} % Romanjii, half-width katakana
  ].

date_time() ->
  Now = erlang:timestamp(),
  {{Yr,Mo,Dy},{Hr,Mn,Sc}} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary(
      [pad4(Yr),$-,pad2(Mo),$-,pad2(Dy),$T,
       pad2(Hr),$:,pad2(Mn),$:,pad2(Sc) ]).

date() ->
  Now = erlang:timestamp(),
  {{Yr,Mo,Dy},_} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary([ pad4(Yr),$-,pad2(Mo),$-,pad2(Dy) ]).

time() ->
  Now = erlang:timestamp(),
  {_,{Hr,Mn,Sc}} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary([ pad2(Hr),$:,pad2(Mn),$:,pad2(Sc) ]).

year() ->
  Now = erlang:timestamp(),
  {{Yr,_,_},_} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary([ pad4(Yr) ]).

year_month() ->
  Now = erlang:timestamp(),
  {{Yr,Mo,_},_} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary([ pad4(Yr),$-,pad2(Mo) ]).

base64(Min,Max) ->
  % NOTE The encoded output base64:encode
  %      is 33% larger than input binary,
  %      hence scale input by 2/3.
  Len = 2 * (Min + rand:uniform(Max - Min)) div 3,
  base64:encode(rand:bytes(Len)).

boolean() ->
  element(rand:uniform(2), {[<<"FALSE">>], [<<"TRUE">>]}).

from_regexp(Pattern) ->
  try
    {_,Generators} = parse_pattern(0,Pattern,[]),
    unicode:characters_to_binary(generate(Generators))
  catch What:Why:Where ->
    throw(
    #{ pattern => Pattern,
       what => What,
       why => Why,
       where => Where}) end.

generate(In) -> generate(In,[]).

generate([],Out) -> Out;
generate([Bin|Rest],Out) when is_binary(Bin); is_integer(Bin) ->
  generate(Rest,[Bin|Out]);
generate([Gen|Rest],Out) when is_function(Gen) ->
  generate(Rest,[Gen()|Out]);
generate([Alternation|Rest],Out) when is_list(Alternation) ->
  Gen = pick_random(Alternation),
  generate([Gen|Rest],Out).


parse_pattern(Pos,Pattern,Out) when Pos < size(Pattern) ->
  case Pattern of
    << _:Pos/binary, $|, _/binary >> -> % alternation
      gen_alternation(Pos + 1,Pattern,Out);

    << _:Pos/binary, $(, _/binary >> -> % grouping begin
      {End,Gen} = gen_group(Pos + 1,Pattern),
      parse_pattern(End,Pattern,[Gen|Out]);
    << _:Pos/binary, $), _/binary >> -> % grouping finish
      {Pos + 1,Out};

    << _:Pos/binary, $[, $^, _/binary >> -> % negated set
      {End,Gen} = negated_range(Pos + 2, Pattern),
      parse_pattern(End,Pattern,[Gen|Out]);

    << _:Pos/binary, $[, _/binary >> -> % character set
      {End,Gen} = gen_range(Pos + 1,Pattern),
      parse_pattern(End,Pattern,[Gen|Out]);
    << _:Pos/binary, ${, _/binary >> -> % repetition range
      {End,Gen} = gen_repeat(Pos + 1,Pattern,hd(Out)),
      parse_pattern(End,Pattern,[Gen|tl(Out)]);

    << _:Pos/binary, $+, _/binary >> -> % repetition >= 1
      Gen = gen_repeat_fun(1,?MAX,hd(Out)),
      parse_pattern(Pos + 1,Pattern,[Gen|tl(Out)]);
    << _:Pos/binary, $*, _/binary >> -> % repetition >= 0
      Gen = gen_repeat_fun(0,?MAX,hd(Out)),
      parse_pattern(Pos + 1,Pattern,[Gen|tl(Out)]);

    << _:Pos/binary, $\\, Gen, _/binary >> ->
      parse_pattern(Pos + 2,Pattern,[Gen|Out]);

    << _:Pos/binary, $., _/binary >> ->
      Gen = gen_dot_char(),
      parse_pattern(Pos + 1,Pattern,[Gen|Out]);

    << _:Pos/binary, _/binary >> ->
      {End,Gen} = gen_literal(Pos,Pattern),
      parse_pattern(End,Pattern,[Gen|Out]) end;
parse_pattern(Pos,_,Out) -> {Pos,Out}.



gen_alternation(Pos,Pattern,[Head|Tail]) ->
  {End,Gen} = parse_pattern(Pos,Pattern,[]),
  if is_list(Head) -> {End,[[Gen|Head]|Tail]};
     is_binary(Head) -> {End,[[Gen,Head]|Tail]};
     is_function(Head) -> {End,[[Gen,Head]|Tail]} end.


gen_literal(Pos,Pattern) -> gen_literal(Pos,0,Pattern).

gen_literal(Pos,Len,Pattern) ->
  N = Pos + Len,
  if N < size(Pattern) ->
    case Pattern of
      << _:N/binary, X, _/binary >> ->
        if X =:= ${ ; X =:= $} ;
           X =:= $( ; X =:= $) ;
           X =:= $[ ; X =:= $] ;
           X =:= $+ ; %%%% one-or-more repetition
           X =:= $* ; %%%% zero-or-more repetiton
           X =:= $| ;
           X =:= $\\ ;
           X =:= $. ->
             Part = binary:part(Pattern,Pos,Len),
            {Pos + Len,Part};
           N < size(Pattern) -> gen_literal(Pos,Len + 1,Pattern) end end;
    true -> Part = binary:part(Pattern,Pos,Len),
            {Pos + Len,Part} end.


gen_group(Pos,Pattern) ->
  {End,Gen} = parse_pattern(Pos,Pattern,[]),
  {End,fun () -> generate(Gen) end}.


negated_range(Pos,Pattern) -> begin_range(Pos,Pattern,[negated]).

gen_range(Pos,Pattern) -> begin_range(Pos,Pattern,[]).

begin_range(Pos,Pattern,Tail) ->
  % Boolean true is include, false is exclude.
  case Pattern of
    << _:Pos/binary, $], _/binary >> ->
      % literal ']' included into set
      gen_each_range(Pos + 1,Pattern,[{true,[$]]}|Tail]);
    _ -> gen_each_range(Pos,Pattern,[{true,[]}|Tail]) end.

gen_each_range(Pos,Pattern,[{Flag,[B,$-,A|Out]}|Rest]) ->
  % expand shorthand to add to the Out list all characters in the sequence.
  % Add one because Min =< range < Max in number_set.
  gen_each_range(Pos,Pattern,[{Flag,[{A,B + 1}|Out]}|Rest]);

gen_each_range(Pos,Pattern,Ranges) when Pos < size(Pattern) ->
  [{Flag,Out}|Rest] = Ranges,
  case Pattern of
    << _:Pos/binary, $\\, A, _/binary >> ->
      gen_each_range(Pos + 2,Pattern,[{Flag,[A|Out]}|Rest]);

    << _:Pos/binary, $], _/binary >> ->
      finish_range(Pos,Pattern,Ranges);

    << _:Pos/binary, $-, $[, _/binary >> ->
      gen_each_range(Pos + 2,Pattern,[{not Flag,[]}|Ranges]);

    << _:Pos/binary, C, _/binary >> ->
      gen_each_range(Pos + 1, Pattern,[{Flag,[C|Out]}|Rest]) end.

finish_range(Pos,Pattern,Ranges) ->
  case lists:reverse(Ranges) of
    [negated|Char_set] ->
      {End,Out} = finish_each_range(Pos,Pattern,Char_set,[]),
      % Negated sets are subset of printable Unicode, for now...
      Negated = number_set:remove(Out,unicode_range()),
      Gen = fun () -> pick_from_range(Negated) end,
      {End,Gen};
     Char_set ->
      {End,Out} = finish_each_range(Pos,Pattern,Char_set,[]),
      Gen = fun () -> pick_from_range(Out) end,
      {End,Gen} end.

finish_each_range(Pos,_,[],Out) ->
  {Pos,Out};
finish_each_range(Pos,Pattern,[{Add,Each}|Ranges],Out) ->
  case Pattern of
    << _:Pos/binary, $], _/binary >> when Add ->
      finish_each_range(Pos + 1,Pattern,Ranges,number_set:insert(Each,Out));
    << _:Pos/binary, $], _/binary >> when not Add ->
      finish_each_range(Pos + 1,Pattern,Ranges,number_set:remove(Each,Out)) end.

% NOTE: result excludes Max.
% Low =< pick_from_range(Range) < High
pick_from_range(Range) when is_list(Range) ->
  pick_from_range(pick_random(Range));
pick_from_range({Low,High}) ->
  Low + rand:uniform(High - Low) - 1.





gen_repeat(Pos,Pattern,Gen) ->
  gen_repeat(Pos,0,Pattern,Gen).

gen_repeat(Pos,N,Pattern,Gen) when Pos < size(Pattern) ->
  case Pattern of
    << _:Pos/binary, $}, _/binary >> ->
      {Pos + 1, gen_repeat_fun(N,Gen)};
    << _:Pos/binary, $,, _/binary >> ->
      gen_repeat_min_max(Pos + 1,N,0,Pattern,Gen);
    << _:Pos/binary, X, _/binary >> when $0 =< X, X =< $9 ->
      gen_repeat(Pos + 1,10 * N + X - $0,Pattern,Gen) end.

gen_repeat_min_max(Pos,Min,Max,Pattern,Gen) when Pos < size(Pattern) ->
  case Pattern of
    << _:Pos/binary, $}, _/binary >> ->
      {Pos + 1, gen_repeat_fun(Min,Max,Gen)};
    << _:Pos/binary, X, _/binary >> when $0 =< X, X =< $9 ->
      gen_repeat_min_max(Pos + 1,Min,10 * Max + X - $0,Pattern,Gen) end.


gen_repeat_fun(N,Gen) ->
  fun () -> (repeat(Gen))(N) end.

gen_repeat_fun(N,N,Gen) when 0 =< N ->
  fun () -> (repeat(Gen))(N) end;
gen_repeat_fun(Min,Max,Gen) when 0 =< Min, Min < Max ->
  fun () -> (repeat(Gen))(random_integer(Min,Max)) end.

random_integer(Min,Max) -> Min + rand:uniform(Max - Min + 1) - 1.

repeat(Bin) when is_binary(Bin) ->
  Size = byte_size(Bin), % TODO: deal with Unicode
  Char = binary_part(Bin,Size,-1),
  Rest = binary_part(Bin,0,Size - 1),
  Loop = fun
    Fn(0,Out) -> lists:reverse(Out);
    Fn(N,Out) when N > 0 -> Fn(N-1,[Char|Out]) end,
  fun (N) -> Loop(N,[Rest]) end;
repeat(Gen) when is_function(Gen) ->
  fun Fn(0) -> []; Fn(N) when N > 0 -> [Gen()|Fn(N-1)] end.







pad2(N) ->
  if N >= 10-> erlang:integer_to_binary(N);
     0 =< N, N < 10 -> [ $0,erlang:integer_to_binary(N) ] end.

pad4(N) ->
  if N >= 1000 -> erlang:integer_to_binary(N);
     N >= 100 -> [ $0, erlang:integer_to_binary(N)];
     true -> [ "00", pad2(N)] end.






pick_random(Choice) when is_list(Choice) ->
  Length = length(Choice),
  N = rand:uniform(Length),
  lists:nth(N,Choice).


gen_dot_char() ->
  % TODO: Unicode UTF8 byte sequences
  fun Fn() ->
    Out = pick_from_range(unicode_range()), % Subset of printable Unicode
    if Out =:= $\n -> Fn(); % Do again if newline. TODO Unicode line breaks?
       true -> Out end end.

%
% Source: https://www.regular-expressions.info/xml.html
%
%  XML Schema regular expressions support the following:
%
%    Character classes,
%      NO: including shorthands,
%      YES: ranges and
%      YES: negated classes.
%    YES: Character class subtraction, with nested subtractions.
%    YES: The dot, which matches any character except line breaks.
%    YES: Alternation and groups.
%    YES: Greedy quantifiers ?, *, + and {n,m}
%    NO: Unicode properties and blocks
%
test() ->
  Patterns =
  [ <<"[b-df-hj-np-tv-z]">>, % simple character class ranges
    <<"[\s-~-[0-9A-Za-z-[aeiou]]]">>, % character class subtraction (re:run fails)
%    <<"<\\i\\c*\\s*>">>, % XML-only shorthand
    <<"[]]">>,
    <<"[^]]">>,
    <<"[^0-9A-Za-z]">>,
    <<"wx*">>,
    <<"(wx)*">>,
    <<"wx+">>,
    <<".*">>,
    <<"[\\[]{0,1}">>,
    <<"(Get|Set)(Value){0,1}">>,
    <<"[A-Z]{3,3}">>,
    <<"[A-Z0-9]{4,4}[A-Z]{2,2}[A-Z0-9]{2,2}([A-Z0-9]{3,3}){0,1}">>,
    <<"[A-Z]{2,2}">>,
    <<"[0-9]{2}">>,
    <<"[a-zA-Z0-9]{4}">>,
    <<"[A-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}">>,
    <<"[A-Z0-9]{18,18}[0-9]{2,2}">>,
    <<"[0-9]{1,15}">>,
    <<"\\+[0-9]{1,3}-[0-9()+\\-]{1,30}">>,
    <<"[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}"
      "-[89ab][a-f0-9]{3}-[a-f0-9]{12}">>],
  Strings = [ {make_value:from_regexp(P),P} || P <- Patterns ],
  [ {test_re(S,P),S,P} || {S,P} <- Strings ].

test_re(S,P) ->
  case re:run(S,<< $^, P/binary, $$ >>) of
    nomatch -> false;
    {match,[{0,N}|_]} -> true = ( N == size(S) ) end.
