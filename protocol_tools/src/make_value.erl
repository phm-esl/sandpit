-module(make_value).

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
    erlang:list_to_binary(generate(Generators))
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
      throw(negated_character_set_not_implemented);
    << _:Pos/binary, $[, _/binary >> -> % character set
      {End,Gen} = gen_range(Pos + 1,Pattern),
      parse_pattern(End,Pattern,[Gen|Out]);
    << _:Pos/binary, ${, _/binary >> -> % repetition range
      ?log("hd(Out) -> ~p.~n",[hd(Out)]),
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

gen_range(Pos,Pattern) -> gen_range(Pos,Pattern,[]).

gen_range(Pos,Pattern,[B,$-,A|Out]) ->
  % expand shorthand to add to the Out list all characters in the sequence.
  gen_range(Pos,Pattern,gen_seq(A,B,Out));

gen_range(Pos,Pattern,Out) when Pos < size(Pattern) ->
  case Pattern of
    << _:Pos/binary, $\\, A, _/binary >> ->
      gen_range(Pos + 2,Pattern,[A|Out]);
    << _:Pos/binary, $], _/binary >> ->
      {Pos + 1, fun () -> pick_random(Out) end};
    << _:Pos/binary, $-, $[, _/binary >> ->
      negate_range(Pos + 2,Pattern,Out);
    << _:Pos/binary, C, _/binary >> ->
      gen_range(Pos + 1, Pattern,[C|Out]) end.

gen_seq(A,B,Out) when A < B -> gen_seq(A + 1, B, [A|Out]);
gen_seq(B,B,Out) -> [B|Out].

negate_range(Pos,Pattern,Out) ->
  negate_range(Pos,Pattern,lists:usort(Out),[]).

negate_range(Pos,Pattern,Within,[B,$-,A|Without]) ->
  negate_range(Pos,Pattern,Within,gen_seq(A,B,Without));

negate_range(Pos,Pattern,Within,Without) ->
  % parse pattern to remove from the Out list any characters that are negated.
  case Pattern of
    << _:Pos/binary, $\\, A, _/binary >> ->
      negate_range(Pos,Pattern,Within,[A|Without]);
    << _:Pos/binary, $-, $[, _/binary >> ->
      % TODO: nested character class subtraction...
      throw( 'TODO' );
    << _:Pos/binary, $], $], _/binary >> ->
      % TODO: support recursion for nesting...
      Out = lists:subtract(Within,Without),
      io:format("Within = ~p.~nWithout = ~p~nOut = ~p.~n",[Within,Without,Out]),
      {Pos + 2, fun () -> pick_random(Out) end};
    << _:Pos/binary, C, _/binary >> ->
      negate_range(Pos + 1, Pattern,Within,[C|Without]) end.



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
    Out = random_integer($\s,$~),
    if Out =:= $\n -> Fn(); % Do again if newline. TODO Unicode line breaks?
       true -> Out end end.


%
%  XML Schema regular expressions support the following:
%
%    Character classes,
%      NO: including shorthands,
%      YES: ranges and
%      NO:  negated classes.
%    PART: Character class subtraction (to one nested level).
%    YES: The dot, which matches any character except line breaks.
%    YES: Alternation and groups.
%    YES: Greedy quantifiers ?, *, + and {n,m}
%    NO: Unicode properties and blocks
%
test() ->
  Patterns =
  [ <<"[b-df-hj-np-tv-z]">>, % simple character class ranges
    <<"[a-z-[aeiou]]">>, % character class subtraction (re:run fails)
%    <<"<\\i\\c*\\s*>">>, % XML-only shorthand
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
