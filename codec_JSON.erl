-module(codec_JSON).

% Suppress compiler warning in case of both
% ATOMIC_NAMES and ATOMIC_VALUES are undefined:
-compile([{nowarn_unused_function,atomic/1}]).

%%%
%%%  https://www.rfc-editor.org/rfc/rfc8259.txt
%%%

-export(
  [ encode/1
  , decode/1
  , test/0
  , test_fragments/0 ]).

-define(backspace,$\b). %  8
-define(tab,$\t).       %  9
-define(newline,$\n).   % 10
-define(formfeed,$\f).  % 12
-define(return,$\r).    % 13
-define(space,$\s).     % 32
-define(quote,$").      % 34
-define(plus,$+).       % 43
-define(comma,$,).      % 44
-define(minus,$-).      % 45
-define(decimal_point,$.). % 46
-define(slash,$/).      % 47
-define(open_square,$\[).
-define(close_square,$\]).
-define(open_brace,${).
-define(close_brace,$}).
-define(colon,$:).      % 58
-define(backslash,$\\). % 92

-define(TRUE_STRING,"true").
-define(FALSE_STRING,"false").
-define(NULL_STRING,"null").

-define(TRUE_BINARY,<< ?TRUE_STRING >>).
-define(FALSE_BINARY,<< ?FALSE_STRING >>).
-define(NULL_BINARY,<< ?NULL_STRING >>).

-define(ATOMIC_NAMES,1).
% When ATOMIC_NAMES is defined, use existing atoms
% as object field namess to replace decoded binary values.

-define(ATOMIC_VALUES,1).
% When ATOMIC_VALUES is defined, use existing atoms
% as values to replace decoded binary values.

-define(ESCAPED_STRINGS,1).
% When ESCAPED_STRINGS is defined, the escape sequences such as \uXXXX for
% Unicode and other back-slash sequences are converted.

encode(In) ->
to_binary(
  case In of
     Array when     is_tuple(Array) ->       encode_array(In);
    Object when      is_map(Object) ->      encode_object(In);
    String when     is_list(String) ->      encode_string(In);
    String when   is_binary(String) ->      encode_string(In);
   Integer when is_integer(Integer) ->  integer_to_binary(In);
     Float when     is_float(Float) ->    float_to_binary(In);
   Literal when    is_atom(Literal) ->     encode_literal(In) end ).

to_binary(List) when is_list(List) ->  erlang:list_to_binary(List);
to_binary(Binary) when is_binary(Binary) -> Binary.

encode_literal(true) -> ?TRUE_BINARY;
encode_literal(false) -> ?FALSE_BINARY;
encode_literal(null) -> ?NULL_BINARY;
encode_literal(Atom) when is_atom(Atom) -> encode_string(Atom).


encode_array(In) ->
  encode_array(tuple_size(In),In,[ ?close_square ]).

encode_array(N,In,Out) when 0 < N ->
  Val = element(N,In),
  encode_array(N - 1, In, [ ?comma , encode(Val) | Out ]);
encode_array(0,_,Out) ->
  case Out of
    [ ?comma | Array ] -> [ ?open_square | Array ];
    [ ?close_square ] -> [ ?open_square, ?close_square ] end. % empty array.


encode_object(In) ->
  case maps:fold(fun encode_object/3,[ ?close_brace ],In) of
    [ ?comma | Object ] -> [ ?open_brace | Object ];
    [ ?close_brace ] -> [ ?open_brace, ?close_brace ] end. % empty object.

encode_object(Key,Val,Out) ->
  [ ?comma, encode_string(Key), ?colon, encode(Val) | Out ].


-ifdef(ESCAPED_STRINGS).

encode_string(In) ->
  case In
    of Atom when is_atom(Atom) -> encode_string(erlang:atom_to_list(Atom))
     ; Binary when is_binary(Binary) -> encode_string(binary_to_list(Binary))
     ; List when is_list(List)
       -> Unicode = unicode:characters_to_binary(escape_string(List))
        , [ ?quote, Unicode, ?quote ] end.

escape_string([]) -> [];
escape_string([In|Rest]) ->
  case In
    of     ?quote -> [?backslash,    ?quote|escape_string(Rest)]
     ; ?backslash -> [?backslash,?backslash|escape_string(Rest)]
     ; ?backspace -> [?backslash,        $b|escape_string(Rest)]
     ;  ?formfeed -> [?backslash,        $f|escape_string(Rest)]
     ;   ?newline -> [?backslash,        $n|escape_string(Rest)]
     ;    ?return -> [?backslash,        $r|escape_string(Rest)]
     ;       ?tab -> [?backslash,        $t|escape_string(Rest)]
     ; Ctrl when 0 =< Ctrl, Ctrl < ?space
       -> << Hi:4, Lo:4 >> = << Ctrl >>
        , [ ?backslash, $u, $0, $0, hex(Hi), hex(Lo) | escape_string(Rest)]
     ; _ -> [In|escape_string(Rest)] end.

hex(N) when 0 =< N, N < 10 -> N + $0;
hex(N) when 10 =< N, N < 16 -> N - 10 + $a.

-else.

encode_string(In) ->
  case In
    of Atom when is_atom(Atom)
       -> [ ?quote, erlang:atom_to_binary(Atom), ?quote ]
     ; Binary when is_binary(Binary)
       -> [ ?quote, Binary, ?quote ]
     ; List when is_list(List)
       -> Unicode = unicode:characters_to_binary(List)
        , [ ?quote, Unicode, ?quote ] end.

-endif.





-spec decode(binary()) -> any().
decode(Binary) when is_binary(Binary) ->
  decode(Binary,fun done/2).

-type fn_done() :: fun ((any(),any()) -> any()).

-spec decode(binary(),fn_done()) -> any().
decode(In,Done) when is_binary(In) ->
  Restore = fun (End,Str) -> Done(End,atomic_value(restore(Str))) end,
  Next = fun Fn (T) ->
    case T of
      << >> -> expect_more(Fn);
      <<       ?minus,    _/binary >> -> number(T,Done,1);
      <<       ?quote, Tail/binary >> -> string(Tail,Restore);
      << ?open_square, Tail/binary >> -> array(Tail,Done);
      <<  ?open_brace, Tail/binary >> -> object(Tail,Done);
      << Digit, _/binary >> when $0 =< Digit, Digit =< $9 ->
        number(T,Done,0);
      _ -> literal(T,Done) end end,
  spaces(In,Next).

-spec literal(binary(), fn_done()) -> any().
literal(In,Done) ->
  case binary:first(In) of
    $f -> literal(In,Done,"false" ++ false);
    $n -> literal(In,Done,"null" ++ null);
    $t -> literal(In,Done,"true" ++ true) end.

literal(Tail,Done,Atom) when is_atom(Atom) ->
  spaces(Tail,Done,Atom);
literal(<< A, Tail/binary >>,Done,[B|Rest]) when A =:= B ->
  literal(Tail,Done,Rest);
literal(<< >>,Done,Rest) ->
  fun (More) -> literal(More,Done,Rest) end.



-spec number(nonempty_binary(), fn_done(), 0 | 1) -> any().
number(In,Done,Pos) ->
  Next = fun (B,P) -> integer(B,Done,P) end,
  digits(In,Next,Pos).

integer(In,Done,Pos) ->
  Float = fun (B,P) -> float(B,Done,P) end,
  case In of
    << _:Pos/binary, ?decimal_point, _/binary >> ->
      digits(In,Float,Pos + 1);
    << Part:Pos/binary, Tail/binary >> ->
      Integer = binary_to_integer(Part),
      spaces(Tail,Done,Integer) end.

float(In,Done,Pos) ->
  case In of
    << _:Pos/binary, Exp, _/binary >> when Exp =:= $e; Exp =:= $E ->
      exp_sign(In,Done,Pos + 1);
    << Part:Pos/binary, Tail/binary >> ->
      Float = binary_to_float(Part),
      spaces(Tail,Done,Float) end.

exp_sign(In,Done,Pos) ->
  Exp = fun (B,P) -> exponent(B,Done,P) end,
  case In of
    << _:Pos/binary >> ->
      fun (More) -> exp_sign(<< In/binary, More/binary >>,Done,Pos) end;
    << _:Pos/binary, Sign, _/binary >> when ?minus =:= Sign; ?plus =:= Sign ->
      digits(In, Exp, Pos + 1);
    _ -> digits(In, Exp, Pos) end.

exponent(In,Done,Pos) ->
  case In of
    << Part:Pos/binary, Tail/binary >> ->
      Float = binary_to_float(Part),
      spaces(Tail,Done,Float) end.

digits(In,Done,Pos) when is_function(Done,2) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) -> digits(<< In/binary, More/binary >>,Done,Pos) end;
    << _:Pos/binary, Digit, _/binary >> when $0 =< Digit, Digit =< $9 ->
      digits(In,Done,Pos + 1);
    _ -> Done(In,Pos) end.


-spec string(binary(), fn_done()) -> any().
string(In,Done) ->
  string_cont(In,Done,0).

string_cont(In,Done,Pos) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) -> string_cont(<< In/binary, More/binary >>,Done,Pos) end;
    << _:Pos/binary, ?backslash, ?quote, _/binary >> ->
      string_cont(In,Done,Pos + 2);
    << Part:Pos/binary, ?quote, Tail/binary >> ->
      spaces(Tail,Done,Part);
    << _:Pos/binary, Char, _/binary >> when ?space =< Char ->
      string_cont(In,Done,Pos + 1) end.



-spec array(binary(), fn_done()) -> any().
array(In,Done) ->
  Next = fun (Tail) ->
    case Tail of
      << ?close_square, Exit/binary >> ->
        spaces(Exit,Done,{}); % empty array
      _ -> array_cell(Tail,Done,[]) end end,
  spaces(In,Next).

array_cell(<< >>,Done,Cells) ->
  Next = fun (In) -> array_cell(In,Done,Cells) end,
  expect_more(Next);
array_cell(In,Done,Cells) ->
  Next = fun Fn(Tail,Cell) ->
    case Tail of
      << >> ->
        Cont = fun (More) -> Fn(More,Cell) end,
        expect_more(Cont);
      << ?comma, Again/binary >> ->
        array_cell(Again,Done,[Cell|Cells]);
      << ?close_square, Exit/binary >> ->
        Array = list_to_tuple(lists:reverse([Cell|Cells])),
        spaces(Exit,Done,Array) end end,
  decode(In,Next).


-spec object(binary(), fn_done()) -> any().
object(In,Done) ->
  Next = fun (Tail) ->
    case Tail of
      << ?close_brace, Exit/binary >> ->
        spaces(Exit,Done,#{}); % empty object
      _ -> object_name(Tail,Done,#{}) end end,
  spaces(In,Next).

object_name(<< >>,Done,Object) ->
  Next = fun (Tail) -> object_name(Tail,Done,Object) end,
  expect_more(Next);
object_name(<< ?quote, In/binary >>,Done,Object) ->
  Next = fun (Tail,Name) ->
    Out = {Object,atomic(restore(Name))},
    Colon = fun (T) -> object_colon(T,Done,Out) end,
    spaces(Tail,Colon) end,
  string(In,Next).

object_colon(<< >>,Done,Out) ->
  Colon = fun (T) -> object_colon(T,Done,Out) end,
  expect_more(Colon);
object_colon(<< ?colon, In/binary >>,Done,{Object,Name}) ->
  Next = fun (Tail,Value) ->
    Out = Object#{ Name => Value },
    Loop = fun (T) -> object_loop(T,Done,Out) end,
    spaces(Tail,Loop) end,
  spaces(In,fun (T) -> decode (T,Next) end).

object_loop(<< >>,Done,Out) ->
  Loop = fun (T) -> object_loop(T,Done,Out) end,
  expect_more(Loop);
object_loop(<< ?close_brace, Exit/binary >>,Done,Out) ->
  spaces(Exit,Done,Out);
object_loop(<< ?comma, Tail/binary >>,Done,Out) ->
  spaces(Tail, fun(T) -> object_name(T,Done,Out) end).


expect_more(Next) -> fun (More) -> spaces(More,Next) end.

-ifdef(ESCAPED_STRINGS).
restore(In) ->
  case binary:split(In,<< ?backslash >>,[global]) of
    [Out] -> Out;
    [Head|Tail] -> esc(Head,Tail) end.
-else.
restore(In) -> In.
-endif.

-ifdef(ESCAPED_STRINGS).
esc(Out,[]) -> Out;
esc(Out,[In|Rest]) ->
  case In of
    << $b, Tail/binary >> ->
      esc(<< Out/binary, ?backspace, Tail/binary >>, Rest);
    << $f, Tail/binary >> ->
      esc(<< Out/binary, ?formfeed, Tail/binary >>, Rest);
    << $n, Tail/binary >> ->
      esc(<< Out/binary, ?newline, Tail/binary >>, Rest);
    << $r, Tail/binary >> ->
      esc(<< Out/binary, ?return, Tail/binary >>, Rest);
    << $t, Tail/binary >> ->
      esc(<< Out/binary, ?tab, Tail/binary >>, Rest);
    << $u,A,B,C,D, Tail/binary >> ->
      E = int4(A),
      F = int4(B),
      G = int4(C),
      H = int4(D),
      << Codepoint:16 >> = << E:4,F:4,G:4,H:4 >>,
      Unicode = unicode:characters_to_binary([Codepoint]),
      esc(<< Out/binary, Unicode/binary, Tail/binary >>, Rest) end.

int4(X) when $0 =< X, X =< $9 -> X - $0;
int4(X) when $A =< X, X =< $F -> 10 + X - $A;
int4(X) when $a =< X, X =< $f -> 10 + X - $a.
-endif.


spaces(In,Done) ->
  case In of
    << Space, Tail/binary >>
       when Space =:= ?space
          ; Space =:= ?tab
          ; Space =:= ?newline
          ; Space =:= ?return -> spaces(Tail,Done);
    _ -> Done(In) end.

spaces(In,Done,Out) ->
  case In of
    << Space, Tail/binary >>
       when Space =:= ?space
          ; Space =:= ?tab
          ; Space =:= ?newline
          ; Space =:= ?return -> spaces(Tail,Done,Out);
    _ -> Done(In,Out) end.

done(<< >>,Out) -> Out.


-ifdef(ATOMIC_VALUES).
atomic_value(Part) when is_binary(Part) -> atomic(Part).
-else.
atomic_value(Part) -> Part.
-endif.

atomic(In) ->
  try erlang:binary_to_existing_atom(In)
  catch error:badarg -> In end.




test() ->
  test(test_decode()),
  test(test_encode([])).

test({Fn,Tests}) ->
  Filter = filter(Fn),
  Failures = lists:filter(Filter,Tests),
  [] = [ {Good,safe(Fn,Test)} || {Good,Test} <- Failures ].

filter(Fn) ->
  fun ({Good,Test}) -> Good /= safe(Fn,Test) end.

safe(Fn,Test) ->
  try Fn(Test) catch What:Why -> {What,Why} end.

test_fragments() ->
  Tests = test_compound(),
  lists:filter(fun (T) -> test_frag(T) end, Tests).

test_frag(Test) -> not test_frag(Test,1).

test_frag({Good,Bin}=Test,Pos) ->
  case Bin of
    << _:Pos/binary >> -> true;
    << Head:Pos/binary, Tail/binary >> ->
      Fn = decode(Head),
      Pass = is_function(Fn) andalso Good =:= Fn(Tail) orelse Good =:= Fn,
      if Pass -> test_frag(Test,Pos + 1);
         not is_function(Fn) -> io:format("Fn = ~p.~n",[Fn]), false;
         true -> io:format("Tail = ~p.~n",[Tail]), false end end.


test_encode(Tail) ->
   { fun encode/1
   , [ {<< $",$" >>,""}
     , {<< "[]" >>,{}}
     , {<< "{}" >>,#{}}
     , {<< "true" >>,true}
     , { << "[{\"文字化けabc..åäöé𝐀\"" % Unicode sequence of octets
                ":{\"mojibake\":false}"
              ",\"integer\":123456"
              ",\"float\":1.23456789000000003637e+23"
              ",\"12\":[true,false,null]}"
             ",\"More data...\""
             ",999999]"/utf8 >>
       , { #{ "文字化けabc..åäöé𝐀" % list of Unicode characters, not octets!
              => #{ "mojibake" => false }
            , "12" => {true,false,null}
            , "integer" => 123456
            , "float" => 1.234567890e23 }
         , "More data..."
         , 999999 } }
     | test_encode_escaped(Tail) ] }.

-ifdef(ESCAPED_STRINGS).
test_encode_escaped(Tail) ->
  [ { << "\"文字化けabc..åäöé𝐀\\r\\n\\u001b\""/utf8 >>
    , "文字化けabc..åäöé𝐀\r\n\e" }
  | Tail ].
-else.
test_encode_escaped(Tail) ->
  [ { << "\"文字化けabc..åäöé𝐀\""/utf8 >>
    , "文字化けabc..åäöé𝐀" }
  | Tail ].
-endif.

test_decode() ->
  { fun decode/1,
    [ {{error,{case_clause,$,}}, << " [,,,] " >>}
    , {{error,{case_clause,$,}}, << " [ 123, , , ] " >>}
    , {{error,function_clause}, << " { , , , } " >>}
    , {{error,function_clause}, << "123a" >> }
    , {{error,function_clause}, << "{\"a\":1,}" >> }
    | test_positive() ] }.

test_positive() ->
  [{                          {},                   << "  [ ] " >>}
  ,{              << "abc123" >>,       << " \t   \"abc123\"  " >>}
  ,{             { << "abc" >> },              << "[\"abc\"]  " >>}
  ,{                     {{},{}},        << "  [ [ ] , [ ] ]  " >>}
  ,{                   {#{},#{}},      << "  [  { } , { }  ]  " >>}
  ,{            {{{},<<"abc">>}},<< "  [ [ [ ] , \"abc\" ] ]  " >>}
  ,{                         #{},                     << "{}  " >>}
  ,{#{ <<"abc">> => <<"defg">> }, << "{ \"abc\" : \"defg\" }  " >>}
  ,{                  1234567890,           << " 1234567890   " >>}
  ,{                 -1234567890,          << " -1234567890   " >>}
  ,{                 123456789.0,          << " 123456789.0   " >>}
  ,{                    123.0e99,             << " 123.0e99   " >>}
  ,{                   -123.0e99,            << " -123.0e99   " >>}
  ,{                   123.0e-99,            << " 123.0e-99   " >>}
  ,{                        true,                   << " true " >>}
  ,{                       false,                  << " false " >>}
  ,{                        null,                   << " null " >>}
  ,{           {true,false,null},<< " [ true , false , null ] " >>}
  | test_unicode() ].

test_unicode() ->
  [{<< "文字化けabc..åäöé𝐀"/utf8 >>,
                               << "\"文字化けabc..åäöé𝐀\""/utf8 >>}
  | test_escape() ].

-ifdef(ESCAPED_STRINGS).
test_escape() ->
  [{<< "\r\n\e€"/utf8 >>, << "  \"\\r\\n\\u001b\\u20ac\"  " >>}
  | test_atomic_values() ].
-else.
test_escape() -> test_atomic_values().
-endif.


-ifdef(ATOMIC_VALUES).
test_atomic_values() ->
  [{  '', << $", $" >>}
  ,{  '', << "\"\"" >>}
  ,{ undefined, << "\"undefined\"" >>}
  ,{ #{infinity => infinity}, << " { \"infinity\" : \n"
                                 "  \"infinity\" } " >>}
  | test_compound() ].
-else.
test_atomic_values() ->
  [{  <<>>, << $", $" >>}
  ,{  <<>>, << "\"\"" >>}
  ,{<< "undefined" >>, << "\"undefined\"" >>}
  ,{ #{infinity => << "infinity" >>}, << " { \"infinity\" : \n"
                                 "  \"infinity\" } " >>}
  | test_compund() ].
-endif.

test_compound() ->
  [ { { #{ << "文字化けabc..åäöé𝐀"/utf8 >>
              => #{ << "mojibake" >> => false }
            , << "12" >> => {true,false,null}
            , integer => 123456
            , float => 1.234567890e23 }
         , << "More data..." >>
         , 999999 }
       , << " [ { \"文字化けabc..åäöé𝐀\" "
                " : { \"mojibake\" : false } "
              " , \"integer\" : 123456 "
              " , \"float\" : 1.23456789000000003637e+23 "
              " , \"12\" : [ true , false , null ] } "
             " , \"More data...\" "
             " , 999999 ]    "/utf8 >> } ].
