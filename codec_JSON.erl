-module(codec_JSON).

% Suppress compiler warning in case of both
% ATOMIC_NAMES and ATOMIC_VALUES are undefiend:
-compile([{nowarn_unused_function,atomic/1}]).

%%%
%%%  https://www.rfc-editor.org/rfc/rfc8259.txt
%%%

-export(
  [ encode/1
  , decode/1 ]).

-export( [ test/0 ] ).

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

-type json_value()
      :: tuple() % array
       | map() % object i.e. hash map of pairs
       | binary() % UTF8 string.
       | number()
       | boolean()
       | 'null'.

-spec encode( In :: json_value() ) -> binary() | {error,term()}.

encode(In) ->
  case encode_each(In,[])
    of Out when is_list(Out) -> list_to_binary(lists:reverse(Out))
     ; Error -> {error,Error} end.

encode_each(In,Out) ->
  try case In
    of   Array when     is_tuple(Array) -> [      encode_array(In) | Out ]
     ;  Object when      is_map(Object) -> [     encode_object(In) | Out ]
     ;  String when     is_list(String) -> [     encode_string(In) | Out ]
     ;  String when   is_binary(String) -> [     encode_string(In) | Out ]
     ; Integer when is_integer(Integer) -> [ integer_to_binary(In) | Out ]
     ;   Float when     is_float(Float) -> [   float_to_binary(In) | Out ]
     ; Literal when    is_atom(Literal) -> [    encode_literal(In) | Out ]
     % any other types produce literal 'null'.
     ; _ -> [ ?NULL_BINARY | Out] end
  % Handle values that fail to encode, emit 'null' instead.
  catch error:badarg -> [ ?NULL_BINARY | Out] end.

encode_literal(true) -> ?TRUE_BINARY;
encode_literal(false) -> ?FALSE_BINARY;
encode_literal(null) -> ?NULL_BINARY;
encode_literal(Atom) when is_atom(Atom) -> encode_string(Atom).

encode_array(In) ->
  Out = encode_array(tuple_size(In),In,[ ?close_square ]),
  case Out of
    [ ?comma | Array ] -> [ ?open_square | Array ];
    [ ?close_square ] -> [ ?open_square, ?close_square ] end. % empty array.

encode_array(N,In,Out) when 0 < N ->
  Val = element(N,In),
  encode_array(N - 1, In, [ ?comma , encode(Val) | Out ]);
encode_array(0,_,Out) -> Out.

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
       -> [ ?quote, erlang:atom_to_list(Atom), ?quote ]
     ; Binary when is_binary(Binary)
       -> [ ?quote, Binary, ?quote ]
     ; List when is_list(List)
       -> Unicode = unicode:characters_to_binary(List)
        , [ ?quote, Unicode, ?quote ] end.

-endif.

decode(Binary) ->
  case decode(0,Binary)
    of {Value,Pos} when is_binary(Value) -> {atomic_value(Value),Pos}
     ; Out -> Out end.

decode(Pos,Binary) when is_binary(Binary) ->
  << Char >> = binary_part(Binary,Pos,1),
  case Char
    of ?quote -> decode_string(Pos + 1,Binary)
     ; ?open_square -> decode_array(Pos + 1,Binary)
     ; ?close_square -> {end_array,Pos + 1}
     ; ?open_brace -> decode_object(Pos + 1,Binary)
     ; ?close_brace -> {end_object,Pos + 1}
     ; ?colon -> {name_separator,Pos + 1}
     ; ?comma -> {value_separator,Pos + 1}
     ; ?minus -> negative(decode_number(Pos + 1,Binary))
     ; Digit when $0 =< Digit, Digit =< $9 -> decode_number(Pos,Binary)
     ; Space when Space =:= ?space
                ; Space =:= ?tab
                ; Space =:= ?newline
                ; Space =:= ?return
       -> decode(Pos + 1,Binary)
     ; _ -> decode_literal(Pos,Binary) end.

decode_string(Pos,Binary) ->
  decode_string(Pos,0,Binary).

decode_string(Pos,Len,Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << ?quote >>
       -> {restore_string(binary_part(Binary,Pos,Len)),Pos + Len + 1}
     ; << Char >> when ?space =< Char
       -> decode_string(Pos,Len + 1,Binary) end.

-ifdef(ESCAPED_STRINGS).

restore_string(Binary) when is_binary(Binary) ->
  case binary:split(Binary,<< ?backslash >>,[global])
    of [Out] -> Out
     ; [Head|Split]
       -> unicode:characters_to_binary([Head|restore_part(Split)]) end.

restore_part([]) -> [];
restore_part([In|Rest]) ->
  Out = case In
    of << $b, Tail/binary >> -> [ ?backspace, Tail ]
     ; << $f, Tail/binary >> -> [ ?formfeed,  Tail ]
     ; << $n, Tail/binary >> -> [ ?newline,   Tail ]
     ; << $r, Tail/binary >> -> [ ?return,    Tail ]
     ; << $t, Tail/binary >> -> [ ?tab,       Tail ]
     ; << $u,A,B,C,D, Tail/binary >>
       -> E = int4(A)
        , F = int4(B)
        , G = int4(C)
        , H = int4(D)
        , << Unicode:16 >> = << E:4,F:4,G:4,H:4 >>
        , [ Unicode, Tail ]
     ; _ -> In end,
   [Out|restore_part(Rest)].

int4(X) when $0 =< X, X =< $9 -> X - $0;
int4(X) when $A =< X, X =< $F -> 10 + X - $A;
int4(X) when $a =< X, X =< $f -> 10 + X - $a.

-else.

restore_string(Binary) -> Binary.

-endif.

decode_array(Pos,Binary) ->
  decode_array(Pos,Binary,[]).

decode_array(Pos,Binary,Out) ->
  case decode(Pos,Binary)
    of {end_array,Done} -> {list_to_tuple(lists:reverse(Out)),Done}
     ; {value_separator,Next} -> decode_array(Next,Binary,Out)
     ; {V,Done} when is_binary(V)
       -> Value = atomic_value(V)
        , decode_array(Done,Binary,[Value|Out])
     ; {Value,Done}
       when Value =:= null
          ; is_boolean(Value)
          ; is_binary(Value)
          ; is_map(Value)
          ; is_tuple(Value)
          ; is_number(Value)
       -> decode_array(Done,Binary,[Value|Out]) end.

decode_object(Pos,Binary) ->
  decode_object(Pos,Binary,#{ }).

decode_object(Pos,Binary,Out) ->
  case decode(Pos,Binary)
    of {end_object,Done} -> {Out,Done}
     ; {value_separator,Next} -> decode_object(Next,Binary,Out)
     ; {N,Colon} when is_binary(N)
       -> Name = atomic_name(N)
        , {name_separator,Next} = decode(Colon,Binary)
        , case decode(Next,Binary)
            of {V,Done} when is_binary(V)
               -> Value = atomic_value(V)
                , decode_object(Done,Binary,Out#{ Name => Value })
             ; {Value,Done}
               when Value =:= null
                  ; is_boolean(Value)
                  ; is_map(Value)
                  ; is_tuple(Value)
                  ; is_number(Value)
               -> decode_object(Done,Binary,Out#{ Name => Value }) end end.

-ifdef(ATOMIC_NAMES).
atomic_name(Name) when is_binary(Name) -> atomic(Name).
-else.
atomic_name(Name) -> Name.
-endif.

-ifdef(ATOMIC_VALUES).
atomic_value(Value) when is_binary(Value), size(Value) > 0 -> atomic(Value);
atomic_value(Value) -> Value.
-else.
atomic_value(Value) -> Value.
-endif.

atomic(Bin) when is_binary(Bin) ->
  try erlang:binary_to_existing_atom(Bin)
  catch error:badarg -> Bin end.

negative({Number,Pos}) when is_number(Number) -> {-Number,Pos}.

decode_number(Pos,Binary) ->
  decode_number(Pos,0,Binary).

decode_number(Pos,Len,Binary) when Pos + Len < size(Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << Digit >> when $0 =< Digit, Digit =< $9
       -> decode_number(Pos,Len + 1,Binary)
     ; << ?decimal_point >>
       -> decode_float(Pos,Len + 1,Binary)
     ; _ -> {binary_to_integer(binary_part(Binary,Pos,Len)),Pos + Len} end;
decode_number(Pos,Len,Binary) ->
  {binary_to_integer(binary_part(Binary,Pos,Len)),Pos + Len}.

decode_float(Pos,Len,Binary) when Pos + Len < size(Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << Digit >> when $0 =< Digit, Digit =< $9
       -> decode_float(Pos,Len + 1,Binary)
     ; << Exp >> when Exp =:= $e; Exp =:= $E
       -> decode_exponent(Pos,Len + 1,Binary)
     ; _ -> {binary_to_float(binary_part(Binary,Pos,Len)),Pos + Len} end;
decode_float(Pos,Len,Binary) ->
  {binary_to_float(binary_part(Binary,Pos,Len)),Pos + Len}.

decode_exponent(Pos,Len,Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << Digit >> when $0 =< Digit, Digit =< $9
       -> decode_exponent_digits(Pos,Len + 1,Binary)
     ; << Sign >> when Sign =:= ?minus; Sign =:= ?plus
       -> decode_exponent_digits(Pos,Len + 1,Binary) end.

decode_exponent_digits(Pos,Len,Binary) when Pos + Len < size(Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << Digit >> when $0 =< Digit, Digit =< $9
       -> decode_exponent_digits(Pos,Len + 1,Binary)
     ; _ -> {binary_to_float(binary_part(Binary,Pos,Len)),Pos + Len} end;
decode_exponent_digits(Pos,Len,Binary) ->
  {binary_to_float(binary_part(Binary,Pos,Len)),Pos + Len}.

decode_literal(Pos,Binary) ->
  case Binary
    of << _:Pos/binary, ?TRUE_STRING, _/binary >>
       -> {'true',Pos + 4}
     ; << _:Pos/binary, ?FALSE_STRING, _/binary >>
       -> {'false',Pos + 5}
     ; << _:Pos/binary, ?NULL_STRING, _/binary >>
       -> {'null',Pos + 4} end.


test() ->
  test_encode(),
  test_decode().

test_encode() ->
  Tests
   = [ {<< $",$" >>,""}
     , {<< "[]" >>,{}}
     , {<< "{}" >>,#{}}
     , {<< "true" >>,true}
     , { << "[{\"文字化けabc..åäöé𝐀\\r\\n\\u001b\"" % Unicode sequence of octets
                ":{\"mojibake\":false}"
              ",\"integer\":123456"
              ",\"float\":1.23456789000000003637e+23"
              ",\"12\":[true,false,null]}"
             ",\"More data...\""
             ",999999]"/utf8 >>
       , { #{ "文字化けabc..åäöé𝐀\r\n\e" % list of Unicode characters, not octets!
              => #{ "mojibake" => false }
            , "12" => {true,false,null}
            , "integer" => 123456
            , "float" => 1.234567890e23 }
         , "More data..."
         , 999999 } } ],
  Failures = lists:dropwhile(fun({A,B}) -> A =:= encode(B) end,Tests),
  [] = [ {A,encode(B)} || {A,B} <- Failures ].

test_decode() ->
  Tests
   = [ {<<>>,<< $",$" >>}
     , {true,<< "true" >>}
     , {false,<< "false" >>}
     , {null,<< "null" >>}
     , {{},<< "[]" >>}
     , {#{},<< "{}" >>}
     , {<< "abc€123\n"/utf8 >>,<< "\"abc\\u20ac123\\n\"" >>}
     , {-1.23e-45,<< "-1.23e-45" >>}
     , {undefined,<< "\"undefined\"" >>}
     , {{undefined},<< "[\"undefined\"]" >>}
     , { { #{ << "文字化けabc..åäöé𝐀\r\n\e"/utf8 >>
              => #{ << "mojibake" >> => false }
            , << "12" >> => {true,false,null}
            , integer => 123456
            , float => 1.234567890e23 }
         , << "More data..." >>
         , 999999 }
       , << "[{\"文字化けabc..åäöé𝐀\\r\\n\\u001b\""
                ":{\"mojibake\":false}"
              ",\"integer\":123456"
              ",\"float\":1.23456789000000003637e+23"
              ",\"12\":[true,false,null]}"
             ",\"More data...\""
             ",999999]"/utf8 >> } ],
  Failures = lists:dropwhile(
    fun({A,B}) -> {A,size(B)} =:= decode(B) end,
    Tests),
  [] = [ {A,decode(B)} || {A,B} <- Failures ].
