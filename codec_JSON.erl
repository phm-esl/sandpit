-module(codec_JSON).

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

-type json_value()
      :: tuple() % array
       | map() % object i.e. hash map of pairs
       | binary() | list() % UTF8 string.
       | number()
       | boolean()
       | 'null'.

-spec encode( In :: json_value() ) -> binary() | {error,term()}.

encode(In) ->
  case encode_each(In,[])
    of Out when is_list(Out) -> list_to_binary(lists:reverse(Out))
     ; Error -> {error,Error} end.

encode_each(In,Out) ->
  case In
    of   Array when     is_tuple(Array) -> [      encode_array(In) | Out ]
     ;  Object when      is_map(Object) -> [     encode_object(In) | Out ]
     ;  String when     is_list(String) -> [     encode_string(In) | Out ]
     ;  String when   is_binary(String) -> [     encode_string(In) | Out ]
     ; Integer when is_integer(Integer) -> [ integer_to_binary(In) | Out ]
     ;   Float when     is_float(Float) -> [   float_to_binary(In) | Out ]
     ;                                _ -> [    encode_literal(In) | Out ] end.

encode_literal(true) -> << "true" >>;
encode_literal(false) -> << "false" >>;
encode_literal(null) -> << "null" >>.

encode_array(In) ->
  [ ?comma | Array ] = encode_array(tuple_size(In),In,[ ?close_square ]),
  [ ?open_square | Array ].

encode_array(N,In,Out) when 0 < N ->
  Val = element(N,In),
  encode_array(N - 1, In, [ ?comma , encode(Val) | Out ]);
encode_array(0,_,Out) -> Out.

encode_object(In) ->
  [ ?comma | Object ] = maps:fold(fun encode_object/3,[ ?close_brace ],In),
  [ ?open_brace | Object ]. 

encode_object(Key,Val,Out) ->
  [ ?comma, encode_string(Key), ?colon, encode(Val) | Out ].

encode_string(Bin) when is_binary(Bin) ->
  encode_string(binary_to_list(Bin));
encode_string(In) when is_list(In) ->
  unicode:characters_to_binary([ ?quote, escape_string(In), ?quote ]).

escape_string([]) -> [];
escape_string([?quote|Rest]) ->
  [?backslash,?quote|escape_string(Rest)];
escape_string([?backslash|Rest]) ->
  [?backslash,?backslash|escape_string(Rest)];
escape_string([?slash|Rest]) ->
  [?backslash,?slash|escape_string(Rest)];
escape_string([?backspace|Rest]) ->
  [?backslash,$b|escape_string(Rest)];
escape_string([?formfeed|Rest]) ->
  [?backslash,$f|escape_string(Rest)];
escape_string([?newline|Rest]) ->
  [?backslash,$n|escape_string(Rest)];
escape_string([?return|Rest]) ->
  [?backslash,$r|escape_string(Rest)];
escape_string([?tab|Rest]) ->
  [?backslash,$t|escape_string(Rest)];
escape_string([Ctrl|Rest]) when 0 =< Ctrl, Ctrl < ?space ->
  << Hi:4, Lo:4 >> = << Ctrl >>,
  [ ?backslash, $u, $0, $0, hex(Hi), hex(Lo) | escape_string(Rest)];
escape_string([Each|Rest]) -> [Each|escape_string(Rest)].

hex(N) when 0 =< N, N < 10 -> N + $0;
hex(N) when 10 =< N, N < 16 -> N - 10 + $a.


decode(Binary) ->
  decode(0,Binary).

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

restore_string(Binary) when is_binary(Binary) ->
  String = unicode:characters_to_list(Binary),
  restore_string(String);

restore_string([]) -> [];
restore_string([?backslash,?quote|Rest]) ->
  [?quote|restore_string(Rest)];
restore_string([?backslash,?backslash|Rest]) ->
  [?backslash|restore_string(Rest)];
restore_string([?backslash,?slash|Rest]) ->
  [?slash|restore_string(Rest)];
restore_string([?backslash,$b|Rest]) ->
  [?backspace|restore_string(Rest)];
restore_string([?backslash,$f|Rest]) ->
  [?formfeed|restore_string(Rest)];
restore_string([?backslash,$n|Rest]) ->
  [?newline|restore_string(Rest)];
restore_string([?backslash,$r|Rest]) ->
  [?return|restore_string(Rest)];
restore_string([?backslash,$t|Rest]) ->
  [?tab|restore_string(Rest)];
restore_string([?backslash,$u,A,B,C,D|Rest]) ->
  E = int4(A),
  F = int4(B),
  G = int4(C),
  H = int4(D),
  << Unicode:16 >> = << E:4,F:4,G:4,H:4 >>,
  [Unicode|restore_string(Rest)];
restore_string([Each|Rest]) -> [Each|restore_string(Rest)].

int4(X) when $0 =< X, X =< $9 -> X - $0;
int4(X) when $A =< X, X =< $F -> 10 + X - $A;
int4(X) when $a =< X, X =< $f -> 10 + X - $a.

decode_array(Pos,Binary) ->
  decode_array(Pos,Binary,[]).

decode_array(Pos,Binary,Out) ->
  case decode(Pos,Binary)
    of {end_array,Done} -> {list_to_tuple(lists:reverse(Out)),Done}
     ; {value_separator,Next} -> decode_array(Next,Binary,Out)
     ; {Value,Done}
       when Value =:= null
          ; is_boolean(Value)
          ; is_list(Value)
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
     ; {Name,Colon} when is_list(Name)
       -> {name_separator,Next} = decode(Colon,Binary)
        , case decode(Next,Binary)
            of {Value,Done}
               when Value =:= null
                  ; is_boolean(Value)
                  ; is_list(Value)
                  ; is_map(Value)
                  ; is_tuple(Value)
                  ; is_number(Value)
               -> decode_object(Done,Binary,Out#{ Name => Value }) end end.

negative({Number,Pos}) when is_number(Number) -> {-Number,Pos}.

decode_number(Pos,Binary) ->
  decode_number(Pos,0,Binary).

decode_number(Pos,Len,Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << Digit >> when $0 =< Digit, Digit =< $9
       -> decode_number(Pos,Len + 1,Binary)
     ; << ?decimal_point >>
       -> decode_float(Pos,Len + 1,Binary)
     ; _ -> {binary_to_integer(binary_part(Binary,Pos,Len)),Pos + Len} end.

decode_float(Pos,Len,Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << Digit >> when $0 =< Digit, Digit =< $9
       -> decode_float(Pos,Len + 1,Binary)
     ; << Exp >> when Exp =:= $e; Exp =:= $E
       -> decode_exponent(Pos,Len + 1,Binary)
     ; _ -> {binary_to_float(binary_part(Binary,Pos,Len)),Pos + Len} end.

decode_exponent(Pos,Len,Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << Digit >> when $0 =< Digit, Digit =< $9
       -> decode_exponent_digits(Pos,Len + 1,Binary)
     ; << Sign >> when Sign =:= ?minus; Sign =:= ?plus
       -> decode_exponent_digits(Pos,Len + 1,Binary) end.

decode_exponent_digits(Pos,Len,Binary) ->
  case binary_part(Binary,Pos + Len,1)
    of << Digit >> when $0 =< Digit, Digit =< $9
       -> decode_exponent_digits(Pos,Len + 1,Binary)
     ; _ -> {binary_to_float(binary_part(Binary,Pos,Len)),Pos + Len} end.


decode_literal(Pos,Binary) ->
  case Binary
    of << _:Pos/binary, "true", _/binary >>
       -> {'true',Pos + 4}
     ; << _:Pos/binary, "false", _/binary >>
       -> {'false',Pos + 5}
     ; << _:Pos/binary, "null", _/binary >>
       -> {'null',Pos + 4} end.


test() ->
  JSON =
  << "[{\"文字化けabc..åäöé𝐀\\r\\n\\u001b\":{\"mojibake\":false}"
      ",\"integer\":123456"
      ",\"float\":1.23456789000000003637e+23"
      ",\"12\":[true,false,null]}"
     ",\"More data...\""
     ",999999]"/utf8 >>,
  Len = byte_size(JSON),

  In = 
    { #{ "文字化けabc..åäöé𝐀\r\n\e" => #{ "mojibake" => false }
       , "12" => {true,false,null}
       , "integer" => 123456
       , "float" => 1.234567890e23 }
    , "More data..."
    , 999999 },
  Out = encode(In),
  Bin = io_lib:write(Out),

  io:put_chars(["Bin = ",Bin,$\n,"Out = ",Out,$\n,"test passed: "]),
  JSON =:= Out andalso {In,Len} =:= decode(Out).
