-module(decode_XML).

-export(
 [ encode/1
 , generate_from_XSD_file/2
 , generate_from_schema/2
 , raw_text/1
 , schema/1
 , trim_namespace/1
 , document/1 ] ).

-export(
 [ parse_pattern/1
 , test/0
 , test_fragments/0 ] ).

-define(space,$\s).
-define(tab,$\t).
-define(return,$\r).
-define(linefeed,$\n).

-define(hyphen,$-).
-define(less_than,$<).
-define(equal,$=).
-define(greater_than,$>).
-define(ampersand,$&).
-define(percent,$%).
-define(semicolon,$;).
-define(slash,$/).

-define(empty_element,"/>").
-define(end_tag,"</").

-define(CDATA_start,"<![CDATA[").
-define(CDATA_end,"]]>").

-define(comment_start,"<!--").
-define(comment_end,"-->").

-define(processsing_instruction_start,"<?").
-define(processsing_instruction_end,"?>").

-define(declaration_start,"<!").
-define(declaration_end,">").

-record(element,
  { name = << >>,
    attributes = #{},
    content = [] }).

-define(dbg(F,A),io:format("~p:~p~n\t"++F,[?FUNCTION_NAME,?LINE|A])).


encode(In) -> erlang:list_to_binary(encode_elements(In)).

encode_elements([]) -> [];
encode_elements([Head|Tail]) -> [encode_elements(Head)|encode_elements(Tail)];
encode_elements(Binary) when is_binary(Binary) -> Binary;
encode_elements(#element{} = Element) ->
  Attr = encode_attributes(Element),
  case Element of
    #element{ name = Name, content = empty} ->
      [ ?less_than, Name, Attr, ?empty_element ];
    #element{ name = Name, content = [] } ->
      [ ?less_than, Name, Attr, ?empty_element ];
    #element{ name = Name, content = Content } ->
      Fill = encode_elements(Content),
      [ ?less_than, Name, Attr, ?greater_than,
        Fill,
        ?end_tag, Name, ?greater_than ] end;
encode_elements({prolog,Prolog}) ->
  [ "<?xml", Prolog, "?>" ].

encode_attributes(Element) ->
  Attr = Element#element.attributes,
  maps:fold(fun encode_each_attr/3,[],Attr).

encode_each_attr(Name,Value,Out) ->
  Quote = quote_value(Value),
  [ ?space, Name, ?equal, Quote, Value, Quote | Out ].

quote_value(Value) -> quote_value(0,Value).

quote_value(Pos,Bin) when Pos < size(Bin) ->
  case Bin of
    << _:Pos/binary, $", _/binary >> -> $';
    << _:Pos/binary, $', _/binary >> -> $";
    _ -> quote_value(Pos + 1,Bin) end;
quote_value(_,_) -> $".

%%%
%%%   The result is the Erlang term representing an XML
%%%   document with #element{} records.
%%%
generate_from_XSD_file(File_name,Type_name) ->
  {ok,Bin} = file:read_file(File_name),
  Document = document(Bin),
  Trimmed = trim_namespace(Document),
  [{element,<<"schema">>,Attrib,_}] = Trimmed,
  Namespace = maps:get(<<"targetNamespace">>,Attrib),
  Schema = schema(Trimmed),
  Top = generate_from_schema(Type_name,Schema),
  {element,<<"Document">>,Doc_attr,Content} = Top,
  [ {prolog,<<" version=\"1.0\" encoding=\"UTF-8\" ">>},
    { element,
      <<"Document">>,
      Doc_attr#{ <<"xmlns">> => Namespace },
      Content } ].

generate_from_schema(Type_name,Schema)
when is_binary(Type_name), is_map(Schema) ->
  Node = maps:get({typedef,Type_name},Schema),
  generate_from_node(Node,Schema).

generate_from_node(Node,Schema) ->
  Name = maps:get(name,Node),
  case Node of
    #{ type := string, minLength := Min, maxLength := Max}
    when is_integer(Min), is_integer(Max) ->
      Bmin = integer_to_binary(Min),
      Bmax = integer_to_binary(Max),
      generate_pattern(
        <<"[ 0-9A-Za-z]{", Bmin/binary, $,, Bmax/binary, $}>> );
    #{ pattern := Pattern } -> generate_pattern(Pattern);
    #{ enumeration := Enum } ->
      #element{
        name = Name,
        content = [pick_random(Enum)] };
    #{ type := string } ->
       generate_TODO(<<"string ", Name/binary >>);
    #{ choice := Choice } ->
      #element{
        name = Name,
        content = [generate_choice(Choice,Schema)] };
    #{ sequence := any } ->
      %
      % They said "sequence of any". Might need <[CDATA[...]]> here?
      %
      Pattern = <<"[ -~]{0,999}">>,
      Content = parse_pattern(Pattern),
      #element{
        name = Name,
        content = << "<![CDATA[", Content/binary, "]]>" >> };
    #{ sequence := Sequence } when is_list(Sequence) ->
      #element{
        name = Name,
        content = generate_sequence(Sequence,Schema) };
    #{ extension := Extend, base := Base } ->
      #element{
        name = Name,
        attributes = generate_attributes(Extend,Schema),
        content = [generate_from_schema(Base,Schema)] };
    #{ type := Type } when is_binary(Type) ->
      #element{
        name = Name,
        content = [generate_from_schema(Type,Schema)] };
    #{ type := date } -> generate_date();
    #{ type := dateTime } -> generate_dateTime();
    #{ type := time } -> generate_time();
    #{ type := year } -> generate_year();
    #{ type := yearMonth } -> generate_yearMonth();
    #{ type := boolean } -> generate_boolean();
    #{ type := base64, minLength := Min, maxLength := Max } ->
      generate_base64(Min,Max);
    #{ type := decimal, totalDigits := Total, fractionDigits := Fract } ->
      %
      % A bit hacky, I confess (-:
      %
      Left = integer_to_binary(Total - Fract),
      Right = integer_to_binary(Fract),
      Pattern = << "([1-9][0-9]{0,",
                   Left/binary,
                   "}).[0-9]{0,",
                   Right/binary,
                   "}" >>,
      parse_pattern(Pattern) end.

%%%
%%%   Following generators are NOT adequate
%%%
generate_TODO(Name) ->
  [<< "Generated value of ",  Name/binary >>].

%%%
%%%   Following generators are adequate:
%%%

generate_attributes(Extend,Schema) ->
  Each = fun (I,O) -> gen_each_attr(I,Schema,O) end,
  lists:foldl(Each,#{},Extend).

gen_each_attr({attribute,Attr},Schema,Out) ->
  #{ name := Name, type := Type } = Attr,
  Value = generate_from_schema(Type,Schema),
  Out#{ Name => Value }.


generate_pattern(Pattern) ->
  [parse_pattern(Pattern)].

parse_pattern(Pattern) ->
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
generate([Bin|Rest],Out) when is_binary(Bin) ->
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

    << _:Pos/binary, $[, _/binary >> ->
      {End,Gen} = gen_range(Pos + 1,Pattern),
      parse_pattern(End,Pattern,[Gen|Out]);
    << _:Pos/binary, ${, _/binary >> ->
      {End,Gen} = gen_repeat(Pos + 1,Pattern,hd(Out)),
      parse_pattern(End,Pattern,[Gen|tl(Out)]);

%    << _:Pos/binary, X, _/binary >> when X =:= $+ ; X =:= $* ->
%      throw(unbounded_repetition_not_implemented);

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
      << _:N/binary, $\\, _/binary >> ->
          gen_literal(Pos,Len + 2,Pattern);
      << _:N/binary, X, _/binary >> ->
        if X =:= ${ ; X =:= $} ;
           X =:= $( ; X =:= $) ;
           X =:= $[ ; X =:= $] ;
%           X =:= $+ ; %%%% one-or-more repetition
%           X =:= $* ; %%%% zero-or-more repetiton
           X =:= $| ->
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
  gen_range(Pos,Pattern,gen_seq(A,B,Out));

gen_range(Pos,Pattern,Out) when Pos < size(Pattern) ->
  case Pattern of
    << _:Pos/binary, $\\, A, _/binary >> ->
      gen_range(Pos + 2,Pattern,[A|Out]);
    << _:Pos/binary, $], _/binary >> ->
      {Pos + 1, fun () -> pick_random(Out) end};
    << _:Pos/binary, C, _/binary >> ->
      gen_range(Pos + 1, Pattern,[C|Out]) end.

gen_seq(A,B,Out) when A < B -> gen_seq(A + 1, B, [A|Out]);
gen_seq(B,B,Out) -> [B|Out].


gen_repeat(Pos,Pattern,Gen) when is_function(Gen) ->
  gen_repeat(Pos,0,Pattern,Gen).

gen_repeat(Pos,N,Pattern,Gen)
when Pos < size(Pattern), is_function(Gen) ->
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

gen_repeat_fun(N,N,Gen) ->
  fun () -> (repeat(Gen))(N) end;
gen_repeat_fun(Min,Max,Gen) when Min < Max ->
  fun () -> (repeat(Gen))(Min + rand:uniform(Max - Min)) end.

repeat(Gen) ->
  fun Fn(0) -> []; Fn(N) when N > 0 -> [Gen()|Fn(N-1)] end.




generate_dateTime() ->
  Now = erlang:timestamp(),
  {{Yr,Mo,Dy},{Hr,Mn,Sc}} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary(
      [pad(4,Yr),$-,pad(2,Mo),$-,pad(2,Dy),$T,
       pad(2,Hr),$:,pad(2,Mn),$:,pad(2,Sc) ]).

generate_date() ->
  Now = erlang:timestamp(),
  {{Yr,Mo,Dy},_} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary([ pad(4,Yr),$-,pad(2,Mo),$-,pad(2,Dy) ]).

generate_time() ->
  Now = erlang:timestamp(),
  {_,{Hr,Mn,Sc}} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary([ pad(2,Hr),$:,pad(2,Mn),$:,pad(2,Sc) ]).

generate_year() ->
  Now = erlang:timestamp(),
  {{Yr,_,_},_} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary([ pad(4,Yr) ]).

generate_yearMonth() ->
  Now = erlang:timestamp(),
  {{Yr,Mo,_},_} = calendar:now_to_universal_time(Now),
  erlang:list_to_binary([ pad(4,Yr),$-,pad(2,Mo) ]).


pad(W,N) when is_integer(N) ->
  Zeros = << "000000" >>,
  Bin = erlang:integer_to_binary(N),
  case W =< byte_size(Zeros) andalso W - byte_size(Bin)
    of Neg when Neg < 0 -> Bin
     ; Pad when Pad =< byte_size(Zeros)
       -> << Zeros:Pad/binary, Bin/binary >> end.






generate_base64(Min,Max) ->
  % NOTE The encoded output base64:encode
  %      is 33% larger than input binary,
  %      hence scale input by 2/3.
  Len = 2 * (Min + rand:uniform(Max - Min)) div 3,
  base64:encode(rand:bytes(Len)).

generate_boolean() ->
  Bool = case rand:uniform(2) of
    1 -> <<"FALSE">>;
    2 -> <<"TRUE">> end,
  [Bool].

generate_sequence(Sequence,Schema) ->
  Each = fun ({element,I},O) ->
    Element = generate_from_node(I,Schema),
    [Element|O] end,
  lists:reverse(lists:foldl(Each,[],Sequence)).

generate_choice(Choice,Schema) ->
  {element,Pick} = pick_random(Choice),
  generate_from_node(Pick,Schema).

pick_random(Choice) ->
  Length = length(Choice),
  N = rand:uniform(Length),
  lists:nth(N,Choice).

%%%
%%%   Extract the raw document text without the markup decoration.
%%%
raw_text(In) ->
  white_space(
    erlang:list_to_binary(raw_each(In)),
    fun (I) -> I end ).

raw_each(#element{ content = Content }) -> raw_each(Content);
raw_each(Text) when is_binary(Text) -> [Text];
raw_each(Content) when is_list(Content) ->
  lists:reverse(raw_each(Content,[]));
raw_each(_) -> [].

raw_each([],Out) -> Out;
raw_each([Each|Rest],Out) ->
  raw_each(Rest,lists:reverse(raw_each(Each),Out)).

%%%
%%%   Remove namespace prefixes from element names.
%%%
trim_namespace(In) -> trim_each(In).

trim_each(#element{} = In) ->
  #element{
     name = Name,
  content = Content } = In,
  [In#element{
       name = trim_name(Name),
    content = trim_namespace(Content) }];
trim_each(Text) when is_binary(Text) ->
  white_space(Text,fun (<<>>) -> []; (I) -> [I] end);
trim_each(Content) when is_list(Content) ->
  lists:reverse(trim_each(Content,[]));
trim_each(_) -> [].

trim_each([],Out) -> Out;
trim_each([Each|Rest],Out) ->
  trim_each(Rest,lists:reverse(trim_each(Each),Out)).

trim_name(Name) when is_binary(Name) ->
  lists:last(binary:split(Name,<<$:>>,[global])).

%%%
%%%   After parsing with document/1, then with
%%%   trim_namespace/1, convert the XSD docuemnt into a map of
%%%   names and types that represent the document schema.
%%%
schema([]) -> [];
schema([ #element{ name = <<"schema">> } = Schema |_]) ->
  schema(Schema#element.content,#{});
schema([_|Rest]) -> schema(Rest).

schema([],Out) -> Out;
schema([#element{} = In|Rest],Out) ->
  #element{ content = Content } = In,
  Keys = #{ <<"name">> => name, <<"type">> => type },
  Attr = get_attributes(Keys,In),
  Name = maps:get(name,Attr),
  case In#element.name of
    <<"element">> ->
      schema(Rest,Out#{ {element,Name} => Attr });
    <<"complexType">> ->
      schema(Rest,Out#{ {typedef,Name} => complexType(Name,Content) });
    <<"simpleType">> ->
      schema(Rest,Out#{ {typedef,Name} => simpleType(Name,Content) }) end;
schema([_|Rest],Out) -> schema(Rest,Out).



simpleType(Name,[#element{ name = <<"restriction">> } = Element]) ->
  #element{
    attributes = Attr,
    content = Content } = Element,
  case trim_name(maps:get(<< "base" >>, Attr)) of
    << "date"       >> -> #{ type => date, name => Name };
    << "dateTime"   >> -> #{ type => dateTime, name => Name };
    << "time"       >> -> #{ type => time, name => Name };
    << "gYear"      >> -> #{ type => year, name => Name };
    << "gYearMonth" >> -> #{ type => yearMonth, name => Name };
    << "boolean"    >> -> #{ type => boolean, name => Name };
    << "base64Binary" >> ->
      base64Binary(Content,#{ type => base64, name => Name });
    << "decimal" >> ->
      decimalType(Content,#{ type => decimal, name => Name });
    << "string" >> ->
      stringType(Content,#{ type => string, name => Name }) end.

base64Binary([],Out) -> Out;
base64Binary([Each|Rest],Out) ->
  Value = get_integer_attribute(<<"value">>,Each),
  case Each#element.name of
    << "minLength" >> -> base64Binary(Rest,Out#{ minLength => Value });
    << "maxLength" >> -> base64Binary(Rest,Out#{ maxLength => Value }) end.

get_integer_attribute(Key,Element) ->
  binary_to_integer(maps:get(Key,Element#element.attributes)).

decimalType([],Out) -> Out;
decimalType([Each|Rest],Out) ->
  Value = get_integer_attribute(<<"value">>,Each),
  case Each#element.name of
    << "fractionDigits" >> -> decimalType(Rest,Out#{ fractionDigits => Value });
    << "totalDigits"    >> -> decimalType(Rest,Out#{ totalDigits => Value });
    << "minInclusive"   >> -> decimalType(Rest,Out#{ minInclusive => Value }) end.

stringType([#element{ name = <<"enumeration">>}|_]=In,Out) ->
  Out#{ enumeration => enumerationType(In) };
stringType([#element{ name = <<"pattern">> }=In],Out) ->
  % value="[A-Z]{3,3}"
  % value="[A-Z0-9]{4,4}[A-Z]{2,2}[A-Z0-9]{2,2}([A-Z0-9]{3,3}){0,1}"
  % value="[A-Z]{2,2}"
  % value="[0-9]{2}"
  % value="[a-zA-Z0-9]{4}"
  % value="[A-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}"
  % value="[A-Z0-9]{18,18}[0-9]{2,2}"
  % value="[0-9]{1,15}"
  % value="\+[0-9]{1,3}-[0-9()+\-]{1,30}"
  % value="[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}"
  patternType(In,Out);
stringType(In,Out) -> bytesType(In,Out).

bytesType([],Out) -> Out;
bytesType([Each|Rest],Out) ->
  case Each#element.name of
    << "minLength" >> ->
      Value = get_integer_attribute(<<"value">>,Each),
      stringType(Rest,Out#{ minLength => Value });
    << "maxLength" >> ->
      Value = get_integer_attribute(<<"value">>,Each),
      stringType(Rest,Out#{ maxLength => Value }) end.

patternType(In,Out) ->
  #{<<"value">> := Value} = get_attributes([<< "value" >>],In),
  Out#{ pattern => Value }.

enumerationType([]) -> [];
enumerationType([#element{ name = << "enumeration" >> } = Element|Rest]) ->
  #{<<"value">> := Value} = get_attributes([<< "value" >>],Element),
  [Value|enumerationType(Rest)].




complexType(Name,[Element]) ->
  #element{
    name = Type,
    content = Content } = Element,
  case Type of
    << "simpleContent" >> -> simpleContent(Content,#{ name => Name });
    << "sequence" >> -> sequence(Content,#{ name => Name });
    << "choice" >> -> choice(Content,#{ name => Name }) end.

simpleContent([#element{ name = <<"extension">> }=Element],Out) ->
  Content = Element#element.content,
  #{ <<"base">> := Base } = get_attributes([<<"base">>],Element),
  Out#{ base => Base, extension => extension(Content) }.


extension([]) -> [];
extension([H|T]) -> [extension(H)|extension(T)];
extension(#element{ name = <<"attribute">> }=Element) ->
  Keys = #{ <<"name">> => name, <<"type">> => type },
  Values = get_attributes(Keys,Element),
  {attribute,Values}.




sequence([#element{ name = << "any" >> }],Out) -> Out#{ sequence => any };
sequence(Content,Out) -> Out#{ sequence => sequence(Content) }.

sequence([]) -> [];
sequence([#element{ name = << "element" >> } = Element|Rest]) ->
  Keys = #{
    <<"name">> => name,
    <<"type">> => type,
    <<"minOccurs">> => {minOccurs,fun erlang:binary_to_integer/1},
    <<"maxOccurs">> => {maxOccurs,fun to_integer/1} },
  Values = get_attributes(Keys,Element),
  [{element,Values}|sequence(Rest)].


to_integer(<<"unbounded">>) -> infinity;
to_integer(Bin) -> erlang:binary_to_integer(Bin).

choice(Content,Out) -> Out#{ choice => choice(Content) }.

choice([]) -> [];
choice([#element{ name = << "element" >> } = Element|Rest]) ->
  Keys = #{
    <<"name">> => name,
    <<"type">> => type },
  Values = get_attributes(Keys,Element),
  [{element,Values}|choice(Rest)].

get_attributes(Keys,Element) when is_list(Keys) ->
  maps:with(Keys,Element#element.attributes);
get_attributes(Keys,Element) when is_map(Keys) ->
  Attr = Element#element.attributes,
  Each = fun (Lookup,Insert,Out) ->
    case Attr of
      #{ Lookup := Val } ->
        case Insert of
          {Ins,Fn} -> Out#{ Ins => Fn(Val) };
          _ -> Out#{ Insert => Val } end;
      #{ } -> Out end end,
  maps:fold(Each,#{},Keys).

%%%
%%%   Parser that hews closely to the grammer as specified in
%%%   https://www.w3.org/TR/xml/
%%%   All XML documents begin with the prolog. No leading spaces.
%%%
document(<< ?processsing_instruction_start, Tail/binary >>) ->
  process_instruct(Tail,fun prolog/2).

prolog(Bin,{process_instruct,<< "xml", _/binary>>=PI}) ->
  Next = fun(T) -> doctypedecl(T,{prolog,PI}) end,
  white_space(Bin,Next).

%%%
%%%   Either one or none of doctypedecl, followed by misc.
%%%
doctypedecl(Bin,Prolog) ->
  Done = fun (I,O) -> doctypedecl_end(I,O) end,
  case Bin of
    << "<!DOCTYPE", Tail/binary >> ->
      Next = fun (T,{declaration,_}=Decl) ->
        misc_space(T,[Decl,Prolog],Done) end,
      declaration(Tail,Next);
    _ -> misc_space(Bin,[Prolog],Done) end.

doctypedecl_end(Bin,Out) ->
  Done = fun (<< >>,Result) -> lists:reverse(Result) end,
  Misc = fun (T,Element) -> misc_space(T,[Element|Out],Done) end,
  case Bin of
    << >> -> Out;
    << ?less_than, Tail/binary >> when Tail /= << >> ->
      Next = fun(T) -> start_tag_name(T,Misc) end,
      white_space(Tail,Next);
    _ ->
      fun (More) when is_binary(More) ->
        doctypedecl_end(<< Bin/binary, More/binary >>,Out) end end.

%%%
%%%   Only exists at the document top level.
%%%
misc_space(Bin,Out,Done) ->
  white_space(Bin,fun (T) -> misc(T,Out,Done) end).

misc(Bin,Out,Done) ->
  Again = fun(I,O) -> misc_space(I,[O|Out],Done) end,
  case Bin of
    << ?processsing_instruction_start, Tail/binary >> ->
      process_instruct(Tail,Again);
    << ?comment_start, Tail/binary >> ->
      comment(Tail,Again);
    _ -> Done(Bin,Out) end.


cdata_section(In,Done) -> cdata_section(In,0,Done).

cdata_section(In,Pos,Done) ->
  case In of
    << >> ->
      fun (More) when is_binary(More) ->
        cdata_section(<< In/binary, More/binary >>,Pos,Done) end;
    << CDATA:Pos/binary, ?CDATA_end, Tail/binary >> ->
      Out = {'CDATA',CDATA},
      result(Tail,Out,Done);
    _ -> cdata_section(In,Pos + 1,Done) end.


comment(In,Done) when is_function(Done)  -> comment(In,0,Done).

comment(In,Pos,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        comment(<< In/binary, More/binary >>,Pos,Done) end;
    << Comment:Pos/binary, ?comment_end, Tail/binary >> ->
      Out = {comment,Comment},
      Done(Tail,Out);
    << _:Pos/binary, ?hyphen,B, _/binary >> when B /= ?hyphen ->
      comment(In,Pos + 1,Done); % double-hyphen MUST NOT occur within comments
    << _:Pos/binary, A, _/binary >> when A /= $- ->
      comment(In,Pos + 1,Done) end.


process_instruct(In,Done) when is_function(Done) ->
  process_instruct(In,0,Done).

process_instruct(In,Pos,Done) ->
  Resume = fun (More) when is_binary(More) ->
        process_instruct(<< In/binary, More/binary >>,Pos,Done) end,
  case In of
    << _:Pos/binary >> -> Resume;
    << _:Pos/binary, $? >> -> Resume;
    << Prolog:Pos/binary, ?processsing_instruction_end, Tail/binary >> ->
      Out = {process_instruct,Prolog},
      Done(Tail,Out);
    _ -> process_instruct(In,Pos + 1,Done) end.


%%%
%%%   doctypedecl
%%%   ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
%%%
%%%   ExternalID
%%%   ::= 'SYSTEM' S SystemLiteral
%%%     | 'PUBLIC' S PubidLiteral S SystemLiteral
%%%
%%%   SystemLiteral
%%%   ::= ('"' [^"]* '"')
%%%     | ("'" [^']* "'")
%%%
%%%   PubidLiteral
%%%   ::= '"' PubidChar* '"'
%%%     | "'" (PubidChar - "'")* "'"
%%%   PubidChar
%%%   ::= #x20
%%%     | #xD
%%%     | #xA
%%%     | [a-zA-Z0-9]
%%%     | [-'()+,./:=?;!*#@$_%]
%%%
%%%   intSubset   ::=   (markupdecl | DeclSep)*
%%%
%%%   markupdecl
%%%   ::=   elementdecl
%%%     | AttlistDecl
%%%     | EntityDecl
%%%     | NotationDecl
%%%     | PI
%%%     | Comment
%%%
%%%   DeclSep   ::=   PEReference | S
%%%
%%%   PEReference   ::=   '%' Name ';'
%%%
declaration(In,Done) -> declaration(In,0,Done).

declaration(In,Pos,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        declaration(<< In/binary, More/binary >>,Pos,Done) end;
    << Declaration:Pos/binary, ?declaration_end, Tail/binary >> ->
      result(Tail,{declaration,Declaration},Done);
    _ -> declaration(In,Pos + 1,Done) end.


entity_reference(In,Done) ->
  reference(In,0,entity_ref,Done).

%%%   PEReference   ::=   '%' Name ';'
%param_entity_reference(In,Done) ->
%  reference(In,0,param_ref,Done).

reference(In,Pos,Type,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        reference(<< In/binary, More/binary >>,Pos,Type,Done) end;
    << Reference:Pos/binary, ?semicolon, Tail/binary >> ->
      result(Tail,{Type,Reference},Done);
    _ -> reference(In,Pos + 1,Type,Done) end.


start_tag_name(In,Done) when is_function(Done) ->
  Next = fun (Tail,Elem_name) -> attr_name(Tail,Elem_name,#{},Done) end,
  token(In,0,Next).

attr_name(In,Elem_name,Attributes,Done) ->
  Next = fun (Tail,Attr_name) ->
    Pair = fun (Exit,Attr_value) ->
      Out = Attributes#{ Attr_name => Attr_value },
      attr_name(Exit,Elem_name,Out,Done) end,
    attr_value(Tail,Pair) end,
  case In of
    << ?empty_element, Tail/binary >> ->
      Out = #element{
        name = Elem_name,
        attributes = Attributes,
        content = empty },
      result(Tail,Out,Done);
    << ?greater_than, Tail/binary >> ->
      Out = #element{
        name = Elem_name,
        attributes = Attributes },
      content(Tail,0,Out,Done);
    _ -> token(In,0,Next) end.

attr_value(In,Done) ->
  case In of
    << $=, $", Tail/binary >> -> quote(Tail,0,$",Done);
    << $=, $', Tail/binary >> -> quote(Tail,0,$',Done) end.

quote(In,Pos,Quote,Done) ->
  % Not parsing Entity Replacement Text, e.g. &...; and %...; are untouched.
  case In of
    << Out:Pos/binary, Q, Tail/binary >> when Q =:= Quote ->
      white_space(Tail, fun (T) -> result(T,Out,Done) end);
    _ -> quote(In,Pos + 1,Quote,Done) end.



%%%   content
%%%   ::= CharData? ( ( element
%%%                   | Reference
%%%                   | CDSect
%%%                   | PI
%%%                   | Comment ) CharData? )*
content(In,Pos,Out,Done) ->
  Content = Out#element.content,
  case In of
    << _:Pos/binary >> -> fun (More) ->
      content(<< In/binary, More/binary >>,Pos,Out,Done) end;
    << Text:Pos/binary, Tail/binary >> ->
      Next = fun(T,Elem) ->
        Update = if
          Pos > 0 -> Out#element{ content = [ Elem,Text | Content ] };
          Pos == 0 -> Out#element{ content = [ Elem | Content ] } end,
        content(T,0,Update,Done) end,
      case Tail of
        << ?end_tag, Rest/binary >> ->
          Update = if
            Pos > 0 -> Out#element{ content = [ Text | Content ] };
            Pos == 0 -> Out end,
          end_tag(Rest,Update,Done);
        << ?ampersand, Rest/binary >> ->
          entity_reference(Rest,Next);
        << ?CDATA_start, Rest/binary >> ->
          cdata_section(Rest,Next);
        << ?comment_start, Rest/binary >> ->
          comment(Rest,Next);
        << ?processsing_instruction_start, Rest/binary >> ->
          process_instruct(Rest,Next);
        << ?less_than, Rest/binary >> when Rest /= << >> ->
          Element = fun(T) -> start_tag_name(T,Next) end,
          white_space(Rest,Element);
        _ -> content(In,Pos + 1,Out,Done) end end.


end_tag(In,Out,Done) ->
  Elem_name = Out#element.name,
  Pos = byte_size(Elem_name),
  Match = fun Resume(Tag) ->
    case Tag of
      << Elem_name:Pos/binary, Tail/binary >> ->
        Content = Out#element.content,
        Update = Out#element{ content = lists:reverse(Content) },
        Next = fun
          Fn(<< ?greater_than, T/binary >>) -> result(T,Update,Done);
          Fn(<< >>) -> fun (More) -> Fn(More) end end,
        white_space(Tail,Next);
      _ when byte_size(Tag) < Pos ->
        fun (More) -> Resume(<< Tag/binary, More/binary >>) end end end,
  white_space(In,Match).


token(In,Pos,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) -> token(<< In/binary, More/binary >>,Pos,Done) end;
    << _:Pos/binary, Ch, _/binary >>
    when Ch /= ?slash,
         Ch /= ?greater_than,
         Ch /= ?equal,
         Ch /= ?linefeed,
         Ch /= ?return,
         Ch /= ?space,
         Ch /= ?tab -> token(In,Pos + 1,Done);
    << Token:Pos/binary, Tail/binary >> when Pos > 0 ->
      white_space(Tail, fun (T) -> result(T,Token,Done) end) end.

white_space(In,Done) ->
  case In of
    << Sp, Tail/binary >>
    when Sp =:= ?linefeed;
         Sp =:= ?return;
         Sp =:= ?space;
         Sp =:= ?tab -> white_space(Tail,Done);
    _ -> Done(In) end.


result(In,Out,Done) when is_function(Done) -> Done(In,Out).


test_cases() ->
  [ { << "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" >>,
      [ {prolog,<<"xml version=\"1.0\" encoding=\"UTF-8\" ">>} ] }
  | test_top_level() ].

test_top_level() ->
  [ { << "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
         "     <!--  commentary with spaces  -->"
         "    <? something or other... ?>"
         "   <notMT> some text <MT/> more waffle </notMT>"
         "   <!-- trailing -->"
         "    <? proc instr ?>"
         "      "
      >>,
      [ {prolog,<<"xml version=\"1.0\" encoding=\"UTF-8\" ">>}
      , {comment,<< "  commentary with spaces  " >>}
      , {process_instruct,<< " something or other... " >>}
      , { element,<<"notMT">>,#{},
          [ <<" some text ">>
          , {element,<<"MT">>,#{},empty}
          , <<" more waffle ">> ] }
      , {comment,<<" trailing ">>}
      , {process_instruct,<< " proc instr " >>}
      ] }
    | test_SoapXML() ].

test_SoapXML() ->
  [ {<< "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
           "<soapenv:Envelope"
           " xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\""
           " xmlns:urn=\"urn:headerblock\""
           " xmlns:trig=\"http://www.huawei.com/pgw/trigger\">"
             "<soapenv:Header>"
               "<urn:Trans>"
                 "<urn:serviceName>UPCC.EE</urn:serviceName>"
                 "<urn:msgId>0</urn:msgId>"
                 "<urn:connId>0</urn:connId>"
               "</urn:Trans>"
             "</soapenv:Header>"
             "<soapenv:Body>"
               "<trig:trigger>"
                 "<trig:object"
                 " objectClass=\"CLID_IP_MAP\""
                 " DN=\"CALLING_STATION_ID=447834316855,O=EE Mobile\""
                 " operation=\"modify\">"
                   "<trig:attribute"
                   " name=\"INFO_SVCS\""
                   " modification=\"replace\">"
                     "<trig:beforeValue>"
                       "<!-- test comment -->"
                       "[\"AACY\",\"BBIB\",\"BERRY\"]"
                     "</trig:beforeValue>"
                     "<trig:afterValue>"
                       "[\"AACY\",\"BBIB\",\"BERRY\",\"CLONE\",\"FUP\"]"
                     "</trig:afterValue>"
                   "</trig:attribute>"
                 "</trig:object>"
               "</trig:trigger>"
               "<!-- test comment -->"
               "<trig:trigger>"
                 "<trig:object"
                 " objectClass=\"CLID_IP_MAP\""
                 " DN=\"CALLING_STATION_ID=447834316855,O=EE Mobile\""
                 " operation=\"modify\">"
                   "<trig:attribute"
                   " name=\"INFO_SVCS\""
                   " modification=\"replace\">"
                     "<trig:beforeValue>"
                       "Something that exist&apos;d"
                     "</trig:beforeValue>"
                     "<wibble>junk</wibble>"
                     "<trig:after"
                     "Value abc=\"12&quot;3\">"
                       "Value that exists now"
                     "</trig:afterValue>"
                   "</trig:attribute>"
                 "</trig:object>"
               "</trig:trigger>"
             "</soapenv:Body>"
           "</soapenv:Envelope>" >>,
   [{prolog,<<"xml version=\"1.0\" encoding=\"UTF-8\" ">>},
    {element,<<"soapenv:Envelope">>,
       #{<<"xmlns:soapenv">> =>
             <<"http://schemas.xmlsoap.org/soap/envelope/">>,
         <<"xmlns:trig">> => <<"http://www.huawei.com/pgw/trigger">>,
         <<"xmlns:urn">> => <<"urn:headerblock">>},
       [{element,<<"soapenv:Header">>,#{},
            [{element,<<"urn:Trans">>,#{},
                 [{element,<<"urn:serviceName">>,#{},[<<"UPCC.EE">>]},
                  {element,<<"urn:msgId">>,#{},[<<"0">>]},
                  {element,<<"urn:connId">>,#{},[<<"0">>]}]}]},
        {element,<<"soapenv:Body">>,#{},
            [{element,<<"trig:trigger">>,#{},
                 [{element,<<"trig:object">>,
                      #{<<"DN">> =>
                            <<"CALLING_STATION_ID=447834316855,O=EE Mobile">>,
                        <<"objectClass">> => <<"CLID_IP_MAP">>,
                        <<"operation">> => <<"modify">>},
                      [{element,<<"trig:attribute">>,
                           #{<<"modification">> => <<"replace">>,
                             <<"name">> => <<"INFO_SVCS">>},
                           [{element,<<"trig:beforeValue">>,#{},
                                [{comment,<<" test comment ">>},
                                 <<"[\"AACY\",\"BBIB\",\"BERRY\"]">>]},
                            {element,<<"trig:afterValue">>,#{},
                                [<<"[\"AACY\",\"BBIB\",\"BERRY\",\"CLONE\",\"FUP\"]">>]}]}]}]},
             {comment,<<" test comment ">>},
             {element,<<"trig:trigger">>,#{},
                 [{element,<<"trig:object">>,
                      #{<<"DN">> =>
                            <<"CALLING_STATION_ID=447834316855,O=EE Mobile">>,
                        <<"objectClass">> => <<"CLID_IP_MAP">>,
                        <<"operation">> => <<"modify">>},
                      [{element,<<"trig:attribute">>,
                           #{<<"modification">> => <<"replace">>,
                             <<"name">> => <<"INFO_SVCS">>},
                           [{element,<<"trig:beforeValue">>,#{},
                                [<<"Something that exist">>,
                                 {entity_ref,<<"apos">>},
                                 <<"d">>]},
                            {element,<<"wibble">>,#{},[<<"junk">>]},
                            {element,<<"trig:afterValue">>,
                                #{<<"abc">> => <<"12&quot;3">>},
                                [<<"Value that exists now">>]}]}]}]}]}]}]} ].

test() ->
  test(test_cases()).

test(Tests) ->
  Filter = fun filter/1,
  Failures = lists:filter(Filter,Tests),
  [{In,Good,document(In)} || {In,Good} <- Failures ].

filter({In,Good}) ->
  Out = document(In),
  Out /= Good.

test_fragments() ->
  Tests = test_cases(),
  lists:filter(fun (T) -> test_frag(T) end, Tests).

test_frag(Test) ->
  not test_frag(Test,2).

test_frag({Bin,Good}=Test,Pos) ->
  case Bin of
    << _:Pos/binary >> -> true;
    << Head:Pos/binary, Tail/binary >> ->
      Fn = document(Head),
      Pass = is_function(Fn) andalso Good =:= Fn(Tail) orelse Good =:= Fn,
      if Pass -> test_frag(Test,Pos + 1);
         not is_function(Fn) -> io:format("Fn = ~p.~n",[Fn]), false;
         true -> io:format("Tail = ~p.~n",[Tail]), false end end.
