-module(schema_xsd).

-export(
 [ encode/1
 , generate_from_XSD_file/1
 , generate_from_schema/2
 , raw_text/1
 , from_document/1
 , trim_namespace/1 ] ).

-record(element,
  { name = << >>,
    attributes = #{},
    content = [] }).

-define(log(F,A),logger:notice("~p:~p~n\t"++F,[?FUNCTION_NAME,?LINE|A])).

encode(In) -> to_binary(encode_elements(In)).

to_binary(Nbr) when is_integer(Nbr) -> erlang:integer_to_binary(Nbr);
to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(List) when is_list(List) -> erlang:list_to_binary(List).

encode_elements([]) -> [];
encode_elements([Head|Tail]) -> [encode_elements(Head)|encode_elements(Tail)];
encode_elements(Binary) when is_binary(Binary) -> Binary;
encode_elements(#element{} = Element) ->
  Attr = encode_attributes(Element),
  case Element of
    #element{ name = Name, content = empty} ->
      [ $<, Name, Attr, $/, $> ];
    #element{ name = Name, content = [] } ->
      [ $<, Name, Attr, $/, $> ];
    #element{ name = Name, content = Content } ->
      Fill = encode_elements(Content),
      [ $<, Name, Attr, $>,
        Fill,
        $<, $/, Name, $> ] end;
encode_elements({prolog,Prolog}) ->
  [ "<?xml", Prolog, "?>" ].

encode_attributes(Element) ->
  Attr = Element#element.attributes,
  maps:fold(fun encode_each_attr/3,[],Attr).

encode_each_attr(Name,Value,Out) ->
  Quote = quote_value(Value),
  [ $\s, Name, $=, Quote, Value, Quote | Out ].

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
generate_from_XSD_file(File_name) ->
  {ok,Bin} = file:read_file(File_name),
  Document = decode_xsd(Bin),
  Trimmed = trim_namespace(Document),
  [{element,<<"schema">>,Attrib,Content}] = Trimmed,
  [Root] = [ X || #element{ name = <<"element">>, attributes = X } <- Content ],
  #{ <<"name">> := Root_name, <<"type">> := Root_type } = Root,
  Namespace = maps:get(<<"targetNamespace">>,Attrib),
  Schema = from_document(Trimmed),
  Top = generate_from_schema(Root_type,Schema),
  [ {prolog,<<" version=\"1.0\" encoding=\"UTF-8\" ">>},
    #element{
      name = Root_name,
      attributes = #{ <<"xmlns">> => Namespace },
      content = Top } ].




decode_xsd(Bin) -> codec_xml:decode(Bin).

generate_from_schema(Type_name,Schema)
when is_binary(Type_name), is_map(Schema) ->
  Typedef = maps:get({typedef,Type_name},Schema),
  generate_from_typedef(Typedef,Schema).

generate_from_typedef(Typedef,Schema) ->
  case Typedef of
    #{ extension := _, base := Base_type_name } ->
      Base_typedef = maps:get({typedef,Base_type_name},Schema),
      generate_from_typedef(Base_typedef,Schema);
    #{ sequence := any } ->
      %
      % They said "sequence of any". Might need <[CDATA[...]]> here?
      %
      Pattern = <<"[ -~]{0,999}">>,
      Content = make_value:from_regexp(Pattern),
      << "<![CDATA[", Content/binary, "]]>" >>;
    #{ sequence := Sequence } when is_list(Sequence) ->
      generate_sequence(Sequence,Schema);
    #{ choice := Choice } ->
        generate_choice(Choice,Schema);
    #{ enumeration := Choice } ->
        generate_choice(Choice,Schema);
    #{ type := string, minLength := Min, maxLength := Max}
    when is_integer(Min), is_integer(Max) ->
      Bmin = to_binary(Min),
      Bmax = to_binary(Max),
      make_value:from_regexp(
        <<"[ 0-9A-Za-z]{", Bmin/binary, $,, Bmax/binary, $}>> );
    #{ pattern := Pattern } -> make_value:from_regexp(Pattern);
    #{ type := date } -> make_value:date();
    #{ type := dateTime } -> make_value:date_time();
    #{ type := time } -> make_value:time();
    #{ type := year } -> make_value:year();
    #{ type := yearMonth } -> make_value:year_month();
    #{ type := boolean } -> make_value:boolean();
    #{ type := base64, minLength := Min, maxLength := Max } ->
      make_value:base64(Min,Max);
    #{ type := decimal, totalDigits := Total, fractionDigits := Fract } ->
      %
      % A bit hacky, I confess (-:
      %
      Left = to_binary(Total - Fract),
      Right = to_binary(Fract),
      Pattern = << "([1-9][0-9]{0,",
                   Left/binary,
                   "}).[0-9]{0,",
                   Right/binary,
                   "}" >>,
      make_value:from_regexp(Pattern) end.

generate_attributes(Type_def,Schema) ->
  case Type_def of
    #{ extension := Extend } ->
      Each = fun (I,O) -> gen_each_attr(I,Schema,O) end,
      lists:foldl(Each,#{},Extend);
    #{ } -> #{ } end.

gen_each_attr({attribute,Attr},Schema,Out) ->
  #{ name := Name, type := Type } = Attr,
  Value = generate_from_schema(Type,Schema),
  Out#{ Name => Value }.

generate_sequence(Sequence,Schema) ->
  Each = fun ({element,I},O) ->
    Element = generate_from_element(I,Schema),
    [Element|O] end,
  lists:reverse(lists:foldl(Each,[],Sequence)).

generate_choice(Choice,Schema) ->
  case make_value:pick_random(Choice) of
    {element,Pick} -> generate_from_element(Pick,Schema);
    Pick when is_binary(Pick) -> Pick end.

generate_from_element(Element,Schema) ->
  Element_name = maps:get(name,Element),
  Type_name = maps:get(type,Element),
  Type_def = maps:get({typedef,Type_name},Schema),
  #element{
    name = Element_name,
    attributes = generate_attributes(Type_def,Schema),
    content = generate_from_typedef(Type_def,Schema) }.

%%%
%%%   Extract the raw document text without the markup decoration.
%%%
raw_text(In) ->
  white_space( to_binary(raw_each(In)) ).

white_space(In) ->
  case In of
    << Sp, Tail/binary >>
    when Sp =:= $\n;
         Sp =:= $\r;
         Sp =:= $\s;
         Sp =:= $\t -> white_space(Tail);
    _ -> In end.


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
  case white_space(Text)
    of << >> -> [];
       Out -> [Out] end;
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
from_document([]) -> [];
from_document([ #element{ name = <<"schema">> } = Schema |_]) ->
  to_typedef_map(Schema#element.content,#{});
from_document([_|Rest]) -> from_document(Rest).

to_typedef_map([],Out) -> Out;
to_typedef_map([#element{} = In|Rest],Out) ->
  #element{ content = Content } = In,
  Keys = #{ <<"name">> => name, <<"type">> => type },
  Attr = get_attributes(Keys,In),
  Name = maps:get(name,Attr),
  case In#element.name of
    <<"element">> ->
      to_typedef_map(Rest,Out#{ {element,Name} => Attr });
    <<"complexType">> ->
      to_typedef_map(Rest,Out#{ {typedef,Name} => complexType(Name,Content) });
    <<"simpleType">> ->
      to_typedef_map(Rest,Out#{ {typedef,Name} => simpleType(Name,Content) }) end;
to_typedef_map([_|Rest],Out) -> to_typedef_map(Rest,Out).



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

