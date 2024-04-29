-module(decode_XML).

-export([
  document/1]).

-export([
  test/0,
  test_fragments/0 ]).

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

-define(dbg(F,A),io:format("~p:~p "++F,[?FUNCTION_NAME,?LINE|A])).

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
      ?dbg("",[]),
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
