-module(decode_XML).

-export([
  decode/1,
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

-define(prolog_start,"<?").
-define(prolog_end,"?>").

-define(declaration_start,"<!").
-define(declaration_end,">").

-record(element,
  { name = << >>,
    attributes = #{},
    content = [] }).

decode(In) when is_binary(In) ->
  Start = fun (Tail) -> decode(Tail,[]) end,
  white_space(In,Start).


decode(In,Done) ->
  case In of
    << >> when is_function(Done) ->
      fun (More) when is_binary(More) ->
        decode(More,Done) end;
    << >> -> Done;
    << ?CDATA_start, Tail/binary >> ->
      cdata_section(Tail,Done);
    << ?comment_start, Tail/binary >> ->
      comment(Tail,Done);
    << ?prolog_start, Tail/binary >> ->
      prolog(Tail,Done);
    << ?declaration_start, Tail/binary >> ->
      declaration(Tail,Done);
    << ?ampersand, Tail/binary >> ->
      entity_reference(Tail,Done);
    << ?percent, Tail/binary >> ->
      param_reference(Tail,Done); % example found in DTD
    << ?less_than, Tail/binary >> when Tail /= << >> ->
      Next = fun(T) -> start_tag_name(T,Done) end,
      white_space(Tail,Next);
    _ ->
      fun (More) when is_binary(More) ->
        decode(<< In/binary, More/binary >>,Done) end end.


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


comment(In,Done) -> comment(In,0,Done).

comment(In,Pos,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        comment(<< In/binary, More/binary >>,Pos,Done) end;
    << Comment:Pos/binary, ?comment_end, Tail/binary >> ->
      Out = {comment,Comment},
      result(Tail,Out,Done);
    << _:Pos/binary, ?hyphen,B, _/binary >> when B /= ?hyphen ->
      comment(In,Pos + 1,Done); % double-hyphen MUST NOT occur within comments
    << _:Pos/binary, A, _/binary >> when A /= $- ->
      comment(In,Pos + 1,Done) end.


prolog(In,Done) -> prolog(In,0,Done).

prolog(In,Pos,Done) ->
  Resume = fun (More) when is_binary(More) ->
        prolog(<< In/binary, More/binary >>,Pos,Done) end,
  case In of
    << _:Pos/binary >> -> Resume;
    << _:Pos/binary, $? >> -> Resume;
    << Prolog:Pos/binary, ?prolog_end, Tail/binary >> ->
      Out = {prolog,Prolog},
      result(Tail,Out,Done);
    _ -> prolog(In,Pos + 1,Done) end.


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

param_reference(In,Done) ->
  reference(In,0,param_ref,Done).

reference(In,Pos,Type,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        reference(<< In/binary, More/binary >>,Pos,Type,Done) end;
    << Reference:Pos/binary, ?semicolon, Tail/binary >> ->
      result(Tail,{Type,Reference},Done);
    _ -> reference(In,Pos + 1,Type,Done) end.


start_tag_name(In,Done) ->
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
        << ?less_than, _/binary >> ->
          decode(Tail,Next);
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


result(In,Out,Done) when is_function(Done) -> Done(In,Out);
result(In,Out,Done) ->
  Next = fun (T) -> [Out|decode(T,Done)] end,
  white_space(In,Next).


test_cases() ->
  [ %{ << "<none/> " >>, [] },
    { << "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" >>,
      [{prolog,<<"xml version=\"1.0\" encoding=\"UTF-8\" ">>}] },
    { << "

  <!-- Some sort of-
  commentary... --> <![CDATA[<![CDATA[]]]]><![CDATA[>]]> < element abc=\"123\" def='987' >
 something... < inside /><deeper>bottom &amp; depth</deeper></ element >
< another empty='true' />

" >>, [ {comment,<< " Some sort of-
  commentary... " >>},
      {'CDATA',<<"<![CDATA[]]">>},
      {'CDATA',<<">">>},
      {element,<< "element" >>,
       #{<<"abc">> => <<"123">>,<<"def">> => <<"987">>},
       [ << "\n something... " >>,
         {element,<<"inside">>,#{},empty},
         {element,<<"deeper">>,#{},[
           <<"bottom ">>,
           {entity_ref,<<"amp">>},
           <<" depth">>]} ]},
      {element,<< "another" >>,
       #{<<"empty">> => <<"true">>},empty} ] },
    {<< "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
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
  Tests = test_cases(),
  Filter = fun filter/1,
  Failures = lists:filter(Filter,Tests),
  [{In,Good,decode(In)} || {In,Good} <- Failures ].

filter({In,Good}) ->
  Out = decode(In),
  Out /= Good.

test_fragments() ->
  Tests = test_cases(),
  lists:filter(fun (T) -> test_frag(T) end, Tests).

test_frag(Test) ->
  not test_frag(Test,1).

test_frag({Bin,Good}=Test,Pos) ->
  case Bin of
    << _:Pos/binary >> -> true;
    << Head:Pos/binary, Tail/binary >> ->
      Fn = decode(Head),
      Pass = is_function(Fn) andalso Good =:= Fn(Tail) orelse Good =:= Fn,
      if Pass -> test_frag(Test,Pos + 1);
         not is_function(Fn) -> io:format("Fn = ~p.~n",[Fn]), false;
         true -> io:format("Tail = ~p.~n",[Tail]), false end end.
