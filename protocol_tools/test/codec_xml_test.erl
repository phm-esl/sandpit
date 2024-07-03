-module(codec_xml_test).

-export(
 [ test_encode/0
 , test/0
 , test_fragments/0 ] ).

%-define(log(F,A),logger:notice("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).
-define(log(F,A),io:format("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).


test_cases() ->
  [ { << "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" >>,
      [ {prolog,<<" version=\"1.0\" encoding=\"UTF-8\" ">>} ] }
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
      [ {prolog,<<" version=\"1.0\" encoding=\"UTF-8\" ">>}
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
   [{prolog,<<" version=\"1.0\" encoding=\"UTF-8\" ">>},
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
  [{In,Good,test_decode(In)} || {In,Good} <- Failures ].

filter({In,Good}) ->
  Out = test_decode(In),
  Out /= Good.

test_decode(In) ->
  Elements = #{
    <<"Envelope">> =>
      #{ <<"Header">> => [],
         <<"Body">> =>
            #{ <<"trigger">> =>
              #{ <<"object">> =>
                #{ <<"attribute">> => [] }} }} },
  Decoded = loop(codec_xml:decode_hook(In),[],Elements),
  Decoded = codec_xml:decode(In).

%%%
%%%   TODO: the extraction loop ought to be in the codec_xml module...
%%%
loop({Element,Fn},In,Result) when is_tuple(Element), is_function(Fn) ->
  %
  % Exit from an XML Element. The Element contains attributes and the contents.
  %
  {element,Token,_Attr,_Contents} = Element,
  Atom = trim(Token),
  [{Atom,Out}|Tail] = In,
  loop(Fn(),Tail,Out#{ Atom := Result });

loop({Token,Fn},In,Out) when is_binary(Token), is_function(Fn) ->
  %
  % Enter an XML element. No contents decoded just yet.
  %
  Atom = trim(Token),
  case Out of
    #{ Atom := Into } when is_map(Into) ->
      loop(Fn(),[{Atom,Out}|In],Into);
    #{ Atom := [] } ->
      {Elem,Skip} = skip(Fn(),[Token]),
      loop(Skip(),In,Out#{ Atom := Elem });
    #{ } ->
      {_,Skip} = skip(Fn(),[Token]),
      loop(Skip(),In,Out) end;

loop(Decoded,[],Out) ->
  %
  % Neither entering nor exiting, must be all done.
  %
  ?log("Out = ~p.~n",[Out]),
  Decoded.

skip({{element,Token,_,_}=Elem,Fn},[Token]) ->
  {Elem,Fn};
skip({{element,Token,_,_},Fn},[Token|Tail]) ->
  skip(Fn(),Tail);
skip({Token,Fn},In) when is_binary(Token) ->
  skip(Fn(),[Token|In]).


%%%
%%%   TODO: test_fragments/0 fails.
%%%
test_fragments() ->
  Tests = test_cases(),
  lists:filter(fun (T) -> test_frag(T) end, Tests).

test_frag(Test) ->
  not test_frag(Test,2).

test_frag({Bin,Good}=Test,Pos) ->
  case Bin of
    << _:Pos/binary >> -> true;
    << Head:Pos/binary, Tail/binary >> ->
      Fn = test_decode(Head),
      Pass = is_function(Fn) andalso Good =:= Fn(Tail) orelse Good =:= Fn,
      if Pass -> test_frag(Test,Pos + 1);
         not is_function(Fn) -> ?log("Fn = ~p.~n",[Fn]), false;
         true -> ?log("Tail = ~p.~n",[Tail]), false end end.





trim(Bin) when is_binary(Bin) ->
  lists:last(binary:split(Bin,<<$:>>,[global])).



test_encode() ->
  Big_tests = [
    { <<"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
        "<Document xmlns=\"value of xmlns\">"
          "<FIToFICstmrCdtTrf>"
            "<GrpHdr>"
              "<MsgId>value of MsgId</MsgId>"
              "<CreDtTm>2024-07-01T16:20:46</CreDtTm>"
              "<NbOfTxs>value of NbOfTxs</NbOfTxs>"
              "<SttlmInf>"
                "<SttlmMtd>value of SttlmMtd</SttlmMtd>"
              "</SttlmInf>"
            "</GrpHdr>"
            "<CdtTrfTxInf>"
              "<PmtId>"
                "<EndToEndId>value of EndToEndId</EndToEndId>"
              "</PmtId>"
              "<IntrBkSttlmAmt Ccy=\"value of Ccy\">"
                "value of IntrBkSttlmAmt"
              "</IntrBkSttlmAmt>"
              "<ChrgBr>value of ChrgBr</ChrgBr>"
              "<Dbtr>"
                "<Nm>value of Dbtr Nm</Nm>"
                "<PstlAdr>"
                  "<AdrTp>"
                    "<Cd>ADDR</Cd>"
                  "</AdrTp>"
                  "<StrtNm>value of Dbtr StrtNm</StrtNm>"
                  "<BldgNb>value of Dbtr BldgNb</BldgNb>"
                  "<PstCd>value of Dbtr PstCd</PstCd>"
                  "<TwnNm>value of Dbtr TwnNm</TwnNm>"
                  "<DstrctNm>value of Dbtr DstrctNm</DstrctNm>"
                "</PstlAdr>"
                "<CtryOfRes>value of Dbtr CtryOfRes</CtryOfRes>"
              "</Dbtr>"
              "<DbtrAgt>"
                "<FinInstnId>"
                  "<BICFI>value of DbtrAgt BICFI</BICFI>"
                "</FinInstnId>"
              "</DbtrAgt>"
              "<CdtrAgt>"
                "<FinInstnId>"
                  "<BICFI>value of CdtrAgt BICFI</BICFI>"
                "</FinInstnId>"
              "</CdtrAgt>"
              "<Cdtr>"
                "<Nm>value of Nm</Nm>"
                "<PstlAdr>"
                  "<AdrTp>"
                    "<Prtry>"
                      "<Id>value of Id</Id>"
                      "<Issr>value of Issr</Issr>"
                    "</Prtry>"
                  "</AdrTp>"
                  "<StrtNm>value of StrtNm</StrtNm>"
                  "<BldgNb>value of BldgNb</BldgNb>"
                  "<PstCd>value of PstCd</PstCd>"
                  "<TwnNm>value of TwnNm</TwnNm>"
                  "<DstrctNm>value of DstrctNm</DstrctNm>"
                "</PstlAdr>"
              "</Cdtr>"
            "</CdtTrfTxInf>"
          "</FIToFICstmrCdtTrf>"
        "</Document>">>,
      [{prolog,<<" version=\"1.0\" encoding=\"UTF-8\" ">>},
       {element,<<"Document">>,
           #{<<"xmlns">> =>
                 <<"urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">>},
           [{element,<<"FIToFICstmrCdtTrf">>,#{},
                [{element,<<"GrpHdr">>,#{},
                     [{element,<<"MsgId">>,#{},
                          [<<"1  2PyxTp  pTbhI k 18Z  I20  jerFO">>]},
                      {element,<<"CreDtTm">>,#{},[<<"2024-07-03T09:50:07">>]},
                      {element,<<"NbOfTxs">>,#{},[<<"016246142879">>]},
                      {element,<<"SttlmInf">>,#{},
                          [{element,<<"SttlmMtd">>,#{},[<<"INDA">>]}]}]},
                 {element,<<"CdtTrfTxInf">>,#{},
                     [{element,<<"PmtId">>,#{},
                          [{element,<<"EndToEndId">>,#{},
                               [<<" T  0z1 Z60 9VexFW BdfuT MF0 Of ">>]}]},
                      {element,<<"IntrBkSttlmAmt">>,
                          #{<<"Ccy">> => <<"SZV">>},
                          [<<"20499164.731">>]},
                      {element,<<"ChrgBr">>,#{},[<<"DEBT">>]},
                      {element,<<"Dbtr">>,#{},
                          [{element,<<"Nm">>,#{},
                               [<<"6 5 5Wyc08GDm5R N7f5 Un 0 2E    KchnF3kU24 0 343V6 5QuL0c40A2YjQb">>]},
                           {element,<<"PstlAdr">>,#{},
                               [{element,<<"AdrTp">>,#{},
                                    [{element,<<"Cd">>,#{},[<<"ADDR">>]}]},
                                {element,<<"StrtNm">>,#{},[<<"7 dp7">>]},
                                {element,<<"BldgNb">>,#{},[<<"kTPW  4bc45 8">>]},
                                {element,<<"PstCd">>,#{},[<<"  3 R v Fd18zdFj">>]},
                                {element,<<"TwnNm">>,#{},[<<" ttDW9N oq tNM48xGG ">>]},
                                {element,<<"DstrctNm">>,#{},[<<"tW T">>]}]},
                           {element,<<"CtryOfRes">>,#{},[<<"JN">>]}]},
                      {element,<<"DbtrAgt">>,#{},
                          [{element,<<"FinInstnId">>,#{},
                               [{element,<<"BICFI">>,#{},[<<"T06QVTIY">>]}]}]},
                      {element,<<"CdtrAgt">>,#{},
                          [{element,<<"FinInstnId">>,#{},
                               [{element,<<"BICFI">>,#{},[<<"1F67TT71J47">>]}]}]},
                      {element,<<"Cdtr">>,#{},
                          [{element,<<"Nm">>,#{},
                               [<<"90 1 CxS   Ye2 Y1xuVcb qG00v164Y0 yhrS  PWl  5h7W5Zv e6mVV 2 Zb4MT6GM s9U2e7l a p8vd97W1 L3qdUH3X7 cO41 vR7VUzs  5flr2733k Ti6 g l43p 29F">>]},
                           {element,<<"PstlAdr">>,#{},
                               [{element,<<"AdrTp">>,#{},
                                    [{element,<<"Prtry">>,#{},
                                         [{element,<<"Id">>,#{},[<<"nQ78">>]},
                                          {element,<<"Issr">>,#{},
                                              [<<"ySZor 33Z83P0sQ 0PO4  37k3">>]}]}]},
                                {element,<<"StrtNm">>,#{},[<<" T2 U40AE">>]},
                                {element,<<"BldgNb">>,#{},[<<"8A KU 261Zg3 ">>]},
                                {element,<<"PstCd">>,#{},[<<" ">>]},
                                {element,<<"TwnNm">>,#{},
                                    [<<"8hfl  c3 879f2S62787sDj  B 1z0w67p">>]},
                                {element,<<"DstrctNm">>,#{},[<<"9  u4COb">>]}]}]}]}]}]}],
       #{{xpath,<<"Document">>} =>
                    #{attr =>
                          #{<<"xmlns">> =>
                                <<"value of xmlns">>}},
                <<"Document">> =>
                    #{<<"FIToFICstmrCdtTrf">> =>
                          #{<<"CdtTrfTxInf">> =>
                                #{{xpath,<<"IntrBkSttlmAmt">>} =>
                                      #{attr =>
                                            #{<<"Ccy">> =>
                                                  <<"value of Ccy">>}},
                                  <<"Cdtr">> =>
                                      #{<<"Nm">> => [<<"value of Nm">>],
                                        <<"PstlAdr">> =>
                                            #{<<"AdrTp">> =>
                                                  #{<<"Prtry">> =>
                                                        #{<<"Id">> =>
                                                              [<<"value of Id">>],
                                                          <<"Issr">> =>
                                                              [<<"value of Issr">>]}},
                                              <<"BldgNb">> =>
                                                  [<<"value of BldgNb">>],
                                              <<"DstrctNm">> =>
                                                  [<<"value of DstrctNm">>],
                                              <<"PstCd">> =>
                                                  [<<"value of PstCd">>],
                                              <<"StrtNm">> =>
                                                  [<<"value of StrtNm">>],
                                              <<"TwnNm">> =>
                                                  [<<"value of TwnNm">>]}},
                                  <<"CdtrAgt">> =>
                                      #{<<"FinInstnId">> =>
                                            #{<<"BICFI">> =>
                                                  [<<"value of CdtrAgt BICFI">>]}},
                                  <<"ChrgBr">> => [<<"value of ChrgBr">>],
                                  <<"Dbtr">> =>
                                      #{<<"CtryOfRes">> =>
                                            [<<"value of Dbtr CtryOfRes">>],
                                        <<"Nm">> => [<<"value of Dbtr Nm">>],
                                        <<"PstlAdr">> =>
                                            #{<<"AdrTp">> =>
                                                  #{<<"Prtry">> =>
                                                        #{<<"Id">> =>
                                                              [<<"value of Dbtr Id">>],
                                                          <<"Issr">> =>
                                                              [<<"value of Dbtr Issr">>]}},
                                              <<"BldgNb">> =>
                                                  [<<"value of Dbtr BldgNb">>],
                                              <<"DstrctNm">> =>
                                                  [<<"value of Dbtr DstrctNm">>],
                                              <<"PstCd">> =>
                                                  [<<"value of Dbtr PstCd">>],
                                              <<"StrtNm">> =>
                                                  [<<"value of Dbtr StrtNm">>],
                                              <<"TwnNm">> =>
                                                  [<<"value of Dbtr TwnNm">>]}},
                                  <<"DbtrAgt">> =>
                                      #{<<"FinInstnId">> =>
                                            #{<<"BICFI">> =>
                                                  [<<"value of DbtrAgt BICFI">>]}},
                                  <<"IntrBkSttlmAmt">> =>
                                      [<<"value of IntrBkSttlmAmt">>],
                                  <<"PmtId">> =>
                                      #{<<"EndToEndId">> =>
                                            [<<"value of EndToEndId">>]}},
                            <<"GrpHdr">> =>
                                #{<<"CreDtTm">> =>
                                      [<<"2024-07-01T16:20:46">>],
                                  <<"MsgId">> => [<<"value of MsgId">>],
                                  <<"NbOfTxs">> => [<<"value of NbOfTxs">>],
                                  <<"SttlmInf">> =>
                                      #{<<"SttlmMtd">> =>
                                     [<<"value of SttlmMtd">>]}}}}}
  }],

  Tests = [
    { <<"<undefined/>">>,
      {element,<<"undefined">>,#{},[]},
      #{} },
    { <<"<undefined>insert</undefined>">>,
      {element,<<"undefined">>,#{},[]},
      #{ undefined => [<<"insert">>] } },
    { <<"<ns:undefined>"
        "<ns:module>insert</ns:module>"
        "</ns:undefined>">>,
      {element,<<"ns:undefined">>,#{},[
        {element,<<"ns:module">>,#{},[]}]},
      #{ undefined => #{ module => [<<"insert">>] }} },
    { <<"<ns:undefined>"
        "<ns:module>insert</ns:module>"
        "</ns:undefined>">>,
      {element,<<"ns:undefined">>,#{},[
        {element,<<"ns:module">>,#{},[]}]},
      #{ <<"undefined">> => #{ <<"module">> => [<<"insert">>] }} },
    { <<"<ns:undefined>"
        "<yadda/>"
        "<ns:module>value of module</ns:module>"
        "</ns:undefined>">>,
      {element,<<"ns:undefined">>,#{},[
        {element,<<"yadda">>,#{},[]},
        {element,<<"ns:module">>,#{},[]}]},
      #{ <<"undefined">> =>
         #{ <<"module">> => [<<"value of module">>],
            <<"yadda">> =>
            #{ <<"blah">> => [<<"value of blah">>] }}}},
    { <<"<ns:undefined>"
        "<yadda>"
        "<blah>value of blah</blah>"
        "</yadda>"
        "<ns:module>value of module</ns:module>"
        "</ns:undefined>">>,
      {element,<<"ns:undefined">>,#{},[
        {element,<<"yadda">>,#{},[
          {element,<<"blah">>,#{},[]}]},
        {element,<<"ns:module">>,#{},[]}]},
      #{ <<"undefined">> =>
         #{ <<"module">> => [<<"value of module">>],
            <<"yadda">> =>
            #{ <<"blah">> => [<<"value of blah">>] }}}}
  | Big_tests ],


  [ {Result,test_encode(Element,Map)}
    || {Result,Element,Map}
    <- Tests, Result /= test_encode(Element,Map) ].

test_encode(Element,Map) ->
  Out = loop_encode(
    codec_xml:encode( Element,Map ) ),
  ?log("====== Out = ~p.~n",[Out]),
  Out.


loop_encode(Result) when is_binary(Result) -> Result;
loop_encode({Fn,_Original,Location}) ->
  [{_Atom,Inject,Attr}|_] = Location,
  Replace = {Inject,Attr},
  ?log("Replace = ~p.~n",[Replace]),
  loop_encode( Fn( Replace ) ).
