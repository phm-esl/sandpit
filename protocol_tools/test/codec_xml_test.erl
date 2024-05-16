-module(codec_xml_test).

-export(
 [ test/0
 , test_fragments/0 ] ).

-define(log(F,A),logger:notice("~p:~p~n\t"++F,[?FUNCTION_NAME,?LINE|A])).


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
  [{In,Good,test_decode(In)} || {In,Good} <- Failures ].

filter({In,Good}) ->
  Out = test_decode(In),
  Out /= Good.

test_decode(In) ->
  %%
  %%  Extraction paths as nested maps:
  %%
  Y = #{ 'Envelope' =>
         #{ 'Header' => #{},
            'Body' =>
              #{ trigger =>
                 #{ attribute => #{} } } } },

  Elements = path_utils:extraction_paths(Y),

  %%
  %%  Extraction paths as nested tuples:
  %%
  X = {'Envelope',[
        {'Header',[ ]},
        {'Body',[
          {trigger,[
            {attribute,[ ]} ]} ]} ]},

  Elements = path_utils:extraction_paths(X),

  Elements = #{
    ['Header','Envelope'] => [],
    [attribute,trigger,'Body','Envelope'] => [] },
  loop(codec_xml:decode_hook(In),[],Elements).



loop({Token,Content,Fn},In,Out) when is_function(Fn) ->
  case to_atom(trim(Token)) of
    [] -> loop(Fn(),In,Out);
    [Atom] ->
      if Content =:= token ->
           loop(Fn(),[Atom|In],Out);
         is_list(Content), hd(In) =:= Atom, is_map_key(In,Out) ->
           loop(Fn(),tl(In),Out#{ In := [Content|maps:get(In,Out)] });
         hd(In) =:= Atom ->
           loop(Fn(),tl(In),Out);
         true ->
           loop(Fn(),In,Out) end end;
loop(Decoded,[],Out) ->
  ?log("Extracted = ~p.~n",[path_utils:rollup(Out)]),
  Decoded.

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

to_atom(Bin) when is_binary(Bin) ->
  try erlang:binary_to_existing_atom(Bin) of Atom when is_atom(Atom) -> [Atom]
  catch error:badarg -> [] end.
