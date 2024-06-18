-module(flow_pacs_test).

-export(
  [ test_multi_values/0
  , test_extraction/0
  , test_insertion/0
  , test_insertion/1
  , test_dir/0
  , test_seek_end/0
  , test_remap/0
  , test_generator_speed/0 ] ).

%%%
%%%   TODO: Either use a map instead of record, or move
%%%         definition to header file.
%%%
-record(element,
  { name = << >>,
    attributes = #{},
    content = [] }).


%-define(log(F,A),logger:notice("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).
-define(log(F,A),io:format("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).
%-define(log(F,A),ok).

test_dir() ->
  Dir = "protocol_tools/priv/",
  {ok,Files} = file:list_dir(Dir),
  XSD_files = [ Dir ++ X || X <- lists:filter(fun pick_xsd/1, Files) ],
  ?log("XSD_files = ~p.~n",[XSD_files]),
  ok = lists:foreach(fun xml_from_xsd/1,XSD_files).

xml_from_xsd(In) ->
  Out = codec_xml:encode(
    schema_xsd:generate_from_XSD_file(In,[minimal]) ),
  File_name = In ++ "-output.XML",
  ?log("File_name = ~p.~n",[File_name]),
  ok = file:write_file(File_name,Out).

pick_xsd(In) ->
  case lists:reverse(In) of
    "dsx." ++ _ -> true;
    "DSX." ++ _ -> true;
    _ -> false end.



test_multi_values() ->
  {ok,Soap_xml} = file:read_file("protocol_tools/priv/OneNDS_addRequest.xml"),
  Extract =
    #{ 'Envelope' =>
      #{ 'Body' =>
        #{ {xpath,'addRequest'} => #{
             attr => #{ <<"language">> => <<"fr">> } },
           'addRequest' =>
          #{ 'object' =>
            #{ 'svProfile' =>
              #{ {xpath,'infoService'} => #{ pick => [4,6] },
                 'infoService' =>
                 #{ {xpath,'infoSvcs'} => #{ pick => [2,5] },
                    'infoSvcs' => [] } } } } } } },


  {Values,Decode} = follow_nested_map(
    codec_xml:decode_hook(Soap_xml),
    Extract,
    []),
  Compact = no_space(Decode),
  Write = codec_xml:encode(Compact),
  ok = file:write_file("tmp.xml",Write),
  {Values,Compact}.


test_extraction() ->
  PACS_008 = generate_pacs_008([]),
  file:write_file("tmp.xml",PACS_008), % write document to file for debugging
  Extract_008 = extraction_maps(pacs_008),
  ?log("Extract_008 = ~p.~n",[Extract_008]),
  Out = follow_nested_map(
    codec_xml:decode_hook(PACS_008),
    Extract_008,
    []),
  {Values,_} = Out,
  Path =
    [ 'Document',
      'FIToFICstmrCdtTrf',
      'CdtTrfTxInf',
      'Dbtr',
      'PstlAdr',
      'TwnNm'],
  TwnNm = value_of(Path,Values),
  {{Path,TwnNm},Out}.

test_insertion() ->
  Extract_008 = extraction_maps(pacs_008),
  test_insertion([{insertions,Extract_008},minimal]).

test_insertion(Options) ->
  ?log("Options = ~p.~n",[Options]),
  PACS_008 = generate_pacs_008(Options),
  file:write_file("original.xml",PACS_008), % write document to file for debugging

  Decoded = codec_xml:decode(PACS_008),
  Extract_008 = extraction_maps(pacs_008),
  PACS_008 = test_insert_loop(
    codec_xml:encode(Decoded,Extract_008),
    fun (I) -> I end),
  Modified = test_insert_loop(
    codec_xml:encode(Decoded,Extract_008),
    fun (_) -> <<"******* CHANGED VALUE *******">> end),
  file:write_file("modified.xml",Modified). % write document to file for debugging

test_insert_loop(In,Switch) ->
  case In of
    {Fn,Value,Atom} when is_function(Fn) ->
      ?log("Atom = ~p.~n\tValue = ~p.~n",[Atom,Value]),
      test_insert_loop(Fn(Switch(Value)),Switch); % Not modifying the Value
    Out -> Out end.



%%%
%%%   Digging into a nested map of key-value pairs:
%%%
value_of(Path,Map) -> lists:foldl( fun maps:get/2, Map, Path ).

follow_nested_map(Decoded,Extracted,[]) when is_list(Decoded) ->
  {Extracted,Decoded};

follow_nested_map({Token,Fn}=Hook,Extract,Path) when is_binary(Token) ->
  case to_atom(trim(Token)) of
    [Atom] when is_map_key(Atom,Extract) ->
      case Extract of
        #{ {xpath,Atom} := Match, Atom := Into } ->
          case is_match(Match) of
            {false,Rematch} ->
              {_,Next} = seek_end(Hook),
              New = Extract#{ {xpath,Atom} := Rematch },
              follow_nested_map(Next(),New,Path);
            {true,Rematch} when is_map(Into) ->
              New = Extract#{
                {xpath,Atom} := Rematch },
              follow_nested_map(Fn(),Into,[{Atom,New}|Path]);
            {true,Rematch} when is_list(Into)  ->
              % collect all matching elements...
              {Element,Next} = seek_end(Hook),
              New = Extract#{
                Atom := [Element|Into],
                {xpath,Atom} := Rematch },
              follow_nested_map(Next(),New,Path) end;
        #{ Atom := Into } when is_map(Into) ->
          % dig into the nest
          follow_nested_map(Fn(),Into,[{Atom,Extract}|Path]);
        #{ Atom := Into } when is_list(Into) ->
          % collect all matching elements...
          {Element,Next} = seek_end(Hook),
          New = [Element|Into],
          follow_nested_map(Next(),Extract#{ Atom := New },Path) end;
    _ ->
      {#element{ name = Token },Next} = seek_end(Hook),
      follow_nested_map(Next(),Extract,Path) end;

follow_nested_map({Element,Fn},Down,[{Atom,Up}|Path])
when is_map(Down), is_map_key(Atom,Up) ->
  #element{ name = Token, attributes = Attr } = Element,
  case to_atom(trim(Token)) of
    [Atom] ->
      %
      % Attributes matching the 'xpath' rule are checked here, after the
      % element is decoded. No match will discard the values from Down.
      %
      Yes = case Up of
        #{ {xpath,Atom} := #{ attr := Match } } -> is_attr_match(Attr,Match);
        #{ } -> true end,
      New = if Yes -> Up#{ Atom := reset_index(Down) };
               true -> Up end,
      follow_nested_map(Fn(),New,Path) end.


is_attr_match(A,B) ->
  maps:fold(
    fun compare_attr/3,
    true,
    maps:intersect_with(fun compare_attr/3,A,B) ).

compare_attr(_,V1,V2) -> V1 == V2.

reset_index(In) -> maps:fold(fun reset_index/3,#{},In).

reset_index({xpath,_}=K,#{ index := _ }=V,Out) ->
  Out#{ K => maps:remove(index,V) };
reset_index(K,V,Out) -> Out#{ K => V }.

is_match(done) ->
  {false,done};
is_match(Match) when not is_map_key(index,Match) ->
  is_match(Match#{ index => 1 });
is_match(#{ index := Index } = Match) ->
  case Match of
    #{ pick := Pick }
    when is_integer(Pick), Index == Pick ->
      {true,Match#{ index := Index + 1 }};
    #{ pick := Pick } when is_list(Pick) ->
      {lists:member(Index,Pick),Match#{ index := Index + 1}};
    #{ } -> {true,Match#{ index := Index + 1 }} end.



test_seek_end() ->
  PACS_008 = generate("pacs.008",[]),
  file:write_file("tmp.xml",PACS_008),
  Decoded = codec_xml:decode(PACS_008),
  Decoded = seek_end(codec_xml:decode_hook(PACS_008)),
  Encoded = codec_xml:encode(Decoded),
  if PACS_008 =:= Encoded -> test_passed;
     true -> test_failed end.

%%%
%%%   seek_end/1 will short-circuit the API hook call-back so that the
%%%   document element Token and all its subelements will be ignored,
%%%   returning control to the calling context when the Token end-element
%%%   is found.
%%%
seek_end({Token,Fn}) when is_binary(Token) ->
  seek_end(Fn(),[Token]);
seek_end(Hook) ->
  Hook.

seek_end({#element{ name = Token },_} = Out,[Token]) ->
  Out;
seek_end({Token,Fn},Path) when is_binary(Token) ->
  seek_end(Fn(),[Token|Path]);
seek_end({#element{ name = Token },Fn},[Token|Path]) ->
  seek_end(Fn(),Path);
seek_end(Decoded,[]) when is_list(Decoded) ->
  Decoded.




%read_pacs_008() ->
%  {ok,PACS_008} = file:read_file("protocol_tools/priv/pacs008.xml"),
%  PACS_008.

% generate_pacs_003() -> generate("pacs.003").

generate_pacs_008(Options) -> generate("pacs.008",Options).

generate(Name,Options) ->
  Dir = "protocol_tools/priv/",
  [File] = filelib:wildcard(Name ++ ".*.[Xx][Ss][Dd]",Dir),
  generate_from_xsd(Dir ++ File,Options).

generate_from_xsd(File_name,Options) ->
  codec_xml:encode(
    schema_xsd:generate_from_XSD_file(File_name,Options) ).



extraction_maps(Type) ->
  GrpHdr = #{'MsgId' => [],
           'CreDtTm' => [],
           'NbOfTxs' => [],
           'SttlmInf' => #{'SttlmMtd' => []}},
  PmtId = #{'EndToEndId' => []},
  PstlAdr = #{'AdrTp' => #{'Cd' => []},
               'StrtNm' => [],
               'BldgNb' => [],
               'PstCd' => [],
               'TwnNm' => [],
               'DstrctNm' => []},
  case Type of
    pacs_003 ->
      #{'Document' =>
        #{'FIToFICstmrDrctDbt' =>
            #{'GrpHdr' => GrpHdr,
              'DrctDbtTxInf' =>
                  #{'PmtId' => PmtId,
                    'IntrBkSttlmAmt' => []}}}};
    pacs_008 ->
      #{'Document' =>
        #{'FIToFICstmrCdtTrf' =>
           #{'GrpHdr' => GrpHdr,
             'CdtTrfTxInf' =>
             #{'PmtId' => PmtId,
               'IntrBkSttlmAmt' => [],
               'ChrgBr' => [],
               'Dbtr' =>
               #{'Nm' => [],
                 'PstlAdr' => PstlAdr,
                 'CtryOfRes' => []},
               'DbtrAgt' => #{'FinInstnId' => #{'BICFI' => []}},
               'CdtrAgt' => #{'FinInstnId' => #{'BICFI' => []}},
               'Cdtr' =>
               #{'Nm' => [],
                 'PstlAdr' => PstlAdr }}}}} end.

no_space(In) when is_list(In) ->
  no_space(In,[]);
no_space(In) -> In.

no_space([],Out) ->
  lists:reverse(Out);
no_space([#element{} = In|Rest],Out) ->
  #element{ content = Content } = In,
  no_space(Rest,[In#element{ content = no_space(Content) }|Out]);
no_space([Bin|Rest],Out) when is_binary(Bin) ->
  case binary:replace(Bin,[<<$\n>>,<<$\r>>,<<$\s>>,<<$\t>>],<<>>,[global]) of
    <<>> -> no_space(Rest,Out);
    _ -> no_space(Rest,[Bin|Out]) end;
no_space([In|Rest],Out) ->
  no_space(Rest,[In|Out]).


trim(Bin) when is_binary(Bin) ->
  lists:last(binary:split(Bin,<<$:>>,[global])).

to_atom(Bin) when is_binary(Bin) ->
  try erlang:binary_to_existing_atom(Bin) of Atom when is_atom(Atom) -> [Atom]
  catch error:badarg -> [] end.




test_remap() ->
  Request = generate_pacs_008([]),
  Extract = extraction_maps(pacs_008),
  {Old,_} = follow_nested_map(
    codec_xml:decode_hook(Request),
    Extract,
    []),
  Remap =
    #{ 'Document' =>
       { 'Document',
          #{ 'FIToFICstmrCdtTrf' =>
             { 'FIToFICstmrDrctDbt',
               #{ 'GrpHdr' => 'GrpHdr',
                  'CdtTrfTxInf' =>
                    { 'DrctDbtTxInf',
                      #{ 'PmtId' => 'PmtId',
                         'IntrBkSttlmAmt' => 'IntrBkSttlmAmt' }} }} }} },
  New = remap(Old,Remap),
  ?log("New = ~p.~n",[New]),
  test_insert(New).

test_insert(New) ->
  {ok,Bin} = file:read_file("protocol_tools/priv/pacs003.xml"),
  Decode = codec_xml:decode(Bin),
  Inserted = codec_xml:encode(insert_values(Decode,New)),
  file:write_file("tmp.xml",Inserted).


remap(In,Map) ->
  element(1,maps:fold( fun remap_each/3, {#{},Map}, In)).

remap_each(Key,Val,{Out,Map}) ->
  case Map of
    #{ Key := {Switch,Into} } ->
      { Out#{ Switch => remap(Val,Into) }, Map };
    #{ Key := Switch } ->
      { Out#{ Switch => Val }, Map };
    #{ } -> {Out,Map} end.

insert_values([],_) -> [];
insert_values([Head|Tail],Values) ->
  [insert_values(Head,Values)|insert_values(Tail,Values)];
insert_values(#element{} = Element,Values) ->
  #element{ name = Token } = Element,
  case to_atom(trim(Token)) of
    [] -> Element;
    [Atom] ->
      case Values of
        #{ Atom := Into } when is_map(Into) ->
        %  ?log("Atom = ~p.~n\tInto = ~p.~n\tElement = ~p.~n",
        %    [Atom,Into,Element]),
          Content = Element#element.content,
          Update = insert_values(Content,Into),
          Element#element{ content = Update };
        #{ Atom := [#element{ attributes = Attr, content = Content }] } ->
        %  ?log("Atom = ~p.~n\tContent = ~p.~n\tElement = ~p.~n",
        %    [Atom,Content,Element]),
          Element#element{ attributes = Attr, content = Content };
        #{ } -> Element end end;
insert_values(Other,_) -> Other.







test_generator_speed() -> test_generator_speed("pacs.008").

test_generator_speed(Name) ->
  Dir = "protocol_tools/priv/",
  [File_name] = filelib:wildcard(Name ++ ".*.[Xx][Ss][Dd]",Dir),
  ?log("File_name = ~p.~n",[File_name]),
  {Namespace,Root_name,Root_type,Schema}
    = schema_xsd:schema_from_file(Dir ++ File_name,[minimal]),
  Self = erlang:self(),
  Test = fun Loop (N) ->
    receive stop -> Self ! {stopped,N}
    after 0 ->
%      codec_xml:decode(
        codec_xml:encode(
          [ {prolog,<<" version=\"1.0\" encoding=\"UTF-8\" ">>},
            #element{
              name = Root_name,
              attributes = #{ <<"xmlns">> => Namespace },
              content = schema_xsd:generate_from_schema(Root_type,Schema)
              } ] ),
%               ),
      Loop( N + 1 ) end end,
  Pid = erlang:spawn(fun () -> Test(0) end),
  receive after 10000 ->
    Pid ! stop,
    receive {stopped,N} -> #{ rate => N / 10.0 }
    after 10000 -> timeout end end.
