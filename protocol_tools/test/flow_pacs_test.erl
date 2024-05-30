-module(flow_pacs_test).

-compile([export_all]).

-export(
  [ test/0
  , test_dir/0 ] ).

%%%
%%%   TODO: Either use a map instead of record, or move
%%%         definition to header file.
%%%
-record(element,
  { name = << >>,
    attributes = #{},
    content = [] }).


%-define(log(F,A),logger:notice("~p:~p~n\t"++F,[?FUNCTION_NAME,?LINE|A])).
%-define(log(F,A),io:format("~p:~p~n\t"++F,[?FUNCTION_NAME,?LINE|A])).
-define(log(F,A),ok).

test_dir() ->
  Dir = "protocol_tools/priv/",
  {ok,Files} = file:list_dir(Dir),
  XSD_files = [ Dir ++ X || X <- lists:filter(fun pick_xsd/1, Files) ],
  ?log("XSD_files = ~p.~n",[XSD_files]),
  ok = lists:foreach(fun xml_from_xsd/1,XSD_files).

xml_from_xsd(In) ->
  Out = codec_xml:encode(
    schema_xsd:generate_from_XSD_file(In) ),
  File_name = In ++ "-output.XML",
  ?log("File_name = ~p.~n",[File_name]),
  ok = file:write_file(File_name,Out).

pick_xsd(In) ->
  case lists:reverse(In) of
    "dsx." ++ _ -> true;
    "DSX." ++ _ -> true;
    _ -> false end.




test() ->
  PACS_008 = generate_pacs_008(),
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
        #{ Atom := Down } when is_map(Down) ->
          ?log("Atom = ~p.~n\tExtract = ~p.~n\tDown = ~p.~n",
               [Atom,Extract,Down]),
          follow_nested_map(Fn(),Down,[{Atom,Extract}|Path]);

        #{ Atom := Old } when is_list(Old) ->
          {Element,Next} = seek_end(Hook),
          New = [Element|Old],
          ?log("Atom = ~p.~n\tExtract = ~p.~n\tNew = ~p.~n",
               [Atom,Extract,New]),
          follow_nested_map(Next(),Extract#{ Atom := New },Path) end;
    _ ->
      ?log("Token = ~p.~n\tExtract = ~p.~n",
           [Token,Extract]),
      {#element{ name = Token },Next} = seek_end(Hook),
      follow_nested_map(Next(),Extract,Path) end;

follow_nested_map({Element,Fn},Down,[{Atom,Up}|Path])
when is_map_key(Atom,Up) ->
  #element{ name = Token } = Element,
  case to_atom(trim(Token)) of
    [Atom] ->
      ?log("Element = ~p.~n\tAtom = ~p.~n",
        [Element,Atom]),
      New = Up#{ Atom := Down },
      follow_nested_map(Fn(),New,Path) end.



-if(0).
test_seek_end() ->
  PACS_008 = generate("pacs.008"),
  file:write_file("tmp.xml",PACS_008),
  Decoded = codec_xml:decode(PACS_008),
  Decoded = seek_end(codec_xml:decode_hook(PACS_008)),
  Encoded = codec_xml:encode(Decoded),
  if PACS_008 =:= Encoded -> test_passed;
     true -> test_failed end.
-endif.
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

%generate_pacs_003() -> generate("pacs.003").

generate_pacs_008() -> generate("pacs.008").

generate(Name) ->
  Dir = "protocol_tools/priv/",
  [File] = filelib:wildcard(Name ++ ".*.[Xx][Ss][Dd]",Dir),
  generate_from_xsd(Dir ++ File).

generate_from_xsd(File_name) ->
  codec_xml:encode(
    schema_xsd:generate_from_XSD_file(File_name) ).



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



trim(Bin) when is_binary(Bin) ->
  lists:last(binary:split(Bin,<<$:>>,[global])).

to_atom(Bin) when is_binary(Bin) ->
  try erlang:binary_to_existing_atom(Bin) of Atom when is_atom(Atom) -> [Atom]
  catch error:badarg -> [] end.
