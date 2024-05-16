-module(flow_pacs_test).

-export(
  [ test/0
  , test_dir/0 ] ).

-define(log(F,A),logger:notice("~p:~p~n\t"++F,[?FUNCTION_NAME,?LINE|A])).


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
  Extract_008 = extraction_paths(pacs_008),
  ?log("Extract_008 = ~p.~n",[Extract_008]),
  loop(codec_xml:decode_hook(PACS_008),[],Extract_008).


loop({Token,Content,Fn},In,Out) when is_function(Fn) ->
  case to_atom(trim(Token)) of
    [] ->
      ?log("Token = ~p.~n",[Token]),
      skip(Fn(),[Token],In,Out);
    [Atom] ->
      ?log("Atom = ~p.~n",[Atom]),
      if Content =:= token ->
           loop(Fn(),[Atom|In],Out);
         is_list(Content), hd(In) =:= Atom, is_map_key(In,Out) ->
           loop(Fn(),tl(In),Out#{ In := [Content|maps:get(In,Out)] });
         hd(In) =:= Atom ->
           loop(Fn(),tl(In),Out);
         true ->
           loop(Fn(),In,Out) end end;
loop(Decoded,[],Out) ->
  Extracted = path_utils:rollup(Out),
  {Extracted,Decoded}.

skip(Hook,[],In,Out) ->
  loop(Hook,In,Out);
skip({Atom,Content,Fn},Skip,In,Out) ->
  ?log("Atom = ~p.~n\tSkip = ~p.~n",[Atom,Skip]),
  if Content =:= token ->
       skip(Fn(),[Atom|Skip],In,Out);
     hd(Skip) =:= Atom ->
       skip(Fn(),tl(Skip),In,Out);
     true ->
       skip(Fn(),Skip,In,Out) end.


%generate_pacs_003() ->
%  generate_from_xsd("protocol_tools/priv/pacs.003.001.10.xsd").

generate_pacs_008() ->
    generate_from_xsd("protocol_tools/priv/pacs.008.001.11.xsd").

generate_from_xsd(File_name) ->
  codec_xml:encode(
    schema_xsd:generate_from_XSD_file(File_name) ).

extraction_paths(Type) ->
  GrpHdr = 
  {'GrpHdr',[
    {'MsgId',[]},
    {'CreDtTm',[]},
    {'NbOfTxs',[]},
    {'SttlmInf',[
      {'SttlmMtd',[]} ]} ]},
  PmtId =
  {'PmtId',[
    {'EndToEndId',[]} ]},
  PstlAdr =
  {'PstlAdr',[
    {'AdrTp',[{'Cd',[]}]},
    {'StrtNm',[]},
    {'BldgNb',[]},
    {'PstCd',[]},
    {'TwnNm',[]},
    {'DstrctNm',[]} ]},
  case Type of
    pacs_003 ->
      path_utils:extraction_paths(
        {'Document',[
          {'FIToFICstmrDrctDbt',[
            GrpHdr,
            {'DrctDbtTxInf',[
              PmtId,
              {'IntrBkSttlmAmt',[]} ]} ]} ]} );
    pacs_008 ->
      path_utils:extraction_paths(
        {'Document',[
          {'FIToFICstmrCdtTrf',[
            GrpHdr,
            {'CdtTrfTxInf',[
              PmtId,
              {'IntrBkSttlmAmt',[]},
              {'ChrgBr',[]},
              {'Dbtr',[
                {'Nm',[]},
                PstlAdr,
                {'CtryOfRes',[]} ]},
              {'DbtrAgt',[
                {'FinInstnId',[
                  {'BICFI',[]}]}]},
              {'CdtrAgt',[
                {'FinInstnId',[
                  {'BICFI',[]}]}]},
              {'Cdtr',[
                {'Nm',[]},
                PstlAdr ]} ]} ]} ]} ) end.












trim(Bin) when is_binary(Bin) ->
  lists:last(binary:split(Bin,<<$:>>,[global])).

to_atom(Bin) when is_binary(Bin) ->
  try erlang:binary_to_existing_atom(Bin) of Atom when is_atom(Atom) -> [Atom]
  catch error:badarg -> [] end.
