-module(flow_pacs_test).

-export([test/0]).

test() ->
  PACS_008 = generate_pacs_008(),
  Extract_008 = extraction_paths(pacs_008),
  loop(codec_xml:decode(PACS_008),[],Extract_008).



loop({Atom,Content,Fn},In,Out) when is_function(Fn) ->
  if Content =:= token ->
       loop(Fn(),[Atom|In],Out);
     is_list(Content), hd(In) =:= Atom, is_map_key(In,Out) ->
       loop(Fn(),tl(In),Out#{ In := [Content|maps:get(In,Out)] });
     hd(In) =:= Atom ->
       loop(Fn(),tl(In),Out);
     true ->
       loop(Fn(),In,Out) end;
loop(Decoded,[],Out) ->
  Extracted = path_utils:rollup(Out),
  {Extracted,Decoded}.


%generate_pacs_003() ->
%  generate_from_xsd("protocol_tools/priv/pacs.003.001.10.xsd").

generate_pacs_008() ->
    generate_from_xsd("protocol_tools/priv/pacs.008.001.11.xsd").

generate_from_xsd(File_name) ->
  codec_xml:encode(
    schema_xsd:generate_from_XSD_file(File_name,<<"Document">>) ).

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

