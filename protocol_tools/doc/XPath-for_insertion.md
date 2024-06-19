# XPath as search & replace tool

Using XPath syntax notation to specify locations and values to be inserted
into XML documents.

Example of PACS-008 message with element text `******* CHANGED VALUE *******`
where values must be inserted:

```
$> tidy -xml -indent modified.xml 
No warnings or errors were found.

<?xml version="1.0" encoding="utf-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">
  <FIToFICstmrCdtTrf>
    <GrpHdr>
      <MsgId>******* CHANGED VALUE *******</MsgId>
      <CreDtTm>******* CHANGED VALUE *******</CreDtTm>
      <NbOfTxs>******* CHANGED VALUE *******</NbOfTxs>
      <SttlmInf>
        <SttlmMtd>******* CHANGED VALUE *******</SttlmMtd>
      </SttlmInf>
    </GrpHdr>
    <CdtTrfTxInf>
      <PmtId>
        <EndToEndId>******* CHANGED VALUE *******</EndToEndId>
      </PmtId>
      <IntrBkSttlmAmt Ccy="FOT">******* CHANGED VALUE
      *******</IntrBkSttlmAmt>
      <ChrgBr>******* CHANGED VALUE *******</ChrgBr>
      <Dbtr>
        <Nm>******* CHANGED VALUE *******</Nm>
        <PstlAdr>
          <AdrTp>
            <Cd>******* CHANGED VALUE *******</Cd>
          </AdrTp>
          <StrtNm>******* CHANGED VALUE *******</StrtNm>
          <BldgNb>******* CHANGED VALUE *******</BldgNb>
          <PstCd>******* CHANGED VALUE *******</PstCd>
          <TwnNm>******* CHANGED VALUE *******</TwnNm>
          <DstrctNm>******* CHANGED VALUE *******</DstrctNm>
        </PstlAdr>
        <CtryOfRes>******* CHANGED VALUE *******</CtryOfRes>
      </Dbtr>
      <DbtrAgt>
        <FinInstnId>
          <BICFI>******* CHANGED VALUE *******</BICFI>
        </FinInstnId>
      </DbtrAgt>
      <CdtrAgt>
        <FinInstnId>
          <BICFI>******* CHANGED VALUE *******</BICFI>
        </FinInstnId>
      </CdtrAgt>
      <Cdtr>
        <Nm>******* CHANGED VALUE *******</Nm>
        <PstlAdr>
          <AdrTp>
            <Cd>******* CHANGED VALUE *******</Cd>
          </AdrTp>
          <StrtNm>******* CHANGED VALUE *******</StrtNm>
          <BldgNb>******* CHANGED VALUE *******</BldgNb>
          <PstCd>******* CHANGED VALUE *******</PstCd>
          <TwnNm>******* CHANGED VALUE *******</TwnNm>
          <DstrctNm>******* CHANGED VALUE *******</DstrctNm>
        </PstlAdr>
      </Cdtr>
    </CdtTrfTxInf>
  </FIToFICstmrCdtTrf>
</Document>
```

The above `modified.xml` file content is generated with a test function:

```
flow_pacs_test:test_insertion().
```

The XPath strings that match the above locations:

```
/Document/FIToFICstmrCdtTrf/GrpHdr/MsgId[text() = "******* CHANGED VALUE *******"]
```

or:

```
//GrpHdr/MsgId[text() = "******* CHANGED VALUE *******"]
```

Positions relative to above node:

```
../CreDtTm[text() = "******* CHANGED VALUE *******"]
../NbOfTxs[text() = "******* CHANGED VALUE *******"]
../SttlmInf/SttlmMtd[text() = "******* CHANGED VALUE *******"]
```

Alternatively, because `GrpHdr` is unique and mandatory and the sequence of
elements it contains are also unique, the XPath strings can be:

```
//MsgId[text() = "******* CHANGED VALUE *******"]
//CreDtTm[text() = "******* CHANGED VALUE *******"]
//NbOfTxs[text() = "******* CHANGED VALUE *******"]
//SttlmMtd[text() = "******* CHANGED VALUE *******"]
```

Restart at a new position located under `CdtTrfTxInf`:

```
//CdtTrfTxInf/PmtId/EndToEndId[text() = "******* CHANGED VALUE *******"]
```

Match both the `Ccy` attribute value, and the element text:

```
//CdtTrfTxInf/IntrBkSttlmAmt[@Ccy="GBP" and text() = "123456.789"]

```

[](https://www.w3.org/TR/xpath-functions/)


To set the values, instead of comparing and matching them, the notation
changes to represent an action.  The
[replace](https://www.w3.org/TR/xpath-functions/#func-replace) function is
used here to alter the values, with the `.*` regexp to replace the entire
element text:

```
//MsgId[replace(text(),".*","new value")]
```

For example, the action might be compounded with a pattern match and an
insertion:

```
//MsgId[replace(text(),"old value","new value")]
```

Inserting an attribute value:

```
//IntrBkSttlmAmt[replace(@Ccy,".*","new value")]
```








# Notations proposal


Sample PACS-008 message:

```
$> tidy -xml -indent protocol_tools/priv/pacs008.xml 
No warnings or errors were found.

<?xml version="1.0"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">
  <FIToFICstmrCdtTrf>
    <GrpHdr>
      <MsgId>pacs8bizmsgid6458556</MsgId>
      <CreDtTm>2024-04-30T11:29:49Z</CreDtTm>
      <NbOfTxs>1</NbOfTxs>
      <SttlmInf>
        <SttlmMtd>INDA</SttlmMtd>
      </SttlmInf>
    </GrpHdr>
    <CdtTrfTxInf>
      <PmtId>
        <EndToEndId>9483133298870858</EndToEndId>
      </PmtId>
      <IntrBkSttlmAmt Ccy="AOA">
      1371584331305.19769</IntrBkSttlmAmt>
      <ChrgBr>CRED</ChrgBr>
      <Dbtr>
        <Nm>Z&lt;OSCIg1n[{IhfLGB/.bTC9%]Khl(D</Nm>
        <PstlAdr>
          <AdrTp>
            <Cd>ADDR</Cd>
          </AdrTp>
          <StrtNm>
          #8q#&gt;G{zC0r3hJ:MLRD/1zF'?2?!K&amp;)bm0(L"$D,62M'BqRGD}:{NmY[UqX</StrtNm>
          <BldgNb>572</BldgNb>
          <PstCd>bVFV</PstCd>
          <TwnNm>pmK8bz@MXW{k1'd./!5ic&gt;U5w/1</TwnNm>
          <DstrctNm>HB&lt;Ha</DstrctNm>
        </PstlAdr>
        <CtryOfRes>GA</CtryOfRes>
      </Dbtr>
      <DbtrAgt>
        <FinInstnId>
          <BICFI>JRBANK12345</BICFI>
        </FinInstnId>
      </DbtrAgt>
      <CdtrAgt>
        <FinInstnId>
          <BICFI>RJBANK98765</BICFI>
        </FinInstnId>
      </CdtrAgt>
      <Cdtr>
        <Nm>Y</Nm>
        <PstlAdr>
          <AdrTp>
            <Cd>ADDR</Cd>
          </AdrTp>
          <StrtNm>&lt;Ie)$</StrtNm>
          <BldgNb>93</BldgNb>
          <PstCd>85d5n4S6jl9</PstCd>
          <TwnNm>t</TwnNm>
          <DstrctNm>-E&gt;;*ABDG4u9]&gt;A</DstrctNm>
        </PstlAdr>
        <CtryOfRes>LY</CtryOfRes>
      </Cdtr>
    </CdtTrfTxInf>
  </FIToFICstmrCdtTrf>
</Document>
```

Demonstration of extracting values into a property list:

```
1> flow_pacs_test:test_multi_values(pacs_008).
{[{currency,[<<"AOA">>]},
  {amount,[[<<"1371584331305.19769">>]]},
  {town_name,[[<<"pmK8bz@MXW{k1'd./!5ic">>,
               {entity_ref,<<"gt">>},
               <<"U5w/1">>]]}],
 #{'Document' =>
       #{'FIToFICstmrCdtTrf' =>
             #{'CdtTrfTxInf' =>
                   #{'IntrBkSttlmAmt' =>
                         [{element,<<"IntrBkSttlmAmt">>,
                                   #{<<"Ccy">> => <<"AOA">>},
                                   [<<"1371584331305.19769">>]}],
                     'Dbtr' =>
                         #{'PstlAdr' =>
                               #{'TwnNm' =>
                                     [{element,<<"TwnNm">>,#{},
                                               [<<"pmK8bz@MXW{k1'd./!5ic">>,
                                                {entity_ref,<<"gt">>},
                                                <<"U5w/1">>]}],...}}}}}
 [{prolog,<<" version=\"1.0\"">>},
  {element,<<"Document">>,
           #{<<"xmlns">> =>
                 <<"urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">>},
           [{element,<<"FIToFICstmrCdtTrf">>,#{},
                     [{element,<<"GrpHdr">>,#{},
                               [{element,<<"MsgId">>,#{},[<<"pacs8bizmsgid645"...>>]},
                                {element,<<"CreDtTm">>,#{},[<<"2024-04-30T1"...>>]},
                                {element,<<"NbOfTxs">>,#{},[<<"1">>]},
                                {element,<<"SttlmInf">>,#{},[{element,...}]}]},
                      {element,<<"CdtTrfTxInf">>,#{},
                               [{element,<<"PmtId">>,#{},[{element,<<"EndT"...>>,#{},...}]},
                                {element,<<"IntrBkSttlmAmt">>,
                                         #{<<"Ccy">> => <<"AOA">>},
                                         [<<"13715843"...>>]},
                                {element,<<"ChrgBr">>,#{},[<<"CRED">>]},
                                {element,<<"Dbtr">>,#{},[{...}|...]},
                                {element,<<"DbtrAgt">>,#{},[...]},
                                {element,<<"Cdtr"...>>,#{},...},
                                {element,<<...>>,...}]}]}]}]}
```

The property list is collected using the following named locations in the
decoded XML document:

```
test_multi_values(pacs_008) ->
  {ok,Binary} = file:read_file("protocol_tools/priv/pacs008.xml"),
  Extract = extraction_maps(pacs_008),
  Path =
  [ { currency,
      [ 'Document',
        'FIToFICstmrCdtTrf',
        'CdtTrfTxInf',
        'IntrBkSttlmAmt',
        #element.attributes,
        <<"Ccy">> ] },
    { amount,
      [ 'Document',
        'FIToFICstmrCdtTrf',
        'CdtTrfTxInf',
        'IntrBkSttlmAmt',
        #element.content ] },
    { town_name,
      [ 'Document',
        'FIToFICstmrCdtTrf',
        'CdtTrfTxInf',
        'Dbtr',
        'PstlAdr',
        'TwnNm',
        #element.content] } ],
  test_multi_values(Binary,Extract,Path).
```

The `Path` broadly corresponds to XPath notations:

```
/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/IntrBkSttlmAmt[@Ccy]
/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/IntrBkSttlmAmt[text()]
/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/PstlAdr/TwnNm[text()]
```

