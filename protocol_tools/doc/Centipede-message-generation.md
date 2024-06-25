

Centipede offers a tool to generate XML messages using the XSD as a
specification.  The data type definitions and value pattern regexp confine
the random values so that they remain valid even if these values are
meaningless to real systems that exchange these message types.

The `schema_xsd` module offers the `generate_from_XSD_file/2` function that
creates a decoded representation of a randomised but valid XML message.  It
accepts a range of options.

## The `'minOccurs'` option

One option is `{maxOccurs,0}` that produces the
most compact (and least useful) XML result where any sequence of elements is
reduced to zero instances:

```
file:write_file(
  "tmp.xml",
  codec_xml:encode(
    schema_xsd:generate_from_XSD_file(
      "protocol_tools/priv/pacs.008.001.11.xsd",
       [{maxOccurs,0}]))).

```

The output `"tmp.xml"` file pretty printed:

```
$> tidy -xml -indent tmp.xml 2>/dev/null
<?xml version="1.0" encoding="utf-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">
  <FIToFICstmrCdtTrf>
    <GrpHdr>
      <MsgId>GAup250BMO</MsgId>
      <CreDtTm>2024-06-25T14:00:13</CreDtTm>
      <NbOfTxs>107</NbOfTxs>
      <SttlmInf>
        <SttlmMtd>INGA</SttlmMtd>
      </SttlmInf>
    </GrpHdr>
  </FIToFICstmrCdtTrf>
</Document>
```



## The `'minimal'` option

The `minimal` option is more relaxed than `{maxOccurs,0}`. A `minimal`
output will ensure that all sequences of elements will contain at least
`minOccurs` instances. If the XSD specifies `minOccurs="1"` the element will
be present in the result.

```
file:write_file(
  "tmp.xml",
  codec_xml:encode(
    schema_xsd:generate_from_XSD_file(
      "protocol_tools/priv/pacs.008.001.11.xsd",
      [minimal]))).
```

The output `"tmp.xml"` file pretty-printed:

```
$> tidy -xml -indent tmp.xml 2>/dev/null
<?xml version="1.0" encoding="utf-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">
  <FIToFICstmrCdtTrf>
    <GrpHdr>
      <MsgId>9 R3 f 1</MsgId>
      <CreDtTm>2024-06-25T13:05:50</CreDtTm>
      <NbOfTxs>878873681483867</NbOfTxs>
      <SttlmInf>
        <SttlmMtd>CLRG</SttlmMtd>
      </SttlmInf>
    </GrpHdr>
    <CdtTrfTxInf>
      <PmtId>
        <EndToEndId>z ln XTG54G Hjes</EndToEndId>
      </PmtId>
      <IntrBkSttlmAmt Ccy="ZBQ">818744263.946</IntrBkSttlmAmt>
      <ChrgBr>SHAR</ChrgBr>
      <Dbtr />
      <DbtrAgt>
        <FinInstnId />
      </DbtrAgt>
      <CdtrAgt>
        <FinInstnId />
      </CdtrAgt>
      <Cdtr />
    </CdtTrfTxInf>
  </FIToFICstmrCdtTrf>
</Document>
```

Some of the message elements are specified as mandatory in the XSD, but may
have no content. The result of a minimal generation doesn't provide "slots"
inside e.g. the `<Dbtr />`, nor the `<FinInstnId />`. In practice these
missing slots can make the result useless except in one test, to confirm
that the absence of values does not break the test subject, nor lead to
unexpected behaviour.

## The `'insertions'` option

Testing normally requires more range of possible values, and a minimal
template has to be expanded to include more slots to accommodate all these
values. The `'insertions'` option and the value map is how the generation is
guided to ensure that slots are provided in the output for these values.

The value of `Insertions_map` that ensures elements are included to provide
slots for inserted values:

```
Insertions_map =
  #{'Document' =>
    #{'FIToFICstmrCdtTrf' =>
        #{'CdtTrfTxInf' =>
            #{'IntrBkSttlmAmt' => [],
              'Dbtr' =>
                #{'PstlAdr' =>
                  #{'TwnNm' => [],
                    'AdrTp' => #{'Cd' => []},
                    'BldgNb' => [],
                    'DstrctNm' => [],
                    'PstCd' => [],
                    'StrtNm' => []},
                  'Nm' => [],
                  'CtryOfRes' => []},
              'Cdtr' =>
                #{'PstlAdr' =>
                  #{'TwnNm' => [],
                    'AdrTp' => #{'Cd' => []},
                    'BldgNb' => [],
                    'DstrctNm' => [],
                    'PstCd' => [],
                    'StrtNm' => []},
                  'Nm' => []},
              'CdtrAgt' => #{'FinInstnId' => #{'BICFI' => []}},
              'ChrgBr' => [],
              'DbtrAgt' => #{'FinInstnId' => #{'BICFI' => []}},
              'PmtId' => #{'EndToEndId' => []}},
          'GrpHdr' =>
            #{'CreDtTm' => [],
              'MsgId' => [],
              'NbOfTxs' => [],
              'SttlmInf' => #{'SttlmMtd' => []}}}}}
```

Generating with the `insertions` option:

```
file:write_file(
  "tmp.xml",
  codec_xml:encode(
    schema_xsd:generate_from_XSD_file(
      "protocol_tools/priv/pacs.008.001.11.xsd",
      [minimal,{insertions,Insertions_map}]))).
```

The result pretty-printed now includes random but valid values instead of
empty elements:

```
$> tidy -xml -indent tmp.xml 2>/dev/null
<?xml version="1.0" encoding="utf-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">
  <FIToFICstmrCdtTrf>
    <GrpHdr>
      <MsgId>uM n t</MsgId>
      <CreDtTm>2024-06-25T13:48:34</CreDtTm>
      <NbOfTxs>666512422082</NbOfTxs>
      <SttlmInf>
        <SttlmMtd>COVE</SttlmMtd>
      </SttlmInf>
    </GrpHdr>
    <CdtTrfTxInf>
      <PmtId>
        <EndToEndId>g1m0 IKuh I72</EndToEndId>
      </PmtId>
      <IntrBkSttlmAmt Ccy="ULM">510468175261.04</IntrBkSttlmAmt>
      <ChrgBr>DEBT</ChrgBr>
      <Dbtr>
        <Nm>5B14aED8oCe 4973rfkl 8 230PNpzA T2 2f sg7k59e 64 y6ue
        mj 2CxE KR e rl 2VLF16JNOK60sKTtF3d12cuo P p s j Teb 5 4 0
        I66a 2 y 6R2</Nm>
        <PstlAdr>
          <AdrTp>
            <Prtry>
              <Id>gxK7</Id>
              <Issr>gxs o4yIs8Z vf7C bchB5w</Issr>
            </Prtry>
          </AdrTp>
          <StrtNm>tW3Y59Ny d684h7r6 81sXI0 sqG</StrtNm>
          <BldgNb>1O s w F6 0 m</BldgNb>
          <PstCd>B6</PstCd>
          <TwnNm>qlz 9vP5y8iVjcEa MS3 iMXT</TwnNm>
          <DstrctNm>g0 9x1hyC z uw142 LG 2 fx</DstrctNm>
        </PstlAdr>
        <CtryOfRes>FS</CtryOfRes>
      </Dbtr>
      <DbtrAgt>
        <FinInstnId>
          <BICFI>QU01FX4THFO</BICFI>
        </FinInstnId>
      </DbtrAgt>
      <CdtrAgt>
        <FinInstnId>
          <BICFI>Q8C6WD4P</BICFI>
        </FinInstnId>
      </CdtrAgt>
      <Cdtr>
        <Nm>NSM 436l6w9V6 hsh</Nm>
        <PstlAdr>
          <AdrTp>
            <Prtry>
              <Id>6s0e</Id>
              <Issr>f7 50O</Issr>
            </Prtry>
          </AdrTp>
          <StrtNm>r0P38 sa L3 Mw7hpiX LH3 1 J 3hamf6o L T 61 Q121
          j1S 7Et</StrtNm>
          <BldgNb>RQV 7V6 XNG55 s</BldgNb>
          <PstCd>W iQ</PstCd>
          <TwnNm>d J8v 1kbW</TwnNm>
          <DstrctNm>v9 N2 7Gs8</DstrctNm>
        </PstlAdr>
      </Cdtr>
    </CdtTrfTxInf>
  </FIToFICstmrCdtTrf>
</Document>
```


