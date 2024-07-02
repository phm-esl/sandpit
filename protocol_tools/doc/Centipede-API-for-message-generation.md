# API for message generation

The message generator offers an application programming interface to allow a
front-end to request a message generated from e.g. an XML schema definition
that has specific elements filled with values also specified in the
request.

The request is subitted to the generator as a JSON document containing a
`base` document, and a `contents` array of objects with a `path` and a
`value` field.

An example of a request to generate an ISO20022 PACS.008 message.  Note that
the generator accepts paths that indicate attributes to be populated with
specifed valeus, such as `[@xmlns]` and `[@Ccy]`:

```
{
 "base":"pacs.008",
 "contents":[

  {"value":"urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11",
   "path":"/Document[@xmlns]"}

  {"value":"MLN",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/IntrBkSttlmAmt[@Ccy]"},

  {"value":" BUJ",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Cdtr/PstlAdr/DstrctNm"},
  {"value":"4iGn9aaz ",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Cdtr/PstlAdr/TwnNm"},
  {"value":"H 1vd 4BG",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Cdtr/PstlAdr/PstCd"},
  {"value":" d FUVq1 K C eC",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Cdtr/PstlAdr/BldgNb"},
  {"value":"T0O8sl r   97W5519  T5 lVW6U5H FA4YCGa8G840A gcg2P0tt  zA1lOLnfs",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Cdtr/PstlAdr/StrtNm"},
  {"value":" AE 7jZfUtvxz6X b dHkWbp",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Cdtr/PstlAdr/AdrTp/Prtry/Issr"},
  {"value":"yU15",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Cdtr/PstlAdr/AdrTp/Prtry/Id"},
  {"value":"YBe ",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Cdtr/Nm"},
  {"value":"G65MZZB201F",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/CdtrAgt/FinInstnId/BICFI"},
  {"value":"7XDLHWIU",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/DbtrAgt/FinInstnId/BICFI"},
  {"value":"HT",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/CtryOfRes"},
  {"value":"6OUWXv 1n hq  91rk 1qj15 4TLx",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/PstlAdr/DstrctNm"},
  {"value":"v7Wz mTUb 77h7",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/PstlAdr/TwnNm"},
  {"value":" D T81izCm",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/PstlAdr/PstCd"},
  {"value":"  ",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/PstlAdr/BldgNb"},
  {"value":"p 8 8y08XY1B52V j Nm 6 ej y19SZ0w  0CNb lc 9iM y k frews2y ",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/PstlAdr/StrtNm"},
  {"value":" Cd1tWzd03O1V  c3wisWtkd 0 3  H74",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/PstlAdr/AdrTp/Prtry/Issr"},
  {"value":"OQ6T",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/PstlAdr/AdrTp/Prtry/Id"},
  {"value":"N2 49qvqUS3KE96 v JvqS8Ys3C O3N 0g 2tD D 1lEa 6Jy M  wqzQ6 jK5 W xs2K8pr822 DLi rviz",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/Dbtr/Nm"},
  {"value":"SLEV",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/ChrgBr"},
  {"value":"87803084726.71",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/IntrBkSttlmAmt"},
  {"value":"9ypIWBl wSo0BZi40m   W jF",
   "path":"/Document/FIToFICstmrCdtTrf/CdtTrfTxInf/PmtId/EndToEndId"},
  {"value":"INGA",
   "path":"/Document/FIToFICstmrCdtTrf/GrpHdr/SttlmInf/SttlmMtd"},
  {"value":"850486628",
   "path":"/Document/FIToFICstmrCdtTrf/GrpHdr/NbOfTxs"},
  {"value":"2024-07-01T16:20:46",
   "path":"/Document/FIToFICstmrCdtTrf/GrpHdr/CreDtTm"},
  {"value":"JhWKH s Iw9 9 7   xLT6r9 ",
   "path":"/Document/FIToFICstmrCdtTrf/GrpHdr/MsgId"}
 ]
}
```

An example of the result generated for the above request:

```
<?xml version="1.0" encoding="utf-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">
  <FIToFICstmrCdtTrf>
    <GrpHdr>
      <MsgId>JhWKH s Iw9 9 7 xLT6r9</MsgId>
      <CreDtTm>2024-07-01T16:20:46</CreDtTm>
      <NbOfTxs>850486628</NbOfTxs>
      <SttlmInf>
        <SttlmMtd>INGA</SttlmMtd>
      </SttlmInf>
    </GrpHdr>
    <CdtTrfTxInf>
      <PmtId>
        <EndToEndId>9ypIWBl wSo0BZi40m W jF</EndToEndId>
      </PmtId>
      <IntrBkSttlmAmt Ccy="MLN">87803084726.71</IntrBkSttlmAmt>
      <ChrgBr>SLEV</ChrgBr>
      <Dbtr>
        <Nm>N2 49qvqUS3KE96 v JvqS8Ys3C O3N 0g 2tD D 1lEa 6Jy M
        wqzQ6 jK5 W xs2K8pr822 DLi rviz</Nm>
        <PstlAdr>
          <AdrTp>
            <Prtry>
              <Id>OQ6T</Id>
              <Issr>Cd1tWzd03O1V c3wisWtkd 0 3 H74</Issr>
            </Prtry>
          </AdrTp>
          <StrtNm>p 8 8y08XY1B52V j Nm 6 ej y19SZ0w 0CNb lc 9iM y k
          frews2y</StrtNm>
          <BldgNb></BldgNb>
          <PstCd>D T81izCm</PstCd>
          <TwnNm>v7Wz mTUb 77h7</TwnNm>
          <DstrctNm>6OUWXv 1n hq 91rk 1qj15 4TLx</DstrctNm>
        </PstlAdr>
        <CtryOfRes>HT</CtryOfRes>
      </Dbtr>
      <DbtrAgt>
        <FinInstnId>
          <BICFI>7XDLHWIU</BICFI>
        </FinInstnId>
      </DbtrAgt>
      <CdtrAgt>
        <FinInstnId>
          <BICFI>G65MZZB201F</BICFI>
        </FinInstnId>
      </CdtrAgt>
      <Cdtr>
        <Nm>YBe</Nm>
        <PstlAdr>
          <AdrTp>
            <Prtry>
              <Id>yU15</Id>
              <Issr>AE 7jZfUtvxz6X b dHkWbp</Issr>
            </Prtry>
          </AdrTp>
          <StrtNm>T0O8sl r 97W5519 T5 lVW6U5H FA4YCGa8G840A
          gcg2P0tt zA1lOLnfs</StrtNm>
          <BldgNb>d FUVq1 K C eC</BldgNb>
          <PstCd>H 1vd 4BG</PstCd>
          <TwnNm>4iGn9aaz</TwnNm>
          <DstrctNm>BUJ</DstrctNm>
        </PstlAdr>
      </Cdtr>
    </CdtTrfTxInf>
  </FIToFICstmrCdtTrf>
</Document>
```