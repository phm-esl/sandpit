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

