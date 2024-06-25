# Centipede template programming manual

Centipede tool to make protocol testing simple, easy and fast.

> Note-0: This is a work-in-progress, subject to change after document reviews.

## Intro

Testing a protocol requires making messages to send into a device, and to
receive messages from it.  The messages are almost always a linear string of
bytes that represent data that could have complex structure.  The structure
of the message, the form it takes, can be ignored for simplicity, and
instead be conceived as an interleaved sequence of binary segments, with the
information values inserted between the segments in “slots” to fill the form
of the message.  For testing purposes this is sufficient, and the simplicity
allows tests to prove invalid input is treated appropriately too, bypassing
the formal constraints of the protocol that might normally prevent such
invalid examples.

Centipede applies this concept of linear message form with slots filled with
values.  This enables tests to be constructed so that the underlying
protocol can be almost entirely abstracted, such that one protocol can be
substituted with another, and the test definition not need any modification.

This document describes how this abstraction of protocol encodings is
achieved in practice.

## Filling slots in the message

First lets focus on the insertion of values into a message form being
filled.  The program code that does this is a loop that iterates over the
encoded form of the message that will pause to accept instruction from the
test application.  Below is a simple Erlang function to illustrate.

The Encoder returned from calling the `codec_xml:encode/2` function is
either one of two things.  It is either:

* The `In` tuple containing a continuation `Fn` along with the proposed `Original` value that the template offers as default, and the proposed `Insertion` tuple that contains the value defined in the `Populated_value_map`. (line 06);

* Or it is `Out`,the entire encoded message binary when all the values have been provided to fill all the slots in the message form (line 09).

[More detail about the `Populated_value_map` parameter below.](#the-value-map)

The `Insertion` is a list of atoms that describe the “insertion path” inside
the message form, except for the head of the list.  At the head is a tuple
containing the atom identifying the slot in the message form, the value to
insert into the slot, and meta-data related to the slot (line 07).

In this example with XML, the meta-data is a map of name-value pairs
representing XML element attributes.  The meta-data may also contain e.g.
a positional index if there are many slots that match the same “insertion
path” so that a sequence of similar elements can be treated appropriately. 
The example does not show this.

This example takes the `Inject` value from the `Insertion` list and calls the
continuation `Fn` with that value to be inserted into the message form.  Then
the process repeats in a loop until the final `Out` binary result is returned
from calling `Fn`.

```
01 start() ->
02	Encoder = codec_xml:encode(Decoded_template,Populated_value_map),
03	insert_loop(Encoder).

04 insert_loop(In) ->
05	case In of
06		{Fn,_Original,Insertion} when is_function(Fn) ->
07			[{_Slot,Inject,_Attr}|_Trail] = Insertion,
08			insert_loop( Fn(Inject) );
09		Out when is_binary(Out) ->
10			Out
11 end.
```

> Note-1: the tuple layout returned by the Fn closure may change subject to document review

## The value map

The structure of the `Populated_value_map` in this example is:

```
#{'Document' =>
  #{'FIToFICstmrDrctDbt' =>
    #{'GrpHdr' =>
      #{'CreDtTm' =>
          <<"**** Changed CreDtTm value ****">>,
        'MsgId' =>
          <<"**** Changed MsgId value ****">>,
        'NbOfTxs' =>
          <<"**** Changed NbOfTxs value ****">>,
        'SttlmInf' =>
          #{'SttlmMtd' =>
              <<"**** Changed SttlmMtd value ****">>}},
            'DrctDbtTxInf' =>
          #{'IntrBkSttlmAmt' =>
              <<"**** Changed IntrBkSttlmAmt value ****">>,
            'PmtId' =>
              #{'EndToEndId' =>
                  <<"**** Changed EndToEndId value ****">>},
            {xpath,'IntrBkSttlmAmt'} =>
              #{attr =>
                #{<<"Ccy">> => <<"**** Changed Ccy value ****">>}}}}}}
```

The map contains keys corresponding to the names of elements within the
message form.  The tree structure of the message form is reflected in the
map, with maps within maps.

In the message template there is a slot for the `"Ccy"` attribute in the
`'IntrBkSttlmAmt'` element.  The `Populated_value_map` describes the
location of this slot in "meta-data", with the `{xpath,'IntrBkSttlmAmt'}`
key that connects the `'attr'` value to the XML attributes for the
`'IntrBkSttlmAmt'` XML element.

> Note-2: the data type `{xpath,...}` that is used as a key for metadata
> should be reviewed, and changed if the key does not fit well. Perhaps
> `{meta,'IntrBkSttlmAmt'}` will avoid implicit XPath interpretation, and
> allow for use in other protocols?

> Note-3: the keys used to identify value slot locations are atoms. This is
> not strictly necessary, the locations could alternatively be identified
> with e.g. `<<"Document">>` instead of `'Document'`, and avoid a conversion
> from binary text string to an existing atom. Searching the Erlang atom
> table acts like a filter to discard loactions of no interest, but may
> offer no practical advantage for the cost of more complex program code.

## Encoded output

The resulting `Out` value looks like this pretty-printed, the actual value
does not have whitespace nor indentation:

```
<?xml version="1.0" encoding="utf-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.11">
  <FIToFICstmrCdtTrf>
    <GrpHdr>
      <MsgId>**** Changed MsgId value ****</MsgId>
      <CreDtTm>**** Changed CreDtTm value ****</CreDtTm>
      <NbOfTxs>**** Changed NbOfTxs value ****</NbOfTxs>
      <SttlmInf>
        <SttlmMtd>**** Changed SttlmMtd value ****</SttlmMtd>
      </SttlmInf>
    </GrpHdr>
    <CdtTrfTxInf>
      <PmtId>
        <EndToEndId>**** Changed EndToEndId value ****</EndToEndId>
      </PmtId>
      <IntrBkSttlmAmt Ccy="**** Changed Ccy value ****">****
      Changed IntrBkSttlmAmt value ****</IntrBkSttlmAmt>
      <ChrgBr>**** Changed ChrgBr value ****</ChrgBr>
      <Dbtr>
        <Nm>**** Changed Nm value ****</Nm>
        <PstlAdr>
          <AdrTp>
            <Prtry>
              <Id>Xgbs</Id>
              <Issr>LO1 Ue J 6e38w1r53 73 99 uUbv0</Issr>
            </Prtry>
          </AdrTp>
          <StrtNm>**** Changed StrtNm value ****</StrtNm>
          <BldgNb>**** Changed BldgNb value ****</BldgNb>
          <PstCd>**** Changed PstCd value ****</PstCd>
          <TwnNm>**** Changed TwnNm value ****</TwnNm>
          <DstrctNm>**** Changed DstrctNm value ****</DstrctNm>
        </PstlAdr>
        <CtryOfRes>**** Changed CtryOfRes value ****</CtryOfRes>
      </Dbtr>
      <DbtrAgt>
        <FinInstnId>
          <BICFI>**** Changed BICFI value ****</BICFI>
        </FinInstnId>
      </DbtrAgt>
      <CdtrAgt>
        <FinInstnId>
          <BICFI>**** Changed BICFI value ****</BICFI>
        </FinInstnId>
      </CdtrAgt>
      <Cdtr>
        <Nm>**** Changed Nm value ****</Nm>
        <PstlAdr>
          <AdrTp>
            <Cd>**** Changed Cd value ****</Cd>
          </AdrTp>
          <StrtNm>**** Changed StrtNm value ****</StrtNm>
          <BldgNb>**** Changed BldgNb value ****</BldgNb>
          <PstCd>**** Changed PstCd value ****</PstCd>
          <TwnNm>**** Changed TwnNm value ****</TwnNm>
          <DstrctNm>**** Changed DstrctNm value ****</DstrctNm>
        </PstlAdr>
      </Cdtr>
    </CdtTrfTxInf>
  </FIToFICstmrCdtTrf>
</Document>
```
