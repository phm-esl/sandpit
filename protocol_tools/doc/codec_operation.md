

# Modifying values

When encoding a message the `codec_xml:encode/2` accepts a map as the second
parameter representing locations of values within the message.  The encode
function returns control to the calling context offering to inspect the
value, and the option to swtich the offered value with an alternative.

For example, the unit test provides a log:

```
flow_pacs_test:test_insert_loop:85
	Atom = ['Document','FIToFICstmrCdtTrf','GrpHdr','MsgId'].
	Value = [<<"r4yNqTmw 48 FH7h 4   dy82Q BnnLBj">>].
flow_pacs_test:test_insert_loop:85
	Atom = ['Document','FIToFICstmrCdtTrf','GrpHdr','CreDtTm'].
	Value = [<<"2024-06-13T17:32:26">>].
flow_pacs_test:test_insert_loop:85
	Atom = ['Document','FIToFICstmrCdtTrf','GrpHdr','NbOfTxs'].
	Value = [<<"4166315">>].
flow_pacs_test:test_insert_loop:85
	Atom = ['Document','FIToFICstmrCdtTrf','GrpHdr','SttlmInf','SttlmMtd'].
	Value = [<<"INGA">>].
flow_pacs_test:test_insert_loop:85
	Atom = ['Document','FIToFICstmrCdtTrf','CdtTrfTxInf','PmtId',
                'EndToEndId'].
	Value = [<<" s8wZi3P  xWzhPFsnS9">>].
...
```

And so on for all the locations specified in the extraction map parameter.

The test writes the `original.xml` file containing encoded message output,
and the `modified.xml`. The commands below pretty-print the files, and shows
the differences:

```
tidy -xml -indent -modify original.xml modified.xml
diff original.xml modified.xml 
```

The diff reveals where the message vales have changed:

```
$> diff original.xml modified.xml 
5,6c5,6
<       <MsgId>an9042 YJ16f r5 x6 D1Q d</MsgId>
<       <CreDtTm>2024-06-13T17:25:20</CreDtTm>
---
>       <MsgId>******* CHANGED VALUE *******</MsgId>
>       <CreDtTm>******* CHANGED VALUE *******</CreDtTm>
8c8
<       <NbOfTxs>327455810212185</NbOfTxs>
---
>       <NbOfTxs>******* CHANGED VALUE *******</NbOfTxs>
14c14
<         <SttlmMtd>INGA</SttlmMtd>
---
>         <SttlmMtd>******* CHANGED VALUE *******</SttlmMtd>
529c529
<         <EndToEndId>xZ3 6m8UJ9dklyi6</EndToEndId>
---
>         <EndToEndId>******* CHANGED VALUE *******</EndToEndId>
547c547,548
<       <IntrBkSttlmAmt Ccy="UTK">126026205.1</IntrBkSttlmAmt>
---
>       <IntrBkSttlmAmt Ccy="UTK">******* CHANGED VALUE
>       *******</IntrBkSttlmAmt>
564c565
<       <ChrgBr>CRED</ChrgBr>
---
>       <ChrgBr>******* CHANGED VALUE *******</ChrgBr>
1615c1616
<         <Nm>Nz 9MFU g cn D0f9 O elX060 9 97 TH3uL 0 96</Nm>
---
>         <Nm>******* CHANGED VALUE *******</Nm>
1618c1619
<             <Cd>HOME</Cd>
---
>             <Cd>******* CHANGED VALUE *******</Cd>
1623,1624c1624,1625
<           <StrtNm>p KX dqZ</StrtNm>
<           <BldgNb>r q05dP7i</BldgNb>
---
>           <StrtNm>******* CHANGED VALUE *******</StrtNm>
>           <BldgNb>******* CHANGED VALUE *******</BldgNb>
1629,1630c1630,1631
<           <PstCd>aZ7 AGx2I</PstCd>
<           <TwnNm>T3fw 4</TwnNm>
---
>           <PstCd>******* CHANGED VALUE *******</PstCd>
>           <TwnNm>******* CHANGED VALUE *******</TwnNm>
1632c1633
<           <DstrctNm>88b38o kPi69Ia</DstrctNm>
---
>           <DstrctNm>******* CHANGED VALUE *******</DstrctNm>
1651c1652
<         <CtryOfRes>ZN</CtryOfRes>
---
>         <CtryOfRes>******* CHANGED VALUE *******</CtryOfRes>
1654,1655c1655
<           <Nm>j 4Ws l9Ir9 UCts3GcKBRtrJ c3l3x LqXP 0039 Sz HvQ RQ2
<           1 l r A9yA</Nm>
---
>           <Nm>******* CHANGED VALUE *******</Nm>
1721c1721
<           <BICFI>05Z6BI8G</BICFI>
---
>           <BICFI>******* CHANGED VALUE *******</BICFI>
1815c1815
<           <BICFI>7U9SJU08V5K</BICFI>
---
>           <BICFI>******* CHANGED VALUE *******</BICFI>
1934,1935c1934
<         <Nm>k 7O v O67 r1W PD M ct09 nP e 3
<         a0pbIdCwyxhi6y2tVLD6odGm3jb7ZOg</Nm>
---
>         <Nm>******* CHANGED VALUE *******</Nm>
1938c1937
<             <Cd>PBOX</Cd>
---
>             <Cd>******* CHANGED VALUE *******</Cd>
1943,1944c1942,1943
<           <StrtNm>5qaG ql Tif0 0jtnd 66 8sz</StrtNm>
<           <BldgNb>X S Brd</BldgNb>
---
>           <StrtNm>******* CHANGED VALUE *******</StrtNm>
>           <BldgNb>******* CHANGED VALUE *******</BldgNb>
1950,1951c1949,1950
<           <PstCd>8</PstCd>
<           <TwnNm>j ip</TwnNm>
---
>           <PstCd>******* CHANGED VALUE *******</PstCd>
>           <TwnNm>******* CHANGED VALUE *******</TwnNm>
1953c1952
<           <DstrctNm>jC 7 Q 64q3o5Wc9 kb Jx GB eXv I</DstrctNm>
---
>           <DstrctNm>******* CHANGED VALUE *******</DstrctNm>
1978,1979c1977
<           <Nm>a f3GDM Xv2632u5g8Dj3hBr2 hm1XEen o xOrLn403 Wav1 KC
<           6D b yWTBj 961JpP</Nm>
---
>           <Nm>******* CHANGED VALUE *******</Nm>
2166,2167c2164
<             <Nm>Oo u 6 i7 Ab2e9l83m 3Q ftumhw z m 7o y66Tx 7 5 ee
<             lq98sw3K27O0WS7jt 77 2202 hpno97BM1 3 5 5 3Y F</Nm>
---
>             <Nm>******* CHANGED VALUE *******</Nm>
2555,2556c2552
<                 <Nm>tj l7 20l4Je 7 Hd 8 82L E 8 c TRW2 RA Il I
<                 tPnXl c 1C1 2 Cpn XxaCduW R QHE0UG VhmIf1t 27</Nm>
---
>                 <Nm>******* CHANGED VALUE *******</Nm>
```
