# Module `make_value`

The `make_value` module offers functions to generate randomised values that
can be specifically constrained using a description resembling regexp
pattern matching. The pattern instead of validating an input will produce a
string that complies with the pattern definition.

This module is put to use in the `schema_xsd:generate_from_schema/2`
function to generate entire XML messages containing random values that are
valid according to the ISO20022 XSD specification.  The XSD describes the
validity of string and number data-type that are more narrowly defined in
terms of regexp patterns, and digit lengths etc.


For example, in the ISO20022 PACS.002 XSD file, the data-type `BranchData3`
is defined as:

```
$> sed -n '/<xs:complexType name="BranchData3"/,/<\/xs:complexType>/p' protocol_tools/priv/pacs.002.001.13.xsd
    <xs:complexType name="BranchData3">
        <xs:sequence>
            <xs:element maxOccurs="1" minOccurs="0" name="Id" type="Max35Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="LEI" type="LEIIdentifier"/>
            <xs:element maxOccurs="1" minOccurs="0" name="Nm" type="Max140Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="PstlAdr" type="PostalAddress24"/>
        </xs:sequence>
    </xs:complexType>
```

The type `"Max35Text"` is a string containing any range of characters with
restrictions to its length:

```
$> sed -n '/<xs:simpleType name="Max35Text"/,/<\/xs:simpleType>/p' protocol_tools/priv/pacs.002.001.13.xsd
    <xs:simpleType name="Max35Text">
        <xs:restriction base="xs:string">
            <xs:minLength value="1"/>
            <xs:maxLength value="35"/>
        </xs:restriction>
    </xs:simpleType>
```

The `"LEIIdentifier"` is a string containing alphanumeric characters of
length 18 that must be followed with two digits:

```
$> sed -n '/<xs:simpleType name="LEIIdentifier"/,/<\/xs:simpleType>/p' protocol_tools/priv/pacs.002.001.13.xsd
    <xs:simpleType name="LEIIdentifier">
        <xs:restriction base="xs:string">
            <xs:pattern value="[A-Z0-9]{18,18}[0-9]{2,2}"/>
        </xs:restriction>
    </xs:simpleType>
```

The `"Max140Text"` can be up to 140 long, and contain any range of
characters:

```
$> sed -n '/<xs:simpleType name="Max140Text"/,/<\/xs:simpleType>/p' protocol_tools/priv/pacs.002.001.13.xsd
    <xs:simpleType name="Max140Text">
        <xs:restriction base="xs:string">
            <xs:minLength value="1"/>
            <xs:maxLength value="140"/>
        </xs:restriction>
    </xs:simpleType>
```
The `"PostalAddress24"` is a sequence of various types derived from the
string-type:

```
1425> sed -n '/<xs:complexType name="PostalAddress24"/,/<\/xs:complexType>/p' protocol_tools/priv/pacs.002.001.13.xsd
    <xs:complexType name="PostalAddress24">
        <xs:sequence>
            <xs:element maxOccurs="1" minOccurs="0" name="AdrTp" type="AddressType3Choice"/>
            <xs:element maxOccurs="1" minOccurs="0" name="Dept" type="Max70Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="SubDept" type="Max70Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="StrtNm" type="Max70Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="BldgNb" type="Max16Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="BldgNm" type="Max35Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="Flr" type="Max70Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="PstBx" type="Max16Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="Room" type="Max70Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="PstCd" type="Max16Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="TwnNm" type="Max35Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="TwnLctnNm" type="Max35Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="DstrctNm" type="Max35Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="CtrySubDvsn" type="Max35Text"/>
            <xs:element maxOccurs="1" minOccurs="0" name="Ctry" type="CountryCode"/>
            <xs:element maxOccurs="7" minOccurs="0" name="AdrLine" type="Max70Text"/>
        </xs:sequence>
    </xs:complexType>
```

The `"AddressType3Choice"` is either a `"AddressType2Code"` or a
`"GenericIdentification30"`:

```
$> sed -n '/<xs:complexType name="AddressType3Choice"/,/<\/xs:complexType>/p' protocol_tools/priv/pacs.002.001.13.xsd
    <xs:complexType name="AddressType3Choice">
        <xs:choice>
            <xs:element name="Cd" type="AddressType2Code"/>
            <xs:element name="Prtry" type="GenericIdentification30"/>
        </xs:choice>
    </xs:complexType>
```

The `"CountryCode"` is strictly two upper-case alphabet characters:

```
$> sed -n '/<xs:simpleType name="CountryCode"/,/<\/xs:simpleType>/p' protocol_tools/priv/pacs.002.001.13.xsd
    <xs:simpleType name="CountryCode">
        <xs:restriction base="xs:string">
            <xs:pattern value="[A-Z]{2,2}"/>
        </xs:restriction>
    </xs:simpleType>
```

The above patterns are repurposed to produce random values within the
XSD "restriction" described by the regexp pattern.


Generate an XML document fragment representing one of the data-types defined
in the XSD, the `"BranchData3"` data-type:

```
1> {ok,Bin} = file:read_file("protocol_tools/priv/pacs.002.001.13.xsd").
2> Pacs002 = codec_xml:decode(Bin).
3> Trim002 = schema_xsd:trim_namespace(Pacs002).
4> Schema002 = schema_xsd:from_document(Trim002).
5> maps:get({typedef,<<"BranchData3">>},Schema002).
#{name => <<"BranchData3">>,
  sequence =>
      [{element,#{name => <<"Id">>,type => <<"Max35Text">>,maxOccurs => 1,
                  minOccurs => 0}},
       {element,#{name => <<"LEI">>,type => <<"LEIIdentifier">>,
                  maxOccurs => 1,minOccurs => 0}},
       {element,#{name => <<"Nm">>,type => <<"Max140Text">>,maxOccurs => 1,
                  minOccurs => 0}},
       {element,#{name => <<"PstlAdr">>,type => <<"PostalAddress24">>,
                  maxOccurs => 1,minOccurs => 0}}]}
6> io:put_chars([
       codec_xml:encode(
         schema_xsd:generate_from_schema(
           <<"BranchData3">>,
           Schema002)),$\n]).
```

The above result of `io:put_char/1` fed through `tidy -xml -indent` to
reveal a pretty-print indented XML output:

```
<Id>N5UP7WZYA84MDv5RK6oHuVWjRm3</Id>
<LEI>BF6YRYL14YV2MVFFBU79</LEI>
<Nm>xU UpxtckTj1TxY6mviORE3KRipDkw di271CyOrEE
CZEs8MaTkLSVzDAHdoXHw4bji8bmVYPIXeDQkQ9MgAX6dwokC40slwlpfftmZXlZQaQ</Nm>
<PstlAdr>
  <AdrTp>
    <Cd>ADDR</Cd>
  </AdrTp>
  <Dept>AJM3FXhT0YNJgPnDiIAI1bZa25TGaYIcbrhBbOJ2UbFs41z4mtZD</Dept>
  <SubDept>STcOU1vGbIrbUe0tvwEfKz3HZJvpWkY6md8aQvqCo3Q5XG
  GPP4FygGUg</SubDept>
  <StrtNm>
  SbrSj41LYdf0qbNmmDlrABDZZG9keCYifFINkPtZzhW5n9CbYbQYs6adAwgD9u</StrtNm>
  <BldgNb>5EEWtUDZB 6</BldgNb>
  <BldgNm>cQYvCzZECU</BldgNm>
  <Flr>05NbT</Flr>
  <PstBx>sQ4a</PstBx>
  <Room>TnpCVMt9hTGvjDrzE0fg83yl1ovrrPWHaE</Room>
  <PstCd>AK0VINT3Enr94Y</PstCd>
  <TwnNm>7fO</TwnNm>
  <TwnLctnNm>x8TWWby6sufSWu4Ur jXktwaTDajN2</TwnLctnNm>
  <DstrctNm>ehGvkXp</DstrctNm>
  <CtrySubDvsn>iYleM</CtrySubDvsn>
  <Ctry>AI</Ctry>
  <AdrLine>9Zq4KEJaMmJR53</AdrLine>
</PstlAdr>
```



The implementation of `make_value` can serve for XML generation, and for
other protocol encoding where similar "restriction" regexp patterns are put
to use, such as in JSON schema files.

Below is a demonstration using a range of patterns found in the ISO20022 XSD
files:

```
Patterns =
 [<<"[A-Z]{3,3}">>,
  <<"[A-Z0-9]{4,4}[A-Z]{2,2}[A-Z0-9]{2,2}([A-Z0-9]{3,3}){0,1}">>,
  <<"[A-Z]{2,2}">>,
  <<"[0-9]{2}">>,
  <<"[a-zA-Z0-9]{4}">>,
  <<"[A-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}">>,
  <<"[A-Z0-9]{18,18}[0-9]{2,2}">>,
  <<"[0-9]{1,15}">>,
  <<"\+[0-9]{1,3}-[0-9()+\-]{1,30}">>,
  <<"[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}"
    "-[89ab][a-f0-9]{3}-[a-f0-9]{12}">>].

[make_value:from_regexp(P) || P <- Patterns].
```

The output, repeated a few times to show variations:

```
[<<"RLS">>,
 <<"EWZINN7L8ZW">>,
 <<"QV">>,
 <<"14">>,
 <<"u3LE">>,
 <<"HU53kvEWtOCqx96gomy3soge3p">>,
 <<"7G25H9RA7LO7S2UR4N46">>,
 <<"50711">>,
 <<"+265-090">>,
 <<"548e3c55-ce4f-4e0a-b09c-162321ce5880">>]

[<<"XVN">>,
 <<"2RZ1XRF8JUL">>,
 <<"QN">>,
 <<"60">>,
 <<"nJpC">>,
 <<"BH23zPvvEhZh3WAm3">>,
 <<"23037R1JOC6NN6S5XY41">>,
 <<"35916808859110">>,
 <<"+359-5))3)-2-117107+889-6+">>,
 <<"c3a5b848-1d6d-42a6-880a-6772fdda97f6">>]

[<<"NVO">>,
 <<"ZIJYIKJ4SX5">>,
 <<"CZ">>,
 <<"28">>,
 <<"pb3T">>,
 <<"AL20nMgbmfG7uXa8UMz9w0kAOSIhGGXo">>,
 <<"6QAR2V5KJGWED0VZQQ17">>,
 <<"0980628">>,
 <<"+90-3))+(66">>,
 <<"e34c22f1-935c-494b-be5e-38c0811020b1">>]
```

The function supports alternation, where either option is chosen at random
with equal frequency:


```
1> make_value:from_regexp(<<"abc|123">>).
<<"123">>
2> make_value:from_regexp(<<"abc|123">>).
<<"abc">>
```

Supports repetition. Note the parenthesis group:
(crashes without the group, might be a bug):

```
1> make_value:from_regexp(<<"(abc){1,9}">>).
<<"abcabcabcabcabcabcabcabc">>
2> make_value:from_regexp(<<"(abc){1,9}">>).
<<"abcabc">>
```

Supports selecting from a range of characters:

```
1> make_value:from_regexp(<<"[A-Z]">>).
<<"J">>
2> make_value:from_regexp(<<"[A-Z]">>).
<<"N">>
3> make_value:from_regexp(<<"[A-Z]">>).
<<"Y">>
```

Does not yet support the patterns for one-or-more `+` nor zero-or-more `*`
because the result is potentially infinite. Support for these two features
may require either imposing an upper limit to the generated output length,
or implementing just-in-time (lazy) evaluation.

