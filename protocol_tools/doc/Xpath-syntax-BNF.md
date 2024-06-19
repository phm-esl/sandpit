# Examples

| You want to pick an element:      | Syntax                                | Example                                |
| --------------------------------- | ------------------------------------- | -------------------------------------- |
| is anywhere in the DOM with tag   | `//tagName`                           | `//span`                               |
| has attribute name & value        | `//tagName[@id="idValue"]`            | `//div[@id="main-product"`             |
| is child of element with tag      | `//parent/child`                      | `//div/span`                           |
| is first child of element         | `//parent/child[1]`                   | `//div[@class="quote"]/span[1]`        |
| is parent of element              | `//child/..`                          | `//span[@class="text"/..`              |
| and get value of an attribute     | `//tagName/@attrName`                 | `//a/@href`                            |
| with matching text                | `//tagName[text() = "pattern"]`       | `//a[text() = "Next"]`                 |
| with matching part of text        | `//tagName[contains(text,"pattern")]` | `//span[contains(text(),"by")]`        |
| last element of a list            | `//parent/child[last()]`              | `//div[@class = "quote"]/span[last()]` |



# Appendix - tutorial

[Comprehensive XPath tutorial](https://www.softwaretestinghelp.com/xml-path-language-xpath-tutorial/)


## Types Of XPath Node

* Element Nodes: nodes directly under the root node. May contain attributes. Is an XML tag.
* Attribute Nodes: attribute of the element node. Element node is the parent. The shortcut forattribute nodes is `@`.
* Text Nodes: All the text inside the element node
* Comment Nodes: in between `<!--` ... `-->`
* Namespaces: used to remove ambiguity between more than one set of the XML element names.
* Processing Instructions: in between `<?` ... `?>`.
* Root Node: contains all the child elements inside it, does not have a parent node.




## Axes In XPath

* Self-axis: Select the Context Node. The XPath expression `self::*` and `.` are equivalent.
* Child axis: Select the children of the Context Node. Excludes Namespace node and the attribute node.
* Parent axis: Select the parent of the context node. Root node parent is empty. `(parent:: State)` and `(../State)` are equivalent.
* Attribute axis: Select the attribute of the context node. `(attribute::name)` and `(@name)` are equivalent.
* Ancestor axis: Select all parents of the context node up to and including the root node.
* Ancestor-or-self: Select the context node in addition to above.
* Descendant axis: Select all the children of the context node, and their children. Namespace node and attribute not part of the descendant axis.
* Descendant-or-self: Select the context node in addition to above.
* Preceding axis: Select all the nodes that come before the context node, without Namespace, ancestors and attributes.
* Preceding-sibling axis: Select all preceding siblings of the context node, with same parent, without Namespace nor attributes.
* Following axis: Select all nodes after the context node in the document. Namespace, attribute, and descendants excluded.
* Following-sibling axis: All nodes that come after the context node with the same parent. Excludes namespace or attribute.
* Namespace: Select the namespace nodes of the context node. Empty if context node is not an element node.




## Datatypes In XPath

Given below are the various Datatypes in XPath.

* Number: floating-point number. Integer datatype does not consider in XPath.
* Boolean: either true or false.
* String: zero or more characters.
* Node-set: set of zero or more nodes.


## Wildcards In XPath

Enlisted below are the Wildcards in XPath.

* An asterisk (*): This will select all the element nodes of the context node. It will select the text nodes, comments, processing instructions and attributes node.
* At-sign with an asterisk (@*): This will select all the attribute nodes of the context node.
* Node(): This will select all the nodes of the context node. These select namespaces, text, attributes, elements, comments and processing instructions.


## XPath Operators

Note: In the below table, e stands for any XPath expression.

| Operators   | Description                                             | Example                           |
| ----------- | ------------------------------------------------------- | --------------------------------- |
| `e1 + e2`   | Additions (if e1 and e2 are numbers)                    | 5 + 2                             |
| `e1 – e2`   | Subtraction (if e1 and e2 are numbers)                  | 10 – 4                            |
| `e1 * e2`   | Multiplication (if e1 and e2 are numbers)               | 3 * 4                             |
| `e1 div e2` | Division (if e1 and e2 are numbers and result is float) | 4 div 2                           |
| `e1 | e2`   | union of two nodes that match e1 and match e2.          | `//State | //country`             |
| `e1 = e2`   | Equals                                                  | `@name = ’T1’`                    |
| `e1 != e2`  | Not Equal                                               | `@name != ’T1’`                   |
| `e1 < e2`   | Test of e1 is less than e2 (‘<’ must be excaped)        | `test=”5 < 9”` result is true()   |
| `e1 > e2`   | Test of e1 is greater than e2 (‘>’ must be excaped)     | `test=”5 > 9”` result is false()  |
| `e1 <= e2`  | Test of e1 is less than or equal to e2.                 | `test=”5 <= 9”` result is false() |
| `e1 >= e2`  | Test of e1 is greater than or equal to e2.              | `test=”5 >= 9”` result is false() |
| `e1 or e2`  | Evaluated if either e1 or e2 are true.                  |                                   |
| `e1 and e2` | Evaluated if both e1 and e2 are true.                   |                                   |
| `e1 mod e2` | Returns floating-point remainder of e1 divided by e2.   | `7 mod 2`                         |



# Appendix

[Original document](https://www.w3.org/2002/11/xquery-xpath-applets/xpath-bnf.html)

## 1.2 BNF

The following grammar uses the same Basic EBNF notation as
[XML](https://www.w3.org/2002/11/xquery-xpath-applets/xpath-bnf.html),
except that grammar symbols always have initial capital letters.  The EBNF
contains the lexemes embedded in the productions.

Note:

Note that the Semicolon character is reserved for future use.

### NON-TERMINALS

```
[13] XPath ::= ExprSequence?

[14] ExprSequence ::= Expr ("," Expr)*

[15] Expr ::= OrExpr

[16] OrExpr ::= AndExpr ( "or" AndExpr )*

[17] AndExpr ::= ForExpr ( "and" ForExpr )*

[18] ForExpr ::= (SimpleForClause "return")* QuantifiedExpr

[19] QuantifiedExpr ::= ((<"some" "$"> | <"every" "$">) VarName "in" Expr ("," "$" VarName "in" Expr)* "satisfies")* IfExpr

[20] IfExpr ::= (<"if" "("> Expr ")" "then" Expr "else")* InstanceofExpr

[21] InstanceofExpr ::= CastableExpr ( <"instance" "of"> SequenceType )?

[22] CastableExpr ::= ComparisonExpr ( <"castable" "as"> SingleType )?

[23] ComparisonExpr
     ::= RangeExpr ( ( ValueComp
       | GeneralComp
       | NodeComp
       | OrderComp ) RangeExpr )?

[24] RangeExpr ::= AdditiveExpr ( "to" AdditiveExpr )?

[25] AdditiveExpr ::= MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*

[26] MultiplicativeExpr ::= UnaryExpr ( ("*" | "div" | "idiv" | "mod") UnaryExpr )*

[27] UnaryExpr ::= ("-" | "+")* UnionExpr

[28] UnionExpr ::= IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*

[29] IntersectExceptExpr ::= ValueExpr ( ("intersect" | "except") ValueExpr )*

[30] ValueExpr ::= ValidateExpr | CastExpr | TreatExpr | PathExpr

[31] PathExpr ::= ("/" RelativePathExpr?) | ("//" RelativePathExpr) | RelativePathExpr

[32] RelativePathExpr ::= StepExpr (("/" | "//") StepExpr)*

[33] StepExpr ::= (ForwardStep | ReverseStep | PrimaryExpr) Predicates

[34] SimpleForClause ::= <"for" "$"> VarName "in" Expr ("," "$" VarName "in" Expr)*

[35] ValidateExpr ::= (<"validate" "{"> | (<"validate" "context"> SchemaGlobalContext ("/" SchemaContextStep)* "{")) Expr "}"

[36] CastExpr ::= <"cast" "as"> SingleType ParenthesizedExpr

[37] TreatExpr ::= <"treat" "as"> SequenceType ParenthesizedExpr

[38] GeneralComp
     ::= "="
       | "!="
       | "<"
       | "<="
       | ">"
       | ">="
[39] ValueComp ::= "eq" | "ne" | "lt" | "le" | "gt" | "ge"

[40] NodeComp ::= "is" | "isnot"

[41] OrderComp ::= "<<" | ">>"

[42] PrimaryExpr ::= Literal | FunctionCall | ("$" VarName) | ParenthesizedExpr

[43] ForwardAxis
     ::= <"child" "::">
       | <"descendant" "::">
       | <"attribute" "::">
       | <"self" "::">
       | <"descendant-or-self" "::">
       | <"following-sibling" "::">
       | <"following" "::">
       | <"namespace" "::">

[44] ReverseAxis
     ::= <"parent" "::">
       | <"ancestor" "::">
       | <"preceding-sibling" "::">
       | <"preceding" "::">
       | <"ancestor-or-self" "::">

[45] NodeTest ::= KindTest | NameTest

[46] NameTest ::= QName | Wildcard

[47] Wildcard ::= "*" | <NCName ":" "*"> | <"*" ":" NCName>

[48] KindTest
     ::= ProcessingInstructionTest
       | CommentTest
       | TextTest
       | AnyKindTest

[49] ProcessingInstructionTest ::= <"processing-instruction" "("> StringLiteral? ")"

[50] CommentTest ::= <"comment" "("> ")"

[51] TextTest ::= <"text" "("> ")"

[52] AnyKindTest ::= <"node" "("> ")"

[53] ForwardStep ::= (ForwardAxis NodeTest) | AbbreviatedForwardStep

[54] ReverseStep ::= (ReverseAxis NodeTest) | AbbreviatedReverseStep

[55] AbbreviatedForwardStep ::= "." | ("@" NameTest) | NodeTest

[56] AbbreviatedReverseStep ::= ".."

[57] Predicates ::= ("[" Expr "]")*

[58] NumericLiteral ::= IntegerLiteral | DecimalLiteral | DoubleLiteral

[59] Literal ::= NumericLiteral | StringLiteral

[60] ParenthesizedExpr ::= "(" ExprSequence? ")"

[61] FunctionCall ::= <QName "("> (Expr ("," Expr)*)? ")"

[62] SchemaContext ::= "context" SchemaGlobalContext ("/" SchemaContextStep)*

[63] SchemaGlobalContext ::= QName | <"type" QName>

[64] SchemaContextStep ::= QName

[65] SingleType ::= AtomicType "?"?

[66] SequenceType ::= (ItemType OccurrenceIndicator) | "empty"

[67] ItemType
     ::= (("element" | "attribute") ElemOrAttrType?)
       | "node"
       | "processing-instruction"
       | "comment"
       | "text"
       | "document"
       | "item"
       | AtomicType
       | "untyped"
       | <"atomic" "value">

[68] ElemOrAttrType ::= (QName (SchemaType | SchemaContext?)) | SchemaType

[69] SchemaType ::= <"of" "type"> QName

[70] AtomicType ::= QName

[71] OccurrenceIndicator ::= ("*" | "+" | "?")?
```

## 1.3 Reserved Function Names

The following is a list of names that may not be used as user function
names, in an unprefixed form.

* `if`
* `typeswitch`
* `item`
* `node`
* `element`
* `attribute`
* `comment`
* `text`
* `processing-instruction`
* `id`
* `key`

## 1.4 Precedence Order

In all cases the grammar defines built-in precedence. In the cases where a
number of statements are a choice at the same production level, the
expressions are always evaluated from left to right.
