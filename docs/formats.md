# Formats

jaq supports reading and writing several data formats.
This section describes these data formats.

## JSON

JSON is specified in [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259).
A good overview of its syntax is given at <https://www.json.org>.

## XJON

The native data format of jaq is a superset of JSON called *XJON*
(eXtended JavaScript Object Notation, pronounced like "action").

XJON extends JSON with following constructs:

- Line comments: `# ... \n` is interpreted as comment
- Special floating-point numbers: `NaN`, `Infinity`, `-Infinity`
- Numbers starting with `+`:
  Every number that may be prefixed with `-` (minus)
  may also be prefixed with `+` (plus), e.g.
  `+7`, `+Infinity`.
- UTF-8 strings with invalid code points:
  The JSON standard is slightly ambiguous whether
  strings may contain invalid UTF-8 code points.
  XJON explicitly allows for invalid code points in UTF-8 strings,
  e.g. the output of `printf '"\xFF"'`.
  This increases compatibility with tools that output such strings (e.g. file names).
  Furthermore, it allows for constant-time loading of strings via [`--raw-input`](#--raw-input),
  where jq takes linear time due to UTF-8 validation.
- Byte strings:
  A byte string is created via `b"..."`, where `...` is a sequence of:

    - bytes in the range 0x20 to (including) 0xFF,
      excluding the ASCII characters `'"'` and `'\'`
    - an escape sequence, starting with a backslash (`'\'`) and followed by
      `b`, `f`, `n`, `r`, `t`, `'"'`, `'\'`, or
      `xHH`, where `HH` is a hexadecimal number

  For example: `b"Here comes \xFF, dadadada\nHere comes \xFF\nAnd I say: \"It's alright\"\x00"`.
  Byte strings of this shape can also be found in other languages, like
  Rust & Python (with leading `b`) and JavaScript & C (without leading `b`).
- Objects with non-string keys:
  Where JSON limits object keys to strings,
  XJON allows arbitrary values as object keys.
  For example: `{null: 0, true: 1, 2: 3, "str": 4, ["arr"]: 5, {}: 6}`

The goal behind XJON was to
support a set of values present in YAML and CBOR, namely
byte strings and objects with non-string keys, while keeping the format
both human-readable and simple & performant to parse, like JSON.

XJON perfectly preserves all values that are processed by jaq.
The same is not true for JSON and `jq`;
for example,
`jq -n 'nan | isnan'` does not yield the same output as
`jq -n 'nan' | jq 'isnan'`,
because `jq` prints NaN as `null`, thus losing information.

Currently, wherever jaq accepts JSON, it also accepts XJON.
That means that
`jaq --from json <<< 'NaN b"Bytes" {1: 2} # Over and out'` yields
`'NaN b"Bytes" {1: 2}`, although the input is XJON, not valid JSON.

## YAML

## CBOR

## TOML

## XML

jaq reads data adhering to the [XML 1.0](https://www.w3.org/TR/xml/) standard.
However, it treats only XML data encoded as UTF-8.

jaq can read XHTML files, but it cannot directly read HTML files.
You can use tools such as
[html2xhtml](https://github.com/jfisteus/html2xhtml) to convert HTML to XHTML.

Mappings between XML to JSON generally have to make a compromise between
"friendliness" and round-tripping;
see "[Experiences with JSON and XML Transformations]".
Here, "friendliness" means that JSON generated from XML has a flat structure,
making it easy to consume it.
Stefan Goessner gives a nice discussion of different "friendly" mappings in
"[Converting Between XML and JSON]".
The take-away message is: "Friendly" mappings lose information.
For that reason, jaq does not use a "friendly" mapping, but rather
a mapping that preserves XML information perfectly,
making it suitable for round-tripping.

[Experiences with JSON and XML Transformations]: https://www.w3.org/2011/10/integration-workshop/p/XML_JSON_mapping_paper.pdf
[Converting Between XML and JSON]: https://www.xml.com/pub/a/2006/05/31/converting-between-xml-and-json.html

As an example, consider the following input:

``` xml
<a href="https://www.w3.org">World Wide Web Consortium (<em>W3C</em>)</a>
```

We can see its internal representation in jaq by:

```
$ echo '<a href="https://www.w3.org">World Wide Web Consortium (<em>W3C</em>)</a>' | jaq --from xml .
```

This yields the following JSON:

``` json
{
  "t": "a",
  "a": { "href": "https://www.w3.org" },
  "c": [
    "World Wide Web Consortium (",
    { "t": "em", "c": [ "W3C" ] },
    ")"
  ]
}
```

### Tags or The TAC Architecture

Tags are represented by "TAC" objects. A TAC object may have the following fields:

- `t`: Name of the tag, such as `h1` for `<h1>...</h1>`.
  This field must always be present in a TAC object.
- `a`: Attributes of the tag, such as `{"id": "foo", style: "color:blue;"}`.
  If this field is present, it must contain an object with string values.
- `c`: Children of the tag, usually an array of XML values.
  If this field is not present, this tag will be interpreted as self-closing (such as `<br/>`).

An example query to obtain all links in an XHTML file:

``` jq
.. | select(.t? == "a") | .a.href
```

We can also transform input XML and yield output XML.
For example, to transform all `em` tags to `i` tags:

``` jq
(.. | select(.t? == "em") | .t) = "i"
```

To yield XML output instead of JSON output, use the option `--to xml`:

```
$ echo '<a href="https://www.w3.org">World Wide Web Consortium (<em>W3C</em>)</a>' | jaq --from xml --to xml '(.. | select(.t? == "em") | .t) = "i"'
<a href="https://www.w3.org">World Wide Web Consortium (<i>W3C</i>)</a>
```

Finally, we can extract all text from an XML file (discarding CDATA blocks):

``` jq
def xml_text: if isstring then . else .c[]? | xml_text end; [xml_text]
```

### Other XML values

- Strings are neither escaped nor unescaped; that means,
   `Tom &amp; Jerry`  in the source XML becomes
  `"Tom &amp; Jerry"` in the target JSON.
- A comment such as `<!-- this comment -->`{.xml} is converted to
  `{"comment": " this comment "}`{.json}.
- A CDATA block such as `<![CDATA[Tom & Jerry]]>`{.xml} is converted to
  `{"cdata": "Tom & Jerry"}`{.json}.
- An XML declaration such as
  `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`{.xml} is converted to
  `{"xmldecl": {"version": "1.0", "encoding": "UTF-8", "standalone": "yes"}}`{.json}.
  (Note that the values given in this declaration, such as the encoding,
  are ignored by jaq's XML parser.)
- A processing instruction such as
  `<?xml-stylesheet href="common.css"?>`{.xml} is converted to
  `{"pi": {"target": "xml-stylesheet", "content": "href=\"common.css\""}}`{.json}.

To put all of this together, consider the following XML file (`examples/test.xhtml`):

``` xml
<?xml version='1.0'?>
<?xml-stylesheet href="common.css"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <body>
    <!-- CDATA blocks do not require escaping -->
    <![CDATA[Hello & goodbye!]]><br/>
  </body>
</html>
```

Running `jaq . examples/test.xhtml` yields the following output:

``` json
{
  "xmldecl": {
    "version": "1.0"
  }
}
{
  "pi": {
    "target": "xml-stylesheet",
    "content": "href=\"common.css\""
  }
}
{
  "doctype": {
    "name": "html"
  }
}
{
  "t": "html",
  "a": {
    "xmlns": "http://www.w3.org/1999/xhtml"
  },
  "c": [
    "\n  ",
    {
      "t": "body",
      "c": [
        "\n    ",
        {
          "comment": " CDATA blocks do not require escaping "
        },
        "\n    ",
        {
          "cdata": "Hello & goodbye!"
        },
        {
          "t": "br"
        },
        "\n  "
      ]
    },
    "\n"
  ]
}
```

