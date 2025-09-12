jaq supports reading and writing the following formats:

- UTF-8 text (`raw`)
- JSON (`json`)
- YAML (`yaml`)
- CBOR (`cbor`)
- TOML (`toml`)
- XML (`xml`)

jaq tries to guess the file format from the file name;
for example, `jaq . file.xml` will try to read `file.xml` as XML file.
Data on the standard input/output will be read/written by default in JSON format.
If you need to override the input/output file format, use `--from` and `--to`,
e.g. `--from xml`.

# Formats

## JSON

jaq can read and write JSON data as specified in [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259).
A good overview of the syntax is given at <https://www.json.org>.

jaq's JSON parser accepts a slight superset of JSON, where
strings may contain non-UTF-8 characters (such as the FF byte) and
objects may contain non-string keys.
The reasoning behind allowing these changes is that they actually make the parser faster and simpler, while continuing to parse valid JSON as before.
For example, the following is invalid JSON, but is allowed in jaq:

    {null: 0, true: 1, 2: 3, "str": 4, ["arr"]: 5, {}: 6}

This change simplifies parsing and makes jaq more flexible,
because it allows using objects as hash maps for arbitrary keys.

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

~~~ xml
<a href="https://www.w3.org">World Wide Web Consortium (<em>W3C</em>)</a>
~~~

We can see its internal representation in jaq by:

~~~
$ echo '<a href="https://www.w3.org">World Wide Web Consortium (<em>W3C</em>)</a>' | jaq --from xml .
~~~

This yields the following JSON:

~~~ json
{
  "t": "a",
  "a": { "href": "https://www.w3.org" },
  "c": [
    "World Wide Web Consortium (",
    { "t": "em", "c": [ "W3C" ] },
    ")"
  ]
}
~~~

### Tags or The TAC Architecture

Tags are represented by "TAC" objects. A TAC object may have the following fields:

- `t`: Name of the tag, such as `h1` for `<h1>...</h1>`.
  This field must always be present in a TAC object.
- `a`: Attributes of the tag, such as `{"id": "foo", style: "color:blue;"}`.
  If this field is present, it must contain an object with string values.
- `c`: Children of the tag, usually an array of XML values.
  If this field is not present, this tag will be interpreted as self-closing (such as `<br/>`).

An example query to obtain all links in an XHTML file:

~~~ jq
.. | select(.t? == "a") | .a.href
~~~

We can also transform input XML and yield output XML.
For example, to transform all `em` tags to `i` tags:

~~~ jq
(.. | select(.t? == "em") | .t) = "i"
~~~

To yield XML output instead of JSON output, use the option `--to xml`:

~~~
$ echo '<a href="https://www.w3.org">World Wide Web Consortium (<em>W3C</em>)</a>' | jaq --from xml --to xml '(.. | select(.t? == "em") | .t) = "i"'
<a href="https://www.w3.org">World Wide Web Consortium (<i>W3C</i>)</a>
~~~

Finally, we can extract all text from an XML file (discarding CDATA blocks):

~~~ jq
def xml_text: if isstring then . else .c[]? | xml_text end; [xml_text]
~~~

### Other XML values

- Strings are neither escaped nor unescaped; that means,
   `Tom &amp; Jerry`  in the source XML becomes
  `"Tom &amp; Jerry"` in the target JSON.
- A comment such as `<!-- this comment -->` is converted to
  `{"comment": " this comment "}`.
- A CDATA block such as `<![CDATA[Tom & Jerry]]>` is converted to
  `{"cdata": "Tom & Jerry"}`.
- An XML declaration such as
  `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>` is converted to
  `{"xmldecl": {"version": "1.0", "encoding": "UTF-8", "standalone": "yes"}}`.
  (Note that the values given in this declaration, such as the encoding,
  are ignored by jaq's XML parser.)
- A processing instruction such as
  `<?xml-stylesheet href="common.css"?>` is converted to
  `{"pi": {"target": "xml-stylesheet", "content": "href=\"common.css\""}}`.

To put all of this together, consider the following XML file (`test.xhtml`):

~~~ xml
<?xml version='1.0'?>
<?xml-stylesheet href="common.css"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <body>
    <!-- CDATA blocks do not require escaping -->
    <![CDATA[Hello & goodbye!]]><br/>
  </body>
</html>
~~~

Running `jaq . test.xhtml` yields the following output:

~~~ json
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
~~~

### CBOR example

I wanted to extract the
[list of examples from the CBOR specification](https://www.rfc-editor.org/rfc/rfc8949.html#name-examples-of-encoded-cbor-da),
in order to create a test suite for the CBOR encoder/decoder in jaq.
For this, I copied the relevant section from the HTML source code and
pasted it into `examples/cbor-examples.xhtml`.
Finally, I came up with the following command to construct
pairs of JSON and corresponding CBOR data:

    jaq '.. | select(.t? == "tr").c | [.[].c?[]]' examples/cbor-examples.xhtml

We can create a series of tests with the following call:

    jaq '.. | select(.t? == "tr").c | [.[].c?[]] | @json "jc(\(.[0]), \(.[1][2:]));"' examples/cbor-examples.xhtml -r

This has been used to create a draft for jaq's CBOR parsing test suite.
