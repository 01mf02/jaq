jaq supports reading and writing the following formats:

- UTF-8 text (`raw`)
- JSON (`json`)
- XML (`xml`)

jaq tries to guess the file format from the file name;
for example, `jaq . file.xml` will try to read `file.xml` as XML file.
Data on the standard input/output will be read/written by default in JSON format.
If you need to override the input/output file format, use `--from` and `--to`,
e.g. `--from xml`.

# Formats

## JSON

jaq reads JSON data as specified in [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259).
A good overview of the syntax is given at <https://www.json.org>.

## XML

jaq reads data adhering to the [XML 1.0](https://www.w3.org/TR/xml/) standard.
However, it treats only XML data encoded as UTF-8.

jaq cannot directly read HTML files --- you can use tools such as
[html2xhtml](https://github.com/jfisteus/html2xhtml) to convert HTML to XHTML.

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
  Note that the values given in this declaration are ignored by the XML parser.
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
