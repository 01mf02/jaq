# Transform the manual HTML as follows:
#
# - Make a table of contents
# - Make automatic heading identifiers lowercase
# - Add playground links to documentation tests

import "hl" as $hl_arr;
reduce $hl_arr[] as {$lang, $code, $ast}
  ({}; . * {($lang): {($code): $ast}}) as $hl |

def sections:
  select(.t? == "section") |
  # remove whitespace
  .c[] |= select(isobject) |
  # spare the first child, which is the header
  .c[1:][] |= sections;

def li:
  {t: "li", c: [
    {t: "a", a: {"href": "#" + .a.id}, c: .c[0].c[0]},
    {t: "ul", c: .c[1:] | map(li)}
  ]};

def transform_section_headers:
  (.. | select(.t? == "section")) |= (
    .a.id       |= ascii_downcase |
    # append anchor link
    .c[1].c     += [{t: "a", a: {href: "#" + .a.id}, c: ["#"]}] |
    # transform h1 to h2, h2 to h3, ...
    .c[1].t[1:] |= (tonumber + 1 | tostring)
  );

def transform_code:
  # XML encoding of `-->`
  "--&gt;" as $arrow |

  def is_test:
    .t? == "code" and
    (has("a") | not) and
    (.c[] | contains($arrow));
  
  def play_link:
    {t: "a",
     a: {
      href: @uri "https://gedenkt.at/jaq/?q=\(.)&amp;j=null",
      target: "_blank",
      rel: "noopener",
      title: "Run example",
      class: "run-example"
     },
     c: [],
    };

  def ast_to_html:
      if isobject then {t: "span", a: {"class": .t}, c: .c | ast_to_html}
    elif isarray then .[] |= ast_to_html
    elif isstring then @html end;

  def code($lang): {t: "code", a: {$lang}, c: $hl[$lang][. | @htmld] | ast_to_html};

  # get contents of all code tags without attributes
  (.. | select(is_test)) |= [
    (.c[] |= (split($arrow) | [(.[0] | code("jq")), " ⟼ ", (.[1] | code("xjon"))])),
    (.c[] |   split($arrow) | .[0] | @htmld | play_link)
  ];

def transform_body:
  transform_section_headers |
  transform_code;

($body | fromxml | transform_body) as $body |
(.. | select(.t?    == "style")).c = [$style] |
(.. | select(.t?    == "ul"   )).c = [$body.c[] | sections | li] |
(.. | select(.a?.id == "main" )).c =  $body.c
