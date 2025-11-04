# Transform the manual HTML as follows:
#
# - Make a table of contents
# - Make automatic heading identifiers lowercase
# - Add playground links to documentation tests

def sections:
  select(.t? == "section") |
  # remove whitespace
  .c[] |= select(isobject) |
  # spare the first child, which is the header
  .c[1:][] |= sections;

def li:
  {t: "li", c: [
    {t: "a", a: {"href": "#" + .a.id}, c: .c[0].c},
    {t: "ul", c: .c[1:] | map(li)}
  ]};

def nav:
  {t: "nav", c: [
    {t: "h2", c: "Contents"},
    {t: "ul", c: [.c[] | sections | li]}
  ]};

def body:
  {t: "div", a: {class: "container"}, c: [
    nav,
    {t: "section", a: {id: "main"}, c: .c}
  ]};

def transform_section_headers:
  (.. | select(.t? == "section") | .a.id) |= ascii_downcase;

def transform_code:
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

  # get contents of all code tags without attributes
  (.. | select(is_test)) |= [
    (.c[] |= (split($arrow) | .[0] + " ⟼ " + .[1])),
    (.c[] |   split($arrow) | .[0] | @htmld | play_link)
  ];

def transform_body:
  transform_section_headers |
  transform_code;

(.. | select(.t? == "style")).c = [$style] |
(.. | select(.t? == "body" ))   = ($body | fromxml | transform_body | body)
