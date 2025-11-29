//! XML support.
use alloc::string::{String, ToString};
use alloc::{borrow::ToOwned, boxed::Box, format, vec::Vec};
use core::fmt::{self, Formatter};
use jaq_json::{bstr, Map, Val};

/// Serialisation error.
#[derive(Debug)]
pub enum SError {
    /// Unknown key with value was found in an object, e.g. `{t: "a", x: 1}`
    InvalidEntry(&'static str, Val, Val),
    /// Object with zero or more than one keys found, e.g. `{}`, `{a: 1, b: 2}`
    SingletonObj(Val),
}

impl fmt::Display for SError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::InvalidEntry(o, k, v) => {
                write!(f, "invalid entry in {o} object: {{\"{k}\": {v}}}")
            }
            Self::SingletonObj(v) => write!(f, "expected singleton object, found: {v}"),
        }
    }
}

impl std::error::Error for SError {}

/// XML value.
pub enum Xml<S> {
    /// XML declaration, e.g. `<?xml version='1.0' encoding='UTF-8' standalone='yes'?>`
    XmlDecl(Vec<(S, S)>),
    /// DOCTYPE directive, e.g. `<!DOCTYPE greeting SYSTEM "hello.dtd" [...]>`
    DocType {
        /// name of the document type, e.g. "greeting"
        name: S,
        /// reference to an external file, e.g. `SYSTEM "hello.dtd"`
        external: Option<S>,
        /// internal definition of the DTD, e.g. `...`
        internal: Option<S>,
    },
    /// Processing instruction, e.g. <?xml-stylesheet type="text/css" href="style.css"?>`
    Pi {
        /// target, e.g. `xml-stylesheet`
        target: S,
        /// content, e.g. `type="text/css" href="style.css"`
        content: Option<S>,
    },
    /// An element consisting of a Tag, an Attribute, and Content
    ///
    /// For example, `<a href="bla">Link</a>`.
    Tac(S, Vec<(S, S)>, Option<Box<Self>>),
    /// A sequence of XML values, e.g. `Hello<br />World`
    Seq(Vec<Self>),
    /// A string, e.g. `Hello world`
    Scalar(Val),
    /// CDATA, e.g. `<![CDATA[text]]>`
    Cdata(S),
    /// Comment, e.g. `<!-- text -->`
    Comment(S),
}

impl<'a> TryFrom<&'a Val> for Xml<&'a [u8]> {
    type Error = SError;
    fn try_from(v: &'a Val) -> Result<Self, Self::Error> {
        use jaq_std::ValT;
        let from_kv = |(k, v): (&'a _, &'a _)| match (k, v) {
            (Val::Str(k, _), Val::Str(v, _)) => Ok((&**k, &**v)),
            _ => Err(SError::InvalidEntry("attribute", k.clone(), v.clone())),
        };
        let from_kvs = |a: &'a Map| a.iter().map(from_kv).collect::<Result<_, _>>();

        let from_tac = |o: &'a Map| {
            let mut t = &b""[..];
            let mut a = Vec::new();
            let mut c = None;
            for (k, v) in o.iter() {
                let fail = || SError::InvalidEntry("tac", k.clone(), v.clone());
                let k = k.as_utf8_bytes().ok_or_else(fail)?;
                match (k, v) {
                    (b"t", Val::Str(s, _)) => t = s,
                    (b"a", Val::Obj(attrs)) => a = from_kvs(attrs)?,
                    (b"c", v) => c = Some(Box::new(v.try_into()?)),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::Tac(t, a, c))
        };
        let from_dt = |o: &'a Map| {
            let mut name = &b""[..];
            let mut external = None;
            let mut internal = None;
            for (k, v) in o.iter() {
                let fail = || SError::InvalidEntry("doctype", k.clone(), v.clone());
                let k = k.as_utf8_bytes().ok_or_else(fail)?;
                match (k, v) {
                    (b"name", Val::Str(s, _)) => name = s,
                    (b"external", Val::Str(s, _)) => external = Some(&**s),
                    (b"internal", Val::Str(s, _)) => internal = Some(&**s),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::DocType {
                name,
                external,
                internal,
            })
        };
        let from_pi = |o: &'a Map| {
            let mut target = &b""[..];
            let mut content = None;
            for (k, v) in o.iter() {
                let fail = || SError::InvalidEntry("pi", k.clone(), v.clone());
                let k = k.as_utf8_bytes().ok_or_else(fail)?;
                match (k, v) {
                    (b"target", Val::Str(s, _)) => target = s,
                    (b"content", Val::Str(s, _)) => content = Some(&**s),
                    _ => Err(fail())?,
                }
            }
            Ok(Self::Pi { target, content })
        };
        let contains_key = |o: &Map, k: &str| o.contains_key(&Val::from(k.to_string()));
        match v {
            Val::Arr(a) => a
                .iter()
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()
                .map(Self::Seq),
            Val::Obj(o) if contains_key(o, "t") => from_tac(o),
            Val::Obj(o) => {
                let mut o = o.iter();
                let (k, v) = match (o.next(), o.next()) {
                    (Some(kv), None) => kv,
                    _ => Err(SError::SingletonObj(v.clone()))?,
                };
                let fail = || SError::InvalidEntry("unknown", k.clone(), v.clone());
                let k = k.as_utf8_bytes().ok_or_else(fail)?;
                match (k, v) {
                    (b"xmldecl", Val::Obj(kvs)) => from_kvs(kvs).map(Self::XmlDecl),
                    (b"doctype", Val::Obj(o)) if contains_key(o, "name") => from_dt(o),
                    (b"cdata", Val::Str(s, _)) => Ok(Self::Cdata(s)),
                    (b"comment", Val::Str(s, _)) => Ok(Self::Comment(s)),
                    (b"pi", Val::Obj(o)) if contains_key(o, "target") => from_pi(o),
                    _ => Err(fail())?,
                }
            }
            Val::Null | Val::Bool(_) | Val::Num(_) | Val::Str(..) => Ok(Self::Scalar(v.clone())),
        }
    }
}

macro_rules! write_kvs {
    ($w:ident, $a:ident, $f:expr) => {{
        $a.iter().try_for_each(|(k, v)| {
            write!($w, " ")?;
            $f(k)?;
            write!($w, "=\"")?;
            $f(v)?;
            write!($w, "\"")
        })
    }};
}

macro_rules! write_val {
    ($w:ident, $v:ident, $fs:expr, $fv:expr) => {{
        match $v {
            Xml::Scalar(Val::Str(s, _)) => $fs(s),
            Xml::Scalar(v) => write!($w, "{v}"),
            Xml::Seq(a) => a.iter().try_for_each($fv),
            Xml::Tac(t, a, c) => {
                write!($w, "<")?;
                $fs(t)?;
                write_kvs!($w, a, $fs)?;
                if let Some(c) = c {
                    write!($w, ">")?;
                    $fv(c)?;
                    write!($w, "</")?;
                    $fs(t)?;
                    write!($w, ">")
                } else {
                    write!($w, "/>")
                }
            }
            Xml::XmlDecl(a) => {
                write!($w, "<?xml")?;
                write_kvs!($w, a, $fs)?;
                write!($w, "?>")
            }
            Self::DocType {
                name,
                external,
                internal,
            } => {
                write!($w, "<!DOCTYPE ")?;
                $fs(name)?;
                if let Some(s) = external {
                    write!($w, " ")?;
                    $fs(s)?;
                }
                if let Some(s) = internal {
                    write!($w, " [")?;
                    $fs(s)?;
                    write!($w, "]")?;
                }
                write!($w, ">")
            }
            Self::Cdata(s) => {
                write!($w, "<![CDATA[")?;
                $fs(s)?;
                write!($w, "]]>")
            }
            Self::Comment(s) => {
                write!($w, "<!--")?;
                $fs(s)?;
                write!($w, "-->")
            }
            Self::Pi { target, content } => {
                write!($w, "<?")?;
                $fs(target)?;
                if let Some(s) = content {
                    write!($w, " ")?;
                    $fs(s)?;
                }
                write!($w, "?>")
            }
        }
    }};
}

impl fmt::Display for Xml<&[u8]> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write_val!(f, self, |s| bstr(s).fmt(f), |v: &Self| v.fmt(f))
    }
}

impl Xml<&[u8]> {
    /// Write an XML value.
    pub fn write(&self, w: &mut dyn std::io::Write) -> std::io::Result<()> {
        write_val!(w, self, |s: &[u8]| w.write_all(s), |v: &Self| v.write(w))
    }
}
