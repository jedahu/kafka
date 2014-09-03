namespace Faux

open System.Xml
open System.Xml.Linq

module XmlSyntax =

    type private Attr = string * string

    let xname : string -> XName =
        fun s -> XName.op_Implicit(s)

    let private attr : string * string -> XAttribute =
        fun (k, v) -> new XAttribute(xname k, v)

    let private elem : XName -> XAttribute seq -> XNode seq -> XElement =
        fun name attrs nodes ->
            new XElement(
                name,
                Array.ofSeq
                    (Seq.append (Seq.map box attrs) (Seq.map box nodes)))

    let xnamespace : string -> XNamespace =
        fun ns -> XNamespace.op_Implicit(ns)

    let xdoc : XElement -> XDocument =
        fun elem -> new XDocument(elem)

    let xelem : string -> Attr seq -> XNode seq -> XElement =
        fun name attrs nodes -> elem (xname name) (Seq.map attr attrs) nodes

    let xelem' : XNamespace -> string -> Attr seq -> XNode seq -> XElement =
        fun ns s attrs nodes -> elem (ns + s) (Seq.map attr attrs) nodes

    let xattr : string -> string -> XAttribute =
        fun k v -> new XAttribute(xname k, v)

    let xstr : string -> XText =
        fun s -> new XText(s)
