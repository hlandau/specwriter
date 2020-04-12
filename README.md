Specwriter
==========

**PRE-ALPHA, WORK IN PROGRESS**

Specwriter is a system for writing natural-language technical specifications
using Scheme. By using the power of S-expressions, including ordinary
functions, macros and backquoting, and SXML (the representation of XML in
S-expressions), you can ergonomically write documents which compile to highly
semantic, clean XML in whatever schema you desire; whether DocBook, XHTML, or a
schema of your own construction.

You can then use the power of SXML (or, if you wish, external tooling such as
an XSLT processor) to “lower” this highly semantic and platonic XML
representation of your document into a more concrete representation, such as
DocBook or XHTML. Or you can use a transformer which consumes your SXML and
produces a non-XML representation such as LaTeX or ConTeXt.

The purpose of Specwriter is to allow the production of technical documents
which can be consumed as XHTML and come across as documents which were designed
for optimal consumption as XHTML; and which can be consumed as PDFs and come
across as documents which were designed for optimal consumption as PDFs.

Because Specwriter documents are literally wrtiten in a full Scheme
environment, the system is also optimally suited to generating documents with
large amounts of generated content; for example, register manuals. You can use
Scheme to import arbitrary machine-readable data from any source and trivially
transform it into SXML. Moreover, since Specwriter can be used to produce
high-quality, semantic XML documents, those resulting documents are easily
machine-read to extract the embedded information. Why have people read through
a 10,000 page PDF of registers, when you can provide them with an XML file
which is both viewable in a web browser and machine intelligible, allowing
automated processing of all register definitions? Either use XHTML directly
with embedded domain-specific semantic annotations, or a domain-specific XML
representation which is rendered viewable in a web browser via an attached XSLT
or CSS stylesheet.

Specwriter makes it easy to choose your own schema for describing your own
content, so you can use whatever schema you think appropriate, and then
optionally transform it into more general formats such as DocBook or XHTML.

Specwriter is tiny! The core of it is a very small amount of ergonomic glue
built on top of SXML. You can consider it a demonstration of the power of
Scheme. Around this core are built various optional modules, such as those
implementing particular schemas; you can use these or write your own.

Because specifications are written in Scheme, you can ergonomically reference
other objects inline in text via lexical references. For example:

    ;; Define a term. This can be referenced lexically. All terms are collated
    ;; in a list inside the Scheme environment, and can be mapped over
    ;; programmatically to automatically populate a Definitions section in the
    ;; output. The use of lexical references to terms ensures integrity of
    ;; references and allows the output to contain hyperlinks to definitions,
    ;; etc.
    (dt table "table" "A table is a surface on which objects can be placed.")

    ;; Define a proword. This is similar to a term but is usually used to
    ;; express a normative requirement. RFCs have prowords such as MUST, MUST
    ;; NOT, SHOULD, SHOULD NOT, MAY, etc. ISO standards have prowords such as
    ;; "shall", "shall not", etc.
    (dpword must "MUST")

    ;; Paragraph referencing the term "table" and the proword "MUST" lexically.
    ;; Note that the quotes around "table" and "MUST" here are actually ending
    ;; a quoted-string then beginning a new one; Scheme does not require any
    ;; spaces here. Simply make your eyes forget you're in a quoted string,
    ;; and accept the convention of placing terms and prowords in quotes to
    ;; refer to them. It's surprisingly ergonomic.
    (p "A "table" "MUST" have four legs.")

Note that this is not a special parsing environment. `dt`, `dpword` and `p` are
simply Scheme macros or functions which define items in the Scheme lexical
environment. The full power of Scheme can be used to produce generated output.

Specwriter is intended to be used with Guile Scheme, but could presumably also
be used with other Scheme implementations without too much trouble.

Repository Structure
--------------------

The following Scheme modules are found in this repository, in the `src` directory:

  - `sxml2`: Renovated SXML implementation supporting serialization,
    deserialization and first-class support for namespaces.
  - `specwriter`: The core of Specwriter. Less than 100 lines!

The following Scheme modules provide support for specific schemas:

  - `schspec-iso`: Defines ergonomics for writing ISO-style standards.
    To my knowledge there is no official XML schema for ISO standards,
    so this is a custom schema.

    The `styles/default` subdirectory contains XSLT transforms targeting XHTML
    and CSS stylesheets which make documents in the `schspec-iso` schema
    viewable in a web browser.

  - `schspec-fig`: Write source code for diagram generation tools tools such as
    Graphviz `dot` and have them be automatically compiled to multiple formats
    (SVG, PNG) as needed.

  - `schspec-regdef`: Produce register documentation from register definitions
    represented as S-expressions.

The `samples` directory contains sample documents demonstrating the
functionality of Specwriter.

Requirements and Dependencies
-----------------------------

  - Guile Scheme
  - The `gcrypt` package for Guile Scheme (e.g. put at `dep/gcrypt/*.{go,scm}`).
