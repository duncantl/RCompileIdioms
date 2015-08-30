\name{genXMLSchemaCCode}
\alias{genXMLSchemaCCode}
\title{Generate C code to read XML documents for a given schema}
\description{
  This function takes an XML schema and generates C code to
  read a specific element/definition in that schema.
  It processes the sub-types and generates routines for those
  also.
  The routines are collected together as they are generated in a
  \code{RoutineCollector}  object.
  We can generate shared routines for different schema element
  using the same collector and so collect all the routines in a single
  location.
}
\usage{
genXMLSchemaCCode(desc, schema, ignore = c(), collector = genCollector())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{desc}{the particular schema description object created via a call to \code{readSchema}}
  \item{schema}{the overall schema object from \code{readSchema}. This
    is used to find other types referenced by the schema object for
    which we are generating the code}
  \item{collector}{a manager collector object for managing the generated
  routines. These routines are generated for the referenced sub-types
  that also need generated routines for reading sub-XML nodes.}
}

\value{
The \code{collector} object which contains the generated routines.
}
\references{
  XML Schema \url{http://www.w3.org/standards/xml/schema}
  Definitive XML Schema, Priscilla Walmsley.
}
\author{
Duncan Temple Lang
}


%\seealso{}

\examples{
library(RCompileIdioms)

# Generate code
s = readSchema(system.file("XML", "Schema", "export-0.10.xsd", package = "RCompileIdioms"),
               followImports = FALSE)
g = genCollector()
k = genXMLSchemaCCode(s[[1]]$PageType, s, collector = g)

out = "wiki.c"
writeCode(g, out)

system("R CMD SHLIB wiki.c")

# Run time

dll = dyn.load(paste("wiki", .Platform$dynlib.ext, sep = ".")

library(XML)
doc = xmlParse(system.file("XML", "SAMPLE", package = "RCompileIdioms"))
pages = getNodeSet(doc, "//x:page", "x")

rpages = lapply(pages, function(p) .Call("R_PageType", p))
}
\keyword{IO}
\concept{compilation}
