\docType{data}
\name{acasEntityHierarchy}
\alias{acasEntityHierarchy}
\alias{acasEntityHierarchyCamel}
\alias{acasEntityHierarchySpace}
\title{The hierarcy of ACAS entities}
\format{\preformatted{ chr [1:5] "protocol" "experiment" "analysisgroup" "treatmentgroup" "subject"
}}
\usage{
acasEntityHierarchy

acasEntityHierarchyCamel

acasEntityHierarchySpace
}
\description{
Each category of entity contains the level below. A
lowercase version (for URLs), a camelCase verison (for
JSON), and a spaced version (for codeNames) exist. Can be
found in \code{racas::acasEntityHierarchy},
\code{racas::acasEntityHierarchyCamel} and
\code{racas::acasEntityHierarchySpace}
}
\keyword{datasets}

