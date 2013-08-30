\docType{data}
\name{createSELExperimentMetaDF}
\alias{createSELExperimentMetaDF}
\title{Creates a data.frame that contains the elements of the Experiment Meta Data section of SEL}
\usage{
  createSELExperimentMetaDF(format = "Generic",
    protocolName = "", experimentName = "", scientist = "",
    notebook = "", page = "", assayDate = "", project = "")

  createSELExperimentMetaDF
}
\value{
  data.frame representing the most common inputs to the SEL
  experiment meta data section
}
\description{
  The return structure represents the standard inputs of
  the experiment meta data section of SEL
}
\details{
  \itemize{ \item format character used during the SEL
  process to determine the type of SEL load (default is
  "Generic") \item protocolName character that is the
  label_text of the protocol \item experimentName character
  that is the label_text of the experiment \item scientist
  character that is the username of the scientist who did
  the experiment \item notebook character that is the
  notebook where the experiment is described \item page
  character that is the page where the experiment is
  described \item assayDate character representing the date
  the experiment was run in the format YYYY-MM-DD \item
  project character that is the project for which the
  experiment was run }
}
\keyword{datasets}

