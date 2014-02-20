\name{saveAcasEntities}
\alias{saveAcasEntities}
\title{Save ACAS entities to the server}
\usage{
saveAcasEntities(entities, acasCategory,
  lsServerURL = racas::applicationSettings$client.service.persistence.fullpath)
}
\arguments{
  \item{entities}{a list of entities}

  \item{acasCategory}{e.g. "experiments", "subjectlabels",
  etc.}

  \item{lsServerURL}{url of ACAS server}
}
\value{
a list, sometimes empty
}
\description{
Save protocols, labels, experiments, etc.
}
