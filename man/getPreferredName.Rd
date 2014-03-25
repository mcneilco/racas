\name{getPreferredName}
\alias{getPreferredName}
\title{Gets Entity Name}
\usage{
getPreferredName(entity)
}
\arguments{
  \item{entity}{an ACAS entity such as a protocol or
  subject}
}
\value{
a string name
}
\description{
Determines the preferred name of an entity (protocol,
experiment, etc.)
}
\details{
returns the name that has \code{preferred==TRUE},
\code{ignored==FALSE}. Ties are broken by the most recent
\code{recordedDate}.
}

