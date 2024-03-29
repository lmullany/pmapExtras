% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mount_S_drive.R
\name{mount_S_drive}
\alias{mount_S_drive}
\title{Function to mount S drive}
\usage{
mount_S_drive(
  project_name = NULL,
  mount_location = NULL,
  username = NULL,
  showdir = F
)
}
\arguments{
\item{project_name}{string name of project (eg. PMAPMullany)}

\item{mount_location}{string path to mount this S drive project}

\item{username}{string username (i.e. jhed id)}

\item{showdir}{(default = FALSE); if set to TRUE will list dir contents after mounting}
}
\description{
When working in idies/crunchr, this function provides a way to mount a specific
S drive project.
}
\examples{
mount_S_drive("PMAPMullany", "home/idies/workspace/SAFE/", username="<jhed>")
}
