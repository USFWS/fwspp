% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_helpers.R
\name{clip_occ}
\alias{clip_occ}
\title{Clip species occurrence records to a FWS property boundary}
\usage{
clip_occ(occ_recs, prop)
}
\arguments{
\item{occ_recs}{a data frame of species occurrence records}

\item{prop}{a FWS property boundary returned by \code{\link{find_fws}}}
}
\value{
a data frame of species occurrence records clipped to a FWS property boundary
}
\description{
Clip species occurrence records to a FWS property boundary
}
