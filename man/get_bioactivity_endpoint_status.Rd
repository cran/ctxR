% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bioactivity-APIs.R
\name{get_bioactivity_endpoint_status}
\alias{get_bioactivity_endpoint_status}
\title{Bioactivity API Endpoint status}
\usage{
get_bioactivity_endpoint_status()
}
\value{
Status of Bioactivity API Endpoints
}
\description{
Bioactivity API Endpoint status
}
\examples{
\dontshow{if (has_ctx_key() & is.na(ctx_key() == 'FAKE_KEY')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
status <- get_bioactivity_endpoint_status()
print(status)
\dontshow{\}) # examplesIf}
}
