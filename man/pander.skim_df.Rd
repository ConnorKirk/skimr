% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skim_print.R
\name{pander.skim_df}
\alias{pander.skim_df}
\title{Produce \code{pander} output of a skimmed data frame}
\usage{
\method{pander}{skim_df}(x, caption = attr(x, "caption"), ...)
}
\arguments{
\item{x}{R object (typically a skimmed data frame)}

\item{caption}{caption(string) to be shown under the table}

\item{...}{other arguments.}
}
\value{
The original \code{skim_df} object.
}
\description{
Produce \code{pander} output of a skimmed data frame
}
\seealso{
\code{\link[pander]{pander}}
}
