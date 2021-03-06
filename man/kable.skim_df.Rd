% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skim_print.R
\name{kable.skim_df}
\alias{kable.skim_df}
\title{Produce \code{kable} output of a skimmed data frame}
\usage{
\method{kable}{skim_df}(x, format = NULL, digits = getOption("digits"),
  row.names = NA, col.names = NA, align = NULL, caption = NULL,
  format.args = list(), escape = TRUE, ...)
}
\arguments{
\item{x}{a skim_df object}

\item{format}{a character string; possible values are latex, html,
markdown, pandoc, and rst; this will be automatically determined if the
function is called within knitr; it can also be set in the global option
knitr.table.format; if format is a function, it must return a character
string}

\item{digits}{the maximum number of digits for numeric columns (passed to
round()); it can also be a vector of length ncol(x) to set the number of
digits for individual columns.}

\item{row.names}{a logical value indicating whether to include row names;
by default, row names are included if rownames(x) is neither NULL nor
identical to 1:nrow(x)}

\item{col.names}{a character vector of column names to be used in the table}

\item{align}{the alignment of columns: a character vector consisting of
'l' (left), 'c' (center) and/or 'r' (right); by default, numeric columns
are right-aligned, and other columns are left-aligned; if align = NULL,
the default alignment is used; alternatively, if length(align) == 1L, the
string will be expanded to a vector of individual letters unless the output
format is LaTeX; for example, 'clc' will be converted to c('c', 'l', 'c')}

\item{caption}{the table caption that precedes the variable type}

\item{format.args}{a list of arguments to be passed to format() to format
table values, e.g. list(big.mark = ',')}

\item{escape}{escape special characters when producing HTML or LaTeX tables}

\item{...}{other arguments.}
}
\value{
The original \code{skim_df} object.
}
\description{
Produce \code{kable} output of a skimmed data frame
}
\seealso{
\code{\link[knitr]{kable}}
}
