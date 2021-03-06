% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_ps.r
\name{fix.parser.inconsistencies}
\alias{fix.parser.inconsistencies}
\title{Changes the txt in a way that the output file after knitr looks similar to the shiny app.}
\usage{
fix.parser.inconsistencies(
  txt,
  fix.lists = TRUE,
  fix.headers = TRUE,
  remove.exercise = TRUE,
  seperate.html = TRUE
)
}
\arguments{
\item{fix.lists}{RMD lists with knitr have to have a trailing blank line. Otherwise the list is only interpreted verbatim. Setting this parameter to `TRUE` (Default) inserts additional blank lines where necessary. Additionally blank lines are removed and replaced with <br><br> when they would interfere with the list structure (i.e. shiny uses the same list but knitr ends the current list.). For some reason tabs to write more readable html tables break the lists as well using the knitr parsing. They are therefore removed.}

\item{fix.headers}{Headers need to have a blank line before them when using knitr.}

\item{remove.exercise}{To start a new Exercise the key word "Exercise" is used. We most likely do not want to have it in the header as it also does not appear in the shiny problem set.}

\item{seperate.html}{knitr.md does sometimes not process html tags correctly if they do not start in a newline}
}
\description{
Several subtle inconsistences exist, where the RMD parser of the shiny app and the RMD parser of knitr differ. This function changes the output code in a way that the expected result of the shiny app can out of the box be achieved by pressing the knitr button.
}
