% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inputValidation.R
\name{checkInputCorrectness}
\alias{checkInputCorrectness}
\title{checkInputCorrectness}
\usage{
checkInputCorrectness(input)
}
\arguments{
\item{input}{GUi Inputs}
}
\value{
TRUE for correct user input, FALSE for a faulty configuration
}
\description{
Main Input checking function, calls all subChecks and creates User-Dialogs for the respective problems.
}
