% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiclass_logistic.R
\name{multiclass_logistic}
\alias{multiclass_logistic}
\title{Multiclass Logistic Regression}
\usage{
multiclass_logistic(form, d)
}
\arguments{
\item{form}{A formula}

\item{d}{A dataframe}
}
\value{
(1) beta coefficients; (2) the misclassification error; (3) a detailed classification table
}
\description{
Describe and implement a classification model generalizing logistic regression to accommodate more than two classes.
}
\examples{
\dontrun{
multiclass_logistic(form = species ~ bill_length_mm + bill_depth_mm, d = penguinsi)
}
}
\author{
Yijun Yang
}
