% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicPlotting.R
\name{ggDigit}
\alias{ggDigit}
\title{Plot a single mnist digit}
\usage{
ggDigit(digit, trueLab, predLab)
}
\arguments{
\item{digit}{A single row from the mnist dataset, i.e. a
1 by 784 vector. This can e.g. be the first digit from the
mnist dataset \code{trainSet$x[1,]}.}

\item{trueLab}{The true label of the digit. Can be a factor or character.}

\item{predLab}{The predicted label, e.g. from a classifier.
Can be a factor or character.}
}
\value{
Return a ggplot object of the digit.
}
\description{
\code{ggDigit} plots a single mnist digit with ggplot.
}
\details{
This functions returns a ggplot object of a single digit. The goal
is to keep the plot as minimal as possible, only showing the true
digit information. Additional parameters are given to create a
title with the digit label and a predicted label.
}
\examples{
# Create some random data to plot
dat <- matrix(sample(0:255,784,replace=TRUE),nrow = 1)
# Get the ggplot object
myRandomNonDigit <- ggDigit(dat)
\dontrun{
# Display the digit
(myRandomNonDigit)
}

}
