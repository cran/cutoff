% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear.R
\name{linear}
\alias{linear}
\title{Significant Cutoff Value for Linear Regression}
\usage{
linear(data, y, x, cut.numb, n.per, p.cut = 0.05, strict = TRUE,
  include = "low", round = 2, adjust = 1)
}
\arguments{
\item{data}{data}

\item{y}{name for y}

\item{x}{name for x}

\item{cut.numb}{number of cutoff points}

\item{n.per}{the least percentage of the smaller group comprised in all patients}

\item{p.cut}{cutoff of p value, default is 0.05}

\item{strict}{logical. TRUE means significant differences for each group
combination were considered. FALSE means considering for any combination}

\item{include}{direction of cutoff point. Any left letter of lower or upper}

\item{round}{digital. Default is 2}

\item{adjust}{numeric value, adjust methord for p value. 1, defaulted, represents Bonferroni. 2 represent formula given by Douglas G in 1994}
}
\value{
a dataframe contains cutoff points value, subject numbers in each group,
    dumb variable, beta of regression and p value.
}
\description{
Significant Cutoff Value for Linear Regression
}
\examples{
linear(data=mtcars,y='qsec',x='disp',
       cut.numb=2,
       n.per=0.25)

linear(data=mtcars,y='qsec',x='disp',
       cut.numb=2,
       n.per=0.25,
       p.cut=0.05,
       strict=TRUE,
       include='low',
       round=2)
linear(data=mtcars,y='qsec',x='disp',
       cut.numb=2,
       n.per=0.25,
       p.cut=0.05,
       strict=FALSE,
       include='low',
       round=2)
}
