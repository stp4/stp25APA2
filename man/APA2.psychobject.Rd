% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression-typ2.R
\name{APA2.psychobject}
\alias{APA2.psychobject}
\title{APA2}
\usage{
\method{APA2}{psychobject}(x, caption = "", note = NULL,
  include.ci = FALSE, include.effect = FALSE,
  output = stp25output::which_output(), ...)
}
\arguments{
\item{x}{lm object.}

\item{include.ci}{Confidence interval}

\item{include.effect}{Text zu Effect_Size}
}
\description{
APA2
}
\examples{

library(psycho)

 
 df <- psycho::affective  # Load a dataset from the psycho package
 #df <- standardize(df)  # Standardize all numeric variables
 
 fit <- lm(Age ~ Salary, data=df)  # Fit a Bayesian linear model
 results <- analyze(fit)  # Format the output
 APA2(results )
 
 
 
 library(lmerTest)
 fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
 
 results <- analyze(fit)
 APA2(results)
}
