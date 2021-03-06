% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/APA.R, R/describe-APA2.R,
%   R/dimred-reliability.R, R/regression-polr.R, R/regression-typ1.R,
%   R/test-corr.R, R/test-surv.R, R/test-t_test.R, R/xtab-xtabs.R
\name{APA}
\alias{APA}
\alias{APA.NULL}
\alias{APA.default}
\alias{test_APA}
\alias{test_APA2}
\alias{APA.formula}
\alias{APA.stp25_reliability}
\alias{APA.polr}
\alias{APA.lm}
\alias{APA.glm}
\alias{APA.biVar}
\alias{APA.survfit}
\alias{APA.survdiff}
\alias{APA.htest}
\alias{APA.ScalarIndependenceTest}
\alias{APA.xtabs}
\alias{APA.table}
\title{APA Style Text-Ausgabe}
\usage{
APA(x, ...)

\method{APA}{NULL}(x, ...)

\method{APA}{default}(x, ...)

test_APA(x)

test_APA2(x)

\method{APA}{formula}(x, data, exclude = NA, max_factor_length = 25)

\method{APA}{stp25_reliability}(x)

\method{APA}{polr}(x, ...)

\method{APA}{lm}(x, include.r = TRUE)

\method{APA}{glm}(x, ...)

\method{APA}{biVar}(x, ...)

\method{APA}{survfit}(fit, ...)

\method{APA}{survdiff}(fit)

\method{APA}{htest}(x, ...)

\method{APA}{ScalarIndependenceTest}(x, ...)

\method{APA}{xtabs}(x, ...)

\method{APA}{table}(x, ...)
}
\arguments{
\item{x}{Objekt fit, formula usw}

\item{...}{weitere Objekte}

\item{data}{data.frame wenn x eine Formel ist}

\item{include.r}{APA.lm: R-Squar}

\item{digits}{Nachkommastellen}

\item{type}{Mittelwert ("mean")}

\item{...}{}
}
\value{
Character Vector mit einem oder meheren Eintraegen
}
\description{
Ausgabe von APA-Style formatiertem Text.

APA.lm F-Test aus lm und Anova

APA.biVar: Korrelation mit Hmisc::spearman2

APA.survfit  Mediane berechnen.

APA.survdiff Log-Rank-Test  berechnen.

APA.htest: T-Test
}
\examples{

APA(mpg ~ cyl, mtcars)
APA(glm(vs ~ mpg, mtcars, family = binomial()))
APA(lm(mpg ~ drat + wt + qsec, mtcars))
APA(aov(mpg ~ drat + wt + qsec, mtcars))



\dontrun{
 library(stpvars)
 APA( ~ rrs0 + rrs1 + g, hyper)
 
 APA(chol0+chol1 ~ g, hyper) das geht nicht
 }
 

APA(Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE))

# Correlation
APA(Hmisc::spearman2(mpg ~ factor(vs), mtcars))

 # T-Test 
APA(coin::wilcox_test(mpg ~ factor(vs), mtcars))
APA(wilcox.test(mpg ~ vs, mtcars))
APA(t.test(mpg ~ vs, mtcars))
}
