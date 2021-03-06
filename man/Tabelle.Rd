% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe-Tabelle.R, R/describe-z-auto-test.R,
%   R/describe-z-tests.R
\name{Tabelle}
\alias{Tabelle}
\alias{Tabelle2}
\alias{Tabelle.default}
\alias{Tabelle.lmerModLmerTest}
\alias{Tabelle.glm}
\alias{Tabelle.lm}
\alias{Describe}
\alias{errate_statistik3}
\alias{conTest}
\alias{catTest}
\title{Tabelle}
\usage{
Tabelle(..., output = FALSE)

Tabelle2(..., output = TRUE)

\method{Tabelle}{default}(..., formula = NULL, fun = NULL,
  type = c("2", "1", "freq", "freq.ci", "mean", "median", "ci",
  "cohen.d", "effsize", "multiresponse", "describe", "correlation",
  "custom_fun"), caption = "Charakteristik", note = "",
  digits = NULL, APA = FALSE, test = FALSE, na.action = na.pass,
  exclude = NA, include.n = TRUE, include.nr = FALSE,
  include.total = FALSE, include.test = test, max_factor_length = 35,
  measure.name = "value")

\method{Tabelle}{lmerModLmerTest}(x, digits = 2, fun = function(x) {   
   c(n = length(x), M = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE)) })

\method{Tabelle}{glm}(x, digits = 2, fun = function(x) {     c(n =
  length(x), M = mean(x, na.rm = TRUE)) })

\method{Tabelle}{lm}(x, digits = 2, fun = function(x) {     c(n =
  length(x), M = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE)) })

Describe(x, ...)

errate_statistik3(..., type = NULL, caption = "", note = "",
  na.action = na.pass, exclude = NA, include.n = TRUE,
  include.nr = FALSE, include.total = FALSE, include.test = test,
  max_factor_length = 35)

conTest(fml, data, test_name = TRUE)

catTest(fml, data, include.test = "chisq.test")
}
\arguments{
\item{...}{Die auszuwertenden Variablen  sex, age="mean", usw}

\item{formula}{An dcast Gruppe ~ .id ist zum Zeilen und Spalten vertauschen}

\item{fun}{Eigene Function am Berechne}

\item{type}{1 oder 2 1 ist kurzes Format 2 int lang}

\item{caption, note}{Uberschrift an Output}

\item{digits}{Kommastellen}

\item{APA}{APA2 Style TRUE/FALSE}

\item{test, include.test}{Signifikanz Test include.test
"wilcox.test","u.test",  "kruskal.test","h.test",  
"chisq.test","t.test",  "aov", "anova", 
"SPSS", "Hmisc" 
"shapiro.test" "KS.test"}

\item{na.action, exclude}{an Formula}

\item{include.n, include.nr, include.total}{Anzahl ausgeben}

\item{max_factor_length}{Fehler bei langen Faktoren abfangen}
}
\value{
Tabelle: data.frame oder list mit data.frame
Tabelle2: HTML
}
\description{
Einfache deskriptive Tabelle die in medizinischen Arbeiten
verwendet werden.
Die Funktion arbeitet Intern mit \code{aggregate} bzw. mit  berechne.default() also aggregate(formula, data,FUN).

Tabelle2:  html-Output Tabelle(...) %>% Output()

conTest (metrische-Skalen) Hilfsfunktion fuer Tabellen

conTest (Kategorien Nominal-Skalen) Hilfsfunktion fuer Tabellen
}
\examples{

library(dplyr)
library(tidyr)
#names(varana)
#  set_my_options(mittelwert=list(median.style="Quantil")) # ++IQR
varana2 <- varana \%>\%
  gather(Zeit, Merkfgk, m1:m4) \%>\%
  mutate(Zeit = factor(Zeit, Cs(m1, m2, m3 , m4), Cs(t0, t1, t2, t3))) \%>\%
  Label(Merkfgk = "Merkfaehigkeit")

Tabelle(Merkfgk ~ Zeit, varana2)

varana2 \%>\% Tabelle(Merkfgk, by =  ~ Zeit)
varana \%>\% Tabelle(m1, m2, m3 , m4)

#- Achtung hier wird eine Liste Ausgegeben
varana \%>\% Tabelle(
  4:7,
  by =  ~ geschl,
  fun = function(x)
    c(
      n = length(na.omit(x)),
      m = mean(x),
      sd = sd(x)
    )
)

varana2 \%>\% Tabelle2(Merkfgk, by=~ Zeit)

 Tabelle(
m1[median] + m2[median] + m3[median] + m4[median] ~ geschl,
varana,
APA = TRUE,
include.n = FALSE,
test = TRUE
)

 c(
"wilcox"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="wilcox")[[1]]$statistics[1],
"h.test"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="h.test")[[1]]$statistics[1],
"anova"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="anova")[[1]]$statistics[1],
"t.test"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="t.test")[[1]]$statistics[1],
"hmisc"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="Hmisc")[[1]]$statistics[1]
)
  
  
   Tabelle(alter ~ geschl, varana, APA=TRUE)

Tabelle(alter ~ geschl, varana, include.n=FALSE, APA=TRUE)
Tabelle(alter ~ geschl, varana, include.nr=TRUE, APA=TRUE)
Tabelle(alter ~ geschl, varana, include.total=TRUE, APA=TRUE)
Tabelle(alter ~ geschl, varana, include.test=TRUE, APA=TRUE)
}
\author{
Wolfgang Peter
}
