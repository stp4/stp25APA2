% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test-mediation.R
\name{Mediation}
\alias{Mediation}
\alias{Mediation2}
\title{Mediation Funktion}
\usage{
Mediation2(model_y_x, model_y_xm, model_m_x, model_y_inter_xm = NULL,
  X = model_info(model_m_x)$x, M = model_info(model_m_x)$y,
  caption = "", type = "tex", ...)
}
\arguments{
\item{model_y_x}{Model y~x}

\item{model_y_xm}{Model y~x+m}

\item{model_m_x}{Model m~x}

\item{X}{Einflussvaria´ble}

\item{M}{Moderator}

\item{caption}{Ueberschrift}

\item{type}{fuer Regression APA_Tabelle()}

\item{...}{not used}

\item{model_inter_xm}{Model y~x*m}
}
\value{
String (HTML)
}
\description{
Einfache Mediation und Moderationsanalyse mit Sobel-Test
Mediation ist gegeben wenn alle Modelle signifikant (1) und (4)

  (1) Y~X
  (2) Y~X+M  (wenn hier M signifikant ist => partielle Mediation)
  (4) M~X

  Moderation ist gegeben wenn die Interaktion (X:M) signifikant ist

  (3) Y~ X + M + X:M
}
\examples{

library(effects)
library(stp25)
graphics.off()
setwd("C:/Users/wpete/Dropbox/3_Forschung/10_temp")
Projekt("html", "Mediation")

Text(
  "
  Mediation ist gegeben wenn alle Modelle signifikant (1) und (4)

  (1) Y~X
  (2) Y~X+M  (wenn hier M signifikant ist => partielle Mediation)
  (4) M~X

  Moderation ist gegeben wenn die Interaktion (X:M) signifikant ist

  (3) Y~ X + M + X:M
  "
)

# Using the Jessor data discussed in Preacher and Kelley (2011), to illustrate
# the methods based on summary statistics.

# An  example data (from Hayes)
DF <- rbind(
  c(-5.00, 25.00, -1.00),
  c(-4.00, 16.00, 2.00),
  c(-3.00, 9.00, 3.00),
  c(-2.00, 4.00, 4.00),
  c(-1.00, 1.00, 5.00),
  c(.00, .00, 6.00),
  c(1.00, 1.00, 7.00),
  c(2.00, 4.00, 8.00),
  c(3.00, 9.00, 9.00),
  c(4.00, 16.00, 10.00),
  c(5.00, 25.00, 13.00),
  c(-5.00, 25.00, -1.00),
  c(-4.00, 16.00, 2.00),
  c(-3.00, 9.00, 3.00),
  c(-2.00, 4.00, 4.00),
  c(-1.00, 1.00, 5.00),
  c(.00, .00, 6.00),
  c(1.00, 1.00, 7.00),
  c(2.00, 4.00, 8.00),
  c(3.00, 9.00, 9.00),
  c(4.00, 16.00, 10.00),
  c(5.00, 25.00, 13.00))
colnames(DF)<-c("emo", "soz.stress", "ausbildung")
DF<- as.data.frame(DF)

#-----------------------------------------------------

Text("Y=emo
     X=soz.stress
     Mediator/Moderator = ausbildung ")
APA2( ~ emo + soz.stress + ausbildung, DF)
model1 <-  lm(emo ~ soz.stress , DF)
model2 <-  lm(emo ~ soz.stress + ausbildung , DF)
model3 <-  lm(ausbildung ~  soz.stress , DF)
model4 <-  lm(emo ~ soz.stress * ausbildung , DF)

Mediation2(model1, model2, model3, model4)


DF<- dapply2(DF, function(x)  scale(x))
model1 <-  lm(emo ~ soz.stress , DF)
model2 <-  lm(emo ~ soz.stress + ausbildung , DF)
model3 <-  lm(ausbildung ~  soz.stress , DF)
model4 <-  lm(emo ~ soz.stress * ausbildung , DF)

Mediation2(model1, model2, model3, model4)
windows(4, 4)
plot(
  allEffects(model4),
  multiline = TRUE,
  main = "", x.var="soz.stress",
  key.args = list(border = 0, columns = 2)
)
SaveData()



End()
}
