
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stp25APA2

Achtung das ist nur ein experimentelles Projekt und wird wieder
gelöscht. Hier sind alle Funktionen vereint die für die Aufbereitung
der Ergebnisse zuständig sind. Die Funktione sind ein Konglomerat aus
Kopien texreg und psych sowie eigene Funktionen die aus Bortz abgeleitet
sind.

## Overview

### Apa-Style Tabellen

  - `APA2` Tabellen aus Formeln und R-Objekten
  - `Tabelle` Deskriptive Analyse
  - `APA_Table` Regressionsanalyse (Kopie von texreg)
  - `APA_Correlation` Korrelationen
  - `APA_Xtabs` Kreuztabellen
  - `Prop_Test` Konfidenzintervallen von Häufikeiten

### Statistik-Methoden

  - `Likert` Analyse von Likertskalen
  - `Kano` Kano-Kodierung
  - `Rangreihe` Rangordnungen von Objekten  
  - `ICC` Intra-Klassen-Korrelation. (Kopie von psych)
  - `Principal, PCA` Faktoranalyse PCA (Kopie von psych)
  - `Reliability, Alpha` Cronbach-Alpha + Mittelwerte (Kopie von psych)
  - `Index` Summenfunktion
  - `VIF` variance inflation factor
  - `R2` Berechnung der R-Quadrats
  - `RMSE` square root of the variance
  - `Mediation` Mediation Funktion Sobel-Test
  - `MetComp, BlandAltman, Kappa` Uebereinstimmung und Praezision von
    Messwerten

### Usage

``` r
# devtools::install_github("stp4/stp25data")

 library(stp25vers)
 
 APA2(alter ~ geschl, varana)
#> 
#> 
#> Table: geschl 
#> 
#> Item    Characteristics   N    männlich (n=15)   weiblich (n=12) 
#> ------  ----------------  ---  ----------------  ----------------
#> alter   bis 30 Jahre      27   27% (4)           25% (3)         
#>         31-50 Jahre            33% (5)           33% (4)         
#>         über 50 Jahre          40% (6)           42% (5)
```

### Literatur

\[1\] Bortz, J. & Doering, N. (2006). Forschungsmethoden und Evaluation
fuer Human-und Sozialwissenschaftler (4. Auflage). Berlin: Springer

\[2\] Philip Leifeld (2013). texreg: Conversion of Statistical Model
Output in R to LaTeX and HTML Tables. Journal of Statistical Software,
55(8), 1-24. URL <http://www.jstatsoft.org/v55/i08/>

\[3\] Revelle, W. (2017) psych: Procedures for Personality and
Psychological Research, Northwestern University, Evanston, Illinois,
USA, <https://CRAN.R-project.org/package=psych> Version = 1.7.8.
