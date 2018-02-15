#' @title Convert Statistical Analysis Objects into APA-Tables 
#' @name stp25APA2
#' @description Convert statistical analysis objects from R into APA-Tabls.
# Namensschema fuer die Funktionen die Ausgaben Erstellen
# APA_       Erstellt aus Objekten Tabellen für die Ausgabe als HTML
#   APA_Tabelle
#   APA_Correlation
# APA2_      Tabellen für die Ausgabe als HTML
# Format_   Hilfsfunktion für die Ausgabe
# Helper_   Hilfsfunktion für die Statistik
# Intern_   Hilfsfunktion für allea ausser Statistik
# x_        Experimentale Funktionen
#
#' 
#' @importFrom stats AIC coef confint fitted logLik model.frame na.omit
#' @importFrom stats predict qnorm qt residuals setNames var
#' @importFrom Hmisc Cs
#' @importFrom utils head
#' 
#' @docType stp25APA2
#' @aliases stp25APA2 stp25APA2-package
NULL
 