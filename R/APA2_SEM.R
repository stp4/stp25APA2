#' @rdname SEM
#' @title SEM
#' @name SEM
#' @description Struckturgleichungs Modelle mit lavaan. Die Funktionen sind Kopieen von lavaan
#' und helfen den Output zu erstellen.
#' SEM ist davei einfach lavaan::sem(x, ...)
#' mehr unter \link{fkv}
#'
#' \subsection{APA2.lavaan}{Ueber APA2 wird die Ausgabe formatiert. Ausgegeben werden die Guetemasse und der ModelFit.
#' Loading ist dabei der standartisierte Estimate und Communality ist die quadrierte Ladung
#'
#' }
#'
#' Chi-Quadrat-Wert
#'
#' ML:  Validitaet des Models H0: empirische Kovarianz entspricht modelltheoretischer Kovarianz
#' Chi-Quadrat/df moeglichst klein (Chi-Quadrat/df<2.5 oder p<0.100)
#' Ist nur zuverlaessig wenn Normalverteilung und ausreichend grosse Stichprobe gegeben ist.
#'
#' Model test Baseline model Chi-Quadrat Null-Modell wenn signifikant dann besteht die Gefahr einer Fehl-Spezifikation
#'
#' Goodness-of-Fit-Index (GFI)
#'
#' Ist vergleichbar mit dem Bestimmtheitsmass in der Regressionsanalyse, also ein Mass fuer die erklaerende Varianz
#' GFI>0.90
#' Adjusted-Goodness-of-Fit-Index (AGFI)
#' Analog wie GFI nur korrigiert durch df und Anzahl an Variablen
#' AGFI>0.90
#'
#' Normed-Fit-Index NFI
#'
#' Vergleicht das Modell mit einem Model bei dem alle Manifesten Variablen un-korreliert angenommen werden
#' NFI>0.90
#' Comparative-Fit-Index
#'
#' Wie NFI nur korrigiert durch df und Anzahl an Variablen
#' CFI>0.90
#'
#' Root-Mean-Square-Error of Approximation (RMSEA)
#' RMSEA<0.05
#'
#' Backhaus Multivariate Analysemethoden 11 AuflageSeite 383
#' Moosbrugger, Kelava 2012 Testtheorie 2. Auflage Seite 339
#'
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return Ein lavaan -Objekt oder html.
#'
#' @examples
#' head(fkv)
#' APA2( ~., fkv, test=T)
#' library(arm)
#' windows(5,5)
#' corrplot(fkv, abs=TRUE, n.col.legend=7)#  corrplot {arm}
#' SaveData( )
#' Principal2(fkv, 5, cut=.35)
#'
#' library(lavaan)
#' library(semPlot)
#'
#'
#' Model<-'
#' Verarbeitung =~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4
#' Coping =~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20
#' Vertrauen =~ F28+F27+F31+F29
#' Religion =~F21+F25+F30+F23+F24
#' Distanz =~F3+F2+F10+F11
#'
#' '
#' fit.Lavaan <- sem( Model, data=fkv)
#' APA2(fit.Lavaan)
#' # parameterEstimates(fit.Lavaan)
#' # Est <- parameterEstimates(fit.Lavaan, ci = FALSE, standardized = TRUE)
#' # #fitMeasures(fit.Lavaan, c("chisq", "df", "pvalue", "cfi", "rmsea"))
#' # #round( inspect(fit.Lavaan,"r2")  ,2)
#' # #parTable(fit.Lavaan)
#' # #show(fit.Lavaan)
#' # anova(fit.Lavaan)
#'
#' semPaths(fit.Lavaan, "std", rotation=2, title = FALSE)
#' title("Std", line = 3)
#' @export
SEM <- function(x, ...) {
  UseMethod("SEM")
}

#' @rdname SEM
#' @export
SEM.default <- function(x, ...) {
  lavaan::sem(x, ...)
}

#' @rdname SEM
#' @export
APA2.lavaan <- function(fit,
                        baseline.model = NULL,
                        caption = "" ,
                        note = "",
                        type = "all",
                        ci = FALSE,
                        est = FALSE,

                        ...) {


  fit_Measures <- as.list(
          lavaan::fitMeasures(
                    fit,
                    fit.measures = c(
                      "chisq",
                      "df",
                      "pvalue",
                      "baseline.chisq" ,
                      "baseline.df",
                      "baseline.pvalue" ,
                      "rmsea" ,
                      "srmr",
                      "gfi",
                      "agfi",
                      "cfi" ,
                      "nfi"
                    ), baseline.model =  baseline.model
                  )
  )


  fit_Measures$df <- fit_Measures$df + 1  # unbekannter Fehler


  fit_Measures<- with(
      fit_Measures,
        rbind(
            "ML"  = c(rndr_Chisq(chisq, df, pvalue), ""),
            "Model test baseline model" = c(rndr_Chisq(baseline.chisq, baseline.df,baseline.pvalue), ""),
            "Number of observations" = c(fit@Data@nobs[[1]], ""),
            "chisq/df" = c(Format2(chisq/df, 2), rndr_Chisq_cfa(chisq, df)),
            "GFI" = c(Format2(gfi, 2), rndr_gfi_cfa(gfi)),
            "AGFI" = c(Format2(agfi, 2), rndr_agfi_cfa(agfi)),
            "NFI"  = c(Format2(nfi, 2), rndr_nfi_cfa(nfi)),
            "CFI" =  c(Format2(cfi, 2), rndr_cfi_cfa(cfi)),
            "RMSEA" = c(Format2(rmsea, 2), rndr_rmsea_cfa(rmsea)),
            "SRMR" =  c(Format2(srmr, 2), rndr_rmsea_cfa(srmr)))
    )

    colnames(fit_Measures) <- c("Anpassungsmass", "Anforderung")
    fit_Measures <- data.frame(Wert = rownames(fit_Measures), fit_Measures)

    # label    est    se      z    pvalue ci.lower ci.upper std.lv std.all std.nox
    #Lhr      0.230 0.051  4.483  0.000   0.129    0.330  0.230   0.230   0.230
    my_std <- lavaan::standardizedSolution(fit)

    #Formatieren dass nur die p werte haben
    result_loadings <-
        data.frame(
            model = ifelse(my_std$lhs==my_std$rhs,
                       my_std$rhs,
                       paste(my_std$lhs, my_std$op, my_std$rhs)
                      ),
            loading = my_std$est.std,
            h2 = ifelse(my_std$op=="=~",
                        my_std$est.std^2, NA), #communality
            SE = my_std$se,
            z.value = my_std$z,
            pvalue = my_std$pvalue
        )

    if (ci | est) {
        my_est <-
          lavaan::parameterEstimates(fit,
                               zstat = FALSE,
                               pvalue = FALSE,
                               ci = FALSE)
        result_loadings <-
            cbind(result_loadings[1], est = my_est$est,  my_std[, 2:ncol(result_loadings)])
    }

    #-- Output
    if (ci) {
        x_ci <- lavaan::parameterEstimates(fit, ci = ci, standardized = TRUE)
        result_loadings$ci.lower = x_ci$ci.lower
        result_loadings$ci.upper = x_ci$ci.upper
    }

    if (type == "all"  | type == "1" | type == "fit")
      Output(fit_Measures,

        caption = caption,
        note = note
      )

    if (type == "all" | type == "2"){
      mysplit <- my_std$lhs==my_std$rhs
      VR<- which(mysplit)
      LV <- which(!mysplit)

      Output(fix_format(result_loadings[LV,]),
             caption = paste("Latent variables ",caption),
             note = note
      )

      Output(fix_format(result_loadings[VR,1:2]),
            caption = paste("Variances",caption),
            note = note,
            col_names= c("Quelle","Variance" )
      )

      }



}






