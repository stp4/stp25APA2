#' @rdname APA
#' @export
APA.polr   <- function(x, ...)
 {
  res <- as.data.frame(lmtest::lrtest(x))
  # likelihood ratio tests
  paste0("-LL=" ,
         round(as.numeric(logLik(x)), 1),
         ", ",
         rndr_Chisq(res$Chisq[2], abs(res$Df[2]), res[2, "Pr(>Chisq)", drop =
                                                        TRUE]))
  
}


#' @rdname APA2
#' @description Ordered Logistic or Probit Regression  
#' https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#' @export
#' @examples 
#'  
#'  #--- Ordered Logistic or Probit Regression 
#'   options(contrasts = c("contr.treatment", "contr.poly"))
#'   house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
#'   # house.plr
#'   APA2(house.plr, note= APA(house.plr))
#' 
#' 
APA2.polr <- function(x,
                      caption = NULL,
                      note = NULL,
                      include.b = TRUE,
                      include.se = TRUE,
                      include.ci = FALSE,
                      include.odds = TRUE,
                      ...) {
  res <- Ordnen(
    x,
    include.b = include.b,
    include.se = include.se,
    include.ci = include.ci,
    include.odds = include.odds,
    ...
  )
  
  if (is.null(caption))
    caption <- paste(attr(res, "caption"),
                     "Obs: ", attr(res, "N"))
  if (is.null(note))
    note <- attr(res, "note")
  
  Output(fix_format(res),
         caption = caption,
         note = note)
  
  invisible(res)
}
