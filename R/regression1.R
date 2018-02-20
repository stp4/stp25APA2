#' @rdname APA
#' @description APA.lm F-Test aus lm und Anova
#' @param include.r APA.lm: R-Squar
#' @export
APA.lm <- function(x, include.r = TRUE) {
  if (any(class(x) == "aov"))
    x <- lm(x)
  fitSummary <- summary(x)
  fstats <- fitSummary$fstatistic
  pValue <-  stats::pf(fstats[['value']],
                       fstats[['numdf']],
                       fstats[['dendf']], lower.tail = FALSE)
  if (include.r)
    rndr_lm(fstats[['value']] ,
            fstats[['numdf']],
            fstats[['dendf']],
            pValue,
            fitSummary$r.squared,
            fitSummary$adj.r.squared)
  else
    rndr_F(fstats[['value']] ,
           fstats[['numdf']],
           fstats[['dendf']],
           pValue)
  
}


#' @rdname APA
#' @export
APA.glm <- function(x, ...) {
  lrtst <-  lmtest::lrtest(x)
  paste0("LogLik=",
         Format2(lrtst[2, 2], 2),
         ", ",
         rndr_X(lrtst[2, 4],
                lrtst[1, 1],
                lrtst[2, 1],
                lrtst[2, 5]))
}
