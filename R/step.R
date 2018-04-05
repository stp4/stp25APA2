#' @rdname APA2
#' @param include.df df mit ausgeben
#'
#' @export
#'
#' @examples
#' # require(stpvers)
#' require(lmerTest)
#' summary(lm1 <- lm(Fertility ~ ., data = swiss))
#' slm1 <- stats::step(lm1)
#' APA2(slm1)
#' 
#' m <- lmerTest::lmer(
#'   Informed.liking ~ Product * Information * Gender +  
#'   (1 | Consumer) + (1 | Product:Consumer),
#'   data = ham
#' )
#' #elimination of non-significant effects
#' s <- lmerTest::step(m)
#' 
#' APA2(s)
#' 
APA2.step <- function(x,
                      caption = "Backward elimination of non-significant effects of linear mixed effects model",
                      note = "",
                      include.se = FALSE,
                      include.df = FALSE,
                      #include.t=TRUE,
                      ...) {
  res <- NULL
  if (any(names(x) %in% "lsmeans.table")) {
    res <- x$diffs.lsmeans.table
    
    if (nrow(res) == 0)
      return("Finde keine Loesung step!")
    res <- cbind(Source = rownames(res), res)
    
    
    names(res)[8] <- "p.value"
    names(res)[3] <- "SE"
    names(res)[6:7] <- c("lwr",   "upr")
    res <- res[c(1, 2, 3, 6, 7, 4, 5, 8)]
    if (!include.se)
      res <- res[, -which(names(res) == "SE")]
    if (!include.df)
      res <- res[, -which(names(res) == "DF")]
    res %>% fix_format() %>% Output("Anova all Data", caption)
    
  }
  else{
    warnings("Nicht getestete Methode")
    res <- broom::tidy(x)
    fix_format(res) %>% Output("Anova all Data", caption = "Broom")
  }
  
  invisible(res)
}
