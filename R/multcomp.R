#' @rdname APA2
#' @export
#' @examples 
#' #----------------------------------------------------------------
#' # multcomp
#' #----------------------------------------------------------------
#' #require(graphics)
#' #
#' #-- breaks ~ wool + tension ----------------------
#' #warpbreaks %>% Tabelle2(breaks, by= ~ wool + tension)
#' summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
#' 
#' #  ANOVA
#' APA2(fm1, caption="ANOVA")
#' 
#' # TukeyHSD
#' TukeyHSD(fm1, "tension", ordered = TRUE) %>%
#'   APA_Table(caption="TukeyHSD" )
#' 
#' #plot(TukeyHSD(fm1, "tension"))
#' #levels(warpbreaks$tension)
#' 
#' # Lm Split
#' fm1_split <-  summary(fm1,
#'                       split=list(tension=list( M=1,  H=3, L=2)),
#'                       expand.split=FALSE)
#' APA2(fm1_split)
#' 
#' 
#' # Multcomp
#' require(multcomp)
#' 
#' fit_Tukey <-glht(fm1,
#'                  linfct=mcp(tension="Tukey"),
#'                  alternative = "less"
#' ) 
#'  
#' APA_Table(fit_Tukey, caption="APA_Table: multcomp mcp Tukey")
#' 
#' APA2(fit_Tukey, caption="APA2: multcomp mcp Tukey")
#' 
#' 
#' 
#' ### contrasts for `tension'
#' K <- rbind("L - M" = c( 1, -1,  0),
#'            "M - L" = c(-1,  1,  0),
#'            "L - H" = c( 1,  0, -1),
#'            "M - H" = c( 0,  1, -1))
#' 
#' warpbreaks.mc <- glht(fm1,
#'                       linfct = mcp(tension = K),
#'                       alternative = "less")
#' APA2(warpbreaks.mc, caption="APA2: multcomp mcp mit Contrasten")
#' ### correlation of first two tests is -1
#' cov2cor(vcov(fm1))
#' 
#' ### use smallest of the two one-sided
#' ### p-value as two-sided p-value -> 0.0232
#' summary(fm1)
#' 
#' 
#' # -- Interaction ------------------------
#' summary(fm2 <- aov(breaks ~ wool * tension, data = warpbreaks))
#' APA_Table(fm2)
#' x <- TukeyHSD(fm2, "tension",
#'               ordered = TRUE) 
#' APA2(x, caption="Interaction: TukeyHSD" )
#' 
#' 
#' 
#' warpbreaks$WW<-interaction(warpbreaks$wool,warpbreaks$tension )
#' mod2<-aov(breaks~WW, warpbreaks)
#' APA2(mod2, caption="ANOVA interaction haendich zu den Daten hinzugefuehgt")


APA2.TukeyHSD <- function(x,
                          caption = " TukeyHSD",
                          note = "", output = stp25output::which_output(),
                          ...) {
 # res <- list()
  for (i in names(x)) {
    rs <-  fix_format(x[[i]])
    rs <- prepare_output(cbind(Source = rownames(rs), rs),
                         caption = caption,
                         note = note)
    Output(rs, output=output)
   # res[[i]] <- rs
  }
  invisible(x)
}



#' @rdname APA2
#' @description APA2.glht  multcomp::glht
#' @param include.ci APA2.glht: Cis
#'
#' @return dataframe mit p.werte
#' @export
#'
#' @examples
#' 
#' 
#' 
#' library(multcomp)
#'  ### multiple linear model, swiss data
#'  lmod <- lm(Fertility ~ ., data = swiss)
#'
#'  ### test of H_0: all regression coefficients are zero
#'  ### (ignore intercept)
#'
#' ### define coefficients of linear function directly
#' K <- diag(length(coef(lmod)))[-1,]
#' rownames(K) <- names(coef(lmod))[-1]
#' K

#' ### set up general linear hypothesis
#' APA2(glht(lmod, linfct = K))
#'
APA2.glht <-
  function(x,
           caption = "Multiple Comparisons of Means",
           note = "", output = stp25output::which_output(),
           include.ci=TRUE,
           # include.se=TRUE,
           # include.t=TRUE,
           level = 0.95,
           ...) {
    #multcomp:::print.summary.glht
    sx <- summary(x)
    pq <- sx$test
    mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
    error <- attr(pq$pvalues, "error")
    
    colnames(mtests) <- c("Estimate",
                          "Std.Error",
                          ifelse(x$df == 0, "z.value", "t.value"),
                          "p.value")
    type <- pq$type
    if (!is.null(error) && error > .Machine$double.eps) {
      sig <- which.min(abs(1 / error - (10 ^ (1:10))))
      sig <- 1 / (10 ^ sig)
    }
    else {
      sig <- .Machine$double.eps
    }
    alt <- switch(
      sx$alternative,
      two.sided = "==",
      less = ">=",
      greater = "<="
    )
    
    res <- data.frame(Source = paste(rownames(mtests), alt, sx$rhs),
                      mtests)
    
    if(include.ci){ # Entweder CI oder SE
      res<- cbind(res[1:2],  confint(x, level=level)$confint[,2:3], res[4:ncol(res)] )
    }
    
    res <- prepare_output(res, caption=caption, note=note)
    Output(fix_format(res), output=output)
    invisible(res)
  }


#' @rdname APA2
#' @export
APA2.multicomp <-function(x, ...) APA2.glht(x, ...)