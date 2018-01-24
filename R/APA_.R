#' @rdname APA
#' @title APA
#' @name APA
#' @description  APA Funktion wie \link{APA2} hier werden aber keine Tabellen oder HTML ausgegeben.
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return  Character Vector mit einem oder meheren Eintraegen
#' @examples
#' APA(mpg ~ cyl, mtcars)
#' APA(coin::wilcox_test(mpg ~ factor(vs), mtcars))
#' APA(Hmisc::spearman2(mpg ~ factor(vs), mtcars))
#' APA(glm(vs ~ mpg, mtcars, family = binomial()))
#' APA(lm(mpg ~ drat + wt + qsec, mtcars))
#' APA(wilcox.test(mpg ~ vs, mtcars))
#' APA(t.test(mpg ~ vs, mtcars))
#'
#' @export
APA <-   function(x, ...) { UseMethod("APA") }

#' @rdname APA
#' @export
APA.ScalarIndependenceTest <- function(x, ...) {
  ## coin::wilcox_test
  capture.output(x)[5]
}
#' @rdname APA
#' @export
APA.biVar <- function(x, ...) {
  ## Hmisc::spearman2 Wilkox-Test
 # capture.output (x)[5]

  x<- unlist(as.list(x))
  names(x) <- c( "rho2",
                 "F", "df1", "df2", "P", "Adjusted.rho2", "n")
  paste0("rho2=", Format2(x[1],2), ", ",
          rndr_F(x[2],
                 x[3],
                 x[4],
                 x[5]))

}


#' @rdname APA
#' @export
APA.default <- function(x, ...) {
cat("\n", class(x), "\n")
}

#' @rdname APA
#' @export
APA.htest <- function(x,  ...){
    if(names(x$statistic)=="t")
          rndr_T(x$statistic,
                x$parameter,
                x$p.value)
    else rndr_W(x$statistic,
                x$p.value)
}
# Format from model.
#' @rdname APA
#' @export
APA.lm <- function(x, ...) {
  if(any(class(x)=="aov")) x<- lm(x)
  fitSummary <- summary(x)
  fstats <- fitSummary$fstatistic
  pValue <-  stats::pf(fstats[['value']],
                       fstats[['numdf']],
                       fstats[['dendf']], lower.tail=FALSE)

        rndr_lm(fstats[['value']] ,
                fstats[['numdf']],
                fstats[['dendf']],
                pValue,
                fitSummary$r.squared,
                fitSummary$adj.r.squared)
}


#' @rdname APA
#' @export
APA.glm <- function(x, ...) {
lrtst <-  lmtest::lrtest(x)
paste0( "LogLik=",Format2(lrtst[2, 2], 2),", ",
        rndr_X(lrtst[2, 4],
               lrtst[1, 1],
               lrtst[2, 1],
               lrtst[2, 5]))
}

#' @rdname APA
#' @export
APA.formula<- function(x, data, type="mean", digits=2, ... ){
#type=c("mean","median")
if(length(data)==0) return("Fehlende Daten")
type <-  match.arg(type )
res<-aggregate(x, data, function(y)
      paste0(
        Format2(mean(y, na.rm=TRUE), digits), " (",
        Format2(mean(y, na.rm=TRUE), digits), ")" ))

apply(res, 1, function(y) {paste(y, collapse=" = ")})
}



