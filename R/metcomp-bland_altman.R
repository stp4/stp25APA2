#' @rdname APA2
#' @export
APA.bland_altman <-
  function(x, ...) {
    paste0("m = ",
           x$stat$Unit[2],
           ", d = [",
           x$stat$Unit[5],
           ", ",
           x$stat$Unit[5],
           "]")
  }


#' @rdname APA2
#' @export
APA2.bland_altman <- function(x,
                              caption = paste0("Difference (", x$name.diff,
                                               "), Mean (",  x$name, ")"),
                              note = "",
                              ...) {
  res <-  prepare_output(x$stat, caption = caption)
  Output(res)
  invisible(res)
}

 print.bland_altman <- function(x, ...){ print(x$stat) }



#' @rdname BlandAltman
#' @title BlandAltman
#' @name BlandAltman
#' @description Diese Funktion ist ein Platzhalter
#' (see \code{\link[haven]{tagged_na}})
#' und zeigt die ...
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return A \code{\link[tibble]{tibble}} with counted tagged NA values.
#'
#' @examples
#' library(haven)
#'
#'

#' @export

BlandAltman<-function(.data, x, ...) {
  UseMethod("BlandAltman")
}

#' @rdname BlandAltman
#' @export
BlandAltman.data.frame <- function(.data, x, ...) {
  #-- hier Fehlt noch die Unterscheidung in 2 oder mehr Vergleiche
  BAP(x, .data, ...)
}
#' @rdname BlandAltman
#' @export
BlandAltman.formula <- function(x, .data, ...) {
  BlandAltman.data.frame(.data, x, ...)
}


#-- Helper Bland Altman
bland.altman.stats<-
function (dfr,
          two = 1.96, #mode = 1,
          conf.int = 0.95,
          digits=2)
{  ##<environment: namespace:BlandAltmanLeh>

  called.with <- nrow(dfr)
  dfr <- na.omit(dfr)
  based.on <- nrow(dfr)
  if (based.on < 2)
    warning("Warning in bland.altman.stats:less than 2 data pairs after deleting NAs.",
            call. = FALSE)
  if (ncol(dfr) > 2)
    warning("Warning in bland.altman.stats:Mehr als 2 Methoden.",
            call. = FALSE)
  diffs <- dfr[[1]] - dfr[[2]]
  means <-  rowMeans(dfr)
  diffs.percent<-diffs/means*100
  diffs.percent[is.infinite(diffs.percent)]<-0

  critical.diff <- two * sd(diffs)
  mean.diffs <- mean(diffs)
  sd.diffs<- sd(diffs)
  lower.limit <- mean.diffs - critical.diff
  upper.limit <- mean.diffs + critical.diff
  lines <- c(lower.limit = lower.limit,
             mean.diffs = mean.diffs,
             upper.limit = upper.limit)
  t1 <- qt((1 - conf.int)/2, df = based.on - 1)
  t2 <- qt((conf.int + 1)/2, df = based.on - 1)

  se.ci<- sqrt(sd(diffs)^2 * 3/based.on)
  se.mean<-sd(diffs)/sqrt(based.on)
  CI.lines <- c(lower.limit.ci.lower = lower.limit + t1 * se.ci,
                lower.limit.ci.upper = lower.limit + t2 * se.ci,
                mean.diff.ci.lower = mean.diffs + t1 * se.mean,
                mean.diff.ci.upper = mean.diffs + t2 * se.mean,
                upper.limit.ci.lower = upper.limit +  t1 * se.ci,
                upper.limit.ci.upper = upper.limit +  t2 * se.ci)
  #--- Prozent


  mean.percent<-mean(diffs.percent)
  ssd.persent<- sd(diffs.percent)
  critical.diff.percent <- two * ssd.persent
  se.ci.percent<- sqrt(ssd.persent^2 * 3/based.on)
  se.mean.percent<-ssd.persent/sqrt(based.on)
  lower.limit.percent = mean.percent-critical.diff.percent
  upper.limit.percent = ssd.persent+critical.diff.percent

  CI.lines.percent <- c(lower.limit.ci.lower = lower.limit.percent + t1 * se.ci.percent,
                lower.limit.ci.upper = lower.limit.percent + t2 * se.ci.percent,
                mean.diff.ci.lower = mean.percent + t1 * se.mean.percent,
                mean.diff.ci.upper = mean.percent + t2 * se.mean.percent,
                upper.limit.ci.lower = upper.limit.percent +  t1 * se.ci.percent,
                upper.limit.ci.upper = upper.limit.percent +  t2 * se.ci.percent)


  res<- list(lines = lines,  # wie oben ll mean ul
              CI.lines = CI.lines,
              lines.percent = c( mean.percent-critical.diff.percent,
                                 mean.percent,
                                 mean.percent+critical.diff.percent),
              CI.lines.percent = CI.lines.percent,

              stat= data.frame(Parameter=c("df (n-1)","difference mean (d)",
                                           "standard deviation (s)", "critical.diff (1.96s)",
                                           "d-1.96s", "d+1.96s"),
                               Unit= c(Format2(based.on - 1, 0),
                                       Format2( c(mean.diffs, sd.diffs,critical.diff,
                                                    lower.limit,upper.limit), digits)
                                       ),
                               Percent= c("",
                                        ffprozent.default(
                                           c(mean.percent, ssd.persent,
                                           critical.diff.percent,
                                           lower.limit.percent,
                                           upper.limit.percent)
                                            ,digits=1)),
                                SE= Format2(c(NA,se.mean, NA,NA, se.ci, se.ci),digits),
                               CI.low= Format2(c(NA,CI.lines[3],NA,NA,CI.lines[1],CI.lines[5]),digits),
                               CI.hig= Format2(c(NA,CI.lines[4],NA,NA,CI.lines[2],CI.lines[6]),digits)
                               ),
              data=cbind(dfr,
                         means,
                         diffs,
                         diffs.percent=diffs.percent)

              )
  class(res)<- c("bland_altman")
  return(res)
}


BAP2<-  function(x, .data, ...){
  APA2.bland_altman(BAP(x, .data, ...))
 }


#--- Helper
BAP<- function(x, .data, ...){
 # cat("\n in BAP ")
  X<-Formula_Data(x, .data)

  ba.stats <- bland.altman.stats( X$Y_data )
  ba.stats$name <-  paste(X$yname, collapse=", ")
  ba.stats$name.diff <-  paste(X$yname[1:2], collapse=" - ")
  ba.stats$met_A <-X$yname[1]
  ba.stats$met_B <-X$yname[2]
  ba.stats$groups= X$X_data
  
 # print(str(ba.stats))
  ba.stats
}

