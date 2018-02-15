#' @name Prop_Test
#' @rdname Prop_Test
#' @title Prop Test
#' @description Ausgabe von Konfidenzintervallen von Haufikeiten
#' @param x data.frame-Objekt
#' @param p Probability 0.50
#' @param n anzahl
#' @param conf.level Conf-Intervall 95
#' @param digits Nachkomnmastellen
#' @return data.frame
#' @export
#' @examples
#' set.seed(234)
#' n<-999
#' library(BayesianFirstAid)
#' smokers  <- c( 83, 90, 129, 70 )
#' patients <- c( 86, 93, 136, 82 )
#'
#'
#' g =gl(3, n/3, labels = c("Control", "Treat A", "Treat B"))
#' g2<- g[sample.int(n)]
#' levels(g2)<- c("male", "female", "female")
#' data<- data.frame(g=g, g2=g2,
#'                   x=rnorm(n) )[sample.int(n)[1:78],]
#' #Prop_Test2(data$g)
#' x<-as.data.frame(table(data$g))$Freq
#  bayes.prop.test(x, rep(nrow(data), nlevels(data$g)), p=1/3)
#'
#' APA2(~g+g2, data, type="freq.ci")
#' APA2(g~g2, data, type="freq.ci")
Prop_Test <- function(x, ...){
  UseMethod("Prop_Test")
}

#' @rdname Prop_Test
#' @export
Prop_Test2 <- function(x, ..., caption="", note="") {
 # Prop.Test2
  Prop_Test.default(x, ...)[c(1,2, 6)] %>% Output(caption, note)

  }

#' @rdname Prop_Test
#' @export
Prop_Test.default<- function(x,
                      p=1/nlevels(x),
                      n=length(na.omit(x)),
                      conf.level=.95,
                      digits = 0,
                      include.ci=TRUE,
                      ...){
  if (length(x)<=0) return("NaN")
  res <- as.data.frame(tab <- table(x))
  res$Prz <- as.data.frame(prop.table(tab))$Freq

  res <- cbind(res,
             t(
               sapply(res$Freq,
                      function(x){
                        ci<-prop.test(as.integer(x),
                                      n, p,
                                      conf.level=conf.level)$conf.int
                        names(ci) <- c("low", "hig")
                        ci})
               ))


  res$Statistics <- paste0( Format2(res$Prz*100, digits = digits), "% CI=[",
                           #-- rechnet falsch
                           #       ffCI(cbind(res$low*100, res$hig*100), digits=digits))
                           Format2(res$low*100, digits = digits), " - ",
                           Format2(res$hig*100, digits = digits) , "]")
  res$n <- ""
  names(res)[1] <- "Characteristics"
  res$n[1] <- n

  res

 # data.frame(Characteristics="dummy", n=1, Statistics="dummy")
}

