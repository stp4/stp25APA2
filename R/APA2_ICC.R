#' @rdname ICC
#' @title ICC Funktion
#' @name ICC
#' @description ICC Intra-Klassen-Korrelation.
#'  Kopie von \link{ICC} aus dem Packet \code{psych}.
#'  psych erwartet eine Matrix in form von:
#'
#'  ICC und ICC2 erlaubt die Verwendung von Formeln \code{ICC(~a+b+c+g, data)}
#'  wobei hier auch die Rater bzw Judge
#'
#'  weitere Metode siehe \link{AD_Index}
#' @param x  Objekt
#' @param data Daten
#' @param caption default = "ICC",
#' @param type 1 bis 6 type = c("all", "ICC1", "ICC2")
#'  c("ICC1" "ICC2", "ICC3" "ICC1k" "ICC2k"  "ICC3k")
#' @param ... Weitere Argumente al psych missing=TRUE, alpha=.05
#' @return ICC gibt ein psych -Objetht retur ICC2 einen data.frame
#' @export
#' @examples
#' ##library(stp25)
#' ##Projekt("html", "ICC")
#' #
#' #Quelle:James Demaree Woolf http://sci-hub.ac/10.1037/0021-9010.69.1.85
#' data<-GetData(
#'   "Judge   item1 item2 item3 item4 item5 item6
#'   1       3     4     3     2     4     3
#'   2       2     3     3     2     4     4
#'   3       4     3     4     3     4     3
#'   4       3     3     2     2     2     4
#'   5       3     2     2     4     2     3
#'   6       4     2     4     3     2     3
#'   7       2     3     2     3     3     4
#'   8       3     4     4     3     3     2
#'   9       4     2     3     4     3     2
#'   10      2     4     3     4     3     2")
#'
#'
#'
#'
#' AD_Index2(~item1+item2+item3+item4+item5+item6, data, type="item")
#'
#' data2<- data.frame(t(data[,-1]))
#' names(data2)<- Cs(J1, J2, J3, J4, J5, J6, J7, J8, J9, J10)
#' AD_Index2(~J1+J2+J3+J4+J5+J6+J7+J8+J9+J10, data2, type="judge")
#'
#'
#'
#' data<- GetData("
#'                Item J1 J2 J3 J4 J5 J6 J7 J8 J9 J10
#'                1  5  4  5  4  5  4  5  4  5  4
#'                2  4  5  4  5  4  5  4  5  4  5
#'                3  5  5  5  5  5  4  4  4  4  4
#'                4  5  4  4  5  5  5  4  5  4  5")
#'
#'
#' AD_Index2(~J1+J2+J3+J4+J5+J6+J7+J8+J9+J10, data)
#' #Quelle:James Demaree Woolf
#' ICC2(~J1+J2+J3+J4+J5+J6+J7+J8+J9+J10, data)
#' ##Head("personality-project")
#' #Quelle  http://www.personality-project.org/r/book/Chapter7.pdf
#' sf <- GetData("
#'               J1 J2 J3 J4 J5 J6
#'               1  1  6  2  3  6
#'               2  2  7  4  1  2
#'               3  3  8  6  5 10
#'               4  4  9  8  2  4
#'               5  5 10 10  6 12
#'               6  6 11 12  4  8")
#' sf  # Intraclass Correlation Coefficient (ICC)
#' ICC2(sf)
#'
#' #AD_Index2(~., data)
#'
#' #End()
#' @export
ICC <- function(x, ...) {
  UseMethod("ICC")
}
#' @rdname ICC
#' @export
ICC.matrix <- function(x, ...) {
psych::ICC(x)
}

#' @rdname ICC
#' @export
ICC.data.frame <- function(x, ...) {
psych::ICC(as.matrix(x))
}

#' @rdname ICC
#' @export
ICC.formula <- function(x, data, ...) {

 X<- Formula_Data(x, data)
psych::ICC(as.matrix(X$Y_data))

}

#' @rdname ICC
#' @export
ICC2 <- function(x, ..., caption="ICC", type=c(1, 4)) {
  ans<- ICC(x, ...)
  APA.ICC(ans, caption=caption, type=type)
}


#' @rdname APA2
#' @export
APA.ICC<- function(x, caption="ICC", type=c(1, 4)){
  n<- paste0("obs=", x$n.obs, ", judge=", x$n.judge )
  ans <-  prepare_output(x$results[type,],
                         caption=caption,
                         N=n)
  fix_format(ans) %>% Output(note= paste("Number of subjects: ", n))

  invisible(ans)
}


