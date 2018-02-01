#' countDigits
#' @description Interne Function wird in Meanci2() verwendet
#' @param x zu pruefendes Objekt
#' @return Anzahl an Nachkommastellen
#' @export
#' @examples
#' countDigits(1.2345)
countDigits <- function(x) {
  x<- signif(x, 3)
  x <- strsplit(as.character(x),"\\.")[[1]][2]
  if (is.na(x))
    0
  else
    nchar(x)
}
