#' @rdname APA
#' @export
APA.default <- function(x, ...) {
  cat("\n", class(x), "\n")
}




#' @rdname APA
#' @export
APA.formula <- function(x,
                        data,
                        type = "mean",
                        digits = 2,
                        ...) {
  #type=c("mean","median")
  if (length(data) == 0)
    return("Fehlende Daten")
  type <-  match.arg(type)
  res <- aggregate(x, data, function(y)
    paste0(Format2(mean(y, na.rm = TRUE), digits), " (",
           Format2(mean(y, na.rm = TRUE), digits), ")"))
  
  apply(res, 1, function(y) {
    paste(y, collapse = " = ")
  })
}





#' @rdname APA2
#' @export
APA2.default <- function(x,
                         ...,
                         caption = "",
                         output = TRUE) {

  Text("Keine Methode fuer ", class(x) ," vorganden!")
}


 



