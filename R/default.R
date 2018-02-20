#' @rdname APA
#' @export
APA.default <- function(x, ...) {
  cat("\nKeine Methode fuer: ", class(x), "\n")
}


#' @rdname APA2
#' @export
APA2.default <- function(x,
                         ...,
                         caption = "",
                         output = TRUE) {

  Text("Keine Methode fuer ", class(x) ," vorganden!")
}

