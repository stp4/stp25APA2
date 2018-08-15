#' prepare_output
#' Ergebnis fuer Output vorbereiten
#' @param x Objekt Vector oder auch Formel
#' @param caption bei verwendung von Formeln
#' @param note Fehlende Werte
#' @param N Dezimalstellen bei zB Mean2
#' @param labels Grenzen der Konfidenzintervalle
#' @param ... Weitere Argumente
#' @return data.frame mit attributen
#' @export
prepare_output<- function(x, #Objekt Liste oder df
                          caption=NULL,
                          note=NULL,
                          N=NULL,
                          labels=NA,
                          ...

){
# dann if(is.list(caption))
# print(c(caption, note, N, labels ))

 if(is.null(caption)) caption<- ""
 if(is.null(note)) note<- ""
 if(is.null(N)) N <- model_info(x)[["N"]]


  attr(x, "caption") =  caption
  attr(x, "note") = note
  attr(x, "N") = N
  attr(x, "labels") = labels

  x
}

