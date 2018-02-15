#' @rdname Ordnen
#' @title Ordnen (Tidy)
#' @name Ordnen
#' @description Diese Funktion meine Version von tidy
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return ein data.frame-Ojekt oder eine Liste von data.frames. Im Attribut N sind die Stichprobengroesse
#' und notes
#' @export
#Tidy#
Ordnen <- function(x, ...) {
  UseMethod("Ordnen")
}
#' @rdname Ordnen
#' @export
Ordnen.default <- function(x, ...) {
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])

  df <- broom::tidy(x)
  if(any(class(x) %in% "merModLmerTest")) {
    df<- cbind(df[1:(ncol(df)-1)], p.value=NA, df[ncol(df)])
    p_value<- lmerTest::summary(x)$coefficients[,5]
    df$p.value[ 1:length(p_value)]<- p_value
  }
  # else if(class(x)=="lmerMod"){
  #   Text(
  #     "
  #     ----
  #     Achtung: Paket library(lmerTest) laden.
  #     Bzw die update() Funktion nicht verwenden.
  #     ----
  #     "
  #   )
  # }
  else {}
  #attr(df,"class2") = info$class
  attr(df, "caption") =  paste0("AV: ", AV)
  attr(df, "note") = paste0("Model: ", info$family[1])
  attr(df, "N") = info$N
  attr(df, "labels") = info$labels

  df
  }



