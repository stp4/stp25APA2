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

#' @rdname Ordnen
#' @export
Ordnen.eff <- function(x, ...) {
  if (names(x)[2] %in% "formula")
    efflist <- list(Effect = x)
  Ordnen.efflist(efflist, ...)
}

#' @rdname Ordnen
#' @export
Ordnen.efflist <- function(x, ...) {
  res_list <- NULL
  for (i in names(x)) {
    info <- model_info(x[[i]])
    AV <- ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
    ans <- as.data.frame(x[[i]])
    n<- ncol(ans)

    ans[1:(n-4) ] <- lapply(ans[1:(n-4)], as.character)


    myN <- aggregate_effect(x[[i]], info$y, info$x)
    #- aggregate verwirft Leere Eintraege
    if (nrow(ans) == nrow(myN)) {
      ans$N <- myN[, ncol(myN)]
      attr(ans, "note") = ""
    }
    else{
      ans$N <- NA
      attr(ans, "note") = "Warnung: Die Stichprobe ist relativ klein sodass die Anzahl nicht berechnet werden kann."
    }
    attr(ans, "caption") =  paste0("AV: ", AV)
    attr(ans, "N") = info$N
    attr(ans, "labels") = info$labels

    res_list[[i]] <- ans
  }
  res_list
}

