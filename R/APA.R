#' APA Style Text-Ausgabe
#' 
#' Ausgabe von APA-Style formatiertem Text.
#' 
#' 
#' @name APA
#' @param x Objekt fit, formula usw
#' @param data data.frame wenn x eine Formel ist
#' @param digits Nachkommastellen
#' @param ... weitere Objekte
#' @return   Character Vector mit einem oder meheren Eintraegen
#' @examples
#' 
#' APA(mpg ~ cyl, mtcars)
#' APA(glm(vs ~ mpg, mtcars, family = binomial()))
#' APA(lm(mpg ~ drat + wt + qsec, mtcars))
#' APA(aov(mpg ~ drat + wt + qsec, mtcars))
#' @export
APA <-   function(x, ...) {
  UseMethod("APA")
}





#' @rdname APA
#' @export
APA.NULL <- function(x, ...) {
  "no input" 
 }






#' APA Style HTML-Tabellen-Ausgabe
#'
#' APA2 erstellt fertigen HTML-Tabellen Output.
#' 
#' @name APA2
#' @param x Ein R Objekt oder eine Formel oder ein data.frame
#' APA2.list: Liste mit Objekten (fits)
#' @param data data.frame wenn x eine Formel ist
#' @param caption,note Ueberschrift an Output
#' @param digits Nachkommastellen 
#'  APA2.list: Kommastellen bei uebergabe einer liste  muss exakt die Reighenfolge eingehalten werden.
#' @param ... weitere Argumente
#' @return html-String ueber cat sowi einen data.frame
#' @export
APA2 <- function(x,  ...) {
  UseMethod("APA2")
}

#' @rdname APA2
#' @export
APA2.NULL <- function(x, ...) {
  Text("no input")
 invisible(data.frame()) 
}




#' APA Syle Table
#' 
#' All functions in stringr start with APA_ and take a Object (formula, data.frame, lm, ...) 
#' as the first argument.
#' 
#' @name APA_
#' @param x An object to be converted into a tidy data.frame or Formula
#' @param data data.frame wenn x eine Formel ist
#' @param digits Nachkommastellen
#' @param caption,note Ueberschrift an Output
#' 
#' @param ... extra arguments
#' @return a data.frame or list with data.frame
#' @export
APA_NULL <- function(x, ...) {
    Text("no input")
    invisible(data.frame()) 
  }









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





#- APA- Test -------------------------------------


#' @rdname APA
#' @export
test_APA <- function(x){
  (lenght(x)>0)  &  is.character(x) 
  
}

#' @rdname APA
#' @export
test_APA2 <- function(x){
  test_res<-TRUE
  if(is.data.frame(x)) {
    test_res <- test_is_data_frame(x)
  }
  else if(is.list(x)){
    for(i in seq_len(length(x)) ){
      test_res<-c(test_res, test_is_data_frame(x[[i]]) )
    }
  }
  else if(is.character(x)){
    test_res<-TRUE
  }
  else {test_res<-FALSE}
  return(all(test_res))
}


test_is_data_frame<- function(x){
  
  if(is.data.frame(x)) {
    if(length(x)==0) {
      warning("Keine Ergebnisse in data.frame")
      return(FALSE)
    }
    else test_attributs(x)
  }
}
test_attributs<- function(x){
  
  res<- list()
  
  if(!is.data.frame(x)) res["class"] <- class(x)[1]
  
  if(is.null(attr(x, "caption")))  res["caption"] <- FALSE
  if(is.null(attr(x, "note")))     res["note"] <- FALSE
  if(is.null(attr(x, "N")))        res["N"] <- FALSE
  if(is.null(attr(x, "labels")))   res["labels"] <- FALSE
  
  if(length(res)==0) TRUE
  else{
    res<- unlist(res)
    warning(paste(paste(names(res), " = ", res), collapse=", "))
    FALSE
  }
}

