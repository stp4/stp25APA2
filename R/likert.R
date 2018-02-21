#' @rdname APA2
#' @param type APA2: c(1, 2), oder c("Freq", "Precent")
#' @param ReferenceZero,labels APA2.likert: ReferenceZero Neutrales Element in Kombination mit
#'  labels c("low", "neutral", "high")
#' @param include.mean APA2.likert: Mittelwerte T/F
#' @param na.exclude APA2.likert: NAs entfernen
#' @param nlevels APA2.likert: nicht ändern
#' @export
APA2.likert <- function(x,
                        caption = "" ,
                        note = "",
                        col_names = NULL,
                        print_col = NULL,
                        ...,
                        ReferenceZero = NULL,
                        type = "percent",
                        include.mean = TRUE,
                        na.exclude = FALSE,
                        nlevels = x$nlevels,
                        labels = c("low", "neutral", "high"),
                        order = FALSE) {
  RowSums2 <-  function(x)  if (is.vector(x))   x  else   rowSums(x, na.rm = TRUE)
  
  if (!is.null(ReferenceZero)) {
    if (is.character(ReferenceZero))
      ReferenceZero <- which(x$levels %in% ReferenceZero)
    else if (!is.numeric(ReferenceZero))
      ReferenceZero <- median(1:nlevels)
    
    cat(ReferenceZero, "\\n")
    
    if (ceiling(ReferenceZero) == floor(ReferenceZero)) {
      freq <- cbind(
        lowrange = RowSums2(x$freq[, 1:(ReferenceZero - 1)]),
        neutral = x$freq[, ReferenceZero],
        highrange = RowSums2(x$freq[, (ReferenceZero + 1):nlevels])
      )
      colnames(freq) <-
        c(
          paste0(labels[1], "(1:", ReferenceZero - 1, ")"),
          paste0(labels[2], "(", ReferenceZero, ")"),
          paste0(labels[3], "(", ReferenceZero + 1, ":", nlevels, ")")
        )
      x$freq <- freq
      x$freq.na <- if (names(x$freq.na)[ncol(x$freq.na)] == "NA")
        cbind(freq, x$freq.na[ncol(x$freq.na)])
      else
        freq
      
    } else{
      freq <-
        cbind(lowrange = RowSums2(x$freq[, 1:floor(ReferenceZero)]),
              highrange = RowSums2(x$freq[, ceiling(ReferenceZero):nlevels]))
      colnames(freq) <-
        c(paste0(labels[1], "(1:", floor(ReferenceZero), ")"),
          paste0(labels[3], "(", ceiling(ReferenceZero), ":", nlevels, ")"))
      x$freq <- freq
      x$freq.na <- if (names(x$freq.na)[ncol(x$freq.na)] == "NA")
        cbind(freq, x$freq.na[ncol(x$freq.na)])
      else
        freq
    }
  }
  
  if (!na.exclude)  x$freq <- x$freq.na
  results2Prozent <- x$freq / x$n * 100
  
  if (type == "percent" | type == 2) x$freq <- ffprozent(results2Prozent, x$freq)
  
  
 if (include.mean)
    ans <-cbind(x$names,
          x$freq,
          n = x$n,
          Mittelwert = x$Mittelwert)
  else
    ans <- cbind(x$names, x$freq)
  
  
  if (!is.logical(order) & order == "decreasing")
    ans <- ans[order(x$m,  decreasing = TRUE),]
  else if (order)
    ans <- ans[order(x$m), ]
  else  (NULL)
  
  ans<- prepare_output(ans,caption = paste0(caption, " (N = ", x$N, ")")  ,
                       note = note ,N=x$N)
  Output(ans,
         col_names = col_names,
         print_col = print_col)
  invisible(ans)
}


#' @rdname APA_ 
#' @export
#' 
#' 
APA_Likert <- function(...){
  res<- Likert(...)
  res[[ "table"]] <- APA2(res)
 invisible( res ) 
}
 


#' @name Likert
#' @rdname Likert
#' @title Likert-Skalen.
#' @description Analyse von Likertskalen
#' @param x Objekt data.frame oder Formula
#' @param ... Alle weiteren Argumente
#' @return a likert class list with the following elements:
#'   results: Ergebnisse der Haufigkeiten fuer zB Grafik
#'   names: Dataframe mit Namen
#'   freq: Data.frame mit nur den Haufigkeiten ohne NAs
#'   freq.na:Data.frame mit nur den Haufigkeiten mit NAs
#'   N: Stichprobengroesse als Zahl
#'   n: Anzahl der Gueltigen Werte je Item (Vektor)
#'   m: Mittelwerte (Vektor)
#'   sd: SD (Vektor)
#'   Mittelwert: M und SD Formatiert (Vektor)
#'   items: Daten
#'   grouping: data frame mit Gruppe (kann auch laenge Null haben)
#'   nlevels Anzahl an Levels
#'   levels: Labels fuer zb scale =list( at=nlevels, ...)
#' @export
#' @examples
#' library(stp25)
#' #' library(lattice)
#' graphics.off()
#'
#' Projekt("xhtml", "649 Poropatis Natascha", "28.03.2017")
#'
#' set.seed(1)
#' n<-100
#' lvs<-c("--","-","o","+","++")
#' DF2<- data.frame(
#'   Magazines=gl(length(lvs),1,n,lvs),
#'   Comic.books=gl(length(lvs),2,n,lvs),
#'   Fiction=gl(length(lvs),3,n,lvs),
#'   Newspapers=gl(length(lvs),5,n,lvs))
#' DF2$Comic.books[sample.int(n/2)]<- lvs[length(lvs)]
#' DF2$Newspapers[sample.int(n/2)]<- lvs[1]
#' DF2$Magazines[sample.int(n/2)]<- lvs[2]
#'
#' DF2<- transform(DF2, Geschlecht= cut( rnorm(n), 2, Cs(m, f)))
#' Res1 <- Likert(~., DF2 )
#' APA2(Res1)
#' APA2(Res2 <- Likert(.~ Geschlecht, DF2 ))
#'
#' require(HH)# ?likertplot
#'
#' windows(7,3)
#'  likertplot( Item   ~ .| Geschlecht , data=Res2$results,
#'              main = '',
#'              ylab = "",
#'              sub = "" ,
#'              xlab = "Prozent",
#'              col = brewer_pal_likert(5),
#'              rightAxis = FALSE,
#'              positive.order = TRUE,
#'              as.percent = TRUE,
#'              auto.key = list(space = "top", columns = 2) ,
#'              layout=c(2,2)
#'  )
#'
#'
#'
#'
#' # data<- Res2$names
#' # data$mean<- Res2$m
#' #  barchart( Item~ mean |Geschlecht, Mymean2, xlim=c(1,5))
#' # windows(3,6)
#' # dotplot(Item ~ mean, data,
#'  #       groups=Geschlecht, xlim=c(.085, 5.15),
#'  #       type=c("p", "l"))
#'
#'
#' APA2(Res1, ReferenceZero=3)
#' APA2(Res1, ReferenceZero="-")
#' APA2(Res2, ReferenceZero=3, na.exclude=TRUE, type="freq")
#'




Likert <- function(x, ...) {
  UseMethod("Likert")
}


#' @rdname Likert
#' @description Farpalette für Likert-plot ist eine Kopie von \code{brewer.pal.likert} kann aber auch schwarz-weiss
#' und hat blau-rot als default
#' @export
#' @param n,name,middle.color,min_gray,max_gray brewer_pal_likert Default 5 Stufen mit Grau in der Mitte
#' @examples
#'
#' brewer_pal_likert( ) #Default 5 Stufen mit Grau in der Mitte
#' #  HH::brewer.pal.likert(5, "RdBu", "gray80")
#'
brewer_pal_likert <- function(n=5,
                              name="RdBu",
                              middle.color="gray80",
                              min_gray=10,
                              max_gray=60
                              ){
if(tolower(name)=="gray"){
  min_gray<- 1- min_gray/100
  max_gray<- 1- max_gray/100
  # if(missing(middle.color)){
  if(n%%2==0){
    n <- (n)/2
    mycols<- gray(seq(min_gray,  max_gray, length.out =n))
    c(rev(mycols), mycols)
  }
  else{
    n<- (n-1)/2+1
    mycols<- gray(seq(min_gray,  max_gray, length.out =n))
    c(rev(mycols), mycols[-1])
  }

  #  }
}else HH::brewer.pal.likert(n, name, middle.color)
}

#' @rdname Likert
#' @param data Data.frame
#' @param grouping (optional) beim direkter ausfuehrung mit dataframes
#' @export
Likert.default <- function(data, grouping = NULL, ...) {
  if(!is.data.frame(data)) {
    warnings("Likert: Nur formula oder data.frames sind erlaubt")
    return(NULL)
  }
  data <- if (!is.null(grouping))
    cbind(grouping, data)
  Formula <- if (is.null(grouping))
    formula(paste("~", paste(names(Formula), collapse = "+")))
  else
    formula(paste(paste(names(Formula), collapse = "+"), "~ grouping"))

  Likert.formula(Formula,
                 data,
                 ...)
}


#' @rdname Likert
#' @param labels wenn die Labels anderst sein sollen
#' @export
Likert.formula<- function(x,
                   data,
                   labels=NULL,
                #   type="Freq",
                   ...){
  X<-Formula_Data(x, data)#

  grouping_vars<- X$xname
  # Erstes Item muss stimmen
  items <- clean_Likert_item(X$Y_data, labels)
  first_levels <- levels(items[,1])
  nlevels<-length(first_levels)

  N_all<- nrow(items)
  result <- NULL
  if(is.null(grouping_vars)){
      xans<-  stp25aggregate::Melt2(cbind(my_id_nr=1:nrow(items),items), id.vars=1) #    Melt2(items)
    xans$value <- factor(xans$value, levels=first_levels)# levels ordnen
    names(xans)[which(names(xans)=="variable")] <- "Item"

    xans_num <- xans
    xans_num$value <- as.numeric(xans_num$value)
    fm1 <- Item ~ value
    fm2 <- Item ~ .
    result <- list(
                     freq = reshape2::dcast(xans, fm1, length, drop=FALSE),
                     n    = reshape2::dcast(xans_num, fm2, function(x) length(na.omit(x)), drop = FALSE ),
                     m    = reshape2::dcast(xans_num, fm2, function(x) mean(x, na.rm=TRUE), drop = FALSE),
                     sd   = reshape2::dcast(xans_num, fm2, function(x) sd(x, na.rm=TRUE), drop = FALSE),
                     statistic = reshape2::dcast(xans_num, fm2, function(x) ffmean( mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)), drop = FALSE)
                )
  }else{
    xans <- stp25aggregate::Melt2(cbind( X$X_data, items), id.vars=1:ncol(X$X_data))

    xans$value <- factor(xans$value, levels=first_levels)
    names(xans)[which(names(xans)=="variable") ] <- "Item"
    xans_num <- xans

    xans_num$value <- as.numeric(xans_num$value)
    fm1<-paste(paste(grouping_vars, collapse="+"), "+ Item ~ value")
    fm2<-paste(paste(grouping_vars, collapse="+"), "+ Item ~ .")
    result <-list(
                   freq = reshape2::dcast(xans, fm1, length, drop=FALSE),
                   n    = reshape2::dcast(xans_num, fm2, function(x) length(na.omit(x)), drop = FALSE),
                   m    = reshape2::dcast(xans_num, fm2, function(x) mean(x, na.rm=TRUE ), drop = FALSE),
                   sd   = reshape2::dcast(xans_num, fm2, function(x) sd(x, na.rm=TRUE ), drop = FALSE),
                   statistic = reshape2::dcast(xans_num, fm2, function(x) ffmean( mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)), drop = FALSE)
               )
       }
  results.with.na <- result$freq # sicherung mit NA

  results.no.na  <- if( names(result$freq)[ncol(result$freq)] == "NA" )
                         result$freq[,- ncol( result$freq )]
                    else result$freq

  Names<-  1:(ncol(results.no.na)- nlevels)

 result <- list( results = results.no.na
                ,names = if(length(Names)==1) results.no.na[Names] else results.no.na[, Names]
                ,freq =  results.no.na[, -Names]
                ,freq.na = results.with.na[, -Names]
                ,N =   N_all ## summe aus n ist unlogisch sum(result$n[,ncol(result$n)])
                ,n =   result$n[,ncol(result$n)]
                ,m =   result$m[,ncol(result$m)]
                ,sd =  result$sd[,ncol(result$sd)]
                ,Mittelwert = result$statistic[,ncol(result$statistic)]
                ,items = items
                ,grouping = if(is.null(grouping_vars)) NULL else data[,grouping_vars]
                ,nlevels = nlevels
                ,levels=first_levels
                )

  class(result) <- 'likert'
  return(result)
}


#' @rdname Likert
#' @description Plot Methode
#' @export
print.likert<-function(x){
  cat("\nnames: ", paste(names(x), collapse=", "),"\n")
    cat("\nresults:  \n ")
    print( head(x$results))
    cat("\nlevels: ", paste(x$levels, collapse=", "),"\n")
}








#-- Fuer Likert  as_identical_factor
clean_Likert_item <- function(items,
                       labels = NULL#, output = "factor"
                       ) {
  #Test der Voraussetzung
if(all(sapply(items, is.factor))
   & (diff(range(sapply(items,nlevels)))==0))
{
   if(is.null(labels)) return(items)
   else return( dapply2(items,
                        function(x) {
                          x<- as.numeric(x)
                          factor(x, 1:length(labels), labels)
                          } ))
}else if(all(sapply(items, is.numeric))){
  if(is.null(labels)){
    labels <-
       unique(unlist(
         lapply(items, function(x)levels(factor(x)))
       ))

     return( dapply2(items, function(x){
                            factor(x,labels)}))
    }else return(
      dapply2(items, function(x){
                     factor(x, 1:length(labels), labels) }))


   }else{

    Text("Error gemischtes Skalenniveau!!")

     print(sapply(items,nlevels))
     print(head(items))

     items <- dapply2(items, as.numeric)
     labels <-
       unique(unlist(
         lapply(items, function(x)levels(factor(x)))
       ))

     return( dapply2(items, function(x){
       factor(x,labels)}))
   }




  # if (any(is.factor(items[,1]))) {
  #   first_item <- if (is.null(labels))
  #     levels(items[,1])
  #   else
  #     labels
  #
  #   items <- dapply2(items, function(x) {
  #     if (any(is.factor(x)))
  #       factor(x, levels = first_item)
  #     else
  #       factor(x, levels = 1:length(first_item), first_item)
  #   })
  # }else {
  #   first_item <-
  #     unique(unlist(
  #       lapply(items, function(x)levels(factor(x)))
  #       ))
  #
  #   items <- dapply2(items, function(x) {
  #     if (is.null(labels))
  #       factor(x, levels = first_item)
  #     else
  #       factor(x, levels = first_item, labels = labels)
  #   })
  # }
 # if (output == "factor")
    #return(items)
  #else
   # return(dapply2(items))
}
