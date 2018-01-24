#' @rdname AD_Index
#' @title  Hypothese Jeder Judge beurteilt das Item gleich
#' @name  AD_index
#' @description Estimating Interrater Agreement With the
#' Average Deviation Index: A User’s Guide MICHAEL J. BURKE WILLIAM P. DUNLAP
#' Basiert auf 642 Raphaela Willitsch
#' wo Estimating Interrater Agreement With the Average Deviation Index: A User’s Guide MICHAEL J. BURKE
#' WILLIAM P. DUNLAP
#' die Methode beschreibt.
#'
#' Estimating within-group interrater reliability with and without response bias.
#' By James, Lawrence R.; Demaree, Robert G.; Wolf, Gerrit
#' Journal of Applied Psychology, Vol 69(1), Feb 1984, 85-98.
#' Abstract
#' Presents methods for assessing agreement among the judgments made by a single group of judges on a single variable in regard to a single target. For example, the group of judges could be editorial consultants, members of an assessment center, or members of a team. The single target could be a manuscript, a lower level manager, or a team. The variable on which the target is judged could be overall publishability in the case of the manuscript, managerial potential for the lower level manager, or a team cooperativeness for the team. The methods presented are based on new procedures for estimating interrater reliability. For such situations, these procedures furnish more accurate and interpretable estimates of agreement than estimates provided by procedures commonly used to estimate agreement, consistency, or interrater reliability. The proposed methods include processes for controlling for the spurious influences of response biases (e.g., positive leniency and social desirability) on estimates of interrater reliability. (49 ref) (PsycINFO Database Record (c) 2016 APA, all rights reserved)
#'
#' Hypothese Jeder Judge beurteilt das Item gleich.
#'
#' rwg: oeherer Wertbessere Uebereinstimmung
#' @param x Objekt Formula oder Vektor
#' @param data Objekt
#' @param A  A=5 Anzahl stufen der  Likertskala
#' @param caption  caption ="Interrater Agreement",
#' @param note Note
#' @param type Lang oder Breit type = "judge" oder "item
#' @param ... weitere Objekte
#'  na.rm normalerweise bna.rm==TRUE soll nicht geaendert werden
#' @return Ein data.frame
#' @examples
#' # data
#' #  Item     x1 x2 x3 x4 x5 x6
#' # Judge 1   3  4  3  2  4  3
#' # Judge 2   2  3  3  2  4  4
#' # Judge 3   4  3  4  3  4  3
#' # Judge 4   3  3  2  2  2  4
#' # Judge 5   3  2  2  4  2  3
#' # Judge 6   4  2  4  3  2  3
#' # Judge 7   2  3  2  3  3  4
#' # Judge 8   3  4  4  3  3  2
#' # Judge 9   4  2  3  4  3  2
#' # Judge 10  2  4  3  4  3  2
#' @export
AD_Index <- function(x, ...) {
  UseMethod("AD_Index")
}

#' @rdname AD_Index
#' @export
AD_Index2 <- function(x, data, ... ,
                      A=5, #A Anzahl stufen der  Likertskala
                      type= "judge", #lang oder breit
                      caption="Interrater Agreement",
                      note="AD index (based on deviations from the item medians)") {

   X <- Formula_Data(x, data)

   if(is.factor(X$Y_data[,1])) {
     #prüfen wie viele stufen
     A <- nlevels(X$Y_data[,1])
     X$Y_data <- dapply2(X$Y_data)
   } else{
     warning("Fuer Interrater Agreement muss
             A  die Anzahl an Stufen der Likertskala angegeben werden!
             Default ist 5 Stufen")
   }

   if(type=="judge") X$Y_data <- data.frame(t(X$Y_data))
  N <- nrow(X$Y_data)
  AD.Index <- sapply(X$Y_data, AD_Index_helper, ...)
  Mean <- sapply(X$Y_data, mean_na_rm, ...)
  SD <- sapply(X$Y_data, var_na_rm, ...)

  J <- length(X$yname)
  sx <- mean(SD, na.rm=TRUE)

  # seite 91 triangular Distribution
  ET <- ifelse( A %% 2 == 0, (A^2+2*A-2)/24, ((A-1)*(A+3))/24 )
  ET_rwg1 <- 1-sx/ET
  ET_rwgj <- (J*ET_rwg1)/(J*ET_rwg1 +  sx/ET)

  # Expected Error uniform Distribution
  EU <- (A^2-1)/12
  EU_rwg1 <- 1-sx/EU
  EU_rwgj <- (J*EU_rwg1)/(J*EU_rwg1 + sx/EU)

  Overall <- data.frame(Item="Overall",
                       Mean=NA, SD=NA,
                       AD.Index= mean(AD.Index) )
  ##Ausgabe
  rbind(
      data.frame(
            Item=GetLabelOrName(X$Y_data),
            Mean, SD, AD.Index,
            stringsAsFactors=FALSE),
      Overall) %>%
  fix_format() %>%
  Output("Estimating Interrater Agreement With the Average Deviation Index",
         note=paste("Number of rater =",N ))
#Expected Error
  data.frame(
           Expected.Error= c("uniform Distribution","triangular Distribution"),
           rwg1=c(EU_rwg1, ET_rwg1),
           rwgj=c(EU_rwgj, ET_rwgj), stringsAsFactors=FALSE) %>%
  fix_format() %>%
  Output(caption="Estimating Within-group interrater reliability",
         note=paste("Number of rater =",N ))

   invisible(AD.Index)
}

#' @rdname AD_Index
#' @export
AD_Index.data.frame <- function(data, ...) {
  AD_Index.formula(~., data, ...)
}


#' @rdname AD_Index
#' @export
AD_Index.formula <- function(x,
                             data,
                             type="judge", ...) {
  X<-  Formula_Data(x,  data)
  if( is.factor(X$Y_data[,1]) ) {
    #prüfen wie viele stufen
    A<- nlevels( X$Y_data[,1] )
    X$Y_data<- dapply2(X$Y_data)
  }  else{
    warning("Fuer Interrater Agreement muss
            A  die Anzahl an Stufen der Likertskala angegeben werden!
            Default ist 5 Stufen")
  }

  if(type=="judge") X$Y_data <- data.frame(t(X$Y_data))

 data.frame(Item=GetLabelOrName(X$Y_data),
            AD.Index=sapply(X$Y_data,
                            AD_Index_helper, ...))

}

#' @rdname AD_Index
#' @export
AD_Index.default <- function(x, ...) {
 data.frame(Item = deparse(substitute(x)),
            AD.Index=AD_Index_helper(x, ...))
}

#- Helper ------------------------------------
AD_Index_helper<- function(x,
                           na.rm=TRUE){
  #- Interrater Agreement
  if(is.factor(x)) x<- as.numeric(x)
  if(na.rm) x<- na.omit(x)
  sum(abs(x-mean(x)))/length(x)
}

mean_na_rm<- function(x, na.rm=TRUE) mean(x, na.rm=na.rm)
# im Artikel sird sd verwendet
var_na_rm<- function(x, na.rm=TRUE) var(x, na.rm=na.rm)


