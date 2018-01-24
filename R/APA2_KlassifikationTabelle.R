#' @rdname Klassifikation
#' @title Klassifikation
#' @name Klassifikation
#' @description
#'  Richtige und falsche Klassifikationen
#'  Bei  2x2 Tabellen der Kappa Test
#'
#' Sensitivity = A/(A+C)
#'
#' Specificity = D/(B+D)
#'
#' Prevalence = (A+C)/(A+B+C+D)
#'
#' PPV = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
#'
#' NPV = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))
#'
#' Detection Rate = A/(A+B+C+D)
#'
#' Detection Prevalence = (A+B)/(A+B+C+D)
#'
#' Balanced Accuracy = (sensitivity+specificity)/2
#'
#' Precision = A/(A+B)
#'
#' Recall = A/(A+C)
#'
#' F1 = (1+beta^2)*precision*recall/((beta^2 * precision)+recall)
#'
#'
#' Klassifikation fuer Binominal-GLM
#' und zeigt die ...
#' @param x glm oder xtab Objekt
#' @param thresh Klassifikation auf Basis der Vorhersage Schwelle bei P=0.5
#' @param caption an Output
#' @param ... weitere Objekte nicht benutzt
#' @return A data.frame Objekt.
#' @export
#' @examples
#'
#' ##https://www.hranalytics101.com/how-to-assess-model-accuracy-the-basics/
#'
#'
#' require(stp25vers)
#' Kappa2(x<-xtabs(~gruppe+lai, hkarz))
#' head(hkarz)
#'
#' fit1<- glm(gruppe~lai, hkarz, family = binomial)
#' #thkarz <- as.data.frame(xtabs(~gruppe+lai, hkarz))
#' #fit2<- glm(Freq ~ gruppe*lai, thkarz, family = poisson())
#'
#' tab <- Klassifikation(x)$statistic[c(1,7,8),]
#' x2 <- Klassifikation(fit1)
#' tab$fit <- x2$statistic[c(1,7,8), 2]
#' tab
#'
#' require(pROC)
#' roc_curve   <- roc(x2$response, x2$predictor)
#' windows(8,8)
#' plot(roc_curve, print.thres = "best",
#'      print.auc=TRUE)
#' abline(v = 1, lty = 2)
#' abline(h = 1, lty = 2)
#' #text(.90, .97, labels = "Ideal Model")
#' #points(1,1, pch = "O", cex = 0.5)

Klassifikation <- function(x, ...) {
  UseMethod("Klassifikation")
}


#' @rdname Klassifikation
#' @export
Klassifikation2<- function(x, ..., caption="",
                           include.xtab=TRUE,
                           include.statistics=TRUE) {
  res <-  Klassifikation(x, ...)
  if(include.xtab)
  Output(res$xtab,
         caption=caption)

  if(include.statistics & !is.null(res$statistic))
    Output(res$statistic,
           caption=caption)

  invisible(res)

}

#' @rdname Klassifikation
#' @description Klassifikation.glm
#' @export
#' @examples
#' #fit1<- glm(gruppe~lai, hkarz, family = binomial)
#' #thkarz <- as.data.frame(xtabs(~gruppe+lai, hkarz))
#' #fit2<- glm(Freq ~ gruppe*lai, thkarz, family = poisson())
#'
Klassifikation.glm<- function(x,
                          thresh = 0.5,
                          caption="Klassifikation",
                          ...){
  response <- all.vars(formula(formula(x)))[1]
  data <- x$model
  predictor <- fitted(x) # vorhergesagte Wahrscheinlichkeit
  data$Response <- data[, response]
  #response  predictor

  mylevels <- if(is.factor(data$response))
                  levels(data$response)
              else 0:1
  data$Predictor <- cut(predictor,
                          breaks=c(-Inf, thresh, Inf), labels=mylevels )

  # Kontingenztafel: tatsaechliche vs. vorhergesagte Kategorie
  cTab <- stats::xtabs( ~ Response + Predictor, data = data)

  if (length(cTab)==4){
   res <- Klassifikation.xtabs(cTab)
    res$response=data$Response
    res$predictor=predictor
    res

  }
  else list(xtab=x,
            statistic=NULL,
            response=Response,
            predictor=predictor)

  #tab2x2 <- length(cTab)==4

  # cTab <-addmargins(cTab, 2)
  # cnames <- colnames(cTab)
  #
  #
  # # colnames(cTab)[length(cnames)]<- "Summe"
  # if(tab2x2){
  #   # prepare_output(cTab, caption=caption,
  #   #                note= paste("Accuracy =",
  #   #                            round((cTab[1,1] +
  #   #                                     cTab[2,2]) /(cTab[1,3] +
  #   #                                                    cTab[2,3]), 2)))
  # } else{prepare_output(cTab, caption=caption, note= note) note<- ""}


}

#' @rdname Klassifikation
#' @description xtabs-Objekt
#' @export
#' @examples
#' hkarz$LAI<- factor(hkarz$lai, 0:1, c("pos", "neg"))
#' APA2(xtabs(~gruppe+LAI, hkarz), test=TRUE, type="fischer")
Klassifikation.xtabs<- function(x,
                                lvs = c("+", "-"),
                                digits = 2,
                               # caption="Klassifikation",
                               # note="",
                                ...){
 if(!length(x)==4) return("Nur mit 2x2 Tabellen moeglich!")

 Positive_Class <-
      paste(attr(x, "dimnames")[[1]][1],
            attr(x, "dimnames")[[2]][1], sep ="/")
 attr(x, "dimnames")[[1]] <- lvs
 attr(x, "dimnames")[[2]] <- lvs
 xtab<-x
 x <- caret::confusionMatrix(x)
 #rndr_(2.5689)
 out <- as.character(c(
   rndr_(x$overall["Accuracy"], digits),
      ffCI(x$overall[c("AccuracyLower", "AccuracyUpper")]),
   rndr_(x$overall["AccuracyNull"], digits),
      rndr_P(x$overall["AccuracyPValue"]),
   rndr_(x$overall["Kappa"], digits),
      rndr_P(x$overall["McnemarPValue"]),
   rndr_(x$byClass, digits)
   ,Positive_Class))




 list(xtab=xtab,
      statistic=
        data.frame(Statistic =
                             c("Accuracy",
                               "95% CI",
                               "No Information Rate",
                               "P-Value [Acc > NIR]",
                               "Kappa",
                               "Mcnemar's Test P-Value",
                               "Sensitivity",
                               "Specificity",
                               "Pos Pred Value" ,
                               "Neg Pred Value",
                               "Precision",
                               "Recall",
                               "F1",
                               "Prevalence",
                               "Detection Rate",
                               "Detection Prevalence" ,
                               "Balanced Accuracy",
                               "Positive Class"
                             )
                             , Value = out,
                 stringsAsFactors = FALSE),
      stat=x,
      response=NULL,
      predictor=NULL
      )
}
