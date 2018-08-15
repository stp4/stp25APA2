#' @rdname APA2
#' @description MANOVA: APA2(x, , test="Wilks") test : "Wilks", "Pillai"
#' @export
#' @examples 
#' 
#' #- manova ---------------------------------------------
#'  
#' ## Set orthogonal contrasts.
#' op <- options(contrasts = c("contr.helmert", "contr.poly"))
#' ## Fake a 2nd response variable
#' npk2 <- within(npk, foo <- rnorm(24))
#' npk2 <- within(npk2, foo2 <- rnorm(24))
#' npk2.aov <- manova(cbind(yield, foo, foo2) ~ block + N * P * K, npk2)
#' #x<-summary(npk2.aov)
#' APA2(npk2.aov) #wilks
#' APA2(npk2.aov, "Pillai")
#' 
#' #npk2.aovE <- manova(cbind(yield, foo) ~  N*P*K + Error(block), npk2)
#' #APA2(npk2.aovE)
#' 
#' 
#'  DF<-GetData(
#' "C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/manova.sav"
#' )
#' 
#' #information from
#' DF$GROUP<- factor(DF$GROUP, 1:3, Cs("website", "nurse ", "video tape" ))
#' #DF %>% Tabelle2(USEFUL, DIFFICULTY, IMPORTANCE, by=~GROUP )
#' 
#' z<- as.matrix(DF[,-1])
#' fit1<- manova(z ~ DF$GROUP)
#' APA2(fit1)
#' summary(fit1)$Eigenvalues
#' 
#' # SPSS
#' # Multivariate Tests of Significance (S = 2, M = 0, N = 13 )
#' #
#' # Test Name       Value  Approx. F Hypoth. DF   Error DF  Sig. of F
#' #
#' # Pillais          .48    3.02       6.00      58.00       .012
#' # Hotellings       .90    4.03       6.00      54.00       .002
#' # Wilks            .53    3.53       6.00      56.00       .005
#' # Roys             .47
#' # Note.. F statistic for WILKS' Lambda is exact.
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' # Univariate F-tests with (2,30) D. F.
#' #
#' # Variable   Hypoth. SS   Error SS Hypoth. MS   Error MS          F  Sig. of F
#' #
#' # USEFUL       52.92424  293.96544   26.46212    9.79885    2.70053       .083
#' # DIFFICUL      3.97515  126.28728    1.98758    4.20958     .47216       .628
#' # IMPORTAN     81.82969  426.37090   40.91485   14.21236    2.87882       .072
#' #
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' #   Eigenvalues and Canonical Correlations
#' #
#' # Root No.    Eigenvalue        Pct.   Cum. Pct.  Canon Cor.
#' #
#' # 1          .892      99.416      99.416        .687
#' # 2          .005        .584     100.000        .072
#' #
#' # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' 
#' 
APA2.manova <- function(x, test="Wilks", caption="MANOVA", note="", output=c("anova", "manova")) {
  aov_result<-NULL
  res<-summary.aov(x)
  for (i in names(res)){
    rs<- res[[i]]
    rs<- cbind(
      Source = gsub(".*\\$", "", rownames(rs)),
      rs[, c("F value", "Df", "Pr(>F)")])
    if(is.null(aov_result))  aov_result <- rs
    else aov_result<- rbind(aov_result, rs)
  }
  aov_result <- prepare_output(
        fix_format(aov_result),
        caption= caption,
        note=note)
  if("anova"  %in%  output)
    Output(aov_result,
           rgroup = names(res),
           n.rgroup = rep(nrow(res[[1]]), (length(res) - 1)))
  
  maov_result <- summary(x, test = test)
  maov_result <- fix_to_data_frame(maov_result$stats)
  maov_result$Source <- gsub(".*\\$", "", maov_result$Source)
  maov_result <- prepare_output(fix_format(maov_result),
                                caption = paste(test, "Test"))
  if ("manova" %in% output)
    Output(maov_result)
  invisible(list(manova = aov_result, test = maov_result)
  )
}






 

#' @rdname APA2 
#' @param fit_predict lda: MASS predict.lda
#' @param newdata lda:model.frame
#' @description LDA (linear discriminants analysis) Erweiterung der MANOVA
#' 
#' @export
#' @examples
#' 
#' #-- LDA -------------------------
#' 
#' library(MASS)
#' #fit2 <- lda(GROUP ~ ., data=DF )
#' #APA2(fit2)
#' #plot(fit2)
#' 
#' 
#' 
#' DF2<- GetData("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/discrim.sav")
#' #--https://stats.idre.ucla.edu/spss/dae/discriminant-function-analysis/
#' DF2$Job <- factor(DF2$JOB, 1:3, Cs("customer service", "mechanic","dispatcher"))
#' DF2$Job2 <- factor(DF2$JOB, c(2,3,1), Cs( "mechanic","dispatcher","customer service"))
#' 
#' #APA2(.~JOB ,DF2)
#' #DF2 %>%  APA_Correlation(OUTDOOR,SOCIAL,CONSERVATIVE )
#' 
#' fit2 <- lda(Job ~ OUTDOOR+SOCIAL+CONSERVATIVE, data=DF2)
#' fit3 <- lda(Job2 ~ OUTDOOR+SOCIAL+CONSERVATIVE, data=DF2)
#' 
#' APA2(fit2)
#' APA2(fit3)
APA2.lda <- function(x,
                     fit_predict = MASS:::predict.lda(x),
                     newdata = model.frame(x), ...){
 #  MASS:::predict.lda
  means <- prepare_output(
              fix_to_data_frame(t(x$means)), ...)

  Output(fix_format(means), "Means")

  scaling <- fix_to_data_frame(x$scaling)
  scaling <- prepare_output(scaling[order(-scaling[,2]), ], ...)

     Output(fix_format(scaling), "Coefficients of linear discriminants")
  svd <- x$svd
  names(svd) <- dimnames(x$scaling)[[2L]]
  if (length(svd) > 1L) {
    svd <- prepare_output(
           data.frame(t(data.frame(
              svd^2/sum(svd^2)))), ...)
   Output(fix_format(svd),"Proportion of trace")
  }

  cTab <- table(newdata[,1], fit_predict$class,
                dnn=c(names(newdata)[1], "Predict"))

     Output(addmargins(cTab),
            "Kontingenztafel tatsaechlicher und vorhergesagter Kategorien")


  cTotal<-c(diag(prop.table(cTab, 1)),
       Total=sum(diag(prop.table(cTab))))*100

  #
  cTotal<- fix_to_data_frame(cTotal)
       Output(fix_format(cTotal),  "prozentuale Uebereinstimmung")

invisible(list(mean=means,
               scal=scaling, svd=svd,
               cTab=cTab, cTotal=cTotal))
}



