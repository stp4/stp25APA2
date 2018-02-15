APA2_multiresponse<- function(Formula,
                              data,
                              caption="",

                          #    type, ## wegen ... nicht benutzt
                              test=FALSE,note="",

                              order=FALSE, decreasing = TRUE,
                              sig_test="fischer.test",
                              na.action=na.pass,
                              use.level=1,


                              ...  ){
#cat("\n in APA2_multiresponse\n")
  myTest <- function () {
    ix <- X$xname[1]
    if(nlevels(X$X_data[, ix]) > 2) sig_test <- "chisq.test"
    res <- NULL
    for (y in X$yname) {
      xtabx <- xtabs(formula(paste("~", y, "+", ix)),
                     cbind(X$X_data,X$Y_data))
      if(sig_test == "fischer.test"){
        xtabx <- fisher.test(xtabx)
        res <- rbind(res,
                     c(y, fischer.test = rndr_fischer(xtabx$estimate,
                                                   xtabx$p.value)))
      }else{
        xtabx <- stats::chisq.test(xtabx)
        res <- rbind(res,
                     c(y, chisq.test = rndr_X(xtabx$statistic,
                                           xtabx$parameter,NULL,
                                           xtabx$p.value)))
   }}
  data.frame(res)
  }

  #-- Funktion --------
  if(is.character(test) & test != "fischer.test") {
    sig_test<-"chisq.test"
    test <- TRUE
  }

  X <- Formula_Data(Formula, data, na.action=na.action)
  if( is_all_dichotom(X$Y_data)){
     if(is.factor(X$Y_data[,1])){
       firstLevel<- levels(X$Y_data[,1])[use.level]
       note <- paste0(note, "\nBenutze den ", use.level, " Level, <", firstLevel, "> als Zaehler. \n")
       X$Y_data<- dapply2(X$Y_data, function(x) ifelse(x==firstLevel, 1 ,0))
     }

     }
  else {
    return(paste("Falsches Datenformat: ",
                 paste(sapply(X$Y_data, class), collapse=", ")))
    }

  #- Formel(~a+s+d) -----
  if(is.null(X$xname)){
       ANS<- Recast2(Formula,
                   data,
                   fun=Prozent,
                   X = X)
  }else{
    #- Formel( a+s+d~gruppe) -----
    if(length(X$xname)!=1){
      Text("Achtung: Mehere y-Variablen werden nicht unterstuetzt")
      X$xname<- X$xname[1]
      }
    formula<-paste0("variable~", X$xname)
    ANS <-  Recast2(Formula, data, fun = Prozent,
                            X = X, #einfach an Recast2 weil sonst die fun mehrmals ausgefuert wird
                            id.var = X$xname,
                            formula = formula  ,
                            labels = TRUE, drop = FALSE, margins = X$xname)

    ncols <- ncol(ANS)
    ANS <- ANS[, c(1, ncols, 2:(ncols-1))]
    names(ANS)[-1] <- paste(names(ANS)[-1],
                            c(paste0("(n=", nrow(X$X_data[1]), ")"),
                              paste0("(n=", table(X$X_data[1], useNA = "ifany"), ")" )))

    if(test) ANS <- cbind(ANS, myTest()[2])

  }

  if(order) {
    my0rder<- Recast2(~., X$Y_data, fun=mean2)
    my0rder<- order(my0rder$value, decreasing =decreasing)
    ANS<- ANS[my0rder,]
  }

  ANS <- prepare_output(ANS, caption, note, nrow(X$Y_data))
  ANS
}
