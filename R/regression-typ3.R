#' Regressions Tabelle
#'
#' Erstellt APA-Style Tabellen aus R-Objekten (lm, glm,lmer, usw). 
#' @rdname APA_Table
#' @param ... Modelle
#' @param caption,note,names,custom.model.names,rgroup rgroup:ype="long" long die Zwischenueberschrift  Beschriftung
#' @param type type = c("default", "broom","long", "texreg", "stargazer", "sjPlot","anova")
#' @param digits Kommastellen auch als Matrix bei type =long
#' @param include.b,include.se,include.beta,include.odds Parameter, b=estimate, OR exp(b)
#' @param include.eta Eta bei Anova
#' @param include.ci,ci.level 95-CI
#' @param include.p,include.stars Sternchen und P-Werte
#' @param include.variance Variance
#' @param include.r,include.pseudo R-Quadrat
#' @param include.ftest,include.loglik noch nicht fertig
#' @param include.custom Liste mit eigenen Einträgen
#' @param include.aic,include.bic AIC BIC
#' @param include.anova    Explizite ANOVA Tabelle  include.anova=TRUE
#' @param single.row Agrument texreg = TRUE
#' 
#' @return invisible data.frame und Output mit html/knit oder Text.
#' @export
#'
#' @examples
#'
#'
#'  
#' ##library(stp25data)
#' ##Projekt("html")
#' summary(schools)
#' ##Head("Lineare-Regression lm")
#' lm1<-lm(score ~ grade + treatment + stdTest, schools)
#' #   type = c("default", "broom", "c", "c", "sjPlot","anova")
#' APA_Table(lm1, caption="default/broom")
#'
#' APA_Table(lm1, type="texreg", caption="texreg")
#' APA_Table(lm1, type="stargazer", caption="stargazer")
#' APA_Table(lm1, type="sjPlot", caption="sjPlot")
#' APA_Table(lm1, type="anova", caption="anova")
#'
#'
#' ##Head("Lineare-Mixed-Effect-Regression")
#' Text("
#'      # lme4
#'      lmer(y ~ 1 + (1 | subjects), data=data)
#'
#'      # nlme
#'      lme(y ~ 1, random = ~ 1 | subjects, data=data)
#'
#'      ")
#'
#' ##Head("nlme::lme", style=3)
#' lme1<-nlme::lme(score ~  grade +treatment  + stdTest , schools, random =~ 1|classroom)
#'
#'
#' APA_Table(lme1, caption="default/broom")
#' APA_Table(lme1, type="texreg", caption="texreg")
#' APA_Table(lme1, type="stargazer", caption="stargazer")
#' APA_Table(lme1, type="sjPlot", caption="sjPlot")
#' APA_Table(lme1, type="anova", caption="anova")
#'
#'
#'
#' ##Head("lmerTest::lmer", style=3)
#' lmer11<-lmerTest::lmer(score ~  grade +treatment  + stdTest + (1|classroom), schools)
#'
#' APA_Table(lmer11, caption="default/broom")
#' APA_Table(lmer11, type="texreg", caption="texreg")
#' APA_Table(lmer11, type="stargazer", caption="stargazer")
#' APA_Table(lmer11, type="sjPlot", caption="sjPlot")
#' APA_Table(lmer11, type="anova", caption="anova")
#'
#' ##Head("lme4::lmer", style=3)
#' lmer12<-lme4::lmer(score ~  grade +treatment  + stdTest + (1|classroom), schools)
#' APA_Table(lmer12, caption="default/broom")
#' APA_Table(lmer12, type="texreg", caption="texreg")
#' APA_Table(lmer12, type="stargazer", caption="stargazer")
#' APA_Table(lmer12, type="sjPlot", caption="sjPlot")
#' APA_Table(lmer12, type="anova", caption="anova")
#'
#' nms<-  c("lm", "lme", "lmerTest", "lme4")
#' # APA_Table(lm1, lmer11, lmer12, caption="default/broom")
#' APA_Table(lm1, lme1, lmer11, lmer12, type="texreg", caption="texreg", names=nms)
#' APA_Table(lm1, lme1,  lmer12, type="stargazer", caption="stargazer", names=nms[-3])
#' APA_Table(lm1, lme1, lmer11, lmer12, type="sjPlot", caption="sjPlot", names=nms)
#' APA_Table(lm1, lme1, lmer11, lmer12, type="anova", caption="anova", names=nms)
#'
#' R2(lm1)
#' R2(lme1)
#' R2(lmer11)
#' R2(lmer12)
#'
#' # texreg::extract(lm1)
#' # texreg::extract(lme1)
#' # texreg::extract(lmer11, include.pseudors = F, include.loglik = TRUE  )
#' # str(texreg::extract(lmer12))
#' #car::vif(fit)
#' #VIF(fit)
#'
#' ##Head("Block Lineare-Mixed-Effect-Regression")
#'
#' sleepstudy<- lme4::sleepstudy
#' fm1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' fm2 <- lmerTest::lmer(Reaction ~ Days + (Days || Subject), sleepstudy)
#' APA_Table(fm1, fm2)
#' APA_Table(fm1, fm2, type="tex")
#' 
#' 
#' fit1<-lm(score ~ grade + treatment, schools)
#' fit2<-lm(score ~ grade + treatment + stdTest, schools)
#' 
#' 
#' 
#' 
#' APA2(list(fit1, fit2))
#' APA_Table(fit1, fit2,
#'           type = "long",
#'           # rgroup = c("Parameter2", "Goodness of fit"),
#'           include.custom = list(Wald = c(
#'             APA(fit1, include.r = FALSE),
#'             APA(fit2, include.r = FALSE)
#'           ))
#'           
#' )
#' 
#'
#' #End()
APA_Table <- function(...,
                      caption = NULL,
                      note =  NULL,
                      output = stp25output::which_output(),
                      type = c("default","long", "long2",
                               "broom",
                               "texreg","stargazer","sjPlot",
                               "anova"),
                      names = NULL,
                      custom.model.names = NULL,
                      digits = NULL,
                      
                     
                      include.b = TRUE,
                      include.se = TRUE,
                      #noch nicht fertig
                      include.t= FALSE,
                      include.beta = FALSE,
                      
                      include.odds = FALSE,
                      include.ci = FALSE,
                      include.odds.ci=FALSE,
                      
                      #Fehler abfangeb
                      include.p = FALSE,
                      include.stars = if (include.p) FALSE  else TRUE,
                      
                      include.variance = TRUE,
                      include.r = TRUE,
                      include.pseudo = FALSE,
                      # noch nicht fertig
                      include.ftest = FALSE,
                      include.loglik = FALSE,
                      # noch nicht fertig
                      
                      include.custom = NULL,
                      
                      include.aic = TRUE,
                      include.bic = include.aic,
                      
                      include.anova = FALSE,
                      include.sumsq = TRUE ,
                      include.meansq = FALSE,
                      include.eta = FALSE,
                      
                      include.gof=TRUE,
                      include.param=TRUE,
                      
                      ci.level = .95,
                      single.row = TRUE,
                      #  col_names = NULL, #c("b", "SE", "p"),
                      rgroup = c("Parameter", "Goodness of fit"),
                      test.my.fun=FALSE,
                      
                      include.effects = c("ran_pars", "fixed"),
                      include.statistic=FALSE,
                      include.test=FALSE,
                      conf.int = TRUE,
                      conf.level = 0.95,
                      conf.method = "Wald",
                      fix_format = FALSE,
                      digits.param = 3,
                      digits.odds = 2,
                      digits.test = 2,
                      digits.beta = 2,
                      format="fg"
                      
                      
                      
                      )
# Parameter Goodness of fit)
{
  if(test.my.fun) cat("\n--------------\nAPA_Table")
  if(include.ftest) Text("Achtung include.ftest noch nicht fertig!")
 # if(include.beta) Text("Achtung include.beta noch nicht fertig!")
   
  result <- NULL
  type <-  match.arg(type, several.ok = TRUE)
  type<- type[1]
  # print(include.pseudo)
  custom_model_names <- function() {
    if (length(myfits) == 1) { 
       ""
    }else{
      if (is.null(names)) paste0("(", 1:length(myfits), ")")  else
        names     
      }
  }
  
  myfits <- list(...)
  
  if (is(myfits[[1]], "list")) {
    myfits <- myfits[[1]]  # hier kommt ein fit_with-Objekt
    if (is.null(names))
      names <- names(myfits)
  }
  
  
  if(test.my.fun){
    cat("\n Input: ")
    for (i in myfits){
      if (length(myfits) ==1) cat(class(i)[1])
        else  cat(class(i)[1], " | ")
    }
   
  }
  
  if (type == "default") {
    if(test.my.fun) cat("\n type = default")
    custom.model.names <- custom_model_names()
    #  print(custom.model.names)
    for (i in  seq_len(length(myfits)) ) {
      x <- type_default(
        myfits[[i]],
        caption = caption,
        note = note, output=output,
        custom.model.names = custom.model.names[i],
        include.b = include.b,
        include.se = include.se,
        include.t = include.t,
        include.beta = include.beta,
        include.eta = include.eta,
        include.odds = include.odds,
        include.ci = include.ci,
                #Fehler abfangeb
        include.p = include.p,
        include.stars = include.stars,
        
        include.variance = include.variance,
        include.r = include.r,
        include.pseudo = include.pseudo,
        # noch nicht fertig
        include.ftest = include.ftest,
        include.loglik = include.loglik,
        # noch nicht fertig
        include.custom = include.custom,
        include.aic = include.aic,
        include.bic = include.bic,
        ci.level = .95,
        test.my.fun = test.my.fun
      )
      result[[i]] <- x
    }
  }
  else{
    if ( type == "broom" ) {
      if(test.my.fun) cat("\n type = broom")
      APA_Table(
        ...,
        caption = caption,
        note = note, output=output,
        type = "default",
        names = names,
        digits = digits,
        single.row = single.row,
        include.stars = include.stars,
        include.p  = include.p ,
        include.variance = include.variance
      )
    }
    
    
    else if ( type == "long2" ) {
      if(test.my.fun) cat("\n type = long\n")
     # print(output)
      
      
      result <- APA2_list(
        myfits,
        caption = caption, note = note , output=output,
        custom.model.names = names ,
        include.param=include.param,  include.gof=include.gof, include.custom = include.custom,
        include.b = include.b, include.se = include.se, include.beta = include.beta,
        include.ci =  include.ci,
        include.odds = include.odds, include.odds.ci=include.odds.ci,
        include.statistic = include.statistic,
        include.p =  include.p,
        include.stars =  include.stars,
        include.df = FALSE,
        include.r = include.r, include.pseudo = include.pseudo,
        include.test = include.test ,
        include.loglik = include.loglik ,
        include.aic =  include.aic,include.bic = include.bic,
        ci.level =  ci.level,
        rgroup = rgroup,
        test.my.fun = test.my.fun,
        digits=digits, digits.param = digits.param,
        digits.odds = digits.odds,
        digits.test = digits.test,
        digits.beta = digits.beta,
        format=format
      )
      
  
      
      
    }
    
    else if ( type == "long" ) {
      if(test.my.fun) cat("\n type = long")
      result <- APA2.list(
        myfits,
        caption = caption ,
        note = note , output=output,
        digits =  digits,
        custom.model.names = names ,
        include.custom = include.custom ,
        include.b = include.b,
        include.ci =  include.ci,
        #Fehler abfangen mit alter schreibweise
        include.odds = include.odds,
        include.se = include.se,
        include.t = include.t,
        include.p =  include.p,
        include.stars =  include.stars,
        
        include.pseudo = include.pseudo,
        include.r = include.r,
        include.ftest = include.ftest ,
        
        include.loglik = include.loglik ,
        include.aic =  include.aic,
        include.bic = include.bic ,
        
        include.gof=include.gof,
        include.param=include.param,
        
        ci.level =  ci.level,
        rgroup = rgroup,
        test.my.fun = test.my.fun
      )
      
      # result <- regression_output(myfits,
      #                          caption = caption, note = note,
      #                          custom.model.names = names,
      #                          col_names= col_names,
      #                          digits = digits,
      #                        
      #                          rgroup = rgroup, # Parameter Goodness of fit
      #                          include.pseudo=include.pseudo,
      #                          include.ftest=include.ftest,
      #                          include.loglik=include.loglik,
      #                          include.ci=include.ci
      #                          )
      
      
    }
    else if ( type == "texreg" ) {
      if(test.my.fun) cat("\n type = texreg")
      result <-  type_texreg(
        myfits,
        caption = caption,
        note = note, output=output,
        digits = digits,
        single.row = single.row,
        include.stars = include.stars,
        include.p = include.p ,
        names = names,
        include.variance = include.variance
      )
    }
    else if ( type =="stargazer" ) {
      if(test.my.fun) cat("\n type = stargazer")
      # myfits<-list(...)
      #if(is.list(myfits[[1]])) myfits <- myfits[[1]]
      type_stargazer(myfits,
                     caption = caption,
                     digits = digits)
    }
    else if (type == "sjPlot" ) {
     # if(test.my.fun) cat("\n type = sjPlot")
      Text("sjPlot ist noch nicht implementiert")
    }
    else{
      #if(test.my.fun) cat("\n type = ist nicht definiert")
    #  cat("\n in else eventuell \n")
      if(type=="anova")  include.anova=TRUE ## Altlast 
    }
  }
  
  
  if (include.anova) {
    #car::ANOVA Type II  print(str(result))
    if (test.my.fun)
      cat("\n include.anova = TRUE")
    result[["anova"]] <- APA_Table_Anova(
      myfits,
      caption = caption,
      note = note,
      output = output,
      names = names,
      include.eta = include.eta,
      include.sumsq = include.sumsq ,
      include.meansq = include.meansq
    )
  }
  
  if(test.my.fun) cat("\nresult: ", class(result), "\nEnde APA_Table()")
  
  
  invisible(result)
}




#' @rdname APA_Table
#' @description \code{type="default"}  Formatierung als breite Tabelle
type_default <- function(x,
                         caption = NULL, output=NA,
                         note = NULL,
                         custom.model.names = NULL,
                         test.my.fun=FALSE,
                         ...) {
  if(test.my.fun) cat("\n  -> type_default()")
 # print(list(...))
  res <-  Ordnen(x, test.my.fun = test.my.fun, ... ) # ist das gleiche wie broom::tidy(x)
  if(test.my.fun) cat("\n    res:", class(res))
  
  if (is.null(caption))
    caption <- paste(attr(res, "caption"),
                     "Obs: ", attr(res, "N"))
  if (is.null(note))
    note <- attr(res, "note")

  #print(output)
  if( !is.logical(output) )
  Output(
    fix_format(res),
    caption = paste(custom.model.names, caption),
    note = note, 
    output=output )
  
  if(test.my.fun) cat("\n  <- type_default()")
  res
}


#' @rdname APA_Table
#' @description \code{type="texreg"} Long-Format Kopie der texreg Funktion
type_texreg <- function(list,
                        caption = "",
                        note = "", output=NA,
                        names = NULL,
                        digits = 2,
                        single.row = TRUE,
                        include.stars = TRUE,
                        include.p  = FALSE,
                        ci.force = FALSE,
                        include.variance = TRUE,                       
                        custom.model.names = if (is.null(names))
                          paste0("(", 1:length(list), ")")
                        else
                          names,
                        center = options()$stp25$apa.style$center,
                        stars = options()$stp25$apa.style$p$stars.value,
                        stars.symbols = options()$stp25$apa.style$p$stars.symbols) {
  # - p-value -------------------------------
  stars.string <- function (pval,
                            stars,
                            star.char,
                            star.prefix,
                            star.suffix,
                            symbol) {
    if (output != "text") {
      paste0('<br>',  "p=", ffpvalue(pval))
    }
    else{
      paste0(" p=", ffpvalue(pval))
    }
  }


  if (include.p) {
    old_stars.string <- texreg:::stars.string  # texreg::stars.string
    assignInNamespace("stars.string", stars.string, "texreg")
  }

  # eigendliche Funktion ----------------------------------------------------

  if (is.null(caption))
    caption <- ""
  if (is.null(note))
    note <- ""
  res <- NULL
  cptn <- paste(Tab(), caption)

  if (output != "text") {
    # "html" oder "markdown"
    reg_result <- texreg::htmlreg(
      list,
      return.string = TRUE,
      single.row = single.row,
      caption.above = TRUE,
      caption = cptn,
      inline.css = TRUE,
      doctype = FALSE,
      html.tag = FALSE,
      head.tag = FALSE,
      body.tag = FALSE,

      stars = stars,
      custom.model.names = custom.model.names,
      ci.force = ci.force,
      center = center,
      digits = digits,
      include.variance = include.variance
    )

    # Text("In texregTable")
    # class(reg_result) <- "texregTable"

    Output.htmlTable(reg_result, output=output)

  }
  else {
    res <-
      texreg::screenreg(
        list,
        single.row = single.row,
        caption.above = caption.above,
        digits = digits,
        stars = stars,
        title = paste(Tab(), "Regression", caption),
        include.variance = include.variance
      )
    # print(res)
  }
  # reset p-value -----------------------------
  if (include.p) {
    assignInNamespace("stars.string", old_stars.string, "texreg")
  }
  texreg:::get.data(list) # ruckgabe der Ergebnisse als liste
}


#' @rdname APA_Table
#' @description \code{type="starganzer"} Stargazer kann nicht mit LmerTest arbeiten.
type_stargazer <- function(list, caption, digits) {
  # output <- options()$prompt[1] == "HTML> "
  # res <- NULL
  if (any(sapply(list, class) %in% "merModLmerTest")) {
    Text("Stargazer kan nicht mit LmerTest arbeiten. Alternative ist lme4::lmer(...)")
    return(NULL)
  }
  # default <-default_caption_note(myfits[[i]])
  if (is.null(caption))
    caption <- "" #default$caption
  caption <- Tab(caption)
#  Output_info$table <<-  c(Output_info$table, caption)
  # if(is.null(note)) note <- "" #default$note
  Output.htmlTable(
    stargazer::stargazer(
      list,
      title = caption,
      decimal.mark = options()$stp4$apa.style$OutDec,
      digits = digits,
      digit.separator = "",
      type = "html"
    ), output=output
  )

  NULL
}

#' @rdname APA_Table
#' @description \code{type="long"} Lang-Format Tabelle
type_long <- function(...)
  APA2.list(...)
