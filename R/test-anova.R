#' @rdname APA2
#' @export
APA2.anova <- function(x, 
                       caption=gsub("\\n", "", paste(attr(x, "heading"), collapse=", ") ),
                       note=paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
                       output = stp25output::which_output(),
                       col_names = NULL,
                       include.eta=FALSE,
                       ...
) {
  APA2.lm(x, caption, note, 
          output =  output ,
          col_names = col_names,
          ...)
}
#' @rdname APA2
#' @description anova: APA2.aov(x, include.eta = TRUE) 
#' @export
#' @examples 
#' 
#' #- ANOVA ---------
#' op <- options(contrasts = c("contr.helmert", "contr.poly"))
#' npk.aov <- aov(yield ~ block + N*P*K, npk) 
#' #summary(npk.aov)
#' #coefficients(npk.aov)
#' 
#' APA2(npk.aov, include.eta = FALSE)
#' 
APA2.aov <- function(x, 
                     caption=NULL,
                     note=paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
                     output = stp25output::which_output(),
                     col_names = NULL,
                     ...
                     ) {
  APA2.lm(x, caption, note, output =  output ,
          col_names = col_names,
          ...)
}

 

#' @rdname APA2
#' @export
APA2.summary.aov <- function(x,
                        caption = "ANOVA",
                        note = "",
                        output = stp25output::which_output(),
                        col_names = NULL,
                        ...) {
  res <- fix_format(broom::tidy(x[[1]]))
  res <- prepare_output(cbind(Source = rownames(res), res),
                        caption = caption,
                        note = note)
  Output(res,
         output =  output ,
         col_names = col_names)
  
  invisible(res)
}

 

#' @rdname APA2
#' @export
#' @examples 
#' 
#' #- One way repeated Measures ---------------------
#' 
#' datafilename="http://personality-project.org/r/datasets/R.appendix3.data"
#' data.ex3=read.table(datafilename,header=T)   #read the data into a table
#' #data.ex3                                      #show the data
#' 
#' aov.ex3 = aov(Recall~Valence+Error(Subject/Valence),data.ex3)
#' 
#' APA2(aov.ex3)
#' 
APA2.aovlist <- function(x,
                         output = stp25output::which_output(),
                         col_names = NULL,
                         ...) {
  x <- summary(x)
  
  x1 <-
    fix_data_frame2(x[[1]][[1]])
  x1 <- cbind(Source = rownames(x1), x1)
  #APA_Table(npk.aov, type="anova")
  Output(x1 , caption = names(x[1]), output = output)
  
  x2 <- fix_data_frame2(x[[2]][[1]])
  x2 <- cbind(Source = rownames(x2), x2)
  Output(x2 , caption = names(x[2]), output = output)
  
  invisible(x)
}




 
#' @rdname APA_Table
#' @description \code{type="anova"} Anova (car::Anova) Funktionen aus APA_Table(..., include.anova=TRUE)
#' @export
APA_Table_Anova <- function(myfits,
                            caption = NULL,
                            note = NULL,
                            output = stp25output::which_output(),
                            names = NULL,
                            include.eta = TRUE,
                            include.sumsq = TRUE ,
                            include.meansq = FALSE,
                            ...)
{
  #cat("\nAPA_Table_Anova()\n")
  result <- list()
  print(names(myfits))
  for (i in seq_len(length(myfits)))  {
    my_input <- model_info(myfits[[i]])
    
    if (length(my_input$x) == 0) {
      result[[i]] <- "Null-Model"
    }
    else{
      res <- Ordnen(
        car::Anova(myfits[[i]]),
        include.eta = include.eta,
        include.sumsq = include.sumsq,
        include.meansq = include.meansq
      )
      if (!is.null(caption)) {
        if (!is.null(names))
          caption <-  names[i]
        else
          caption <-  attr(res, "caption")
      }
      
      if (is.null(note))
        note <- attr(res, "note")
      
      res <-  stp25output::fix_format(res)
      
      stp25output::Output(res,
             caption = caption,
             note = note,
             output = output)
      result[[i]] <- res
    }
  } # -end for
  result
}
