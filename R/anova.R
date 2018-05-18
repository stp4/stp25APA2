#' @rdname APA2
#' @export
APA2.anova <- function(x, 
                       caption=gsub("\\n", "", paste(attr(x, "heading"), collapse=", ") ),
                       note=paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
                       output = which_output(),
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
                     output = which_output(),
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
                        output = which_output(),
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
                         output = which_output(),
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
#' @description \code{type="anova"} Anova Funktionen
#' @export
APA_Table_Anova <- function(myfits,
                            caption, note,
                            names,
                            include.eta)
{
  result <- NULL
  for ( i in seq_len(length(myfits)) )  {
    if (length(model_info(myfits[[i]])$x) != 0) {
      res <- Ordnen(car::Anova(myfits[[i]]),
                    include.eta=include.eta
                    )  
      
      if (is.null(caption))
        caption <- attr(res, "caption")##paste(, "Obs: ", attr(res,"N"))
      if (is.null(note))
        note <- attr(res, "note")
      
      # if (include.eta &
      #     is(myfits[[i]], "lm")  & !is(myfits[[i]], "glm")) {
      #   k <- ncol(res)
      #   res <-
      #     cbind(res[, -k], etaSquared2(myfits[[i]], 2, FALSE), res[k])
      # }
      
      if(!is.null(names)) caption <-  names[i]
      fix_format(res)  %>%
        Output(caption = caption, note = note)
      
    } else
      res <- "Null-Model"
    result[[i]] <- res
  }
  result
}