#' @rdname model_info
#' @title model_info
#' @name model_info
#' @description Extrahiert die Variablen namen die Labels und die Daten
#' @param x Fit Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return Liste mit c("class",  "family", "y", "x", "labels", "N")
#' @export
#' @examples
#' model_info(mpg ~ cyl)
#' model_info(glm(vs ~ mpg, mtcars, family = binomial()))
#' model_info(lm(mpg ~ drat + wt + qsec, mtcars))
#' model_info(wilcox.test(mpg ~ vs, mtcars))
#' model_info(t.test(mpg ~ vs, mtcars))
model_info <- function(x, ...) {
  UseMethod("model_info")
}

#' @rdname model_info
#' @export
model_info.default<- function(x, ...){
 list(
    class =class(x) ,
    family = NULL ,
    y = NULL ,
    x = NULL ,
    labels = NULL,
    N=NULL
  )
}

#' @rdname model_info
#' @export

model_info.formula<- function(x, ...){
  fm <- Formula_Names(x)
  list(
    class = class(x) ,
    family = NULL ,
    y = fm$yname,
    x = fm$xname,
    labels = NULL,
    N=NULL
  )
}

#' @rdname model_info
#' @export
model_info.eff<-function(x,...){
  list(
    #formula=fm,
    class = class(x)[1],
    family =  x$family[1:2]  ,
    y = x$response,
    x = names(x$variables) ,
    # data = x$model,
    labels = GetLabelOrName(x$data),
    N=nrow(x$data)
  )


}

#' @rdname model_info
#' @export
model_info.htest<- function(x, ...){
  #model_info(Hmisc::spearman2(mpg ~ factor(vs), mtcars))
  if(names(x$statistic)=="t"){
    list(
      class ="t.test" ,
      family = x$methode ,
      y = strsplit(x$data.name, " by ")[[1]][1] ,
      x = strsplit(x$data.name, " by ")[[1]][2] ,
      labels = NULL,
      N=NULL
    )}else { list(
      class ="wilcox.test" ,
      family = x$methode ,
      y = strsplit(x$data.name, " by ")[[1]][1] ,
      x = strsplit(x$data.name, " by ")[[1]][2] ,
      labels = NULL,
      N=NULL
    )}

}

#' @rdname model_info
#' @export
model_info.lm  <- function(x, ...)  model_info_glm(x, ...)

#' @rdname model_info
#' @export
model_info.glm  <- function(x, ...)  model_info_glm(x, ...)

#' @rdname model_info
#' @export
model_info.anova  <- function(x, ...) {
  fm <- formula(x)
  y <- gsub("Response: ", "", attr(x, "heading")[2])
  x <- rownames(x)
  x <-  x[-length(x)]
  if (length(grep("\\:", x)) > 0)
    x <- x[-grep("\\:", x)]
  labels <- c(y, x)
  names(labels) <- labels
    list(
      class = "anova",
      family = c("anova", ""),
      y = y,
      x = x,
      labels =  labels,
      N=NA
    )


  }

# helper lm glm
model_info_glm <- function(x, ...) {
  fm <- formula(x)
list(
      class = class(x)[1],
      family = unlist(family(x)[1:2]) ,
      y = all.vars(formula(fm))[1],
      x = all.vars(formula(fm))[-1],
      labels = GetLabelOrName(x$model),
      N=nrow(x$model))
}

#' @rdname model_info
#' @export
model_info.merModLmerTest  <- function(x, ...)  model_info_lmer(x, ...)
#' @rdname model_info
#' @export
model_info.lmerTest  <- function(x, ...)  model_info_lmer(x, ...)
#' @rdname model_info
#' @export
model_info.glmerMod  <- function(x, ...)  model_info_lmer(x, ...)

#--   "merModLmerTest"  "lmerTest" "glmerMod"
model_info_lmer <- function(x, ...) {
  fm <- formula(x)

      list(
        class = class(x)[1],
        family = unlist(family(x)[1:2]) ,
        y = all.vars(formula(fm))[1],
        x = all.vars(formula(fm))[-1],
        # data = x@frame,
        labels = GetLabelOrName(x@frame),
        N=nrow(x@frame)
      )

}

#' @rdname model_info
#' @export
model_info.ScalarIndependenceTest<- function(x, ...){
  #model_info(coin::wilcox_test(mpg ~ factor(vs), mtcars))

  list(
    class ="coin" ,
    family = NULL ,
    y = NULL ,
    x = NULL ,
    labels = NULL,
    N=NULL
  )
}

#' @rdname model_info
#' @export
model_info.biVar<- function(x, ...){
  #model_info(Hmisc::spearman2(mpg ~ factor(vs), mtcars))

  list(
    class ="biVar" ,
    family = NULL ,
    y = NULL ,
    x = NULL ,
    labels = NULL,
    N=NULL
  )
}



#' @rdname model_info
#' @export
model_info.survfit <- function(x, ...) model_info_surv(x,...)


#' @rdname model_info
#' @export
model_info.coxph <- function(x, ...) model_info_surv(x,...)


model_info_surv<- function(x, ...) {
  fm <- formula(x)
  list(
    class = class(x)[1],
    family = NULL,
    y = all.vars(formula(fm))[1],
    x = all.vars(formula(fm))[-1],
    labels = NULL,
    N=x$n)
}

#' @rdname model_info
#' @export
model_info.survdiff <- function(x, ...) {
  fm <- formula(x)
  list(
    class = class(x)[1],
    family = NULL,
    y = all.vars(formula(fm))[1],
    x = all.vars(formula(fm))[-1],
    labels = NULL,
    N=sum(x$n))
}


#' @rdname model_info
#' @export
model_info.ICC <- function(x, ...) {
### # geht irgendwie noch nicht
  list(
    class =  class(x)[1] ,
    family = NULL,
    y = NULL,
    x = NULL,
    labels = NULL,
    N=paste0("obs=", x$n.obs, ", judge=", x$n.judge ))
}
