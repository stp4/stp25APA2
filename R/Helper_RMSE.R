#' @rdname RMSE
#' @title RMSE
#' @name RMSE
#' @description ## The RMSE is the square root of the variance of the residuals.
#' Compute the root mean squared error
#' (see \code{\link{sigma}})
#' @param x fit-Objekt lm glm
#' @param ... weitere Objekte nicht benutzt
#' @return Data.frame mit Namen
#'
#' @examples
#' #library(haven)
#'
#'
#' @export
RMSE<- function(x, ...){
  UseMethod("RMSE")

}

#' @rdname RMSE
#' @export
RMSE.default <- function(x,...)
{
  data.frame(sigma=sigma(x),
    RMSE = sqrt(mean(x$residuals^2)))
}

#' @rdname RMSE
#' @export
RMSE.mlm <- function(x,...)
{
 broom::fix_data_frame(
   data.frame(sigma=sigma(fit1),
             RMSE= apply(fit1$residuals, 2 ,
                         FUN=function(rr) sqrt(mean(rr^2)))))
}




