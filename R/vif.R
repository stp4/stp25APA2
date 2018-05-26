#' variance inflation factor
#' 
#'  VIF  values over 5 are troubling, should probably investigate anything over 2.5.
#'  
#' @author Wolfgang Peter
#' @param x fit-Objekt
#' @param caption,note Ueberschrift an Output
#' @param ... weitere Einstellungen
#' @export
#' @name VIF
#' @examples
#' 
#' \dontrun{
#'  fit<-lm(prestige ~ income + education, data=Duncan)
#'  car::vif(fit)
#'  VIF(fit)
#' }
#' 
VIF <- function(x, ...) {
  UseMethod("VIF")
}

#' @rdname VIF
#' @description VIF.default lm Methode aus car::vif
#' @export
VIF.default <- function(fit, ...) {
  
  
 # terms <- labels(terms(fit))
 # n.terms <- length(terms)
 # if(n.terms < 2) 
 # else  
    
    car::vif(fit)
}


#' @rdname APA_
#' @description  APA_vif:  variance inflation factor, aka VIF
#' @export
APA_vif <- function(...,
                    caption = "VIF",
                    notes = "") {
  fits <- list(...)
  res <- list()
  i <- 1
  for (x in fits) {
    res_vif <-  VIF(x, ...) 
  
    res_vif <- prepare_output(
      data.frame(Source =  names(res_vif),  
                 VIF= Format2(as.vector(res_vif), 2) ),
      caption = paste(caption, model_info(x)$y),
      notes = notes
    )
    
    res[[i]] <- res_vif
    i <- i + 1
    Output(res_vif)
  }
  invisible(res)
}


#' @rdname VIF
#' @export
VIF2 <- function(...,
                 caption = "VIF",
                 notes = "")  {
  APA_vif(..., caption = caption, notes = notes)
}


#' @rdname VIF
#' @description  VIF.merModLmerTest Quelle https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/
#' @export
VIF.merModLmerTest <- function(fit, ...) {
  #print("VIF.merModLmerTest")
  round(vif.mer(fit), 2)
}
# VIF.lmerTest <- function(fit, ...)  {
#   print("VIF.lmerTest")
#   vif.mer(fit)
#   }
#--https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.



vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- stats::vcov(fit)
  nam <- names(lme4::fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns),-(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v) ^ 0.5
  v <- diag(solve(v / (d %o% d)))
  names(v) <- nam
  v
}

kappa.mer <- function (fit,
                       scale = TRUE,
                       center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
  X <- fit@pp$X
  nam <- names(lme4::fixef(fit))
  
  ## exclude intercepts
  nrp <- sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X <- X[,-(1:nrp), drop = FALSE]
    nam <- nam[-(1:nrp)]
  }
  
  if (add.intercept) {
    X <- cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
  } else {
    kappa(scale(X, scale = scale, center = scale), exact = exact)
  }
}

colldiag.mer <- function (fit,
                          scale = TRUE,
                          center = FALSE,
                          add.intercept = TRUE) {
  ## adapted from perturb::colldiag, method in Belsley, Kuh, and
  ## Welsch (1980).  look for a high condition index (> 30) with
  ## more than one high variance propotion.  see ?colldiag for more
  ## tips.
  result <- NULL
  if (center)
    add.intercept <- FALSE
  if (is.matrix(fit) || is.data.frame(fit)) {
    X <- as.matrix(fit)
    nms <- colnames(fit)
  }
  else if (class(fit) == "mer") {
    nms <- names(lme4::fixef(fit))
    X <- fit@X
    if (any(grepl("(Intercept)", nms))) {
      add.intercept <- FALSE
    }
  }
  X <- X[!is.na(apply(X, 1, all)),]
  
  if (add.intercept) {
    X <- cbind(1, X)
    colnames(X)[1] <- "(Intercept)"
  }
  X <- scale(X, scale = scale, center = center)
  
  svdX <- svd(X)
  svdX$d
  condindx <- max(svdX$d) / svdX$d
  dim(condindx) <- c(length(condindx), 1)
  
  Phi = svdX$v %*% diag(1 / svdX$d)
  Phi <- t(Phi ^ 2)
  pi <- prop.table(Phi, 2)
  colnames(condindx) <- "cond.index"
  if (!is.null(nms)) {
    rownames(condindx) <- nms
    colnames(pi) <- nms
    rownames(pi) <- nms
  } else {
    rownames(condindx) <- 1:length(condindx)
    colnames(pi) <- 1:ncol(pi)
    rownames(pi) <- 1:nrow(pi)
  }
  
  result <- data.frame(cbind(condindx, pi))
  zapsmall(result)
}

maxcorr.mer <- function (fit,
                         exclude.intercept = TRUE) {
  so <- summary(fit)
  corF <- so@vcov@factors$correlation
  nam <- names(lme4::fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0 & exclude.intercept) {
    corF <- corF[-(1:ns),-(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  corF[!lower.tri(corF)] <- 0
  maxCor <- max(corF)
  minCor <- min(corF)
  if (abs(maxCor) > abs(minCor)) {
    zapsmall(maxCor)
  } else {
    zapsmall(minCor)
  }
}


