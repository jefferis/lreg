#' An S4 class to Logistic Regression.
#'
#' @export
#'  
#' @slot coefficients Coefficients
#' @slot var Variance Covariance Matrix
#' @slot deviance Deviance
#' @slot predictors Predictors of the model
#' @slot iterations No of iterations for convergence

setClass(
   Class = "lreg5"
 , slots =  list(
        coefficients="numeric"
      , var="matrix"
      , deviance="numeric"
      , predictors="character"
      , iterations="numeric"
     )
  )


lreg5 <-
  function(X, y, predictors=colnames(X), max.iter=10,
        tol=1E-6, constant=TRUE, ...) {
    if (!is.numeric(X) || !is.matrix(X))
        stop("X must be a numeric matrix")
    if (!is.numeric(y) || !all(y == 0 | y == 1))
        stop("y must contain only 0s and 1s")
    if (nrow(X) != length(y))
        stop("X and y contain different numbers of observations")
    if (constant) {
        X <- cbind(1, X)
        colnames(X)[1] <- "Constant"
    }
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        var.b <- solve(crossprod(X, p * (1 - p) * X))
        b <- b + var.b %*% crossprod(X, y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    dev <- -2*sum(y*log(p) + (1 - y)*log(1 - p))
    result <- new("lreg5", coefficients=as.vector(b), var=var.b,
        deviance=dev, predictors=predictors, iterations=it)
    result
}

setMethod("show", signature(object="lreg5"),
    definition=function(object) {
            coef <- object@coefficients
            names(coef) <- object@predictors
            print(coef)
        }
    )


setMethod("summary", signature(object="lreg5"),
    definition=function(object, ...) {
            b <- object@coefficients
            se <- sqrt(diag(object@var))
            z <- b/se
            table <- cbind(b, se, z, 2*(1-pnorm(abs(z))))
            colnames(table) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
            rownames(table) <- object@predictors
            printCoefmat(table)
            cat("\nDeviance =", object@deviance,"\n")
        }
    )
