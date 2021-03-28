#' @title stepback.Rd
#' @param y dependent variable for selection
#' @param d select the independent variables
#' @description When choosing 'd', and d is a dataframe, then the entire dataframe can be used (look at example), although when this is the case you have to make sure the dependent variable is de-selected
#' @examples data(weight)
#' @examples stepback(weight$wgt, weight[, -1], alpha = 0.2)
#' @export
stepback<-
  function (y = y, d = d, alpha = 0.05)
  {
    d <- do.call(cbind, unname(Map(function(x, z) {
      tmp <- as.data.frame(model.matrix(~x - 1))
      if (ncol(tmp) == 1 & class(tmp[[1]]) == "numeric") {
        names(tmp) <- paste0(names(tmp), z)
      }
      tmp
    }, d, names(d))))
    names(d) <- sub("^x", "", names(d))

    lm1 <- lm(y ~ ., data = d)
    result <- summary(lm1)
    max <- max(result$coefficients[, 4][-1], na.rm = TRUE)
    while (max > alpha) {
      varout <- names(result$coefficients[, 4])[result$coefficients[,
                                                                    4] == max]
      pos <- position(matrix = d, vari = varout)
      d <- d[, -pos]
      if (length(result$coefficients[, 4][-1]) == 2) {
        min <- min(result$coefficients[, 4][-1], na.rm = TRUE)
        lastname <- names(result$coefficients[, 4])[result$coefficients[,
                                                                        4] == min]
      }
      if (is.null(dim(d))) {
        d <- as.data.frame(d)
        colnames(d) <- lastname
      }
      lm1 <- lm(y ~ ., data = d)
      result <- summary(lm1)
      max <- max(result$coefficients[, 4][-1], na.rm = TRUE)
      if (length(result$coefficients[, 4][-1]) == 1) {
        max <- result$coefficients[, 4][-1]
        if (max > alpha) {
          max = 0
          lm1 <- lm(y ~ 1)
        }
      }
    }
    return(lm1)
  }

position<-
  function (matrix, vari)
  {
    a <- colnames(matrix)
    b <- a == vari
    c <- c(1:length(a))
    d <- c[b]
    return(d)
  }


