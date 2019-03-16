#' @name Counter
#' @title Calculate count of values in a list or vector
#' @description Handy function to calculate count of values given in a list or vector
#' @param data should be a vector or list of input values
#' @param sort a logical value, to sort the result or not
#' @param decreasing a logical value, the order of sorting to be followed
#'
#' @return count of values in a list
#' @export
#'
#' @examples
#' d <- list(c('i','am','bad'),c('you','are','also','bad'))
#' counts <- Counter(d, sort=TRUE, decreasing=TRUE)
Counter <- function(data, sort=TRUE, decreasing=FALSE){

    if(is.null(data)) stop("Please provide input data values")
    if(is.data.frame(data) | is.matrix(data))
        stop("Please provide a vector of values
             instead of a data frame or matrix")

    if(is.list(data)) data <- unlist(data)

    op <- table(data) # vals
    n_op <- names(op)

    dimnames(op) <- NULL
    output <- list()
    for(i in seq(n_op)){
        output[[n_op[i]]] <- op[i]
    }

    if(sort) return (output[order(unlist(output), decreasing = decreasing)])
    else return (output)

}


#' @name testdata
#' @title Internal function
#' @description Used to check the input data format
#' @param X should be a data frame or data.table
#' @param y should be a string specifying the dependent variable
#'
#' @return null
#' @keywords internal
#' @export
testdata <- function(X, y, model=NA){

    # X should be a matrix
    if (!(inherits(X, c("data.table", "data.frame"))))
        stop("Your data format should be a data.table or data.frame.")

    if(!(y %in% names(X)))
        stop(sprintf("%s not available in training data", y))

    # check in case target variable contains float values or NA values
    if(any(is.na(X[[y]])))
        stop("The target variable contains NA values.")

    if(model %in% c('lmtrainer')){
        if(any(vapply(X, class, FUN.VALUE = character(1))
               %in% c("factor", "character")))
            stop(strwrap("There are factor or character values in the data set.
                         Please convert to numeric."))
    }

}

#' @name check_package
#' @title Internal function
#' @description Used to check the package is installed
#' @param X should be a string containing package name
#'
#' @return null
#' @keywords internal
#' @export
check_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(paste0("Need Package " , pkg,  "needed for this function to work. Please install it."),
             call. = FALSE)
    }
}

