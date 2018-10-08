#' @name Counter
#' @title Calculate count of values in a list
#' @description Handy to calculate count of values in a sorted order
#' @param data should be a vector or list of input values
#' @param sort a logical value, to sort the result or not
#' @param decreasing a logical value, the order of sorting to be followed
#'
#' @return list of values in a sorted order
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
