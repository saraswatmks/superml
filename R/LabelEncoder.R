#' Label Encoder
#' @description Encodes and decodes categorical variables into integer values and vice versa.
#' This is a commonly performed task in data preparation during model training, because all machine learning models require
#' the data to be encoded into numerical format. It takes a vector of character or factor values and encodes them into numeric.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' lbl = LabelEncoder$new()
#' lbl$fit(x)
#' lbl$fit_transform(x)
#' lbl$transform(x)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialise the instance of the encoder}
#'   \item{\code{$fit(vector)}}{creates a memory of encodings but doesn't return anything}
#'   \item{\code{$transform(vector)}}{based on encodings learned in \code{fit} method is applies the transformation}
#'   \item{\code{$fit_transform(vector)}}{encodes the data and keep a memory of encodings simultaneously}
#'   \item{\code{$inverse_transform(vector)}}{encodes the data and keep a memory of encodings simultaneously}
#' }
#' @section Arguments:
#' \describe{
#'  \item{LabelEncoder}{A \code{LabelEncoder} object}
#'  \item{data}{a vector or list containing the character / factor values}
#' }
#' @export
#' @examples
#' data_ex <- data.frame(Score = c(10,20,30,4), Name=c('Ao','Bo','Bo','Co'))
#' lbl <- LabelEncoder$new()
#' data_ex$Name <- lbl$fit_transform(data_ex$Name)
#' decode_names <- lbl$inverse_transform(data_ex$Name)

LabelEncoder <- R6Class("LabelEncoder", public = list(

    input_data = NA,
    encodings = NA,
    decodings = NA,

    initialize = function(data){
        if(!(missing(data))) self$input_data <- data
    },


    fit = function(data_col){

        self$input_data <- data_col
        self$encodings <- private$encoder(self$input_data)
        self$decodings <- private$decoder(self$encodings)

    },

    fit_transform = function(data_col){
        self$fit(data_col)
        return (self$encodings)

    },

    transform = function(data_col){
        return (self$encodings)
    },

    inverse_transform = function(coded_col){
        return (self$decodings)
    }

),
    private = list(

    mapper = NA,

    encoder = function(data_col){

        # this is to handle if the input value is factor, it should be character
        data_col <- as.character(data_col)

        all_values <- unique(data_col)
        maps <- list()

        for(i in seq(all_values))  maps[all_values[i]] <- i
        private$mapper <- maps

        mapper <- vapply(data_col, function(x) maps[[x]],
                         FUN.VALUE = numeric(1), USE.NAMES = F)
        return (mapper)

    },

    decoder = function(coded_col){
        return (vapply(coded_col,
                       function(x)
                           names(which(private$mapper == x)),
                       FUN.VALUE = character(1), USE.NAMES = F))
    }

    )
)

