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
#'   \item{\code{$fit()}}{creates a memory of encodings but doesn't return anything}
#'   \item{\code{$transform()}}{based on encodings learned in \code{fit} method is applies the transformation}
#'   \item{\code{$fit_transform()}}{encodes the data and keep a memory of encodings simultaneously}
#'   \item{\code{$inverse_transform()}}{encodes the data and keep a memory of encodings simultaneously}
#' }
#' @section Arguments:
#' \describe{
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
    fit_model = FALSE,

    # nothing to initialise in this class
    fit = function(data_col){

        self$input_data <- private$check_data(data_col)
        self$encodings <- private$encoder(self$input_data)
        self$decodings <- private$decoder(self$encodings)
        self$fit_model <- TRUE

    },

    fit_transform = function(data_col){

        data_col <- private$check_data(data_col)
        self$fit(data_col)

        vals <- private$mapper(data_col,
                               self$encodings,
                               convert_type = NULL,
                               output_type = numeric(1))
        return (vals)

    },

    transform = function(data_col){

        if(!(isTRUE(self$fit_model)))
            stop("Please run fit before using transform.")

        data_col <- private$check_data(data_col)

        # all values in the new vector should be in encodings
        if(!(all(data_col %in% names(self$encodings)))){
            message(strwrap("There are new values in this vector which weren't
                 available during fit. Replacing those values with 'NA'"))

            # replace new values with 'NA'
            val_index <- which(!(data_col %in% names(self$encodings)))
            data_col[val_index] <- "NA"

        }

        vals <- private$mapper(data_col,
                               self$encodings,
                               convert_type = NULL,
                               output_type = numeric(1))
        return (vals)
    },

    inverse_transform = function(coded_col){

        #check if all values exist in decode
        if(!(all(coded_col %in% self$encodings))){
            stop(strwrap("There are new values in this data which weren't
                    available during fit. Please fix the issue."))
        }

        # write here
        vals <- private$mapper(coded_col,
                               self$decodings,
                               convert_type = as.character,
                               output_type= character(1))
        return (vals)

    }

),
    private = list(

    encoder = function(data_col){

        # this is to handle if the input value is factor, it should be character
        data_col <- as.character(data_col)

        all_values <- unique(data_col)
        maps <- list()

        # because the encoding should start with zero (i - 1)
        for(i in seq(all_values))  maps[all_values[i]] <- i-1
        return (maps)

    },

    decoder = function(en_output){

        # reverse the encoded list - make values as names and vice versa
        f <- as.list(names(en_output))
        names(f) <- en_output
        return(f)

    },

    mapper = function(input_vec, coded_list, convert_type ,output_type){

        if(is.null(convert_type)){
            # this is to avoid using convert_type argument for
            # transform, fit_transform
            return (vapply(input_vec,
                           function(x) coded_list[[x]],
                           FUN.VALUE = output_type,
                           USE.NAMES = F))
        }

        return(vapply(input_vec,
                      function(x) coded_list[[convert_type(x)]],
                      FUN.VALUE = output_type,
                      USE.NAMES = F))
    },


    check_data = function(data_col){

        # fix data issues here
        if(any(is.na(data_col))){
            message("The data contains NA values. Imputing NA with 'NA' ")
            data_col <- replace(data_col, which(is.na(data_col)), "NA")
        }

        if(any((data_col == ""))){
            message("The data contains blank values. Imputing them with 'NA' ")
            data_col <- replace(data_col, which(data_col == ""), "NA")
        }

        if(any((data_col == " "))){
            message("The data contains blank values. Imputing them with 'NA' ")
            data_col <- replace(data_col, which(data_col == " "), "NA")
        }

        return(data_col)

    }
    )
)



