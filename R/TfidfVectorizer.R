#' TfIDF(Term Frequency Inverse Document Frequency) Vectorizer
#'
#' @description  Provides an easy way to create tf-idf matrix of features in R. It consists of fit, transform
#'               methods (similar to sklearn) to generate tf-idf features.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' tf_object = TfIdfVectorizer$new(max_df=1, min_df=1, max_features=1, smooth_idf=TRUE)
#' tf_object$fit(sentences)
#' tf_matrix = tf_object$transform(sentences)
#' tf_matrix = tf_object$fit_transform(sentences) ## alternate
#' }
#' @section Methods:
#' \describe{
#'     \item{\code{$new()}}{Initialise the instance of the vectorizer}
#'     \item{\code{$fit()}}{creates a memory of count vectorizers but doesn't return anything}
#'     \item{\code{$transform()}}{based on encodings learned in \code{fit} method, returns the tf-idf matrix }
#'     \item{\code{$fit_transform()}}{returns tf-idf matrix}
#' }
#' @section Arguments:
#' \describe{
#'     \item{sentences}{input vector or list consisting of text}
#'     \item{min_df}{consider tokens which occur in atleast this % documents, value lies between 0 and 1}
#'     \item{max_df}{consider tokens which occur in maximum this % documents, value lies between 0 and 1}
#'     \item{max_features}{use top features sorted by count to be used in creating tf-idf features}
#'     \item{smooth_idf}{to prevent zero division, adds one to document frequencies, as if an extra document was seen containing every term in the collection exactly once}
#'  }
#' @export
#' @examples
#' df <- data.frame(sents = c('i am alone in dark.',
#'                            'mother_mary a lot',
#'                            'alone in the dark?',
#'                            'many mothers in the lot....'))
#' tf <- TfIdfVectorizer$new(smooth_idf = TRUE, min_df = 0.3)
#' tf_features <- tf$fit_transform(df$sents)

TfIdfVectorizer <- R6Class("TfIdfVectorizer",
                               inherit = CountVectorizer,
                               public = list(

    sentences = NA,
    max_df = 1,
    min_df = 1,
    max_features = 1,
    smooth_idf = TRUE,

    initialize = function(max_df, min_df, max_features, smooth_idf){
        super$initialize(max_df, min_df, max_features)
        if(!(missing(smooth_idf))) self$smooth_idf <- smooth_idf

    },

    fit = function(sentences){
        self$sentences <- sentences
        super$fit(self$sentences)
        return(invisible(self))
    },

    fit_transform = function(sentences){
        self$fit(sentences)
        temp <- super$transform(sentences)
        return (private$gettfmatrix(temp, smooth_idf = self$smooth_idf))
    },

    transform = function(sentences){

        if(is.null(self$sentences))
            stop("Please run fit before applying transformation.")
        temp <- super$transform(sentences)
        return (private$gettfmatrix(temp, smooth_idf = self$smooth_idf))

    }),

    private = list(
        gettfmatrix = function(countmatrix, smooth_idf = TRUE){

            tf  <- countmatrix

            if(isTRUE(smooth_idf)) idf <- log(1 + nrow(tf) /
                                                  (1 + colSums(tf))) + 1
            else idf <- log(nrow(tf) / colSums(tf))

            tfidf <- tf

            for(word in names(idf)){
                tfidf[, word] <- tf[, word] * idf[word]
            }

            return (tfidf)
        }
    )
)
