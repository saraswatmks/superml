#' TfIDF(Term Frequency Inverse Document Frequency) Vectorizer
#'
#' Creates a tf-idf matrix
#'
#' @details
#' Given a list of text, it creates a sparse matrix consisting of tf-idf score for tokens from the text.
#'
#' @export
TfIdfVectorizer <- R6Class("TfIdfVectorizer",
                               inherit = CountVectorizer,
                               public = list(
    #' @field sentences a list containing sentences
    sentences = NA,
    #' @field max_df When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.
    max_df = 1,
    #' @field min_df When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.
    min_df = 1,
    #' @field max_features use top features sorted by count to be used in bag of words matrix.
    max_features = 1,
    #' @field smooth_idf logical, to prevent zero division, adds one to document frequencies, as if an extra document was seen containing every term in the collection exactly once
    smooth_idf = TRUE,


    #' @details
    #' Create a new `TfIdfVectorizer` object.
    #'
    #' @param min_df numeric, When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.
    #' @param max_df nuemric, When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.
    #' @param max_features integer, use top features sorted by count to be used in bag of words matrix.
    #' @param smooth_idf logical, to prevent zero division, adds one to document frequencies, as if an extra document was seen containing every term in the collection exactly once
    #'
    #' @return A `TfIdfVectorizer` object.
    #'
    #' @examples
    #' TfIdfVectorizer$new(smooth_idf = TRUE, min_df = 0.3)

    initialize = function(min_df, max_df, max_features, smooth_idf){
        super$initialize(max_df, min_df, max_features)
        if (!(missing(smooth_idf))) self$smooth_idf <- smooth_idf

    },

    #' @details
    #' Fits the TfIdfVectorizer model on sentences
    #'
    #' @param sentences a list of text sentences
    #' @return NULL
    #'
    #' @examples
    #' sents = c('i am alone in dark.','mother_mary a lot',
    #'           'alone in the dark?', 'many mothers in the lot....')
    #' tf = TfIdfVectorizer$new(smooth_idf = TRUE, min_df = 0.3)
    #' tf$fit(sents)

    fit = function(sentences){
        self$sentences <- sentences
        super$fit(self$sentences)
        return(invisible(self))
    },


    #' @details
    #' Fits the TfIdfVectorizer model and returns a sparse matrix of count of tokens
    #'
    #' @param sentences a list of text sentences
    #' @return a sparse matrix containing tf-idf score for tokens in each given sentence
    #'
    #' @examples
    #' \donttest{
    #' sents <- c('i am alone in dark.','mother_mary a lot',
    #'          'alone in the dark?', 'many mothers in the lot....')
    #' tf <- TfIdfVectorizer$new(smooth_idf = TRUE, min_df = 0.1)
    #' tf_matrix <- tf$fit_transform(sents)
    #' }


    fit_transform = function(sentences){
        self$fit(sentences)
        return(private$gettfmatrix(self$model, smooth_idf = self$smooth_idf))
    },

    #' @details
    #' Returns a matrix of tf-idf score of tokens
    #'
    #' @param sentences a list of new text sentences
    #' @return a sparse matrix containing tf-idf score for tokens in each given sentence
    #'
    #' @examples
    #' \donttest{
    #' sents = c('i am alone in dark.','mother_mary a lot',
    #'           'alone in the dark?', 'many mothers in the lot....')
    #' new_sents <- c("dark at night",'mothers day')
    #' tf = TfIdfVectorizer$new(min_df=0.1)
    #' tf$fit(sents)
    #' tf_matrix <- tf$transform(new_sents)
    #' }

    transform = function(sentences){

        if (is.null(self$sentences))
            stop("Please run fit before applying transformation.")
        temp <- super$transform(sentences)
        return(private$gettfmatrix(temp, smooth_idf = self$smooth_idf))

    }),

    private = list(
        gettfmatrix = function(countmatrix, smooth_idf = TRUE){

            tf  <- countmatrix

            if (isTRUE(smooth_idf)) idf <- log(1 + nrow(tf) /
                                                  (1 + colSums(tf))) + 1
            else idf <- log(nrow(tf) / colSums(tf))

            tfidf <- tf

            for (word in names(idf)) {
                tfidf[, word] <- tf[, word] * idf[word]
            }

            return(tfidf)
        }
    )
)
