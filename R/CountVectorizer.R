#' Count Vectorizer
#' @description Creates CountVectorizer Model.
#' Given a list of text, it generates a bag of words model and returns a data frame consisting of BOW features.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' bst = CountVectorizer$new(min_df=1, max_df=1, max_features=1)
#' bst$fit(sentences)
#' bst$fit_transform(sentences)
#' bst$transform(sentences)
#' }
#'
#' @section Methods:
#' \describe{
#'     \item{\code{$new()}}{Initialise the instance of the vectorizer}
#'     \item{\code{$fit()}}{creates a memory of bag of words}
#'     \item{\code{$transform()}}{based on encodings learned in \code{fit} method, return a bag of words matrix }
#'     \item{\code{$fit_transform()}}{simultaneouly fits and transform words and returns bag of words of matrix}
#' }
#' @section Arguments:
#' \describe{
#'     \item{sentences}{input vector or list consisting of text}
#'     \item{min_df}{consider tokens which occur in atleast this % documents, value lies between 0 and 1}
#'     \item{max_df}{consider tokens which occur in maximum this % documents, value lies between 0 and 1}
#'     \item{max_features}{use top features sorted by count to be used in bag of words matrix}
#' }
#' @export
#' @examples
#' df <- data.frame(sents = c('i am alone in dark.','mother_mary a lot',
#'                            'alone in the dark?',
#'                            'many mothers in the lot....'))
#'
#' # fits and transforms on the entire data in one go
#' bw <- CountVectorizer$new(min_df = 0.3)
#' tf_features <- bw$fit_transform(df$sents)
#'
#' # fit on entire data and do transformation in train and test
#' bw <- CountVectorizer$new()
#' bw$fit(df$sents)
#' tf_features <- bw$transform(df$sents)

CountVectorizer <- R6Class("CountVectorizer", public = list(

    sentences = NA,
    tokens_encoder = NA,
    max_df = 1,
    min_df = 1,
    max_features = 1,
    all_tokens = NA, ## tokens to use to create matrix
    top_tokens = NA,
    doc_tokens = NA,

    initialize = function(max_df, min_df, max_features){
        if(!(missing(max_df))) self$max_df <- max_df
        if(!(missing(min_df))) self$min_df <- min_df
        if(!(missing(max_features))) self$max_features <- max_features
        private$check_args(self$max_df)
        private$check_args(self$min_df)
        private$check_args(self$max_features)

    },

    fit = function(sentences){

        self$sentences <- sentences
        self$all_tokens <- private$tokens_limiter(self$sentences,
                                                  max_df = self$max_df,
                                                  min_df = self$min_df)
        ## frequency count of terms
        self$top_tokens <- private$private_top_tokens

        ## count of how many times a term has appear across documents
        self$doc_tokens <- private$private_doc_tokens
        # self$tokens_encoder <- list()
        #
        # for(j in seq_along(unique(self$all_tokens))){
        #         self$tokens_encoder[[j]] <- self$all_tokens[j]
        # }

        return(invisible(self))

    },

    fit_transform = function(sentences){
        self$fit(sentences)
        use_token <- names(self$doc_tokens[1:round(length(self$doc_tokens)
                                                   *self$max_features)])
        return (private$getmatrix(self$sentences,
                                  use_tokens = use_token))
    },

    transform = function(sentences){

        if(is.null(self$sentences))
            stop("Please run fit before applying transformation.")

        use_token <- names(self$doc_tokens[1:round(length(self$doc_tokens)
                                                   *self$max_features)])
        return (private$getmatrix(self$sentences,
                                  use_tokens = use_token))

    }),

    private =  list(

        private_top_tokens = NA,
        private_doc_tokens = NA,

        word_split = function(sentences){
            return (tm::Boost_tokenizer(sentences))
        },

        tokens_limiter = function(sentences, max_df=1, min_df=1){

            top_tokens <- list() ## sorted dictionary

            temp_tokens <- private$word_split(sentences)
            temp_len <- length(sentences)

            for(j in temp_tokens){
                if(!(j %in% names(top_tokens))){
                    top_tokens[[j]] <- 1
                } else if(j %in% names(top_tokens)){
                    top_tokens[[j]] <- top_tokens[[j]] + 1
                }

            }

            # sort the list by values
            private_top_tokens <- top_tokens[order(unlist(top_tokens),
                                                   decreasing = T)]

            doc_tokens <- list() ## count token in documents

            for(j in unique(temp_tokens)){
                for(k in sentences){
                    l <- j %in% private$word_split(k)
                    if(l){
                        if(!(j %in% names(doc_tokens))){
                            doc_tokens[[j]] <- 1
                        } else if(j %in% names(doc_tokens)){
                            doc_tokens[[j]] <- doc_tokens[[j]] + 1
                        }
                    }
                }
            }

            lower_limit <- round(temp_len * min_df) # percentage
            upper_limit <- round(temp_len * max_df)

            if(min_df > max_df) {
                stop("min_df cannot be greater than max_df.
                     Please use another value.")
            } else if(min_df == 1 & max_df == 1) {
                filtered_tokens <- names(doc_tokens)
            } else {
                filtered_tokens <- names(doc_tokens[doc_tokens > lower_limit &
                                                    doc_tokens < upper_limit])
            }

            private$private_doc_tokens <- doc_tokens[order(unlist(doc_tokens),
                                                           decreasing = T)]
            return(filtered_tokens)

        },


        getmatrix = function(sentences, use_tokens){

            output_matrix <- c()

            tokens <- use_tokens
            n_rows <- length(sentences)

            for(i in seq(n_rows)){
                s <- vector(mode = "integer", length = length(tokens))

                words <- private$word_split(sentences[i])

                for(j in words){
                    k <- which(tokens == j)
                    s[k] <- 1
                }
                output_matrix <- rbind(output_matrix, s)
            }

            row.names(output_matrix) <- NULL
            output_matrix <- data.frame(output_matrix)
            colnames(output_matrix) <- tokens
            return(output_matrix)

        },

        check_args = function(x){
            if(x < 0 | x > 1)
                stop(sprintf('The value should be between 0 and 1', x))
        }

    )

)



