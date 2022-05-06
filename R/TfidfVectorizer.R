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
    max_features = NULL,
    #' @field ngram_range The lower and upper boundary of the range of n-values for different word n-grams or char n-grams to be extracted. All values of n such such that min_n <= n <= max_n will be used. For example an ngram_range of c(1, 1) means only unigrams, c(1, 2) means unigrams and bigrams, and c(2, 2) means only bigrams.
    ngram_range = c(1,1),
    #' @field split splitting criteria for strings, default: " "
    split = " ",
    #' @field lowercase convert all characters to lowercase before tokenizing
    lowercase = TRUE,
    #' @field regex regex expression to use for text cleaning.
    regex = "[^a-zA-Z0-9 ]",
    #' @field remove_stopwords a list of stopwords to use, by default it uses its inbuilt list of standard stopwords
    remove_stopwords = TRUE,
    #' @field smooth_idf logical, to prevent zero division, adds one to document frequencies, as if an extra document was seen containing every term in the collection exactly once
    smooth_idf = TRUE,
    #' @field norm logical, if TRUE, each output row will have unit norm ‘l2’: Sum of squares of vector elements is 1. if FALSE returns non-normalized vectors, default: TRUE
    norm = TRUE,


    #' @details
    #' Create a new `TfIdfVectorizer` object.
    #'
    #' @param min_df numeric, When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.
    #' @param max_df numeric, When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.
    #' @param max_features integer, Build a vocabulary that only consider the top max_features ordered by term frequency across the corpus.
    #' @param ngram_range vector, The lower and upper boundary of the range of n-values for different word n-grams or char n-grams to be extracted. All values of n such such that min_n <= n <= max_n will be used. For example an ngram_range of c(1, 1) means only unigrams, c(1, 2) means unigrams and bigrams, and c(2, 2) means only bigrams.
    #' @param regex character, regex expression to use for text cleaning.
    #' @param remove_stopwords list, a list of stopwords to use, by default it uses its inbuilt list of standard english stopwords
    #' @param split character, splitting criteria for strings, default: " "
    #' @param lowercase logical, convert all characters to lowercase before tokenizing, default: TRUE
    #' @param smooth_idf logical, to prevent zero division, adds one to document frequencies, as if an extra document was seen containing every term in the collection exactly once
    #' @param norm logical, if TRUE, each output row will have unit norm ‘l2’: Sum of squares of vector elements is 1. if FALSE returns non-normalized vectors, default: TRUE
    #' @param parallel logical,  speeds up ngrams computation using n-1 cores, defaults: TRUE
    #'
    #' @return A `TfIdfVectorizer` object.
    #'
    #' @examples
    #' TfIdfVectorizer$new()

    initialize = function(min_df,
                          max_df,
                          max_features,
                          ngram_range,
                          regex,
                          remove_stopwords,
                          split,
                          lowercase,
                          smooth_idf,
                          norm
                          ){
        super$initialize(min_df = min_df,
                         max_df = max_df,
                         max_features = max_features,
                         ngram_range = ngram_range,
                         remove_stopwords = remove_stopwords,
                         regex = regex,
                         split = split,
                         lowercase = lowercase)
        if (!(missing(min_df)))
            self$min_df <- min_df
        if (!(missing(max_df)))
            self$max_df <- max_df
        if (!(missing(max_features)))
            self$max_features <- max_features
        if (!(missing(ngram_range)))
            self$ngram_range <- ngram_range
        if (!(missing(regex)))
            self$regex <- regex
        if (!(missing(remove_stopwords)))
            self$remove_stopwords <- remove_stopwords
        if (!(missing(split)))
            self$split <- split
        if (!(missing(smooth_idf)))
            self$smooth_idf <- smooth_idf
        if (!(missing(norm)))
            self$norm <- norm


        private$check_args(self$max_df, what = 'max_df')
        private$check_args(self$min_df, what = 'min_df')


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
        super$fit(sentences)
        return(invisible(self))
    },


    #' @details
    #' Fits the TfIdfVectorizer model and returns a sparse matrix of count of tokens
    #'
    #' @param sentences a list of text sentences
    #' @return a sparse matrix containing tf-idf score for tokens in each given sentence
    #'
    #' @examples
    #' \dontrun{
    #' sents <- c('i am alone in dark.','mother_mary a lot',
    #'          'alone in the dark?', 'many mothers in the lot....')
    #' tf <- TfIdfVectorizer$new(smooth_idf = TRUE, min_df = 0.1)
    #' tf_matrix <- tf$fit_transform(sents)
    #' }


    fit_transform = function(sentences){
        self$fit(sentences)
        return(private$gettfmatrix(self$model,
                                   sentences = self$sentences,
                                   norm = self$norm,
                                   smooth_idf = self$smooth_idf))
    },

    #' @details
    #' Returns a matrix of tf-idf score of tokens
    #'
    #' @param sentences a list of new text sentences
    #' @return a sparse matrix containing tf-idf score for tokens in each given sentence
    #'
    #' @examples
    #' \dontrun{
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
        bow <- super$transform(sentences)
        return(private$gettfmatrix(bow,
                                   sentences = sentences,
                                   norm = self$norm,
                                   smooth_idf = self$smooth_idf))

    }),

    private = list(
        gettfmatrix = function(countmatrix, sentences, norm, smooth_idf = TRUE){

            # create idf matrix
            tokens <- colnames(countmatrix)
            total_docs <- nrow(countmatrix)

            # Number of documents with term t in it
            n_docs_with_token <- sapply(tokens, function(x) sum(grepl(pattern = paste0("\\b", x,"\\b"), x = sentences)))

            # add 1 to avoid zero division error
            idf_ <- log((total_docs + as.numeric(smooth_idf)) / (n_docs_with_token + as.numeric(smooth_idf))) + 1

            tfidf <- countmatrix
            for (word in tokens) {
                tfidf[, word] <- tfidf[, word] * idf_[[word]]
            }

            if (isTRUE(norm)) {
                # normalize rows
                # tfidf = Matrix::Diagonal(x = 1 / sqrt(Matrix::rowSums(tfidf^2))) %*% tfidf
                # tfidf = diag(x = 1 / sqrt(rowSums(tfidf^2))) %*% tfidf
                # if (any(is.na(tfidf))) {
                #     tfidf[is.na(tfidf)] <- 0
                # }
                tfnames <- colnames(tfidf)
                tfidf = superml:::normalise2d(tfidf)
                colnames(tfidf) <- tfnames

                return(tfidf)
            }

            return(tfidf)
        }
    )
)
