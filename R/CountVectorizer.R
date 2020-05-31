#' Count Vectorizer
#'
#' Creates CountVectorizer Model.
#'
#' @details
#' Given a list of text, it generates a bag of words model and returns a sparse matrix consisting of token counts.
#'
#' @export
CountVectorizer <- R6::R6Class(
    "CountVectorizer",
    public = list(
        #' @field sentences a list containing sentences
        sentences = NA,
        #' @field max_df When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.
        max_df = 1,
        #' @field min_df When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.
        min_df = 1,
        #' @field max_features Build a vocabulary that only consider the top max_features ordered by term frequency across the corpus.
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
        #' @field model internal attribute which stores the count model
        model = NULL,

        #' @details
        #' Create a new `CountVectorizer` object.
        #'
        #' @param min_df numeric, When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.
        #' @param max_df numeric, When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.
        #' @param max_features integer, Build a vocabulary that only consider the top max_features ordered by term frequency across the corpus.
        #' @param ngram_range vector, The lower and upper boundary of the range of n-values for different word n-grams or char n-grams to be extracted. All values of n such such that min_n <= n <= max_n will be used. For example an ngram_range of c(1, 1) means only unigrams, c(1, 2) means unigrams and bigrams, and c(2, 2) means only bigrams.
        #' @param regex character, regex expression to use for text cleaning.
        #' @param split character, splitting criteria for strings, default: " "
        #' @param lowercase logical, convert all characters to lowercase before tokenizing, default: TRUE
        #' @param remove_stopwords list, a list of stopwords to use, by default it uses its inbuilt list of standard english stopwords
        #'
        #' @return A `CountVectorizer` object.
        #'
        #' @examples
        #' cv = CountVectorizer$new(min_df=0.1)

        initialize = function(min_df,
                              max_df,
                              max_features,
                              ngram_range,
                              regex,
                              remove_stopwords,
                              split,
                              lowercase
                              ) {
            if (!(missing(max_df)))
                self$max_df <- max_df
            if (!(missing(min_df)))
                self$min_df <- min_df
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
            if (!(missing(lowercase)))
                self$lowercase <- lowercase

            private$check_args(self$max_df, what = 'max_df')
            private$check_args(self$min_df, what = 'min_df')

        },

        #' @details
        #' Fits the countvectorizer model on sentences
        #'
        #' @param sentences a list of text sentences
        #' @return NULL
        #'
        #' @examples
        #' sents = c('i am alone in dark.','mother_mary a lot',
        #'           'alone in the dark?', 'many mothers in the lot....')
        #' cv = CountVectorizer$new(min_df=0.1)
        #' cv$fit(sents)

        fit = function(sentences) {
            self$sentences <- private$preprocess(
                sentences,
                regex = self$regex,
                lowercase = self$lowercase,
                remove_stopwords = self$remove_stopwords
            )
            ## pass cleaned sentences here
            ## this function returns a vector of tokens to be
            ## used as subset in next steps
            use_tokens <- private$get_tokens(
                self$sentences,
                min_df = self$min_df,
                max_df = self$max_df,
                max_features = self$max_features,
                ngram_range = self$ngram_range,
                split = self$split
            )

            self$model <- private$get_bow_df(self$sentences,
                                             use_tokens = use_tokens)


        },

        #' @details
        #' Fits the countvectorizer model and returns a sparse matrix of count of tokens
        #'
        #' @param sentences a list of text sentences
        #' @return a sparse matrix containing count of tokens in each given sentence
        #'
        #' @examples
        #' sents = c('i am alone in dark.','mother_mary a lot',
        #'          'alone in the dark?', 'many mothers in the lot....')
        #' cv <- CountVectorizer$new(min_df=0.1)
        #' cv_count_matrix <- cv$fit_transform(sents)

        fit_transform = function(sentences) {
            self$fit(sentences)
            return(self$model)
        },

        #' @details
        #' Returns a matrix of count of tokens
        #'
        #' @param sentences a list of new text sentences
        #' @return a sparse matrix containing count of tokens in each given sentence
        #'
        #' @examples
        #' sents = c('i am alone in dark.','mother_mary a lot',
        #'           'alone in the dark?', 'many mothers in the lot....')
        #' new_sents <- c("dark at night",'mothers day')
        #' cv = CountVectorizer$new(min_df=0.1)
        #' cv$fit(sents)
        #' cv_count_matrix <- cv$transform(new_sents)

        transform = function(sentences) {
            if (is.null(self$sentences))
                stop("Please run fit before applying transformation.")

            # apply the same transformation on test
            sentences <- private$preprocess(
                sentences,
                regex = self$regex,
                lowercase = self$lowercase,
                remove_stopwords = self$remove_stopwords
            )

            # use the same column names from self$model
            return(
                private$get_bow_df(
                    sentences,
                    use_tokens = colnames(self$model)
                )
            )

        }
        ),

    private =  list(

        preprocess = function(sentences, regex="[^0-9a-zA-Z ]", lowercase, remove_stopwords){

            # check na values
            if (any(is.na(sentences))) {
                stop("Found NAs in the given text. Cannot process NA values.")
            }

            # this function returns cleaned sentences
            s <- gsub(regex, "", sentences)
            # trim whitespace
            s <- trimws(s)
            # convert to lowercase
            if (isTRUE(lowercase)) {
                s <- sapply(s, tolower, USE.NAMES = F)
            }

            if (isTRUE(remove_stopwords)) {

                path = system.file("stopwords","english.txt", package = "superml")
                stopwords <- read.csv(path, header = F, stringsAsFactors = F)[,1]

                # remove stopwords from sentences
                sw_pattern = paste0("\\b(?:", paste(stopwords, collapse = "|"), ")\\b ?")
                s <- gsub(pattern = sw_pattern, replacement = '', s, perl = T)
                # trim whitespace
                s <- trimws(s)

                return(s)

            }
            return(s)
        },


        get_tokens = function(sentences, min_df=1, max_df=1, ngram_range = NULL, max_features=NULL, split=NULL){


            # sentences should be preprocessed sentences
            # if n_gram is not use
            # or if n_gram is (1,1) tokenize by space
            if (is.null(ngram_range) | all(ngram_range == 1) ) {
                tokens_counter <- superml:::superTokenizer(sentences)


            } else {
                # create tokens using gram range
                # do validation check
                if (is.vector(ngram_range) & length(ngram_range) != 2) {
                    stop("ngram_range must have a min-max value for tokens length. Try using: c(1, 2)")
                }

                # fast c++
                tokens_counter <- sapply(sentences, function(x) superml:::superNgrams(x, ngram_range = ngram_range, sep = split))
            }

            # sort the tokens by frequency
            # tokens_counter <- data.table(col = unlist(tokens_counter, use.names = FALSE))
            # radix sorting is faster
            # tokens_counter <- tokens_counter[,.N,keyby = col][order(N, decreasing = T)]$col
            tokens_counter <- superml:::SortOccurence(unlist(tokens_counter, use.names = FALSE))

            # check for default features
            if (is.null(max_features) & (max_df == 1) & (min_df == 1)) {
                # return all the tokens
                return(tokens_counter)
            }

            # Check max feature

            # max_feature will override other two parameters (min_df, max_df)
            if (!(is.null(max_features))) {
                if (max_features > length(tokens_counter)) {
                    # return all possible tokens
                    return(tokens_counter)

                }

                if (max_features >= 1) {
                    return(tokens_counter[1:max_features])
                }

            }

            # get proportion of tokens across documents
            docs_count <- sapply(tokens_counter,
                                 function(x)
                                     (sum(grepl(pattern = paste0("\\b", x,"\\b"), sentences)) / length(sentences)))
            # Check min_df and max_df

            if (min_df == 1 & max_df != 1) {
                # use max_df
                return(names(docs_count)[docs_count <= max_df])
            } else if (min_df != 1 & max_df == 1) {
                # use min_df
                return(names(docs_count)[docs_count >= min_df])
            } else if (min_df != 1 & max_df != 1) {
                # now filter documents on given min_df or max_df value
                return(names(docs_count)[(docs_count >= min_df) & (docs_count <= max_df)])
            }

        },

        get_bow_df = function(sentences, use_tokens=NULL){

            # check is tokens exists
            if (length(use_tokens) < 1) {
                stop("No tokens found matching the given criteria. Please use a larger value. ")
            }

            f <- superml:::superCountMatrix(sentences, use_tokens)

            return(f)

        },

        check_args = function(x, max_value, what){
            if (what == 'max_features') {
                if (x < 0 )
                    stop(sprintf('The value for %s cannot be below zero', x))
                if (x > max_value) {
                    stop(sprintf('The value for %s cannot be more than max. possible features', x))
                }
            }

            if (what %in% c('min_df','max_df')) {
                if (x < 0 | x > 1)
                    stop(sprintf('The value for %s cannot be below zero', x))

            }

        }

    )

    )
