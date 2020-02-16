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
        #' @field regex regex expression to use for text cleaning.
        regex = "[^a-zA-Z0-9 ]",
        #' @field model internal attribute which stores the count model
        model = NULL,
        #' @field remove_stopwords a list of stopwords to use, by default it uses its inbuilt list of standard stopwords
        remove_stopwords = TRUE,

        #' @details
        #' Create a new `CountVectorizer` object.
        #'
        #' @param min_df numeric, When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.
        #' @param max_df numeric, When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.
        #' @param max_features integer, Build a vocabulary that only consider the top max_features ordered by term frequency across the corpus.
        #' @param ngram_range vector, The lower and upper boundary of the range of n-values for different word n-grams or char n-grams to be extracted. All values of n such such that min_n <= n <= max_n will be used. For example an ngram_range of c(1, 1) means only unigrams, c(1, 2) means unigrams and bigrams, and c(2, 2) means only bigrams.
        #' @param regex character, regex expression to use for text cleaning.
        #' @param remove_stopwords list, a list of stopwords to use, by default it uses its inbuilt list of standard english stopwords
        #' @param split character, splitting criteria for strings, default: " "
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
                              split) {
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

        preprocess = function(sentences, regex="[^0-9a-zA-Z ]", remove_stopwords){

            # this function returns cleaned sentences
            s <- gsub(regex, " ", sentences)
            # trim whitespace
            s <- trimws(s)

            # check na values
            if (any(is.na(sentences))) {
                stop("Found NAs in the given text. Cannot process NA values.")
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


        super_tokenizer = function(text, ngram_range, split = " "){

            ngram_min = ngram_range[1]
            ngram_max = ngram_range[2]

            tokens = c()

            for (i in text) {
                vec = unlist(strsplit(i, split = split))
                token_len = length(vec)
                if (token_len == ngram_min) {
                    tokens = c(tokens, paste(vec, collapse = split))
                    next
                }

                tk = c()
                for (w in seq(token_len - ngram_min)) {
                    for (x in seq(ngram_min, ngram_max)) {

                        if (w + x - 1 <= token_len) {
                            v = paste(vec[w:(w + x - 1)], collapse = split)
                            tk = c(tk, v)
                        }
                    }
                }

                tokens = c(tokens, tk)

            }
            return(tokens)
        },


        get_tokens = function(sentences, min_df=1, max_df=1, ngram_range = NULL, max_features=NULL, split=NULL){

            # sentences should be preprocessed sentences
            # if n_gram is not use
            # or if n_gram is (1,1) tokenize by space
            if (is.null(ngram_range) | all(ngram_range == 1) ) {
                tokens_counter <- sort(unique(tm::Boost_tokenizer(sentences)))
            } else {
                # create tokens using gram range
                # do validation check
                if (is.vector(ngram_range) & length(ngram_range) != 2) {
                    stop("ngram_range must have a min-max value for ngrams. Try using: c(1, 2)")
                }

                tokens_counter <- sort(unique(super_tokenizer(sentences, ngram_range = ngram_range, split = split)))
            }


            # check for default features
            if (is.null(max_features) & (max_df == 1) & (min_df == 1)) {
                # return all the tokens
                return(tokens_counter)
            }

            # Check max feature

            # max_feature will override other two parameters (min_df, max_df)
            if (!(is.null(max_features))) {
                if (max_features > length(tokens_counter)) {
                    # use all possible features
                    max_features <- length(tokens_counter)

                }
                # use all tokens
                if (max_features == 1) {
                    return(tokens_counter)
                }

                if (max_features > 1) {
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

        CJ.dt = function(X,Y) {
            stopifnot(is.data.table(X),is.data.table(Y))
            k = NULL
            X = X[, c(k = 1, .SD)]
            setkey(X, k)
            Y = Y[, c(k = 1, .SD)]
            setkey(Y, NULL)
            X[Y, allow.cartesian = TRUE][, k := NULL][]
        },


        gsub_match = function(token, sent) {
            return(gsub(pattern = paste0("\\b", token,"\\b"),  replacement = "", x = sent))
        },

        get_bow_df = function(sentences, use_tokens=NULL){

            f <- data.table(index = seq(sentences), docs = sentences)
            t <- data.table(tokens = use_tokens)
            f <- CJ.dt(f, t)[order(index)]

            # use char differences, faster solution
            f[, char_diff := nchar(docs) - nchar(mapply(gsub_match, tokens, docs))]
            f[, char_diff := as.integer(char_diff / nchar(tokens))]

            f <- dcast(f, index ~ tokens, value.var = 'char_diff')[,-1]

            return(as.matrix(f[, ..use_tokens]))

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
