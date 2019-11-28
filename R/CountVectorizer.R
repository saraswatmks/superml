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
        #' @field max_features use top features sorted by count to be used in bag of words matrix.
        max_features = 1,
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
        #' @param max_df nuemric, When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.
        #' @param max_features integer, use top features sorted by count to be used in bag of words matrix.
        #' @param regex character, regex expression to use for text cleaning.
        #' @param remove_stopwords list, a list of stopwords to use, by default it uses its inbuilt list of standard stopwords
        #' @param split character, splitting criteria for strings, default: " "
        #'
        #' @return A `CountVectorizer` object.
        #'
        #' @examples
        #' cv = CountVectorizer$new(min_df=0.1)

        initialize = function(min_df,
                              max_df,
                              max_features,
                              regex,
                              remove_stopwords,
                              split) {
            if (!(missing(max_df)))
                self$max_df <- max_df
            if (!(missing(min_df)))
                self$min_df <- min_df
            if (!(missing(max_features)))
                self$max_features <- max_features
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
                max_features = self$max_features
            )

            self$model <- private$get_bow_df(self$sentences,
                                             split_rule = self$split,
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
            use_token <- attr(self$model, "dimnames")[[2]]

            return(
                private$get_bow_df(
                    sentences,
                    use_tokens = use_token,
                    model = self$model,
                    ctransform = TRUE
                )
            )

        }
        ),

    private =  list(

        preprocess = function(sentences, regex="[^0-9a-zA-Z ]", remove_stopwords){


            # this function returns cleaned sentences
            s <- gsub(regex, " ", sentences)

            if (isTRUE(remove_stopwords)) {

                path = system.file("stopwords","english.txt", package = "superml")
                stopwords <- read.csv(path, header = F, stringsAsFactors = F)[,1]

                # remove stopwords from sentences
                sw_pattern = paste0("\\b(?:", paste(stopwords, collapse = "|"), ")\\b ?")
                s <- gsub(pattern = sw_pattern, replacement = '', s, perl = T)

                return(s)

            }
            return(s)
        },


        get_tokens = function(sentences, min_df=1, max_df=1, max_features=1, split=NULL){


            # sentences should be preprocessed sentences

            # here and use only those tokens which are necessarily required
            # instead of creating a matrix for all the words
            tokens_counter <- sort(table(tm::Boost_tokenizer(sentences)), decreasing = TRUE)

            # max features should not be greater than max. value
            if (max_features > length(tokens_counter))
                stop('max_features cannot be greater than maximum possible
                     features. Please pass a smaller value.')

            # max_feature will override other two parameters (min_df, max_df)
            # this is default value, use all tokens
            if (max_features == 1) {
                return(names(tokens_counter))
            }

            if (max_features > 1) {
                return(names(tokens_counter)[1:max_features])
            }

            # min_df = keep tokens that occur in atleast this % documents (lower_limit)
            # max_df = keep tokens that occur in at max this % documents (upper_limit)

            if (min_df > max_df) {
                stop("min_df cannot be greater than max_df.
                     Please use another value.")
            } else if (min_df == 1 & max_df == 1) {
                return(names(tokens_counter))
            }

            # get upper and lower limit values
            lower_limit <- round(length(sentences) * min_df)
            upper_limit <- round(length(sentences) * max_df)

            return(list(lower = lower_limit, upper = upper_limit))

        },

        get_bow_df = function(sentences,
                              split_rule=" ",
                              use_tokens=NULL,
                              model = NULL,
                              ctransform=FALSE){

            # calculate count of tokens across all documents
            f <- rbindlist(
                lapply(
                    Map(
                        cbind, seq(sentences),
                        strsplit(sentences, split = split_rule)), data.table), fill=TRUE)

            f <- Matrix::Matrix(Matrix::as.matrix(dcast.data.table(f,
                                                                   V1 ~ V2,
                                                                   fun.aggregate = length,
                                                                   value.var = "V2")[,-1]), sparse = TRUE)

            if (isTRUE(ctransform)) {

                common_cols <- colnames(model)[(colnames(model) %in% colnames(f))]
                different_cols <- setdiff(colnames(model), common_cols)
                test_rows <- nrow(f)
                tr_output <- do.call(cbind,
                                     sapply(different_cols,
                                            function(x) list(rep_len(0, test_rows))))

                return(do.call(cbind, list(f[,common_cols], tr_output)))

            } else {
                return(f[,use_tokens])
            }

        },

        check_args = function(x, max_value, what){
            if (what == 'max_features') {
                if (x < 0 )
                    stop(sprintf('The value for %s cannot be below zero', x))
                if (x > max_value) {
                    stop(sprintf('The value for %s cannot be
                             more than max. possible features', x))
                }
            }

            if (what %in% c('min_df','max_df')) {
                if (x < 0 | x > 1)
                    stop(sprintf('The value for %s cannot be below zero', x))

            }

        }

    )

    )
