#' Best Matching(BM25)
#'
#' Computer BM25 distance between sentences/documents.
#'
#' @details
#' BM25 stands for Best Matching 25. It is widely using for ranking documents and a preferred method than TF*IDF scores.
#' It is used to find the similar documents from a corpus, given a new document. It is popularly used in information retrieval systems.
#' This implementation uses multiple cores for faster and parallel computation.
#'
#' @export

bm25 <- R6::R6Class("bm25", public = list(

    #' @field corpus a list containing sentences
    corpus = NA,
    #' @field use_parallel enables parallel computation, defaults to FALSE
    use_parallel = FALSE,

    #' @details
    #' Create a new `bm25` object.
    #'
    #' @param corpus list, a list containing sentences
    #' @param use_parallel logical, enables parallel computation, defaults to FALSE. if TRUE uses n - 1 cores.
    #'
    #' @return A `bm25` object.
    #'
    #' @examples
    #' example <- c('white audi 2.5 car','black shoes from office',
    #'              'new mobile iphone 7','audi tyres audi a3',
    #'              'nice audi bmw toyota corolla')
    #' obj <- bm25$new(example, use_parallel=FALSE)

    initialize = function(corpus, use_parallel){
        if (!(missing(corpus))) self$corpus <- private$transform(corpus)
        if (!(missing(use_parallel))) self$use_parallel <- use_parallel

        if (isTRUE(self$use_parallel)){
            message('Computation will be done parallelly using all CPU cores.')
        } else {
            message('to activate parallel computation, set use_parallel=TRUE')
        }

    },


    #' @details
    #' Returns a list of the most similar sentence
    #'
    #' @param document character, for this value we find most similar sentences.
    #' @param topn integer, top n sentences to retrieve
    #' @return a vector of most similar documents
    #'
    #' @examples
    #' example <- c('white audi 2.5 car','black shoes from office',
    #'              'new mobile iphone 7','audi tyres audi a3',
    #'              'nice audi bmw toyota corolla')
    #' get_bm <- bm25$new(example, use_parallel=FALSE)
    #' input_document <- c('white toyota corolla')
    #' get_bm$most_similar(document = input_document, topn = 2)

    most_similar = function(document, topn=1){
        # sort the list by values
        aa <- private$compute(document, self$corpus, self$use_parallel)
        ## return(aa) contains a list of documents & scores
        return(names(aa[order(unlist(aa), decreasing = T)][seq(topn)]))
    }
), private = list(

    calculate_idf = function(q, corpus){

        # check the token exists in how many documents
        q_in_ns <- sum(vapply(corpus,
                              function(x) q %in% x,
                              FUN.VALUE = logical(1)))

        # check token length
        corpus_len <- length(corpus)
        return(log((corpus_len - q_in_ns + 0.5) / (q_in_ns + 0.5)))

    },

    # tokenize the input
    transform = function(corpus){
        return (vapply(corpus,
                       function(x) strsplit(x, split = " "),
                       FUN.VALUE = list(1)))
    },


    bmscore = function(q, document_from_corpus, corpus){

        # constant values
        b <- 0.75
        k1 <- 1.25

        freq_q <- sum(document_from_corpus == q)
        doc_len <- length(document_from_corpus)
        mean_doc_len <- mean(vapply(corpus, length, FUN.VALUE = integer(1)))
        return (private$calculate_idf(q, corpus) *
                    ((freq_q * (k1 + 1)) /
                         ((freq_q + k1) *
                              (1 - b + b *
                                   (doc_len / mean_doc_len)))))

    },

    compute = function(document, corpus, use_parallel){

        if(isTRUE(self$use_parallel)){
            # devtools uses 2 cores max. to check parallel processes
            # but here removed parameters to set cores.
            cores <- parallel::detectCores()
              # use single core

            message('using ', cores, ' cores for computation')
        } else {
            cores <- 1 # use single core
        }

        # document = your document should be a tokenized vector
        document <- unlist(strsplit(document, split = " "))
        temp_score <- parallel::mclapply(corpus,
                                         function(x) sum(vapply(document, function(y)
                                             private$bmscore(y, x, corpus), FUN.VALUE = numeric(1)))
                                         , mc.cores = cores)

        names(temp_score) <- names(corpus)
        return(temp_score)
    }



))




