#' Best Matching(BM25)
#'
#' @description BM25 stands for Best Matching 25. It is widely using for ranking documents and a prefered method than TF*IDF scores.
#' It is used to find the similar documents from a corpus, given a new document. It is populary used in information retrieval systems.
#' This implementation ses multiple cores for faster and parallel computation.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' bm25 = bm25$new(corpus, n_cores)
#' bm25$most_similar(input_document, topn)
#' bm25$compute(input_document)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialise the instance of the class. Here you pass the complete corpus of the documents}
#'   \item{\code{$most_similar()}}{it returns the topn most similar documents from the corpus}
#'   \item{\code{$compute()}}{it returns a similarity score for all the documents in the corpus, given a sentence}
#' }
#' @section Arguments:
#' \describe{
#'  \item{corpus}{a list containing sentences}
#'  \item{use_parallel}{boolean value used to activate parallel computation, defaults to FALSE}
#' }
#' @export
#' @examples
#' example <- c('white audi 2.5 car','black shoes from office',
#'              'new mobile iphone 7','audi tyres audi a3',
#'              'nice audi bmw toyota corolla')
#' get_bm <- bm25$new(example, use_parallel=FALSE)
#' input_document <- c('white toyota corolla')
#' get_bm$most_similar(document = input_document, topn = 2)
bm25 <- R6::R6Class("bm25", public = list(

    ## corpus should be a list of sentences
    corpus = NA,
    use_parallel = FALSE,

    initialize = function(corpus, use_parallel){
        if(!(missing(corpus))) self$corpus <- self$transform(corpus)
        if(!(missing(use_parallel))) self$use_parallel <- use_parallel

        if(isTRUE(self$use_parallel)){
            message('Computation will be done parallelly using all CPU cores.')
        } else {
            message('to activate parallel computation, set use_parallel=TRUE')
        }

    },

    # tokenize the input
    transform = function(corpus){
        return (vapply(corpus,
                       function(x) strsplit(x, split = " "),
                       FUN.VALUE = list(1)))
    },

    calculate_idf = function(q, corpus=self$corpus){

        # check the token exists in how many documents
        q_in_ns <- sum(vapply(corpus,
                              function(x) q %in% x,
                              FUN.VALUE = logical(1)))

        # check token length
        corpus_len <- length(corpus)
        return(log((corpus_len - q_in_ns + 0.5) / (q_in_ns + 0.5)))

    },

    bmscore = function(q, document_from_corpus, corpus=self$corpus){

        # constant values
        b <- 0.75
        k1 <- 1.25

        freq_q <- sum(document_from_corpus == q)
        doc_len <- length(document_from_corpus)
        mean_doc_len <- mean(vapply(corpus, length, FUN.VALUE = integer(1)))
        return (self$calculate_idf(q, corpus) *
                    ((freq_q * (k1 + 1)) /
                         ((freq_q + k1) *
                              (1 - b + b *
                                   (doc_len / mean_doc_len)))))

    },

    compute = function(document, corpus = self$corpus, cores=self$n_cores){

        if(isTRUE(self$use_parallel)){
            # devtools uses 2 cores max. to check parallel processes
            # but here removed parameters to set cores.
            if(self$n_cores == "auto") cores <- parallel::detectCores()
            else cores <- self$n_cores

            message('using ', cores, ' cores for computation')
        }

        # for non parallel computation, use 1 core.
        cores <- 1

        # document = your document should be a tokenized vector
        document <- unlist(strsplit(document, split = " "))

        temp_score <- parallel::mclapply(corpus,
                    function(x) sum(vapply(document, function(y)
                    self$bmscore(y, x, corpus), FUN.VALUE = numeric(1)))
                    , mc.cores = cores)

        names(temp_score) <- names(corpus)
        return(temp_score)
    },

    most_similar = function(document, corpus=self$corpus, topn=1){
        # sort the list by values
        aa <- self$compute(document, corpus)
        return(names(aa[order(unlist(aa), decreasing = T)][seq(topn)]))
    }
))

