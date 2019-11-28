#' Naive Bayes Trainer
#'
#' Trains a probabilistic naive bayes model
#'
#' @details
#' Trains a naive bayes model. It is built on top high performance naivebayes R package.
#'
#' @export

NBTrainer <- R6Class('NBTrainer', public = list(

    #' @field prior numeric vector with prior probabilities. vector with prior probabilities of the classes.
    #'               If unspecified, the class proportions for the training set are used.
    #'               If present, the probabilities should be specified in the order of the factor levels.
    prior = NULL,
    #' @field laplace value used for Laplace smoothing. Defaults to 0 (no Laplace smoothing)
    laplace = 0,
    #' @field usekernel if TRUE, density is used to estimate the densities of metric predictors
    usekernel = FALSE,
    #' @field model for internal use
    model = NULL,

    #' @details
    #' Create a new `NBTrainer` object.
    #'
    #' @param prior numeric, prior numeric vector with prior probabilities. vector with prior probabilities of the classes.
    #'               If unspecified, the class proportions for the training set are used.
    #'               If present, the probabilities should be specified in the order of the factor levels.
    #' @param laplace nuemric, value used for Laplace smoothing. Defaults to 0 (no Laplace smoothing)
    #' @param usekernel logical, if TRUE, density is used to estimate the densities of metric predictors
    #'
    #' @return A `NBTrainer` object.
    #'
    #' @examples
    #' data(iris)
    #' nb <- NBTrainer$new()

    initialize = function(prior, laplace, usekernel){

        if (!(missing(prior))) self$prior <- prior
        if (!(missing(laplace))) self$laplace <- laplace
        if (!(missing(usekernel))) self$usekernel <- usekernel
        superml::check_package("naivebayes")

    },

    #' @details
    #' Fits the naive bayes model
    #'
    #' @param X data.frame containing train features
    #' @param y character, name of target variable
    #' @return NULL, trains and saves the model in memory
    #'
    #' @examples
    #' data(iris)
    #' nb <- NBTrainer$new()
    #' nb$fit(iris, 'Species')

    fit = function(X, y){

        superml::testdata(X, y)

        # for naive bayes, the dep variable should be character or vector
        # are you fucking kidding me ?
        X[[y]] <- as.character(X[[y]])

        f <- as.formula(paste(y , paste("~ .")))
        self$model <- naivebayes::naive_bayes(f
                                              ,data = X
                                              ,laplace = self$laplace
                                              ,prior = self$prior
                                              ,usekernel = self$usekernel)

    },

    #' @details
    #' Returns predictions from the model
    #'
    #' @param X data.frame containing test features
    #' @param type character, if the predictions should be labels or probability
    #' @return NULL, trains and saves the model in memory
    #'
    #' @examples
    #' data(iris)
    #' nb <- NBTrainer$new()
    #' nb$fit(iris, 'Species')
    #' y <- nb$predict(iris)

    predict = function(X, type="class"){

        return(stats::predict(self$model, X, type = type))

    }


))

