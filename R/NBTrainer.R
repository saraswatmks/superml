#' Naive Bayes Trainer
#' @description Trains a naive bayes model. It is built on top high performance naivebayes R package.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' nbt = NBTrainer$new(prior=NULL, laplace=0, usekernel=FALSE)
#' nbt$fit(X_train, "target")
#' prediction <- bst$predict(X_test)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialises an instance of naive bayes model}
#'   \item{\code{$fit()}}{fits model to an input train data and trains the model.}
#'   \item{\code{$predict()}}{returns predictions by fitting the trained model on test data.}
#' }
#' @section Arguments:
#' \describe{
#'  \item{prior}{for detailed explanation of parameters, check: https://cran.r-project.org/package=naivebayes}
#'  \item{prior}{numeric vector with prior probabilities. vector with prior probabilities of the classes.
#'               If unspecified, the class proportions for the training set are used.
#'               If present, the probabilities should be specified in the order of the factor levels.}
#'  \item{laplace}{value used for Laplace smoothing. Defaults to 0 (no Laplace smoothing)}
#'  \item{usekernel}{if TRUE, density is used to estimate the densities of metric predictors}
#' }
#' @export
#' @examples
#' data(iris)
#' nb <- NBTrainer$new()
#' nb$fit(iris, 'Species')
#' y <- nb$predict(iris)
NBTrainer <- R6Class('NBTrainer', public = list(

    prior = NULL,
    laplace = 0,
    usekernel = FALSE,
    model=NULL,

    initialize = function(prior, laplace, usekernel){

        if(!(missing(prior))) self$prior <- prior
        if(!(missing(laplace))) self$laplace <- laplace
        if(!(missing(usekernel))) self$usekernel <- usekernel

    },


    fit = function(X, y){

        superml::testdata(X, y)

        # for naive bayes, the dep variable should be character or vector
        # are you fucking kidding me ?
        X[[y]] <- as.character(X[[y]])

        f <- as.formula(paste(y , paste("~ .")))
        self$model <- naivebayes::naive_bayes(f
                                              ,data=X
                                              ,laplace = self$laplace
                                              ,prior = self$prior
                                              ,usekernel = self$usekernel)

    },

    predict = function(X, type="class"){

        return (stats::predict(self$model, X, type=type))

    }


))

