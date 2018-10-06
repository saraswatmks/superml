#' Linear Models Trainer
#' @description Trains linear models such as Logistic, Lasso or Ridge regression model. It uses glmnet R package in the backend. Lasso regression can be used
#' as a variable selection method also. This class provides fit, predict, cross valdidation functions.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' bst = LMTrainer$new(family, weights, alpha, nlambda, standardize.response)
#' bst$fit(X_train, "target")
#' prediction <- bst$predict(X_test)
#' bst$cv_model(X_train, "target", nfolds=4, parallel=TRUE)
#' cv_prediction <- bst$cv_predict(X_test)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(family, weights, alpha, nlambda, standardize.response)}}{Initialises an instance of random forest model}
#'   \item{\code{$fit(X_train, "target")}}{fit model to an input train data (data frame or data table) and trains the model.}
#'   \item{\code{$predict(X_test)}}{returns predictions by fitting the trained model on test data.}
#'   \item{\code{$cv_model(X_train, "target", nfolds=4, parallel=TRUE)}}{Using k-fold cross validation technique, finds the best value of lambda}
#'   \item{\code{$cv_predict(X_test)}}{Using the best value of lambda, makes predictions on the test data}
#' }
#' @section Arguments:
#' \describe{
#'  \item{family}{type of regression to perform, values can be "gaussian" ,"binomial", "multinomial","mgaussian"}
#'  \item{weights}{observation weights. Can be total counts if responses are proportion matrices. Default is 1 for each observation}
#'  \item{alpha}{The elasticnet mixing parameter, alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.}
#'  \item{nlambda}{the number of lambda values - default is 100}
#'  \item{standardize.response}{This is for the family="mgaussian" family, and allows the user to standardize the response variables}
#' }
#' @export
#' @examples
#' LINK <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
#' housing <- read.table(LINK)
#' names <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
#'            "RAD","TAX","PTRATIO","B","LSTAT","MEDV")
#' names(housing)  <-  names
#'
#' # single model
#' lf <- LMTrainer$new(family = 'gaussian')
#' lf$fit(X = housing, y = 'MEDV')
#' predictions <- lf$predict(df = housing)
#'
#' # cross validation model
#' lf$cv_model(X = housing, y = 'MEDV', nfolds = 5, parallel = T)
#' predictions <- lf$cv_predict(df = housing)
LMTrainer <- R6Class("LMTrainer", public = list(

    family = NULL,
    weights = NULL,
    # 1 = lasso (variable selection)
    # 0 = Ridge
    alpha = 1,
    nlambda = 100,
    standardize = T,
    standardize.response = F,
    model = NULL,
    cvmodel = NULL,
    Flag = F,
    is_lasso = F,
    iid_names = NULL,

    initialize = function(family,
                          weights,
                          alpha,
                          nlambda,
                          standardize.response){
        if(!(missing(family))) self$family <- family
        if(!(missing(weights))) self$weights <- weights
        if(!(missing(alpha))) self$alpha <- alpha
        if(!(missing(nlambda))) self$lambda <- nlambda
        if(!(missing(standardize.response))) self$standardize.response <- standardize.response

    },

    fit = function(X, y){

        self$iid_names <- private$check_data(X = X, y = y)

        # set default value for weights
        self$weights <- rep(1, nrow(X))
        self$model <- glmnet::glmnet(x = as.matrix(X[, self$iid_names]),
               y = X[[y]],
               family = self$family,
               weights = self$weights,
               alpha = self$alpha,
               nlambda = self$nlambda,
               standardize = self$standardize,
               standardize.response = self$standardize.response)

        if(self$alpha == 1) self$is_lasso <- TRUE

    },

    predict = function(df, lambda = NULL){

        if(is.null(lambda)) lambda <- min(self$model$lambda)
        return(stats::predict(self$model,
                              as.matrix(df[, c(self$iid_names)]),
                              s = lambda,
                              type = "response"))
    },

    cv_model = function(X, y, nfolds, parallel){

        self$iid_names <- private$check_data(X, y)

        if(isTRUE(parallel)){
            cl <- parallel::makeCluster(parallel::detectCores())
            doParallel::registerDoParallel(cl)
            message("Starting parallel clusters.")
        }

        self$weights <- rep(1, nrow(X))
        self$cvmodel <- glmnet::cv.glmnet(x = as.matrix(X[,self$iid_names])
                                          ,y = X[[y]]
                                          ,weights = self$weights
                                          ,lambda = NULL
                                          ,nfolds = nfolds
                                          ,parallel = parallel)

        # this flag is for variable importance
        self$Flag <- TRUE
        message("Computation done.")
        message("Stopping clusters.")
        parallel::stopCluster(cl)

    },

    cv_predict = function(df, lambda = NULL){

        if(is.null(lambda)){
            if(isTRUE(self$Flag)) lambda <- min(self$cvmodel$lambda.min)
            else print("Please run the cv_model function.")
        }

        return(stats::predict(self$cvmodel,
                              as.matrix(df[, c(self$iid_names)]),
                              s = lambda,
                              type="response"))

    },

    imp_features = function(){

        if(self$is_lasso){
            return(coef(self$cv_model))
        } else {
            print("Feature selection only happens in Lasso Regression.
                  Please run the model with alpha=1.")
        }

    }),

    private = list(

    check_data = function(X, y) {
        if (!(inherits(X, c("data.table", "data.frame"))))
            stop("Your data format should be a data.table or data.frame.")

        if(!(y %in% names(X)))
            stop("The dependent variable is not available in the data.")

        if(any(is.na(X[[y]])))
            stop("The dependent variable contains missing values.
                 Please remove/impute missing values.")

        if(any(unlist(sapply(X, class)) %in% c("factor", "character")))
            stop("There are factor or character values in the data set.
                 Please convert to numeric.")

        iid_names <- setdiff(colnames(X), y)
        return(iid_names)
    }
))
