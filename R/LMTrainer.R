#' Linear Models Trainer
#'
#' Trains regression, lasso, ridge model in R
#'
#' @details
#' Trains linear models such as Logistic, Lasso or Ridge regression model. It is built on glmnet R package.
#' This class provides fit, predict, cross valdidation functions.
#'
#' @export

LMTrainer <- R6Class("LMTrainer", public = list(

    #' @field family type of regression to perform, values can be "gaussian" ,"binomial", "multinomial","mgaussian"
    family = NULL,
    #' @field weights observation weights. Can be total counts if responses are proportion matrices. Default is 1 for each observation
    weights = NULL,
    #' @field alpha The elasticnet mixing parameter, alpha=1 is the lasso penalty, alpha=0 the ridge penalty, alpha=NULL is simple regression
    alpha = NULL,
    #' @field lambda the number of lambda values - default is 100
    lambda = 100,
    #' @field standardize normalise the features in the given data
    standardize = FALSE,
    #' @field standardize.response normalise the dependent variable between 0 and 1, default = FALSE
    standardize.response = FALSE,
    #' @field model internal use
    model = NULL,
    #' @field cvmodel internal use
    cvmodel = NULL,
    #' @field Flag internal use
    Flag = FALSE,
    #' @field is_lasso internal use
    is_lasso = FALSE,
    #' @field iid_names internal use
    iid_names = NULL,

    #' @details
    #' Create a new `LMTrainer` object.
    #'
    #' @param family character, type of regression to perform, values can be "gaussian" ,"binomial", "multinomial","mgaussian"
    #' @param weights numeric, observation weights. Can be total counts if responses are proportion matrices. Default is 1 for each observation
    #' @param alpha integer, The elasticnet mixing parameter, alpha=1 is the lasso penalty, alpha=0 the ridge penalty, alpha=NULL is simple regression
    #' @param lambda integer, the number of lambda values - default is 100
    #' @param standardize.response logical, normalise the dependent variable between 0 and 1, default = FALSE
    #'
    #' @return A `LMTrainer` object.
    #'
    #' @examples
    #' \dontrun{
    #' LINK <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
    #' housing <- read.table(LINK)
    #' names <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
    #'            "RAD","TAX","PTRATIO","B","LSTAT","MEDV")
    #' names(housing)  <-  names

    #' lf <- LMTrainer$new(family = 'gaussian', alpha=1)
    #' }

    initialize = function(family,
                          weights,
                          alpha,
                          lambda,
                          standardize.response){
        if (!(missing(family))) self$family <- family
        if (!(missing(weights))) self$weights <- weights
        if (!(missing(alpha))) self$alpha <- alpha
        if (!(missing(lambda))) self$lambda <- lambda
        if (!(missing(standardize.response))) {
            self$standardize.response <- standardize.response
        }
        superml::check_package("glmnet")

    },

    #' @details
    #' Fits the LMTrainer model on given data
    #'
    #' @param X data.frame containing train featuers
    #' @param y character, name of target variable
    #' @return NULL, train the model and saves internally
    #'
    #' @examples
    #' \dontrun{
    #' LINK <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
    #' housing <- read.table(LINK)
    #' names <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
    #'            "RAD","TAX","PTRATIO","B","LSTAT","MEDV")
    #' names(housing)  <-  names

    #' lf <- LMTrainer$new(family = 'gaussian', alpha=1)
    #' lf$fit(X = housing, y = 'MEDV')
    #' }

    fit = function(X, y){

        superml::testdata(X, y)
        self$iid_names <- setdiff(colnames(X), y)

        # set default value for weights
        self$weights <- rep(1, nrow(X))

        if(is.null(self$alpha)){

            # run simple gaussian regression
            f <- as.formula(paste(y , paste("~ .")))
            self$model <- stats::glm(formula = f
                             ,data = X
                             ,weights = self$weights
                             ,family = self$family)

        } else {
            DX <- as.matrix(setDT(X)[, self$iid_names, with=F])
            self$model <- glmnet::glmnet(x = DX,
                             y = X[[y]],
                             family = self$family,
                             weights = self$weights,
                             alpha = self$alpha,
                             nlambda = self$lambda,
                             standardize = self$standardize,
                             standardize.response = self$standardize.response)

            if (self$alpha == 1) self$is_lasso <- TRUE
        }
    },

    #' @details
    #' Returns predictions for test data
    #'
    #' @param df data.frame containing test features
    #' @param lambda integer, the number of lambda values - default is 100. By default it picks the best value from the model.
    #' @return vector, a vector containing predictions
    #'
    #' @examples
    #' \dontrun{
    #' LINK <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
    #' housing <- read.table(LINK)
    #' names <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
    #'            "RAD","TAX","PTRATIO","B","LSTAT","MEDV")
    #' names(housing)  <-  names

    #' lf <- LMTrainer$new(family = 'gaussian', alpha=1)
    #' lf$fit(X = housing, y = 'MEDV')
    #' predictions <- lf$cv_predict(df = housing)
    #' }

    predict = function(df, lambda = NULL){

        in_type <- ifelse(self$family == "binomial", "response","link")

        if(is.null(self$alpha)){
            return(stats::predict.glm(object = self$model,
                                  newdata = df,
                                  type = "response"))
        } else {
            if(is.null(lambda)) lambda <- min(self$model$lambda)
            return(glmnet::predict.glmnet(object = self$model,
                    newx = as.matrix(setDT(df)[, c(self$iid_names), with=F]),
                    s = lambda,
                    type = "link"))
        }
    },

    #' @details
    #' Train regression model using cross validation
    #'
    #' @param X data.frame containing test features
    #' @param y character, name of target variable
    #' @param nfolds integer, number of folds
    #' @param parallel logical, if do parallel computation. Default=FALSE
    #' @param type.measure character, evaluation metric type. Default = deviance
    #' @return NULL, trains the model and saves it in memory
    #'
    #' @examples
    #' \dontrun{
    #' LINK <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
    #' housing <- read.table(LINK)
    #' names <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
    #'            "RAD","TAX","PTRATIO","B","LSTAT","MEDV")
    #' names(housing)  <-  names

    #' lf <- LMTrainer$new(family = 'gaussian', alpha=1)
    #' lf$cv_model(X = housing, y = 'MEDV', nfolds = 5, parallel = FALSE)
    #' }

    cv_model = function(X, y, nfolds, parallel, type.measure="deviance"){

        superml::testdata(X, y)
        self$iid_names <- setdiff(colnames(X), y)
        if (isTRUE(parallel)){
            cl <- makeCluster(detectCores())
            doParallel::registerDoParallel(cl)
            message("Starting parallel clusters.")
        }

        self$weights <- rep(1, nrow(X))
        DX <- as.matrix(setDT(X)[, self$iid_names, with = FALSE])

        self$cvmodel <- glmnet::cv.glmnet(x = DX
                                  ,y = X[[y]]
                                  ,weights = self$weights
                                  ,lambda = NULL
                                  ,nfolds = nfolds
                                  ,parallel = parallel
                                  ,type.measure = type.measure
                                  )

        # this flag is for variable importance
        self$Flag <- TRUE
        message("Computation done.")
        if (isTRUE(parallel)) {
            message("Stopping clusters.")
            stopCluster(cl)
        }

    },

    #' @details
    #' Get predictions from the cross validated regression model
    #'
    #' @param df data.frame containing test features
    #' @param lambda integer, the number of lambda values - default is 100. By default it picks the best value from the model.
    #' @return vector a vector containing predicted values
    #'
    #' @examples
    #' \dontrun{
    #' LINK <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
    #' housing <- read.table(LINK)
    #' names <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
    #'            "RAD","TAX","PTRATIO","B","LSTAT","MEDV")
    #' names(housing)  <-  names

    #' lf <- LMTrainer$new(family = 'gaussian', alpha=1)
    #' lf$cv_model(X = housing, y = 'MEDV', nfolds = 5, parallel = FALSE)
    #' predictions <- lf$cv_predict(df = housing)
    #' }

    cv_predict = function(df, lambda=NULL){

        if (is.null(lambda)) {
            if (isTRUE(self$Flag)) lambda <- min(self$cvmodel$lambda.1se)
            else print("Please run the cv_model function.")
        }
        DX <- as.matrix(setDT(df)[, c(self$iid_names), with = FALSE])
        return(glmnet::predict.glmnet(object = self$cvmodel,
                              newx = DX,
                              s = lambda,
                              type = "response"))
    },

    #' @details
    #' Get feature importance using model coefficients
    #'
    #' @return a matrix containing feature coefficients
    #'
    #' @examples
    #' \dontrun{
    #' LINK <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
    #' housing <- read.table(LINK)
    #' names <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS",
    #'            "RAD","TAX","PTRATIO","B","LSTAT","MEDV")
    #' names(housing)  <-  names

    #' lf <- LMTrainer$new(family = 'gaussian', alpha=1)
    #' lf$cv_model(X = housing, y = 'MEDV', nfolds = 5, parallel = FALSE)
    #' predictions <- lf$cv_predict(df = housing)
    #' coefs <- lf$get_importance()
    #' }

    get_importance = function(){

        if (self$Flag) {
            return(as.matrix(coef(self$cvmodel)))
        } else {
            print("Feature selection only happens in Lasso Regression.
                  Please run the model with alpha=1.")
        }

    })

)

