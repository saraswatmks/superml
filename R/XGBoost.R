#' Extreme Gradient Boosting Trainer
#' @description Trains a Extreme Gradient Boosting Model. XGBoost belongs to a family of boosting algorithms
#'              that creates an ensemble of weak learner to learn about data.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' bst = XGBTrainer$new(booster, objective, nthread, silent=0, n_estimators=100, learning_rate=0.3,
#'                      gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1,
#'                      lambda=1, alpha = 0, eval_metric, print_every = 50, feval, early_stopping, maximize,
#'                      custom_objective, save_period, save_name, xgb_model, callbacks, verbose,
#'                      num_class, weight, na_missing)
#' bst$fit(X_train, "target", valid=NULL)
#' tfidf$predict(X_test)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialises an instance of xgboost model}
#'   \item{\code{$fit()}}{fits model to an input train data using given parametes.}
#'   \item{\code{$cross_val()}}{performs cross validation on train data}
#'   \item{\code{$predict()}}{returns predictions by fitting the trained model on test data.}
#' }
#' @section Arguments:
#' \describe{
#'  \item{params}{the detailed version of these arguments can found in xgboost documentation \url{http://xgboost.readthedocs.io/en/latest/parameter.html}}
#'  \item{booster}{the trainer type, the values are \code{gbtree(default)}, \code{gblinear}, \code{dart:gbtree}}
#'  \item{objective}{specify the learning task. Check the link above for all possible values.}
#'  \item{max_features}{the number of features to consider when looking for the best split
#'      Possible values are \code{auto},\code{sqrt},\code{log},\code{None}}
#'  \item{n_thread}{number of parallel threads used to run, default is to run using all threads available}
#'  \item{silent}{0 means printing running messages, 1 means silent mode}
#'  \item{n_estimators}{number of trees to grow, default = 100}
#'  \item{learning_rate}{Step size shrinkage used in update to prevents overfitting. Lower the learning rate, more time it takes in training, value lies between between 0 and 1. Default = 0.3}
#'  \item{max_depth}{the maximum depth of each tree, default = 6}
#'  \item{nfold}{set number of folds for cross validation}
#'  \item{stratified}{if stratified sampling to be done or not, default is TRUE}
#'  \item{folds}{the list of CV folds' indices - either those passed through the folds parameter or randomly generated.}
#'  \item{gamma}{Minimum loss reduction required to make a further partition on a leaf node of the tree. The larger gamma is, the more conservative the algorithm will be. Value lies between 0 and infinity, Default = 0}
#'  \item{min_child_weight}{Minimum sum of instance weight (hessian) needed in a child. If the tree partition step results in a leaf node with the sum of instance weight less than min_child_weight, then the building process will give up further partitioning. In linear regression task, this simply corresponds to minimum number of instances needed to be in each node. The larger min_child_weight is, the more conservative the algorithm will be. Value lies between 0 and infinity. Default = 1}
#'  \item{subsample}{Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees. and this will prevent overfitting. Subsampling will occur once in every boosting iteration. Value lies between 0 and 1. Default = 1}
#'  \item{colsample_bytree}{Subsample ratio of columns when constructing each tree. Subsampling will occur once in every boosting iteration. Value lies between 0 and 1. Default = 1}
#'  \item{lambda}{L2 regularization term on weights. Increasing this value will make model more conservative. Default = 1}
#'  \item{alpha}{L1 regularization term on weights. Increasing this value will make model more conservative. Default = 0}
#'  \item{eval_metric}{Evaluation metrics for validation data, a default metric will be assigned according to objective }
#'  \item{print_every}{print training log after n iterations. Default = 50}
#'  \item{feval}{custom evaluation function}
#'  \item{early_stopping}{Used to prevent overfitting, stops model training after this number of iterations if there is no improvement seen}
#'  \item{maximize}{If feval and early_stopping_rounds are set, then this parameter must be set as well. When it is TRUE, it means the larger the evaluation score the better. }
#'  \item{custom_objective}{custom objective function}
#'  \item{save_period}{when it is non-NULL, model is saved to disk after every save_period rounds, 0 means save at the end.}
#'  \item{save_name}{the name or path for periodically saved model file.}
#'  \item{xgb_model}{a previously built model to continue the training from. Could be either an object of class xgb.Booster, or its raw data, or the name of a file with a previously saved model.}
#'  \item{callbacks}{a list of callback functions to perform various task during boosting. See callbacks. Some of the callbacks are automatically created depending on the parameters' values. User can provide either existing or their own callback methods in order to customize the training process.}
#'  \item{verbose}{If 0, xgboost will stay silent. If 1, xgboost will print information of performance. If 2, xgboost will print some additional information. Setting verbose > 0 automatically engages the cb.evaluation.log and cb.print.evaluation callback functions.}
#'  \item{watchlist}{what information should be printed when verbose=1 or verbose=2. Watchlist is used to specify validation set monitoring during training. For example user can specify watchlist=list(validation1=mat1, validation2=mat2) to watch the performance of each round's model on mat1 and mat2}
#'  \item{num_class}{set number of classes in case of multiclassification problem}
#'  \item{weight}{a vector indicating the weight for each row of the input.}
#'  \item{na_missing}{by default is set to NA, which means that NA values should be considered as 'missing' by the algorithm. Sometimes, 0 or other extreme value might be used to represent missing values. This parameter is only used when input is a dense matrix.}
#' }
#' @export
#' @examples
#' library(data.table)
#' df <- copy(iris)
#'
#' # convert characters/factors to numeric
#' df$Species <- as.numeric(as.factor(df$Species))-1
#'
#' # initialise model
#' xgb <- XGBTrainer$new(objective = 'multi:softmax',
#'                       maximize = FALSE,
#'                       eval_metric = 'merror',
#'                       num_class=3,
#'                       n_estimators = 2)
#' xgb$fit(df, 'Species')
#'
#' # do cross validation to find optimal value for n_estimators
#' xgb$cross_val(X = df, y = 'Species',nfolds = 3, stratified = TRUE,
#'               early_stopping = 1)
#' best_iter <- xgb$cv_model$best_iteration
#' xgb$show_importance()
#'
#' # make predictions
#' preds <- xgb$predict(as.matrix(iris[,1:4]))
#' preds

XGBTrainer <- R6Class(
    "XGBTrainer",
    public = list(

        booster = "gbtree",
        objective = "reg:linear",
        nthread = parallel::detectCores(),
        silent = 0,
        n_estimators = 100,
        learning_rate = 0.3,
        gamma = 0,
        max_depth = 6,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 1,
        lambda = 1,
        alpha = 0,
        eval_metric = NULL,
        print_every = 50,
        feval = NULL,
        early_stopping = NULL,
        maximize = NULL,
        custom_objective = NULL,
        save_period = NULL,
        save_name = NULL,
        xgb_model = NULL,
        callbacks = NULL,
        verbose = 1,
        watchlist = list(),
        num_class = NULL,
        weight = NULL,
        na_missing = NULL,
        # this stores the feature names for model importance
        feature_names = NULL,
        cv_model = NULL,

        initialize = function(booster,
                              objective,
                              nthread,
                              silent,
                              n_estimators,
                              learning_rate,
                              gamma,
                              max_depth,
                              min_child_weight,
                              subsample,
                              colsample_bytree,
                              lambda,
                              alpha,
                              eval_metric,
                              print_every,
                              feval,
                              early_stopping,
                              maximize,
                              custom_objective,
                              save_period,
                              save_name,
                              xgb_model,
                              callbacks,
                              verbose,
                              num_class,
                              weight,
                              na_missing
                              ) {

            if(!(missing(booster))) self$booster <- booster
            if(!(missing(objective))) self$objective <- objective
            if(!(missing(nthread))) self$nthread <- nthread
            if(!(missing(silent))) self$silent <- silent
            if(!(missing(n_estimators))) self$n_estimators <- n_estimators
            if(!(missing(learning_rate))) self$learning_rate <- learning_rate
            if(!(missing(gamma))) self$gamma <- gamma
            if(!(missing(max_depth))) self$max_depth <- max_depth
            if(!(missing(min_child_weight))) self$min_child_weight <- min_child_weight
            if(!(missing(subsample))) self$subsample <- subsample
            if(!(missing(colsample_bytree))) self$colsample_bytree <- colsample_bytree
            if(!(missing(lambda))) self$lambda <- lambda
            if(!(missing(alpha))) self$alpha <- alpha
            if(!(missing(eval_metric))) self$eval_metric <- eval_metric
            if(!(missing(print_every))) self$print_every <- print_every
            if(!(missing(feval))) self$feval <- feval
            if(!(missing(early_stopping))) self$early_stopping <- early_stopping
            if(!(missing(maximize))) self$maximize <- maximize
            if(!(missing(custom_objective))) self$custom_objective <- custom_objective
            if(!(missing(save_period))) self$save_period <- save_period
            if(!(missing(save_name))) self$save_name <- save_name
            if(!(missing(xgb_model))) self$xgb_model <- xgb_model
            if(!(missing(callbacks))) self$callbacks <- callbacks
            if(!(missing(verbose))) self$verbose <- verbose
            if(!(missing(num_class))) self$num_class <- num_class
            if(!(missing(weight))) self$weight <- weight
            if(!(missing(na_missing))) self$na_missing <- na_missing

        },

        cross_val = function(X,
                             y,
                             nfolds=5,
                             stratified=TRUE,
                             folds=NULL,
                             early_stopping=50){

            self$feature_names <- setdiff(names(X), y)

            params_list <- list(booster = self$booster,
                               objective = self$objective,
                               nthread = self$nthread,
                               silent = self$thread,
                               nrounds = self$n_estimators,
                               eta = self$learning_rate,
                               gamma = self$gamma,
                               max_depth = self$max_depth,
                               min_child_weight = self$min_child_weight,
                               subsample = self$subsample,
                               colsample_bytree = self$colsample_bytree,
                               lambda = self$lambda,
                               alpha = self$alpha,
                               eval_metric = self$eval_metric,
                               num_class = self$num_class)


            all_data <- private$prepare_data_train(X, y)

            message("starting with cv training...")
            self$cv_model <- xgb.cv(params = params_list
                                     , data = all_data$train
                                     , nrounds = self$n_estimators
                                     , nfold = nfolds
                                     , missing = self$na_missing
                                     , stratified = stratified
                                     , folds = folds
                                     , verbose = self$verbose
                                     , print_every_n = self$print_every
                                     , early_stopping_rounds = early_stopping
                                     , maximize = self$maximize
                                     , callbacks = self$callbacks
                                     , feval = self$feval)

        },


        fit = function(X, y, valid = NULL) {

            self$feature_names <- setdiff(names(X), y)
            params_list <- list(booster = self$booster,
                               objective = self$objective,
                               nthread = self$nthread,
                               silent = self$thread,
                               nrounds = self$n_estimators,
                               eta = self$learning_rate,
                               gamma = self$gamma,
                               max_depth = self$max_depth,
                               min_child_weight = self$min_child_weight,
                               subsample = self$subsample,
                               colsample_bytree = self$colsample_bytree,
                               lambda = self$lambda,
                               alpha = self$alpha,
                               eval_metric = self$eval_metric,
                               num_class = self$num_class)

            # remove null parameters from the param_list
            null_params <- names(which(vapply(params_list,
                                              is.null,
                                              FUN.VALUE = logical(1))))

            if (length(null_params) > 0){
                for(x in null_params){
                    params_list[[x]] <- NULL
                }
            }


            # generate data for training
            all_data <- private$prepare_data_train(X, y, valid)


            message("starting with training...")
            private$trained_model <- xgb.train(params = params_list
                                                , data = all_data$train
                                                , nrounds = self$n_estimators
                                                , watchlist = all_data
                                                , print_every_n = self$print_every
                                                , early_stopping_rounds = self$early_stopping
                                                , maximize = self$maximize
                                                , save_period = self$save_period
                                                , save_name = self$save_name
                                                , xgb_model = self$xgb_model
                                                , callbacks = self$callbacks
                                                , verbose = self$verbose)


        },

        predict = function(df) {

            # pretty important for subset data here
            # must subset data using features on which model was trained
            # if we don't do it, the prediction changes completely and gets
            # worse.

            df <- as.data.table(df)
            dtest <- xgb.DMatrix(data = as.matrix(df[, self$feature_names, with=FALSE]))

            return(stats::predict(private$trained_model, dtest))
        },

        show_importance = function(type="plot", topn=10){

            mat <- xgb.importance(feature_names = self$feature_names,
                                  model = private$trained_model)
            if(type == "plot"){
                xgb.plot.importance(importance_matrix = mat, top_n = topn)
            } else if (type == "table"){
                return(mat)
            }

        }

    ),

    private = list(

        trained_model = NA,

        prepare_data_train = function(X, y, valid= NULL) {
            if (!(inherits(X, c("data.table", "data.frame"))))
                stop("Your data format should be a data.table or data.frame.")

            if (!(y %in% names(X)))
                stop(strwrap("The dependent variable y is not available
                     in the data set."))

            if(any(!(vapply(X, is.numeric, FUN.VALUE = logical(1)))))
                stop(strwrap("The data contains character or categorical variable.
                     Please convert it to numeric or integer"))

            if(!(is.numeric(X[[y]])))
                stop("The dependent variable is not numeric.")

            if(!(is.null(valid))){

                if(ncol(valid) != ncol(X))
                    stop("The validation and train data set have
                         unequal number of columns.")

                if(!all(colnames(X) %in% colnames(valid)))
                    stop(strwrap("Train and validation data has some issue
                         in column names.Make sure they are same."))
            }

            message("converting the data into xgboost format..")

            X <- as.data.table(X)
            if(!(is.null(valid))){
                valid <- as.data.table(valid)
                dtrain <- xgb.DMatrix(data = as.matrix(X[, setdiff(names(X), y), with=FALSE]),
                                      label = X[[y]])
                dvalid <- xgb.DMatrix(data = as.matrix(valid[, setdiff(names(valid), y), with=FALSE]),
                                      label = valid[[y]])
                return (list(train = dtrain,
                             val = dvalid))

            } else {
                dtrain <-xgb.DMatrix(data = as.matrix(
                    X[, setdiff(names(X), y), with=FALSE]), label = X[[y]])
                return (list(train = dtrain))
            }

        }
    )
)

