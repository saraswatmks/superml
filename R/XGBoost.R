#' Extreme Gradient Boosting Trainer
#'
#' Trains a XGBoost model in R
#'
#' @details
#' Trains a Extreme Gradient Boosting Model. XGBoost belongs to a family of boosting algorithms
#' that creates an ensemble of weak learner to learn about data. It is a wrapper for original xgboost
#' R package, you can find the documentation here: \url{http://xgboost.readthedocs.io/en/latest/parameter.html}
#'
#' @export

XGBTrainer <- R6Class(
    "XGBTrainer",
    public = list(

        #' @field booster the trainer type, the values are \code{gbtree(default)}, \code{gblinear}, \code{dart:gbtree}
        booster = "gbtree",
        #' @field objective specify the learning task. Check the link above for all possible values.
        objective = "reg:linear",
        #' @field nthread number of parallel threads used to run, default is to run using all threads available
        nthread = parallel::detectCores(),
        #' @field silent 0 means printing running messages, 1 means silent mode
        silent = 0,
        #' @field n_estimators number of trees to grow, default = 100
        n_estimators = 100,
        #' @field learning_rate Step size shrinkage used in update to prevents overfitting. Lower the learning rate, more time it takes in training, value lies between between 0 and 1. Default = 0.3
        learning_rate = 0.3,
        #' @field gamma Minimum loss reduction required to make a further partition on a leaf node of the tree. The larger gamma is, the more conservative the algorithm will be. Value lies between 0 and infinity, Default = 0
        gamma = 0,
        #' @field max_depth the maximum depth of each tree, default = 6
        max_depth = 6,
        #' @field min_child_weight Minimum sum of instance weight (hessian) needed in a child. If the tree partition step results in a leaf node with the sum of instance weight less than min_child_weight, then the building process will give up further partitioning. In linear regression task, this simply corresponds to minimum number of instances needed to be in each node. The larger min_child_weight is, the more conservative the algorithm will be. Value lies between 0 and infinity. Default = 1
        min_child_weight = 1,
        #' @field subsample Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees. and this will prevent overfitting. Subsampling will occur once in every boosting iteration. Value lies between 0 and 1. Default = 1
        subsample = 1,
        #' @field colsample_bytree Subsample ratio of columns when constructing each tree. Subsampling will occur once in every boosting iteration. Value lies between 0 and 1. Default = 1
        colsample_bytree = 1,
        #' @field lambda L2 regularization term on weights. Increasing this value will make model more conservative. Default = 1
        lambda = 1,
        #' @field alpha L1 regularization term on weights. Increasing this value will make model more conservative. Default = 0
        alpha = 0,
        #' @field eval_metric Evaluation metrics for validation data, a default metric will be assigned according to objective
        eval_metric = NULL,
        #' @field print_every print training log after n iterations. Default = 50
        print_every = 50,
        #' @field feval custom evaluation function
        feval = NULL,
        #' @field early_stopping Used to prevent overfitting, stops model training after this number of iterations if there is no improvement seen
        early_stopping = 50,
        #' @field maximize If feval and early_stopping_rounds are set, then this parameter must be set as well. When it is TRUE, it means the larger the evaluation score the better.
        maximize = NULL,
        #' @field custom_objective custom objective function
        custom_objective = NULL,
        #' @field save_period when it is non-NULL, model is saved to disk after every save_period rounds, 0 means save at the end.
        save_period = NULL,
        #' @field save_name the name or path for periodically saved model file.
        save_name = NULL,
        #' @field xgb_model a previously built model to continue the training from. Could be either an object of class xgb.Booster, or its raw data, or the name of a file with a previously saved model.
        xgb_model = NULL,
        #' @field callbacks a list of callback functions to perform various task during boosting. See callbacks. Some of the callbacks are automatically created depending on the parameters' values. User can provide either existing or their own callback methods in order to customize the training process.
        callbacks = NULL,
        #' @field verbose If 0, xgboost will stay silent. If 1, xgboost will print information of performance. If 2, xgboost will print some additional information. Setting verbose > 0 automatically engages the cb.evaluation.log and cb.print.evaluation callback functions.
        verbose = 1,
        #' @field watchlist what information should be printed when verbose=1 or verbose=2. Watchlist is used to specify validation set monitoring during training. For example user can specify watchlist=list(validation1=mat1, validation2=mat2) to watch the performance of each round's model on mat1 and mat2
        watchlist = list(),
        #' @field num_class set number of classes in case of multiclassification problem
        num_class = NULL,
        #' @field weight a vector indicating the weight for each row of the input.
        weight = NULL,
        #' @field na_missing by default is set to NA, which means that NA values should be considered as 'missing' by the algorithm. Sometimes, 0 or other extreme value might be used to represent missing values. This parameter is only used when input is a dense matrix.
        na_missing = NULL,
        #' @field feature_names internal use, stores the feature names for model importance
        feature_names = NULL,
        #' @field cv_model internal use
        cv_model = NULL,


        #' @details
        #' Create a new `XGBTrainer` object.
        #'
        #' @param booster the trainer type, the values are \code{gbtree(default)}, \code{gblinear}, \code{dart:gbtree}
        #' @param objective specify the learning task. Check the link above for all possible values.
        #' @param nthread number of parallel threads used to run, default is to run using all threads available
        #' @param silent 0 means printing running messages, 1 means silent mode
        #' @param n_estimators number of trees to grow, default = 100
        #' @param learning_rate Step size shrinkage used in update to prevents overfitting. Lower the learning rate, more time it takes in training, value lies between between 0 and 1. Default = 0.3
        #' @param gamma Minimum loss reduction required to make a further partition on a leaf node of the tree. The larger gamma is, the more conservative the algorithm will be. Value lies between 0 and infinity, Default = 0
        #' @param max_depth the maximum depth of each tree, default = 6
        #' @param min_child_weight Minimum sum of instance weight (hessian) needed in a child. If the tree partition step results in a leaf node with the sum of instance weight less than min_child_weight, then the building process will give up further partitioning. In linear regression task, this simply corresponds to minimum number of instances needed to be in each node. The larger min_child_weight is, the more conservative the algorithm will be. Value lies between 0 and infinity. Default = 1
        #' @param subsample Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees. and this will prevent overfitting. Subsampling will occur once in every boosting iteration. Value lies between 0 and 1. Default = 1
        #' @param colsample_bytree Subsample ratio of columns when constructing each tree. Subsampling will occur once in every boosting iteration. Value lies between 0 and 1. Default = 1
        #' @param lambda L2 regularization term on weights. Increasing this value will make model more conservative. Default = 1
        #' @param alpha L1 regularization term on weights. Increasing this value will make model more conservative. Default = 0
        #' @param eval_metric Evaluation metrics for validation data, a default metric will be assigned according to objective
        #' @param print_every print training log after n iterations. Default = 50
        #' @param feval custom evaluation function
        #' @param early_stopping Used to prevent overfitting, stops model training after this number of iterations if there is no improvement seen
        #' @param maximize If feval and early_stopping_rounds are set, then this parameter must be set as well. When it is TRUE, it means the larger the evaluation score the better.
        #' @param custom_objective custom objective function
        #' @param save_period when it is non-NULL, model is saved to disk after every save_period rounds, 0 means save at the end.
        #' @param save_name the name or path for periodically saved model file.
        #' @param xgb_model a previously built model to continue the training from. Could be either an object of class xgb.Booster, or its raw data, or the name of a file with a previously saved model.
        #' @param callbacks a list of callback functions to perform various task during boosting. See callbacks. Some of the callbacks are automatically created depending on the parameters' values. User can provide either existing or their own callback methods in order to customize the training process.
        #' @param verbose If 0, xgboost will stay silent. If 1, xgboost will print information of performance. If 2, xgboost will print some additional information. Setting verbose > 0 automatically engages the cb.evaluation.log and cb.print.evaluation callback functions.
        #' @param num_class set number of classes in case of multiclassification problem
        #' @param weight a vector indicating the weight for each row of the input.
        #' @param na_missing by default is set to NA, which means that NA values should be considered as 'missing' by the algorithm. Sometimes, 0 or other extreme value might be used to represent missing values. This parameter is only used when input is a dense matrix.
        #'
        #' @return A `XGBTrainer` object.
        #'
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

            if (!(missing(booster))) self$booster <- booster
            if (!(missing(objective))) self$objective <- objective
            if (!(missing(nthread))) self$nthread <- nthread
            if (!(missing(silent))) self$silent <- silent
            if (!(missing(n_estimators))) self$n_estimators <- n_estimators
            if (!(missing(learning_rate))) self$learning_rate <- learning_rate
            if (!(missing(gamma))) self$gamma <- gamma
            if (!(missing(max_depth))) self$max_depth <- max_depth
            if (!(missing(min_child_weight))) self$min_child_weight <- min_child_weight
            if (!(missing(subsample))) self$subsample <- subsample
            if (!(missing(colsample_bytree))) self$colsample_bytree <- colsample_bytree
            if (!(missing(lambda))) self$lambda <- lambda
            if (!(missing(alpha))) self$alpha <- alpha
            if (!(missing(eval_metric))) self$eval_metric <- eval_metric
            if (!(missing(print_every))) self$print_every <- print_every
            if (!(missing(feval))) self$feval <- feval
            if (!(missing(early_stopping))) self$early_stopping <- early_stopping
            if (!(missing(maximize))) self$maximize <- maximize
            if (!(missing(custom_objective))) self$custom_objective <- custom_objective
            if (!(missing(save_period))) self$save_period <- save_period
            if (!(missing(save_name))) self$save_name <- save_name
            if (!(missing(xgb_model))) self$xgb_model <- xgb_model
            if (!(missing(callbacks))) self$callbacks <- callbacks
            if (!(missing(verbose))) self$verbose <- verbose
            if (!(missing(num_class))) self$num_class <- num_class
            if (!(missing(weight))) self$weight <- weight
            if (!(missing(na_missing))) self$na_missing <- na_missing
            superml::check_package("xgboost")

        },

        #' @details
        #' Trains the xgboost model using cross validation scheme
        #'
        #' @param X data.frame
        #' @param y character, name of target variable
        #' @param nfolds integer, number of folds
        #' @param stratified logical, whether to use stratified sampling
        #' @param folds the list of CV folds' indices - either those passed through the folds parameter or randomly generated.
        #' @return NULL, trains a model and saves it in memory
        #'
        #' @examples
        #' \dontrun{
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
        #'
        #' # do cross validation to find optimal value for n_estimators
        #' xgb$cross_val(X = df, y = 'Species',nfolds = 3, stratified = TRUE)
        #' }

        cross_val = function(X,
                             y,
                             nfolds=5,
                             stratified=TRUE,
                             folds=NULL){

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
            self$cv_model <- xgboost::xgb.cv(params = params_list
                                     , data = all_data$train
                                     , nrounds = self$n_estimators
                                     , nfold = nfolds
                                     , missing = self$na_missing
                                     , stratified = stratified
                                     , folds = folds
                                     , verbose = self$verbose
                                     , print_every_n = self$print_every
                                     , early_stopping_rounds = self$early_stopping
                                     , maximize = self$maximize
                                     , callbacks = self$callbacks
                                     , feval = self$feval)

        },

        #' @details
        #' Fits the xgboost model on given data
        #'
        #' @param X data.frame, training data
        #' @param y character, name of target variable
        #' @param valid data.frame, validation data
        #' @return NULL, trains a model and keeps it in memory
        #'
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

            if (length(null_params) > 0) {
                for (x in null_params) {
                    params_list[[x]] <- NULL
                }
            }


            # generate data for training
            all_data <- private$prepare_data_train(X, y, valid)


            message("starting with training...")
            private$trained_model <- xgboost::xgb.train(params = params_list
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

        #' @details
        #' Returns predicted values for a given test data
        #'
        #' @param df data.frame, test data set
        #' @return xgboost predictions
        #'
        #' @examples
        #' #' library(data.table)
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
        #' # make predictions
        #' preds <- xgb$predict(as.matrix(iris[,1:4]))

        predict = function(df) {

            # pretty important for subset data here
            # must subset data using features on which model was trained
            # if we don't do it, the prediction changes completely and gets
            # worse.

            df <- as.data.table(df)
            dtest <- xgboost::xgb.DMatrix(data = as.matrix(df[, self$feature_names, with = FALSE]))

            return(stats::predict(private$trained_model, dtest))
        },

        #' @details
        #' Shows feature importance plot
        #'
        #' @param type character, could be 'plot' or 'table'
        #' @param topn integer, top n features to display
        #' @return a table or a plot of feature importance
        #'
        #' @examples
        #' \dontrun{
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
        #' xgb$show_importance()
        #' }

        show_importance = function(type="plot", topn=10){

            mat <- xgboost::xgb.importance(feature_names = self$feature_names,
                                  model = private$trained_model)
            if (type == "plot") {
                xgboost::xgb.plot.importance(importance_matrix = mat, top_n = topn)
            } else if (type == "table") {
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

            if (any(!(vapply(X, is.numeric, FUN.VALUE = logical(1)))))
                stop(strwrap("The data contains character or categorical variable.
                     Please convert it to numeric or integer"))

            if (!(is.numeric(X[[y]])))
                stop("The dependent variable is not numeric.")

            if (!(is.null(valid))) {

                if (ncol(valid) != ncol(X))
                    stop("The validation and train data set have
                         unequal number of columns.")

                if (!all(colnames(X) %in% colnames(valid)))
                    stop(strwrap("Train and validation data has some issue
                         in column names.Make sure they are same."))
            }

            message("converting the data into xgboost format..")

            X <- as.data.table(X)
            if (!(is.null(valid))) {
                valid <- as.data.table(valid)
                dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X[, setdiff(names(X), y), with = FALSE]),
                                      label = X[[y]])
                dvalid <- xgboost::xgb.DMatrix(data = as.matrix(valid[, setdiff(names(valid), y), with = FALSE]),
                                      label = valid[[y]])
                return(list(train = dtrain,
                             val = dvalid))

            } else {
                dtrain <- xgboost::xgb.DMatrix(data = as.matrix(
                    X[, setdiff(names(X), y), with = FALSE]), label = X[[y]])
                return(list(train = dtrain))
            }

        }
    )
)

