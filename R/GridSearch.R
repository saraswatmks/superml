#' Grid Search CV
#'
#' Runs grid search cross validation scheme to find best model training parameters.
#'
#' @details
#' Grid search CV is used to train a machine learning model with multiple combinations
#' of training hyper parameters and finds the best combination of parameters which optimizes the evaluation metric.
#' It creates an exhaustive set of hyperparameter combinations and train model on each combination.
#'
#' @export
GridSearchCV <- R6Class(
    "GridSearchCV",
    public = list(
        #' @field trainer superml trainer object, could be either XGBTrainer, RFTrainer, NBTrainer etc.
        trainer = NA,
        #' @field parameters a list of parameters to tune
        parameters = NA,
        #' @field n_folds number of folds to use to split the train data
        n_folds = NA,
        #' @field scoring scoring metric used to evaluate the best model, multiple values can be provided. currently supports: auc, accuracy, mse, rmse, logloss, mae, f1, precision, recall
        scoring = NA,
        #' @field evaluation_scores parameter for internal use
        evaluation_scores = NA,

        #' @details
        #' Create a new `GridSearchCV` object.
        #'
        #' @param trainer superml trainer object, could be either XGBTrainer, RFTrainer, NBTrainer etc.
        #' @param parameters list, a list of parameters to tune
        #' @param n_folds integer, number of folds to use to split the train data
        #' @param scoring character, scoring metric used to evaluate the best model, multiple values can be provided.
        #' currently supports: auc, accuracy, mse, rmse, logloss, mae, f1, precision, recall
        #'
        #' @return A `GridSearchCV` object.
        #'
        #' @examples
        #' rf <- RFTrainer$new()
        #' gst <-GridSearchCV$new(trainer = rf,
        #'                       parameters = list(n_estimators = c(100),
        #'                                         max_depth = c(5,2,10)),
        #'                                         n_folds = 3,
        #'                                         scoring = c('accuracy','auc'))

        initialize = function(trainer = NA,
                              parameters = NA,
                              n_folds = NA,
                              scoring = NA) {
            self$trainer <- trainer
            self$parameters <- parameters
            self$n_folds <- n_folds
            self$scoring <- scoring
        },

        #' @details
        #' Trains the model using grid search
        #'
        #' @param X data.frame or data.table
        #' @param y character, name of target variable
        #' @return NULL
        #'
        #' @examples
        #' rf <- RFTrainer$new()
        #' gst <-GridSearchCV$new(trainer = rf,
        #'                       parameters = list(n_estimators = c(100),
        #'                                         max_depth = c(5,2,10)),
        #'                                         n_folds = 3,
        #'                                         scoring = c('accuracy','auc'))
        #' data("iris")
        #' gst$fit(iris, "Species")

        fit = function(X, y) {
            # check trainer if it is valid
            if (!(class(self$trainer)[1] %in% private$available_trainers))
                stop(sprintf(
                    strwrap(
                        "Detected trainer is invalid.
                        It should belong to one of %s classes"
                    ),
                    paste(private$available_trainers, collapse = ",")
                ))

            # convert if the dependent variable is a factor or character
            if (is.character(X[[y]]) | is.factor(X[[y]])) {
                lbl <- LabelEncoder$new()
                X[[y]] <- lbl$fit_transform(X[[y]])
            }


            # use grid search
            print('entering grid search')
            param_grid <- expand.grid(self$parameters)
            print(sprintf("In total, %s models will be trained",
                          nrow(param_grid)))

            self$evaluation_scores <- private$runGrid(param_grid, X, y)

        },

        #' @details
        #' Returns the best parameters
        #'
        #' @param metric character, which metric to use for evaluation
        #' @return a list of best parameters
        #'
        #' @examples
        #' rf <- RFTrainer$new()
        #' gst <-GridSearchCV$new(trainer = rf,
        #'                       parameters = list(n_estimators = c(100),
        #'                                         max_depth = c(5,2,10)),
        #'                                         n_folds = 3,
        #'                                         scoring = c('accuracy','auc'))
        #' data("iris")
        #' gst$fit(iris, "Species")
        #' gst$best_iteration()

        best_iteration = function(metric = NULL) {
            if (is.null(metric)) {
                metric <- self$scoring[1]
            }

            return(as.list(self$evaluation_scores[which.max(self$evaluation_scores[[paste0(metric, "_avg")]]), ]))
        }

    ),
    private = list(# skip prepare_data, assume the data given is in correct format
        available_trainers = c("XGBTrainer",
                               "RFTrainer",
                               "NBTrainer"),

    runGrid = function(param_grid, X, y) {
        # create output data frame
        # we add index to avoid replacement error

        ff <- do.call(rbind, apply(param_grid, 1, function(row) {
            df <- data.frame(index = 0)

            for (name in names(row)) {
                self$trainer[[name]] <- row[name]
                df[[name]] <- row[name]
            }

            # create folds
            set.seed(10)
            mfolds <-  caret::createFolds(y = X[[y]],
                                          k = self$n_folds,
                                          list = T)

            # get values
            folds_metric <- vector("list", length(self$scoring))
            names(folds_metric) <- self$scoring


            for (f in mfolds) {
                x_train <- as.data.table(X)[-f]
                x_valid <- as.data.table(X)[f]

                # now train the model
                m <- self$trainer$fit(x_train, y)
                preds <- self$trainer$predict(x_valid)

                for (j in self$scoring) {
                    folds_metric[[j]] <- c(folds_metric[[j]],
                                           private$get_metrics(j,
                                                            x_valid[[y]],
                                                            preds))
                }

            }

            for (name in names(folds_metric)) {
                df[[paste0(name, '_avg')]] <- mean(folds_metric[[name]])
                df[[paste0(name, '_sd')]] <-
                    sd(folds_metric[[name]])
            }
            return(subset(df, select = -c(index)))
        }))

        return(ff)

    },

    get_metrics = function(metric, act, pred) {
        if (metric == "auc") {
            return(auc(act, pred))
        } else if (metric == "accuracy") {
            return(accuracy(act, pred))
        } else if (metric == "mse") {
            return(mse(act, pred))
        } else if (metric == "rmse") {
            return(rmse(act, pred))
        } else if (metric == "logloss") {
            return (logLoss(act, pred))
        } else if (metric == "mae") {
            return(mae(act, pred))
        } else if (metric == "f1") {
            return(f1(act, pred))
        } else if (metric == "precision") {
            return(precision(act, pred))
        } else if (metric == "recall") {
            return(recall(act, pred))
        }
    }
))
