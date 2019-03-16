#' Grid Search CV
#' @description Grid search CV is used to train a machine learning model with multiple combinations
#' of training hyper parameters and finds the best combination of parameters which optimizes the evaluation metric.
#' It creates an exhaustive set of hyperparameter combinations and train model on each combination.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' gst = GridSearchTrainer$new(trainer, parameters, n_folds, scoring)
#' gst$fit(X, y)
#' gst$best_iteration(metric)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialises an instance of grid search cv}
#'   \item{\code{$fit()}}{fit model to an input train data and trains the model.}
#'   \item{\code{$best_iteration()}}{returns best iteration based on a given metric. By default, uses the first scoring metric}
#' }
#' @section Arguments:
#' \describe{
#'  \item{trainer}{superml trainer object, could be either XGBTrainer, RFTrainer, NBTrainer etc.}
#'  \item{parameters}{list containing parameters}
#'  \item{n_folds}{number of folds to use to split the train data}
#'  \item{scoring}{scoring metric used to evaluate the best model, multiple values can be provided.
#'                currently supports: auc, accuracy, mse, rmse, logloss, mae, f1, precision, recall}
#' }
#' @export
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
GridSearchCV <- R6Class(
    "GridSearchCV",
    public = list(
        trainer = NA,
        # is a initialised class
        parameters = NA, # is a list # p = list(a = c(), b = c(), c = c())
        n_folds = NA,
        scoring = NA, # should be a vector of scoring metrics
        evaluation_scores = NA,

        initialize = function(trainer = NA,
                              parameters = NA,
                              n_folds = NA,
                              scoring = NA
                              ) {
            self$trainer <- trainer
            self$parameters <- parameters
            self$n_folds <- n_folds
            self$scoring <- scoring
        },

        get_metrics = function(metric, act, pred) {
            if (metric == "auc") {
                return (auc(act, pred))
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
        },

        runGrid = function(param_grid, X, y) {
            # create output data frame
             # we add index to avoid replacement error
            out_df <- data.frame()
            ff <- do.call(rbind, apply(param_grid, 1, function(row) {

                df <- data.frame(index=0)

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
                                               self$get_metrics(j,
                                                                x_valid[[y]],
                                                                preds))
                    }

                }

                for(name in names(folds_metric)){
                    df[[paste0(name,'_avg')]] <- mean(folds_metric[[name]])
                    df[[paste0(name,'_sd')]] <- sd(folds_metric[[name]])
                }
                return (subset(df, select = -c(index)))
            }))

            return (ff)

        },

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
            if(is.character(X[[y]]) | is.factor(X[[y]])){
                lbl <- LabelEncoder$new()
                X[[y]] <- lbl$fit_transform(X[[y]])
            }


            # use grid search
            print('entering grid search')
            param_grid <- expand.grid(self$parameters)
            print(sprintf("In total, %s models will be trained",
                          nrow(param_grid)))

            self$evaluation_scores <- self$runGrid(param_grid, X, y)

        },

        best_iteration = function(metric=NULL){

            if(is.null(metric)){
                metric <- self$scoring[1]
            }

            return (as.list(
                self$evaluation_scores[
                    which.max(self$evaluation_scores[[
                        paste0(metric,"_avg")]]), ]))
        }

    ),
    private = list(
        # skip prepare_data, assume the data given is in correct format
        available_trainers = c("XGBTrainer",
                               "RFTrainer",
                               "NBTrainer")
    )
)

