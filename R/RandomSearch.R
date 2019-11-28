#' Random Search CV
#'
#' Hyperparameter tuning using random search scheme.
#'
#' @details
#' Given a set of hyper parameters, random search trainer provides a faster way of hyper parameter tuning.
#' Here, the number of models to be trained can be defined by the user.
#'
#' @export

RandomSearchCV <- R6Class(
    "RandomSearchTrainer",
     inherit = GridSearchCV,
      public = list(

          #' @field n_iter number of models to be trained
           n_iter = NA,

           #' @details
           #' Create a new `RandomSearchTrainer` object.
           #'
           #' @param trainer superml trainer object, must be either XGBTrainer, LMTrainer, RFTrainer, NBTrainer
           #' @param parameters list, list containing parameters
           #' @param n_folds integer, number of folds to use to split the train data
           #' @param scoring character, scoring metric used to evaluate the best model, multiple values can be provided.
           #'                currently supports: auc, accuracy, mse, rmse, logloss, mae, f1, precision, recall
           #' @param n_iter integer, number of models to be trained
           #'
           #' @return A `RandomSearchTrainer` object.
           #'
           #' @examples
           #' rf <- RFTrainer$new()
           #' rst <-RandomSearchCV$new(trainer = rf,
           #'                             parameters = list(n_estimators = c(100,500),
           #'                             max_depth = c(5,2,10,14)),
           #'                             n_folds = 3,
           #'                             scoring = c('accuracy','auc'),
           #'                             n_iter = 4)

           initialize = function(trainer = NA,
                                 parameters = NA,
                                 n_folds = NA,
                                 scoring = NA,
                                 n_iter = NA) {
              super$initialize(trainer, parameters, n_folds, scoring)
              self$n_iter <- n_iter
           },

           #' @details
           #' Train the model on given hyperparameters
           #'
           #' @param X data.frame containing features
           #' @param y character, name of target variable
           #' @return NULL, tunes hyperparameters and stores the result in memory
           #'
           #' @examples
           #' rf <- RFTrainer$new()
           #' rst <-RandomSearchCV$new(trainer = rf,
           #'                             parameters = list(n_estimators = c(100,500),
           #'                             max_depth = c(5,2,10,14)),
           #'                             n_folds = 3,
           #'                             scoring = c('accuracy','auc'),
           #'                             n_iter = 4)
           #' data("iris")
           #' rst$fit(iris, "Species")
           #' rst$best_iteration()

           fit = function(X,y){
               # use random search

               param_grid <- expand.grid(self$parameters)

               # check value of n_iter
               if (self$n_iter < 0)
                   stop("The value of n_iter must be > 0.")

               if (self$n_iter > nrow(param_grid))
                   stop("The value of n_iter cannot be greater
                        than total number of possible models.")

               param_grid <- param_grid[sample(nrow(param_grid), self$n_iter), ]
               print(sprintf("In total, %s models will be trained",
                             nrow(param_grid)))

               self$evaluation_scores <- private$runGrid(param_grid, X, y)

           }

    )
)
