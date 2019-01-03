#' Random Search CV
#' @description Given a set of hyper parameters, random search trainer provides a faster way of hyper parameter tuning.
#' Here, the number of models to be trained can be defined by the user.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' rst = RandomSearchTrainer$new(trainer, parameters, n_folds, scoring, n_iter)
#' rst$fit(X_train, "target")
#' rst$best_iteration(metric)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialises an instance of random search cv}
#'   \item{\code{$fit()}}{fit model to an input train data and trains the model.}
#'   \item{\code{$best_iteration()}}{returns best iteration based on a given metric. By default, uses the first scoring metric}
#' }
#' @section Arguments:
#' \describe{
#'  \item{trainer}{superml trainer object, must be either XGBTrainer, LMTrainer, RFTrainer, NBTrainer}
#'  \item{parameters}{list containing parameters}
#'  \item{n_folds}{number of folds to use to split the train data}
#'  \item{scoring}{scoring metric used to evaluate the best model, multiple values can be provided.
#'                currently supports: auc, accuracy, mse, rmse, logloss, mae, f1, precision, recall}
#'  \item{n_iter}{number of models to be trained}
#' }
#' @export
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
RandomSearchCV <- R6Class(
    "RandomSearchTrainer",
     inherit = GridSearchCV,
      public=list(
           n_iter = NA,
           initialize = function(trainer = NA,
                                 parameters = NA,
                                 n_folds = NA,
                                 scoring = NA,
                                 n_iter = NA) {
              super$initialize(trainer, parameters, n_folds, scoring)
              self$n_iter <- n_iter
           },

           fit = function(X,y){
               # use random search
               param_grid <- expand.grid(self$parameters)

               # check value of n_iter
               if(self$n_iter < 0)
                   stop("The value of n_iter must be > 0.")

               if(self$n_iter > nrow(param_grid))
                   stop("The value of n_iter cannot be greater
                        than total number of possible models.")

               param_grid <- param_grid[sample(nrow(param_grid), self$n_iter), ]
               print(sprintf("In total, %s models will be trained",
                             nrow(param_grid)))

               self$evaluation_scores <- self$runGrid(param_grid, X, y)

           }

    )
)
