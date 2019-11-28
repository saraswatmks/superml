#' Random Forest Trainer
#'
#' Trains a random forest model.
#'
#' @details
#' Trains a Random Forest model. A random forest is a meta estimator that fits a number of decision tree classifiers on various sub-samples of the dataset and use averaging to improve the predictive accuracy and control over-fitting.
#' This implementation uses ranger R package which provides faster model training.
#'
#' @export

RFTrainer <- R6Class("RFTrainer",
      public = list(
          #' @field n_estimators the number of trees in the forest, default= 100
          n_estimators = 100,
          #' @field max_features the number of features to consider when looking for the best split.
          #'                      Possible values are \code{auto(default)} takes sqrt(num_of_features),
          #'                          \code{sqrt} same as auto,
          #'                          \code{log} takes log(num_of_features),
          #'                          \code{none} takes all features
          max_features = "auto",
          #' @field max_depth the maximum depth of each tree
          max_depth = 5,
          #' @field min_node_size the minumum number of samples required to split an internal node
          min_node_size = 1,
          #' @field criterion the function to measure the quality of split. For classification, \code{gini} is used which
          #'      is a measure of gini index. For regression, the \code{variance} of responses is used.
          criterion = NULL,
          #' @field classification whether to train for classification (1) or regression (0)
          classification = 1,
          #' @field verbose show computation status and estimated runtime
          verbose = TRUE,
          #' @field seed seed value
          seed = 42,
          #' @field class_weights weights associated with the classes for sampling of training observation
          class_weights = NULL,
          #' @field always_split vector of feature names to be always used for splitting
          always_split = NULL,
          #' @field importance Variable importance mode, one of 'none', 'impurity', 'impurity_corrected', 'permutation'. The 'impurity' measure is the Gini index for classification, the variance of the responses for regression. Defaults to "impurity"
          importance = "impurity",

          #' @details
          #' Create a new `RFTrainer` object.
          #'
          #' @param n_estimators integer, the number of trees in the forest, default= 100
          #' @param max_depth integer, the maximum depth of each tree
          #' @param max_features integer, the number of features to consider when looking for the best split.
          #'                      Possible values are \code{auto(default)} takes sqrt(num_of_features),
          #'                          \code{sqrt} same as auto,
          #'                          \code{log} takes log(num_of_features),
          #'                          \code{none} takes all features
          #' @param min_node_size integer, the minumum number of samples required to split an internal node
          #' @param classification integer, whether to train for classification (1) or regression (0)
          #' @param class_weights weights associated with the classes for sampling of training observation
          #' @param always_split vector of feature names to be always used for splitting
          #' @param verbose logical, show computation status and estimated runtime
          #' @param save_model logical, whether to save model
          #' @param seed integer, seed value
          #' @param importance Variable importance mode, one of 'none', 'impurity', 'impurity_corrected', 'permutation'. The 'impurity' measure is the Gini index for classification, the variance of the responses for regression. Defaults to "impurity"
          #' @return A `RFTrainer` object.
          #'
          #' @examples
          #' data("iris")
          #' bst <- RFTrainer$new(n_estimators=10,
          #'                      max_depth=4,
          #'                      classification=1,
          #'                      seed=42,
          #'                      verbose=TRUE)

          initialize = function(n_estimators,
                                max_depth,
                                max_features,
                                min_node_size,
                                classification,
                                class_weights,
                                always_split,
                                verbose,
                                save_model,
                                seed,
                                importance
                                ){

              if (!(missing(n_estimators))) self$n_estimators <- n_estimators
              if (!(missing(max_features))) self$max_features <- max_features
              if (!(missing(max_depth)))    self$max_depth <- max_depth
              if (!(missing(min_node_size))) self$min_node_size <- min_node_size
              if (!(missing(classification)))
                  self$classification <- classification

              if (self$classification == 1) self$criterion <- "gini"
              else if (self$classification == 0) self$criterion <- "variance"

              if (!(missing(seed))) self$seed <- seed
              if (!(missing(class_weights)))  self$class_weights <- class_weights
              if (!(missing(always_split))) self$always_split <- always_split
              superml::check_package("ranger")

          },

          #' @details
          #' Trains the random forest model
          #'
          #' @param X data.frame containing train features
          #' @param y character, name of the target variable
          #' @return NULL, trains and saves the model in memory
          #'
          #' @examples
          #' data("iris")
          #' bst <- RFTrainer$new(n_estimators=10,
          #'                      max_depth=4,
          #'                      classification=1,
          #'                      seed=42,
          #'                      verbose=TRUE)
          #' bst$fit(iris, 'Species')

          fit = function(X, y) {

              superml::testdata(X, y)

              # dependent variable should be passed as factor if classification
              if (self$classification == 1) X[[y]] <- as.factor(X[[y]])

              # set max_features parameter
              if (self$max_features %in% c('auto','sqrt')) {
                  self$max_features <- round(sqrt(ncol(X)))
              } else if (self$max_features == 'log') {
                  self$max_features <- round(log(ncol(X)))
              } else if (self$max_features == 'none') {
                  self$max_features <- ncol(X)
              }

              # check for crazy values in max_features
              if (self$max_features < 1)
                  assert_that('max_features value should be > 1.')

              if (self$max_features == 1)
                  message('You are training this model with 1 feature.
                          Please check.')

              private$trained_model <- ranger::ranger(
                  dependent.variable.name = y,
                  data = X,
                  mtry = self$max_features,
                  num.trees = self$n_estimators,
                  min.node.size = self$min_node_size,
                  splitrule = self$criterion,
                  case.weights = self$class_weights,
                  always.split.variables = self$always_split,
                  verbose = self$verbose,
                  seed = self$seed,
                  importance = self$importance
              )

          },

          #' @details
          #' Return predictions from random forest model
          #'
          #' @param df data.frame containing test features
          #' @return a vector containing predictions
          #'
          #' @examples
          #' data("iris")
          #' bst <- RFTrainer$new(n_estimators=10,
          #'                      max_depth=4,
          #'                      classification=1,
          #'                      seed=42,
          #'                      verbose=TRUE)
          #' bst$fit(iris, 'Species')
          #' predictions <- bst$predict(iris)

          predict = function(df) {

              return(stats::predict(private$trained_model, df)$predictions)
          },

          #' @details
          #' Returns feature importance from the model
          #'
          #' @return a data frame containing feature predictions
          #'
          #' @examples
          #' data("iris")
          #' bst <- RFTrainer$new(n_estimators=50,
          #'                      max_depth=4,
          #'                      classification=1,
          #'                      seed=42,
          #'                      verbose=TRUE)
          #' bst$fit(iris, 'Species')
          #' predictions <- bst$predict(iris)
          #' bst$get_importance()
          get_importance = function(){

              tmp <- ranger::importance(private$trained_model)
              return(data.frame(tmp[order(tmp, decreasing = TRUE)]))
          }
      ),

      private = list( trained_model = NA )
)

