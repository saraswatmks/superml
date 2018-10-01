#' Random Forest Model
#' @description Trains a Random Forest model. A random forest is a meta estimator that fits a number of decision tree classifiers on various sub-samples of the dataset and use averaging to improve the predictive accuracy and control over-fitting.
#' This implementation uses ranger R package which provides faster model training.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' bst = RandomForestTrainer$new(n_estimators, max_features, max_depth, min_node_size, criterion,classification, class_weights, verbose, seed,always_split)
#' bst$fit(X_train, y_train)
#' prediction <- bst$predict(X_test)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(n_estimators, max_features, max_depth, min_node_size, criterion,classification, class_weights, verbose, seed,always_split)}}{Initialises an instance of random forest model}
#'   \item{\code{$fit(X_train, y_train)}}{fit model to an input train data and trains the model.}
#'   \item{\code{$predict(X_test)}}{returns predictions by fitting the trained model on test data.}
#' }
#' @section Arguments:
#' \describe{
#'  \item{n_estimators}{the number of trees in the forest, default= 100}
#'  \item{max_features}{the number of features to consider when looking for the best split
#'      Possible values are \code{auto(default)},\code{sqrt},\code{log},\code{None}}
#'  \item{max_depth}{the maximum depth of each tree}
#'  \item{min_node_size}{the minumum number of samples required to split an internal node}
#'  \item{criterion}{the function to measure the quality of split. For classification, \code{gini} is used which
#'      is a measure of gini index. For regression, the \code{variance} of responses is used.}
#'  \item{classification}{whether to train for classification (1) or regression (0)}
#'  \item{class_weights}{weights associated with the classes for sampling of training observation}
#'  \item{verbose}{show computation status and estimated runtime}
#'  \item{always_split}{vector of feature names to be always used for splitting}
#'  \item{seed}{set seed}
#' }
#' @export
#' @examples
#' data("iris")
#' bst <- RandomForestClassifier$new(n_estimators=100, max_depth=4, classification=1, seed=42, verbose=TRUE)
#' bst$fit(iris, 'Species')
#' pred <- bst$predict(iris)

RFTrainer <- R6Class("RandomForestTrainer",
      public = list(
          n_estimators = 100,
          max_features = "auto",
          max_depth = 5,
          min_node_size = 1,
          criterion = NULL,
          classification = 1,
          verbose = NULL,
          seed = 42,
          class_weights = NULL,
          always_split = NULL,

          initialize = function(n_estimators,
                                max_depth,
                                max_features,
                                min_node_size,
                                classification,
                                class_weights,
                                always_split,
                                verbose,
                                save_model,
                                seed){

              if(!(missing(n_estimators))) self$n_estimators <- n_estimators
              if (!(missing(max_features))) self$max_features <- max_features
              if (!(missing(max_depth)))    self$max_depth <- max_depth
              if (!(missing(min_node_size))) self$min_node_size <- min_node_size
              if (!(missing(classification))) self$classification <- classification
              if (!(missing(max_features))) self$max_features <- max_features
              else  self$max_features <- round(sqrt(length(private$iid_names)))
              if (self$classification == 1) self$criterion <- "gini"
              else if(self$classification == 0) self$criterion <- "variance"
              if(!(missing(seed))) self$seed <- seed
              if(!(missing(class_weights)))  self$class_weights <- class_weights
              if(!(missing(always_split))) self$always_split <- always_split

          },

          fit = function(X, y) {
              private$check_data(X, y)

              private$trained_model <- ranger::ranger(
                  dependent.variable.name = y,
                  data = X,
                  mtry = self$max_features,
                  min.node.size = self$min_samples_split,
                  num.trees = self$n_estimators,
                  splitrule = self$criterion,
                  case.weights = self$class_weights,
                  always.split.variables = self$always_split,
                  verbose = self$verbose,
                  seed = self$seed
              )

          },

          predict = function(df) {

              p <- ranger::predictions(private$trained_model,
                                      df[, c(private$iid_names)])
              return(p)
          }
      ),

      private = list(

          iid_names <- NA,
          trained_model <- NA,

          check_data = function(X, y) {
              if (!(inherits(X, c("data.table", "data.frame"))))
                  stop("Your data format should be a data.table or data.frame.")

              if(!(y %in% names(X)))
                  stop("The dependent variable is not available in the data.")

              iid_names <- setdiff(colnames(X), y)
          }
    )
)
