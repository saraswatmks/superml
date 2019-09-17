#' K Nearest Neighbours Trainer
#' @description Trains a k nearest neighbour model using fast search algorithms. KNN is a supervised learning
#'              algorithm which is used for both regression and classification problems.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' bst = KNNTrainer$new(k=1, prob=FALSE, algorithm=NULL, type="class")
#' bst$fit(X_train, X_test, "target")
#' bst$predict(type)
#' }
#' @section Methods:
#' \describe{
#'     \item{\code{$new()}}{Initialise the instance of the trainer}
#'     \item{\code{$fit()}}{trains the knn model and stores the test prediction}
#'     \item{\code{$predict()}}{returns predictions}
#' }
#' @section Arguments:
#' \describe{
#'     \item{k}{number of neighbours to predict}
#'     \item{prob}{if probability should be computed, default=FALSE}
#'     \item{algorithm}{algorithm used to train the model, possible values are 'kd_tree','cover_tree','brute'}
#'     \item{type}{type of problem to solve i.e. regression or classification, possible values are 'reg' or 'class'}
#' }
#' @export
#' @examples
#' data("iris")
#'
#' iris$Species <- as.integer(as.factor(iris$Species))
#'
#' xtrain <- iris[1:100,]
#' xtest <- iris[101:150,]
#'
#' bst <- KNNTrainer$new(k=3, prob=TRUE, type="class")
#' bst$fit(xtrain, xtest, 'Species')
#' pred <- bst$predict(type="raw")
KNNTrainer <- R6Class("KNNTrainer", public = list(
    k=1,
    prob=FALSE,
    algorithm=NULL,
    type="class",
    model = NA,

    initialize = function(k, prob, algorithm, type){
        if(!(missing(k))) self$k <- k
        if(!(missing(prob))) self$prob <- prob
        if(!(missing(algorithm))) self$algorithm <- algorithm
        if(!(missing(type))) self$type <- type
        superml::check_package("FNN")
    },

    fit = function(train, test, y){

        data <- private$prepare_data(train, test, y)

        if(self$type == "class"){
            self$model <- FNN::knn(train = data$train
                              ,test = data$test
                              ,cl = data$y
                              ,k = self$k
                              ,prob = self$prob
                              ,algorithm = self$algorithm)
        } else if (self$type == "reg"){
            self$model <- FNN::knn.reg(train = data$train
                                  ,test = data$test
                                  ,y = data$y
                                  ,k = self$k
                                  ,algorithm = self$algorithm)
        }
    },

    predict = function(type="raw"){

        if(self$type == "class"){
            if(type=="raw"){
                return(as.numeric(as.character(self$model)))
            } else if(type=="prob"){
                return(attr(self$model, "prob"))
            }
        } else if (self$type == "reg"){
            return(self$model$pred)
        }

    }),

    private = list(

        prepare_data = function(train, test, y){

            train <- as.data.table(train)
            test <- as.data.table(test)

            if(!(y %in% names(train)))
                stop(sprintf("%s not available in training data", y))

            # get dependent variable and store temporarily 
            y_temp <- train[[y]]

            # select all independent features
            train <- train[,setdiff(names(train), y), with=F]

            # subset from test, just in case if the dependet variable is in test
            test <- test[, setdiff(names(test), y), with=F]
            
            # set dependent variable to y
            y <- y_temp

            if(ncol(test) != ncol(train))
                stop(sprintf('Train and test data have
                             unequal independent variables.'))

            if(any(vapply(train, is.factor, logical(1)))
               | any(vapply(train, is.character, logical(1))))
                stop("Train data contains non-numeric variables.
                     Please convert them into integer.")

            if(any(vapply(test, is.factor, logical(1)))
               | any(vapply(test, is.character, logical(1))))
                stop("Test data contains non-numeric variables.
                     Please convert them into integer.")

            # check in case target variable contains float values or NA values
            if(any(is.na(y)))
                stop("The target variable contains NA values.")

            if(self$type=="class"){
                if(is.numeric(y)){
                    if(!(all(y == floor(y))))
                        stop("The target variable contains float values")
                }
            }

            return(list(train = train, test= test, y = y))
        }

    )
)


