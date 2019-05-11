#' Support Vector Machines Trainer
#' @description Trains a support vector machine (svm) model. It is based on the magnificently fast speed liquidSVM R package.
#' It provides a more unified interface over the package retaining all its functionality.
#'
#' The model is intelligently trained with a default set of hyper parameters. Also, there are inbuilt
#' grid setups which can be easily initialised. It has capability to support batch processing of data
#' to avoid memory errors. It supports binary classification, multi classification, regression models
#'
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' svm = SVMTrainer$new(type=NULL,scale=TRUE, gammas=NULL, lambdas=NULL, c_values=NULL,
#'                      predict.prob=FALSE, verbose=NULL, ncores=NULL, partition_choice=0,
#'                      seed=-1, grid_choice=NULL, useCells=FALSE, mc_type=NULL,
#'                      adaptivity_control=0)
#' svm$fit(X_train, y_train)
#' prediction <- svm$predict(X_test)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialises an instance of svm model}
#'   \item{\code{$fit()}}{fits model to an input train data and trains the model.}
#'   \item{\code{$predict()}}{returns predictions by fitting the trained model on test data.}
#' }
#' @section Arguments:
#' \describe{
#'  \item{params}{for detailed explanation on parameters,
#'              refer to original documentation https://cran.r-project.org/web/packages/e1071/e1071.pdf}
#'  \item{type}{type of model to train, possible values: "bc" = binary classification, "mc" = multiclassification,
#'               "ls" = least square regression, "qt" = quantile regression}
#'  \item{scale}{normalises the feature between 0 and 1, default = TRUE}
#'  \item{gammas}{bandwidth of the kernel, default value is chosen from a list of gamma values generated internally}
#' }
#' @export
#' @examples
#' data(iris)
#' ## Multiclassification
#' svm <- SVMTrainer$new()
#' svm$fit(iris, "Species")
#' p <- svm$predict(iris)
#'
#' ## Least Squares
#' svm <- SVMTrainer$new()
#' svm$fit(trees, "Height")
#' p <- svm$predict(trees)
SVMTrainer <- R6Class('SVMTrainer', public = list(

    valid_types = c("C-classification",
                    "nu-classification",
                    "one-classification",
                    "eps-regression",
                    "nu-regression"),
    valid_kernels = c("linear",
                      "polynomial",
                      "radial basis",
                      "sigmoid"),
    remove_cols=NULL,
    y=NULL,
    model=NULL,
    type= NULL,
    kernel="radial",
    scale=TRUE,
    degree=3,
    gamma=NULL,
    coef0=0,
    cost=1,
    class.weights=NULL,
    cross=0,
    fitted=TRUE,
    probability=FALSE,
    subset=NULL,
    cachesize=40,
    tolerance=0.001,
    epsilon=0.1,
    shrinking=TRUE,
    na.action=stats::na.omit,

    initialize = function(type,
                          kernel,
                          scale,
                          degree,
                          gamma,
                          coef0,
                          cost,
                          class.weights,
                          cross,
                          fitted,
                          probability,
                          subset,
                          cachesize,
                          tolerance,
                          epsilon,
                          shrinking,
                          na.action
                          ){

        if(!(missing(type))) self$type <- type
        if(!(missing(kernel))) self$kernel <- kernel
        if(!(missing(scale))) self$scale <- scale
        if(!(missing(degree))) self$degree <- degree
        if(!(missing(gamma))) self$gamma <- gamma
        if(!(missing(coef0))) self$coef0 <- coef0
        if(!(missing(cost))) self$cost <- cost
        if(!(missing(class.weights))) self$class.weights <- class.weights
        if(!(missing(cross))) self$cross <- cross
        if(!(missing(fitted))) self$fitted <- fitted
        if(!(missing(probability))) self$probability <- probability
        if(!(missing(subset))) self$subset <- subset
        if(!(missing(na.action))) self$na.action <- na.action

        superml::check_package("e1071")
        print(paste0("For classification, target variable must be factor type.",
                " For regression, target variable must be numeric type."))

    },

    fit = function(X, y){

        self$y <- y

        if (is.data.table(X)) X <- data.frame(X)

        superml::testdata(X, y)

        # remove columns names which starts with a number, eg. 1bstff
        self$remove_cols <- names(X)[grepl(pattern = "^\\d", x = setdiff(names(X), y))]

        dataX <- X[, setdiff(names(X), c(self$remove_cols, y))]

        # by default, take all column
        self$gamma <- 1 / ncol(dataX)

        # e1071 checks for subset ! missing argument in svm function,
        # hence we need to pass something so we pass all rows
        if(is.null(self$subset)) self$subset <- seq(1, nrow(dataX))

        # set parameters
        if(is.null(self$type)){
            if(is.factor(X[[y]])){
                self$type <- "C-classification"
            } else {
                self$type <- "eps-regression"
            }
        }

        self$model <- e1071::svm(x = dataX
                                 ,y = X[[y]]
                                 ,type = self$type
                                 ,kernel = self$kernel)
                                 # ,scale = self$scales
                                 # ,degree = self$degree
                                 # ,gamma = self$gamma
                                 # ,coef0 = self$coef0
                                 # ,cost = self$cost
                                 # ,class.weights = self$class.weights
                                 # ,cross = self$cross
                                 # ,fitted = self$fitted
                                 # ,probability = self$probability
                                 # ,cachesize=self$cachesize
                                 # ,tolerance=self$tolerance
                                 # ,epsilon=self$epsilon
                                 # ,shrinking=self$shrinking
                                 # )


    },

    predict = function(X){

        if(is.data.table(X)) X <- data.frame(X)

        if(self$y %in% names(X)) X <- X[, setdiff(names(X), self$y)]
        if(!(is.null(self$remove_cols))) X[, setdiff(names(X),
                                                     c(self$remove_cols))]

        return(stats:::predict(self$model, X))
    }


))

