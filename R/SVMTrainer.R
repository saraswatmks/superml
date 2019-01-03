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
#' svm = SVMTrainer$new(type=NULL,scale=TRUE, gammas=NULL, lambdas=NULL, c_values=NULL,predict.prob=FALSE,
#'                      verbose=NULL, ncores=NULL, partition_choice=0,
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
#'              refer to original documentation https://cran.r-project.org/package=liquidSVM}
#'  \item{type}{type of model to train, possible values: "bc" = binary classification, "mc" = multiclassification,
#'               "ls" = least square regression, "qt" = quantile regression}
#'  \item{scale}{normalises the feature between 0 and 1, default = TRUE}
#'  \item{gammas}{bandwidth of the kernel, default value is chosen from a list of gamma values generated internally}
#'  \item{lambdas}{regularization parameter}
#'  \item{c_values}{cost parameter}
#'  \item{predict.prob}{If TRUE then final prediction is probability else labels. This also restricts the choices of mc_type to c("OvA_ls","AvA_ls").}
#'  \item{verbose}{display the progress to standard output, possible values are 0, 1}
#'  \item{ncores}{number of cores to use for parallel processing, possible values are 0 (default), -1}
#'  \item{partition_choice}{optimization parameter to train on large data sets, possible value are: 0 (disables partitioning) , 6 (high speed), 5 (best error)}
#'  \item{seed}{random seed, default = -1}
#'  \item{grid_choice}{internal grid used for convenient hyperparameter tuning of gammas, lambdas, possible values are: 0,1,2,-1,-2}
#'  \item{useCells}{activates batch processing, set it to TRUE in case of out of memory errors}
#'  \item{mc_type}{configure multiclassification variant like OnevsAll, AllvsAll, possible values are: "AvA_hinge", "OvA_ls", "OvA_hinge", "AvA_ls"}
#'  \item{quantile}{do quantile regression, default=FALSE}
#'  \item{weights}{weights to be used in quantile regression, default is c(0.05, 0.1, 0.5, 0.9, 0.95)}
#' }
#' @export
#' @examples
#' data(iris)
#' ## Multiclassification
#' svm <- SVMTrainer$new(type="mc")
#' svm$fit(iris, "Species")
#' p <- svm$predict(iris)
#'
#' ## Least Squares
#' svm <- SVMTrainer$new(type="ls")
#' svm$fit(trees, "Height")
#' p <- svm$predict(trees)
#'
#' ## Quantile regression
#' svm <- SVMTrainer$new(type="qt")
#' svm$fit(trees,"Height")
#' p <- svm$predict(trees)
SVMTrainer <- R6Class('SVMTrainer', public = list(

    type=NULL,
    scale = TRUE,
    predict.prob = FALSE,
    verbose = NULL, # verbose = display
    ncores = NULL, # ncores = threads
    partition_choice = 0, # default 0, 6 high speed, 5 best error
    seed= -1,
    grid_choice = NULL, # build-in validation scheme to select the best model
    useCells = FALSE, # use if run into memory issues
    mc_type = NULL, # to be used only for multiclassification
    adaptivity_control = 0,
    gammas = NULL,
    lambdas = NULL,
    c_values=NULL,
    quantile = NULL,
    weights = c(0.05, 0.1, 0.5, 0.9, 0.95),
    model = NULL,

    initialize = function(type,
                          scale,
                          gammas,
                          lambdas,
                          c_values,
                          predict.prob,
                          verbose,
                          ncores,
                          partition_choice,
                          seed,
                          grid_choice,
                          useCells,
                          mc_type,
                          quantile,
                          weights){

        if(!(missing(type))) self$type <- type
        if(!(missing(scale))) self$scale <- scale
        if(!(missing(gammas))) self$gammas <- gammas
        if(!(missing(lambdas))) self$lambdas <- lambdas
        if(!(missing(predict.prob))) self$predict.prob <- predict.prob
        if(!(missing(verbose))) self$verbose <- verbose
        if(!(missing(ncores))) self$ncores <- ncores
        if(!(missing(partition_choice))) self$partition_choice<-partition_choice
        if(!(missing(seed))) self$seed <- seed
        if(!(missing(grid_choice))) self$grid_choice <- grid_choice
        if(!(missing(useCells))) self$useCells <- useCells
        if(!(missing(mc_type))) self$mc_type <- mc_type
        if(!(missing(c_values))) self$c_values <- c_values
        if(!(missing(quantile))) self$quantile <- quantile
        if(!(missing(weights))) self$quantile <- weights

    },

    fit = function(X, y){

        superml::testdata(X, y)

        # remove columns names which starts with a number, eg. 1bstff
        cols <- names(X)[grepl(pattern = "^\\d", x = names(X))]

        # by default, take all columns
        f <- as.formula(paste(y , paste("~ .")))

        if(length(cols) > 0){
            message(strwrap(sprintf("Removing invalid columns.
                    The names should not start with a number: %s",
                                    paste(cols, collapse=","))))
            f <- as.formula(paste(y , "~", paste(setdiff(names(X),
                                                         c(cols, y)),
                                                 collapse = "+")))
        }




        if(is.null(self$type))
            stop("Type cannot be left as null. Please provide a valid value.")

        if(self$type %in% c('mc','bc')){

            self$model <- liquidSVM::mcSVM(f # formula
                                         ,X # data
                                         ,scale = self$scale
                                         ,predict.prob = self$predict.prob
                                         ,display = self$verbose
                                         ,threads = self$ncores
                                         ,partition_choice = self$partition_choice
                                         ,random_seed = self$seed
                                         ,grid_choice = self$grid_choice
                                         ,useCells = self$useCells
                                         ,mc_type = self$mc_type
                                         ,gammas = self$gammas
                                         ,lambdas = self$lambdas
                                         ,c_values = self$c_values)

        }

        if(self$type == "qt"){

            self$model <- liquidSVM::qtSVM(f # formula
                                           ,X # data
                                           ,scale = self$scale
                                           ,weights = self$weights
                                           ,display = self$verbose
                                           ,threads = self$ncores
                                           ,partition_choice = self$partition_choice
                                           ,random_seed = self$seed
                                           ,grid_choice = self$grid_choice
                                           ,useCells = self$useCells
                                           ,mc_type = self$mc_type
                                           ,gammas = self$gammas
                                           ,lambdas = self$lambdas
                                           ,c_values = self$c_values)

        }


        if(self$type == "ls"){

            self$model <- liquidSVM::lsSVM(f # formula
                                           ,X # data
                                           ,scale = self$scale
                                           ,weights = self$weights
                                           ,display = self$verbose
                                           ,threads = self$ncores
                                           ,partition_choice = self$partition_choice
                                           ,random_seed = self$seed
                                           ,grid_choice = self$grid_choice
                                           ,useCells = self$useCells
                                           ,mc_type = self$mc_type
                                           ,gammas = self$gammas
                                           ,lambdas = self$lambdas
                                           ,c_values = self$c_values)

        }

    },

    predict = function(X){

        #return(liquidSVM:::predict.liquidSVM(self$model, X))
        return(stats:::predict(self$model, X))
    }


))

