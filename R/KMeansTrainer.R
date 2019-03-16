#' K-Means Trainer
#' @description Trains a unsupervised K-Means clustering algorithm. It borrows mini-batch k-means function from
#' ClusterR package written in c++, hence it is quite fast.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' kmt = KMeansTrainer$new(clusters, batch_size = 10, num_init=1, max_iters=100,
#'                         init_fraction=1, initializer = "kmeans++", early_stop_iter = 10,
#'                         verbose=FALSE, centroids=NULL, tol = 1e-04, tol_optimal_init=0.3,
#'                         seed=1, max_clusters=NA)
#' bst$fit(X_train, y_train=NULL)
#' prediction <- bst$predict(X_test)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Initialises an instance of k-means model}
#'   \item{\code{$fit()}}{fit model to an input train data}
#'   \item{\code{$predict()}}{returns cluster predictions for each row of given data}
#' }
#' @section Arguments:
#' \describe{
#'  \item{params}{for explanation on parameters, please refer to the documentation of MiniBatchKMeans function in clusterR package \url{https://CRAN.R-project.org/package=ClusterR}}
#'  \item{find_optimal}{Used to find the optimal number of cluster during \code{fit} method. To use this, make sure the value for max_clusters > 0.}
#' }
#' @export
#' @examples
#' data <- rbind(replicate(20, rnorm(1e4, 2)),
#'              replicate(20, rnorm(1e4, -1)),
#'              replicate(20, rnorm(1e4, 5)))
#' km_model <- KMeansTrainer$new(clusters=2, batch_size=30, max_clusters=6)
#' km_model$fit(data, find_optimal = FALSE)
#' predictions <- km_model$predict(data)
KMeansTrainer <- R6Class("KMeansTrainer", public = list(

    clusters=NA,
    batch_size =10,
    num_init = 1,
    max_iters=100,
    init_fraction=1,
    initializer = "kmeans++",
    early_stop_iter=10,
    verbose=FALSE,
    centroids=NULL,
    tol = 1e-04,
    tol_optimal_init = 0.3,
    seed = 1,
    model = NA,
    max_clusters = NA,

    initialize = function(clusters,
                          batch_size = 10,
                          num_init=1,
                          max_iters=100,
                          init_fraction=1,
                          initializer = "kmeans++",
                          early_stop_iter = 10,
                          verbose=FALSE,
                          centroids=NULL,
                          tol = 1e-04,
                          tol_optimal_init=0.3,
                          seed=1,
                          max_clusters=NA){
        if(!(missing(clusters))) self$clusters <- clusters
        if(!(missing(batch_size))) self$batch_size <- batch_size
        if(!(missing(num_init))) self$num_init <- 1
        if(!(missing(max_iters))) self$max_iters <- max_iters
        if(!(missing(init_fraction))) self$init_fraction <- init_fraction
        if(!(missing(initializer))) self$initializer <- initializer
        if(!(missing(early_stop_iter))) self$early_stop_iter <- early_stop_iter
        if(!(missing(verbose))) self$verbose <- verbose
        if(!(missing(centroids))) self$centroids <- centroids
        if(!(missing(tol))) self$tol <- tol
        if(!(missing(tol_optimal_init)))
            self$tol_optimal_init <- tol_optimal_init
        if(!(missing(seed))) self$seed <- seed
        if(!(missing(max_clusters))) self$max_clusters <- max_clusters
        superml::check_package("ClusterR")

    },

    fit = function(X, y=NULL, find_optimal = FALSE){

        # X should be a matrix
        if(!(is.matrix(X) | is.data.frame(X)) )
            stop("X should be a matrix or a data frame")

        if(isTRUE(find_optimal)){
            message('Finding optimal number of clusters
                    based on variance explained')
            f <- ClusterR::Optimal_Clusters_KMeans(X, max_clusters = self$max_clusters)
            self$clusters <- which.max(f[-1])+1
        }

        message(sprintf('Using %d clusters to learn from data', self$clusters))
        self$model <- ClusterR::MiniBatchKmeans(data = X
                                      ,clusters = self$clusters
                                      ,batch_size = self$batch_size
                                      ,num_init = self$num_init
                                      ,max_iters = self$max_iters
                                      ,init_fraction = self$init_fraction
                                      ,initializer = self$initializer
                                      ,early_stop_iter = self$early_stop_iter
                                      ,verbose = self$verbose
                                      ,CENTROIDS = self$centroids
                                      ,tol=self$tol
                                      ,tol_optimal_init = self$tol_optimal_init
                                      ,seed = self$seed)
    },

    predict = function(X){
        return(ClusterR::predict_MBatchKMeans(X, CENTROIDS = self$model$centroids))
    })

)


