#' K-Means Trainer
#'
#' Trains a k-means machine learning model in R
#'
#' @details
#' Trains a unsupervised K-Means clustering algorithm. It borrows mini-batch k-means function from
#' ClusterR package written in c++, hence it is quite fast.
#'
#' @export

KMeansTrainer <- R6Class("KMeansTrainer", public = list(

    #' @field clusters the number of clusters
    clusters = NA,
    #' @field batch_size the size of the mini batches
    batch_size = 10,
    #' @field num_init number of times the algorithm will be run with different centroid seeds
    num_init = 1,
    #' @field max_iters the maximum number of clustering iterations
    max_iters = 100,
    #' @field init_fraction percentage of data to use for the initialization centroids (applies if initializer is kmeans++ or optimal_init). Should be a float number between 0.0 and 1.0.
    init_fraction = 1,
    #' @field initializer the method of initialization. One of, optimal_init, quantile_init, kmeans++ and random.
    initializer = "kmeans++",
    #' @field early_stop_iter continue that many iterations after calculation of the best within-cluster-sum-ofsquared-error
    early_stop_iter = 10,
    #' @field verbose either TRUE or FALSE, indicating whether progress is printed during clustering
    verbose = FALSE,
    #' @field centroids a matrix of initial cluster centroids. The rows of the CENTROIDS matrix should be equal to the number of clusters and the columns should be equal to the columns of the data
    centroids = NULL,
    #' @field tol a float number. If, in case of an iteration (iteration > 1 and iteration < max_iters) "tol" is greater than the squared norm of the centroids, then kmeans has converged
    tol = 1e-04,
    #' @field tol_optimal_init tolerance value for the ’optimal_init’ initializer. The higher this value is, the far appart from each other the centroids are.
    tol_optimal_init = 0.3,
    #' @field seed integer value for random number generator (RNG)
    seed = 1,
    #' @field model use for internal purpose
    model = NA,
    #' @field max_clusters either a numeric value, a contiguous or non-continguous numeric vector specifying the cluster search space
    max_clusters = NA,

    #' @details
    #' Create a new `KMeansTrainer` object.
    #'
    #' @param clusters numeric, When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.
    #' @param batch_size nuemric, When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.
    #' @param num_init integer, use top features sorted by count to be used in bag of words matrix.
    #' @param max_iters character, regex expression to use for text cleaning.
    #' @param init_fraction list, a list of stopwords to use, by default it uses its inbuilt list of standard stopwords
    #' @param initializer character, splitting criteria for strings, default: " "
    #' @param early_stop_iter continue that many iterations after calculation of the best within-cluster-sum-ofsquared-error
    #' @param verbose either TRUE or FALSE, indicating whether progress is printed during clustering
    #' @param centroids a matrix of initial cluster centroids. The rows of the CENTROIDS matrix should be equal to the number of clusters and the columns should be equal to the columns of the data
    #' @param tol a float number. If, in case of an iteration (iteration > 1 and iteration < max_iters) "tol" is greater than the squared norm of the centroids, then kmeans has converged
    #' @param tol_optimal_init tolerance value for the ’optimal_init’ initializer. The higher this value is, the far appart from each other the centroids are.
    #' @param seed integer value for random number generator (RNG)
    #' @param max_clusters either a numeric value, a contiguous or non-continguous numeric vector specifying the cluster search space
    #'
    #' @return A `KMeansTrainer` object.
    #'
    #' @examples
    #' data <- rbind(replicate(20, rnorm(1e4, 2)),
    #'              replicate(20, rnorm(1e4, -1)),
    #'              replicate(20, rnorm(1e4, 5)))
    #' km_model <- KMeansTrainer$new(clusters=2, batch_size=30, max_clusters=6)

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
        if (!(missing(clusters))) self$clusters <- clusters
        if (!(missing(batch_size))) self$batch_size <- batch_size
        if (!(missing(num_init))) self$num_init <- 1
        if (!(missing(max_iters))) self$max_iters <- max_iters
        if (!(missing(init_fraction))) self$init_fraction <- init_fraction
        if (!(missing(initializer))) self$initializer <- initializer
        if (!(missing(early_stop_iter))) self$early_stop_iter <- early_stop_iter
        if (!(missing(verbose))) self$verbose <- verbose
        if (!(missing(centroids))) self$centroids <- centroids
        if (!(missing(tol))) self$tol <- tol
        if (!(missing(tol_optimal_init)))
            self$tol_optimal_init <- tol_optimal_init
        if (!(missing(seed))) self$seed <- seed
        if (!(missing(max_clusters))) self$max_clusters <- max_clusters
        superml::check_package("ClusterR")

    },

    #' @details
    #' Trains the KMeansTrainer model
    #'
    #' @param X data.frame or matrix containing features
    #' @param y NULL only kept here for superml's standard way
    #' @param find_optimal logical, to find the optimal clusters automatically
    #' @return NULL
    #'
    #' @examples
    #' data <- rbind(replicate(20, rnorm(1e4, 2)),
    #'              replicate(20, rnorm(1e4, -1)),
    #'              replicate(20, rnorm(1e4, 5)))
    #' km_model <- KMeansTrainer$new(clusters=2, batch_size=30, max_clusters=6)
    #' km_model$fit(data, find_optimal = FALSE)

    fit = function(X, y=NULL, find_optimal = FALSE){

        # X should be a matrix
        if (!(is.matrix(X) | is.data.frame(X)) )
            stop("X should be a matrix or a data frame")

        if (isTRUE(find_optimal)){
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

    #' @details
    #' Returns the prediction on test data
    #'
    #' @param X data.frame or matrix
    #' @return a vector of predictions
    #'
    #' @examples
    #' data <- rbind(replicate(20, rnorm(1e4, 2)),
    #'              replicate(20, rnorm(1e4, -1)),
    #'              replicate(20, rnorm(1e4, 5)))
    #' km_model <- KMeansTrainer$new(clusters=2, batch_size=30, max_clusters=6)
    #' km_model$fit(data, find_optimal = FALSE)
    #' predictions <- km_model$predict(data)

    predict = function(X){
        return(ClusterR::predict_MBatchKMeans(X, CENTROIDS = self$model$centroids))
    })

)


