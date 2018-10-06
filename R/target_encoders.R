#' @name kFoldMean
#' @title kFoldMean Calculator
#' @description Calculates out-of-fold mean features (also known as target encoding) for train and test data. Make sure to rbind both train and test into one data frame.
#' This strategy is widely used to avoid overfitting or causing leakage while creating features using the target variable.
#' @param train_df train dataset
#' @param test_df test dataset
#' @param colname name of categorical column
#' @param target the target or dependent variable, should be a string.
#' @param n_fold the number of folds to use for doing calculation, default=5
#' @param seed the seed value, to ensure reproducibility,
#'             it could be any positive value, default=42
#'
#' @return a train and test data table with out-of-fold mean value
#'         of the target for the given categorical variable
#' @export
#' @examples
#' train <- data.table(region=c('del','csk','rcb','del','csk','pune','guj','del'),
#'                     win = c(0,1,1,0,0,0,0,1))
#' test <- data.table(region=c('rcb','csk','rcb','del','guj','pune','csk','kol'))
#' train_result <- kFoldMean(train_df = train,
#'                           test_df = test,
#'                           colname = 'region',
#'                           target = 'win',
#'                           seed = 1220)$train
#'
#' test_result <- kFoldMean(train_df = train,
#'                          test_df = test,
#'                          colname = 'region',
#'                          target = 'win',
#'                          seed = 1220)$test
#'
#' df <- data.table(cat_one = rep(c('A','B','C','D','E'), 10),
#'                  target = sample.int(100, 50))
#'
#' mean_values <- kFoldMean(df = df,
#'                          cat_var = 'cat_one',
#'                          target = 'target',
#'                          n_fold = 5)
kFoldMean <- function(train_df, test_df, colname, target, n_fold = 5, seed=42){

    # check if its data frame
    if(!(is.data.frame(train_df) | is.data.table(train_df)))
      stop("Please check the format of your train data.
           It should be a data.table or data.frame")

    if(!(is.data.frame(test_df) | is.data.table(test_df)))
        stop("Please check the format of your train data.
             It should be a data.table or data.frame")

    assert_that(all(c(colname, target) %in% names(train_df)))
    assert_that(all(c(colname) %in% names(test_df)))

    if(any(is.na(train_df[[target]])))
        stop("The target column contains NA values. Halting computation.")

    # just to be sure, convert the table to data table
    setDT(train_df)
    setDT(test_df)

    # calculate global mean
    globalmean <- mean(train_df[[target]])

    # set target col name
    target_col <- "mean_var"

    # this creates a new column with global mean
    temp_count  <- NULL
    train_df[, (target_col) := globalmean]

    # add 5 times the mean value later in the for loop
    test_df[, (target_col) := 0]
    test_df[, temp_count := 0]

    # create folds
    base::set.seed(seed)
    mfolds <- caret::createFolds(y = train_df[[target]], k = n_fold, list = T)

    # get values
    for(f in mfolds){

        x_train <- train_df[-f, c(colname, target), with = F]
        x_valid <- train_df[f, c(colname, target), with = F]

        means <- x_train[, mean(get(target), na.rm = TRUE), colname]
        setnames(means, "V1", target_col)

        ## map out of fold mean values to categories in validation data
        x_valid <- means[x_valid[, ..colname], on = colname]
        x_valid[is.na(get(target_col)), (target_col) := globalmean]

        train_df[f, (target_col) := x_valid[[target_col]]]
        x_valid[, temp_count := 1]

        test_df[x_valid, ":="(mean_var = mean_var + i.mean_var,
                              temp_count = temp_count + i.temp_count),
                              on = colname]

    }

    # avoiding zero division error
    test_df[min(temp_count), temp_count := 1]

    # divide test values by frequency
    test_df[, (target_col) := get(target_col) / temp_count]

    # ensure the encoding values match the cat values
    return (list(train = train_df[, c(colname, target_col), with = F],
                 test = test_df[, c(colname, target_col), with = F]))
}



#' @name smoothMean
#' @title smoothMean Calculator
#' @description Calculates target encodings using a smoothing parameter and count of categorical variables.
#' This approach is more robust to possibility of leakage and avoid overfitting.
#' @param train_df train dataset
#' @param test_df test dataset
#' @param colname name of categorical column
#' @param target name of target column
#' @param min_samples_leaf minimum samples to take category average into account
#' @param smoothing smoothing effect to balance categorical average vs prior
#' @param noise_level random noise to add, optional
#'
#' @return a train and test data table with mean encodings
#'         of the target for the given categorical variable
#' @export
#' @examples
#' train <- data.table(region=c('del','csk','rcb','del','csk','pune','guj','del'),
#'                     win = c(0,1,1,0,0,1,0,1))
#' test <- data.table(region=c('rcb','csk','rcb','del','guj','pune','csk','kol'))
#'
#' # calculate encodings
#' train_mean <- target_encode(train_df = train,
#'                             test_df = test,
#'                             colname = 'region',
#'                             target = 'win')$train
#'
#' test_mean <- target_encode(train_df = train,
#'                            test_df = test,
#'                            colname = 'region',
#'                            target = 'win')$test
smoothMean <- function(train_df, test_df, colname, target, min_samples_leaf=1, smoothing=1, noise_level=0){

    # check if column exists
    assert_that(all(c(colname, target) %in% names(train_df)))
    assert_that(all(c(colname) %in% names(test_df)))

    if(any(is.na(train_df[[target]])))
        stop("The target column contains NA values. Halting computation.")

    # make sure we work on data tables
    train_df <- as.data.table(train_df)
    test_df <- as.data.table(test_df)

    # combine data - class of target changes - becomes char from numeric >:(
    # compute target mean
    averages <- train_df[, .(count = .N, mean = mean(get(target))), colname]

    # compute smoothing
    smoothing <- 1 / (1 + exp(-(averages[["count"]] -
                                    min_samples_leaf) / smoothing))

    #cal prior
    prior <- mean(train_df[[target]])

    # smooth target
    averages[["target"]] <- prior * (1 - smoothing) +
                                    averages[["mean"]] * smoothing

    # drop columns
    averages[, c("mean", "count") := NULL]

    trn_df <- averages[train_df[, ..colname], on = colname][is.na(get(target)), (target) := prior]
    tst_df <- averages[test_df[, ..colname], on = colname][is.na(get(target)), (target) := prior]

    add_noise <- function(vec, noise_level=0){
        return(vec * (1 + noise_level * stats::rnorm(length(vec))))
    }

    trn_df[[target]] <- add_noise(trn_df[[target]], noise_level = noise_level)
    tst_df[[target]] <- add_noise(tst_df[[target]], noise_level = noise_level)


    return(list(train = trn_df, test = tst_df))
}
