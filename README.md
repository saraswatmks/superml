
SuperML
=======

The goal of SuperML is to provide sckit-learn's `fit`,`predict`,`transform` standard way of building machine learning models in R. It is build on top of latest r-packages which provides optimized way of training machine learning models.

Installation
------------

You can install superml from github with:

``` r
# install.packages("devtools")
devtools::install_github("saraswatmks/superml")
```

Description
-----------

In superml, every machine learning algorithm is called as a `trainer`. Following is the list of trainers available as of today:<br/>

-   LMTrainer: used to train linear, logistic, ridge, lasso models
-   RFTrainer: Random Forest Model
-   KNNTrainer: K-Nearest Neighbour Models
-   KMeansTrainer: KMeans Model
-   XGBTrainer: XGBoost Model

In addition, there are other useful functions to support modeling tasks such as:

-   CountVectorizer: Create Bag of Words model
-   TfidfVectorizer: Create TF-IDF feature model
-   LabelEncoder: Convert categorical features to numeric
-   GridSearch: For hyperparameter optimization
-   RandomSearch: For hyperparameter optimization
-   kFoldMean: Target encoding
-   smoothMean: Target encoding

Usage
-----

Any machine learning model can be trained using the following steps:

``` r
data(iris)
library(superml)
rf <- RFTrainer$new(n_estimators = 100)
rf$fit(iris, "Species")
pred <- rf$predict(iris)
```

Documentation
-------------

The documentation can be found here: [SuperML Documentation](https://saraswatmks.github.io/superml/)
