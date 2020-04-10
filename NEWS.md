# superml 0.5.3

* Update bm25 function
* Add functions for dot product, svd, normalise matrix
* Add documentation for rcpp functions
* Fixed bugs.

# superml 0.5.2

* Introduce rcpp and rewrite base functions in rcpp to create count/tfidf matrix
* Much faster execution with core rcpp functions.
* Deprecate `parallel` argument, since turned out to be slower than c++.
* Fixed bugs.

# superml 0.5.1

* Fix bugs in CountVectorizer & TfidfVectorizer
* Add ngram_range feature
* Add more documentation on vectorizers

# superml 0.5.0

* glmnet changed its api. Fix Failing tests. Temporary fix by using "donttest" on its examples. Updates package documentation to support R6 classes.

# superml 0.4.0

* Fixed svm error. Replaced liquidSVM with e1071 R package.

# superml 0.3.0

* Moved some imports to suggests

# superml 0.2.0

* Added new trainers: SVM, NaiveBayes
* Renamed gridsearch, randomsearch trainers to CV
* Fixed documentation

# superml 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Fixed travis-ci integration    

