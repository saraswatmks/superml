% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CountVectorizer.R
\name{CountVectorizer}
\alias{CountVectorizer}
\title{Count Vectorizer}
\description{
Creates CountVectorizer Model.
}
\details{
Given a list of text, it generates a bag of words model and returns a sparse matrix consisting of token counts.
}
\examples{

## ------------------------------------------------
## Method `CountVectorizer$new`
## ------------------------------------------------

cv = CountVectorizer$new(min_df=0.1)

## ------------------------------------------------
## Method `CountVectorizer$fit`
## ------------------------------------------------

sents = c('i am alone in dark.','mother_mary a lot',
          'alone in the dark?', 'many mothers in the lot....')
cv = CountVectorizer$new(min_df=0.1)
cv$fit(sents)

## ------------------------------------------------
## Method `CountVectorizer$fit_transform`
## ------------------------------------------------

sents = c('i am alone in dark.','mother_mary a lot',
         'alone in the dark?', 'many mothers in the lot....')
cv <- CountVectorizer$new(min_df=0.1)
cv_count_matrix <- cv$fit_transform(sents)

## ------------------------------------------------
## Method `CountVectorizer$transform`
## ------------------------------------------------

sents = c('i am alone in dark.','mother_mary a lot',
          'alone in the dark?', 'many mothers in the lot....')
new_sents <- c("dark at night",'mothers day')
cv = CountVectorizer$new(min_df=0.1)
cv$fit(sents)
cv_count_matrix <- cv$transform(new_sents)
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{sentences}}{a list containing sentences}

\item{\code{max_df}}{When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.}

\item{\code{min_df}}{When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.}

\item{\code{max_features}}{Build a vocabulary that only consider the top max_features ordered by term frequency across the corpus.}

\item{\code{ngram_range}}{The lower and upper boundary of the range of n-values for different word n-grams or char n-grams to be extracted. All values of n such such that min_n <= n <= max_n will be used. For example an ngram_range of c(1, 1) means only unigrams, c(1, 2) means unigrams and bigrams, and c(2, 2) means only bigrams.}

\item{\code{split}}{splitting criteria for strings, default: " "}

\item{\code{lowercase}}{convert all characters to lowercase before tokenizing}

\item{\code{regex}}{regex expression to use for text cleaning.}

\item{\code{remove_stopwords}}{a list of stopwords to use, by default it uses its inbuilt list of standard stopwords}

\item{\code{model}}{internal attribute which stores the count model}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CountVectorizer-new}{\code{CountVectorizer$new()}}
\item \href{#method-CountVectorizer-fit}{\code{CountVectorizer$fit()}}
\item \href{#method-CountVectorizer-fit_transform}{\code{CountVectorizer$fit_transform()}}
\item \href{#method-CountVectorizer-transform}{\code{CountVectorizer$transform()}}
\item \href{#method-CountVectorizer-clone}{\code{CountVectorizer$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CountVectorizer-new"></a>}}
\if{latex}{\out{\hypertarget{method-CountVectorizer-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CountVectorizer$new(
  min_df,
  max_df,
  max_features,
  ngram_range,
  regex,
  remove_stopwords,
  split,
  lowercase
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{min_df}}{numeric, When building the vocabulary ignore terms that have a document frequency strictly lower than the given threshold, value lies between 0 and 1.}

\item{\code{max_df}}{numeric, When building the vocabulary ignore terms that have a document frequency strictly higher than the given threshold, value lies between 0 and 1.}

\item{\code{max_features}}{integer, Build a vocabulary that only consider the top max_features ordered by term frequency across the corpus.}

\item{\code{ngram_range}}{vector, The lower and upper boundary of the range of n-values for different word n-grams or char n-grams to be extracted. All values of n such such that min_n <= n <= max_n will be used. For example an ngram_range of c(1, 1) means only unigrams, c(1, 2) means unigrams and bigrams, and c(2, 2) means only bigrams.}

\item{\code{regex}}{character, regex expression to use for text cleaning.}

\item{\code{remove_stopwords}}{list, a list of stopwords to use, by default it uses its inbuilt list of standard english stopwords}

\item{\code{split}}{character, splitting criteria for strings, default: " "}

\item{\code{lowercase}}{logical, convert all characters to lowercase before tokenizing, default: TRUE}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Create a new `CountVectorizer` object.
}

\subsection{Returns}{
A `CountVectorizer` object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{cv = CountVectorizer$new(min_df=0.1)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CountVectorizer-fit"></a>}}
\if{latex}{\out{\hypertarget{method-CountVectorizer-fit}{}}}
\subsection{Method \code{fit()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CountVectorizer$fit(sentences)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sentences}}{a list of text sentences}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Fits the countvectorizer model on sentences
}

\subsection{Returns}{
NULL
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{sents = c('i am alone in dark.','mother_mary a lot',
          'alone in the dark?', 'many mothers in the lot....')
cv = CountVectorizer$new(min_df=0.1)
cv$fit(sents)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CountVectorizer-fit_transform"></a>}}
\if{latex}{\out{\hypertarget{method-CountVectorizer-fit_transform}{}}}
\subsection{Method \code{fit_transform()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CountVectorizer$fit_transform(sentences)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sentences}}{a list of text sentences}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Fits the countvectorizer model and returns a sparse matrix of count of tokens
}

\subsection{Returns}{
a sparse matrix containing count of tokens in each given sentence
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{sents = c('i am alone in dark.','mother_mary a lot',
         'alone in the dark?', 'many mothers in the lot....')
cv <- CountVectorizer$new(min_df=0.1)
cv_count_matrix <- cv$fit_transform(sents)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CountVectorizer-transform"></a>}}
\if{latex}{\out{\hypertarget{method-CountVectorizer-transform}{}}}
\subsection{Method \code{transform()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CountVectorizer$transform(sentences)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sentences}}{a list of new text sentences}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Returns a matrix of count of tokens
}

\subsection{Returns}{
a sparse matrix containing count of tokens in each given sentence
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{sents = c('i am alone in dark.','mother_mary a lot',
          'alone in the dark?', 'many mothers in the lot....')
new_sents <- c("dark at night",'mothers day')
cv = CountVectorizer$new(min_df=0.1)
cv$fit(sents)
cv_count_matrix <- cv$transform(new_sents)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CountVectorizer-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CountVectorizer-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CountVectorizer$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
