#include <boost/algorithm/string/join.hpp>
#include <boost/tokenizer.hpp>
#include <regex>
#include <RcppArmadillo.h>

// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
CharacterVector superSplit(std::string str, char sep = ' ') {
    std::stringstream ss(str);
    std::string item;
    CharacterVector elems;
    while (std::getline(ss, item, sep)) {
       // fix to filter out empty spaces
        if (item.length() > 1){
            elems.push_back(std::move(item));
        }
    }
    return elems;
}

// Convert a string into a vector of ngram tokens
// [[Rcpp::export]]
std::vector<std::string> superNgrams(std::string str, NumericVector ngram_range, char sep = ' '){

    CharacterVector cv;
    cv = superSplit(str, sep);

    int ngram_min = ngram_range[0] - 1;
    int ngram_max = ngram_range[1] - 1;

    std::vector<std::string> output;

    // CharacterVector A = CharacterVector::create("w","o","r","l","d");
    // std::string val_str = collapse(A);

    // cout << val_str << endl;
    for(int i=0;i < cv.size(); i++){
        // cout << cv[i] << endl;
        int counter = 0;
        for(int j=ngram_min; j <= ngram_max; j++){
            counter = i + j;

            if (counter < cv.size()){

                StringVector r = cv[Range(i, counter)];
                std::vector<std::string> rx = as<std::vector<string>>(r);
                std::string b =  boost::algorithm::join(rx, " ");
                output.push_back(b);

            }
        }
    }

    return output;

}

// Converts a vector of string into a vector a tokens, strips from all punctuations
// [[Rcpp::export]]
std::vector<std::string> superTokenizer(std::vector<std::string> string){

    std::vector<std::string> output;

    for(auto i: string){

        boost::tokenizer<> tok(i);
        for(boost::tokenizer<>::iterator beg=tok.begin(); beg!=tok.end(); ++beg){
            output.push_back(*beg);
        }

    }

    return output;

}

// Creates bow matrix
// [[Rcpp::export]]
NumericMatrix superCountMatrix(std::vector<std::string> sent, std::vector<std::string> tokens){

    NumericMatrix mat(sent.size() , tokens.size());

    for(int i=0; i < sent.size(); i++){
        std::string s = sent[i];
        IntegerVector tem(tokens.size());
        for(int j=0; j < tokens.size(); j++){
            regex e = std::regex("\\b" + tokens[j] +  "\\b");
            string m = regex_replace (s, e, "");

            int x1 = s.size();
            int new_size = m.size();
            int net = x1 - new_size;
            int diff = net / tokens[j].size();
            tem[j] = diff;

        }

        mat(i, _) = tem;

    }

    colnames(mat) = wrap(tokens);
    return mat;

}

//' @name dot
//' @title Dot product similarity in vectors
//' @description Computes the dot product between two given vectors.
//'
//' @param a numeric vector
//' @param b numeric vector
//' @param norm logical, compute normalised dot product, default=True
//'
//' @return numeric vector containing sdot product score
//' @export
//'
//' @examples
//' a <- runif(5)
//' b <- runif(5)
//' s <- dot(a, b)
//'
// [[Rcpp::export]]
float dot(NumericVector a, NumericVector b, bool norm=true){

    arma::vec ay = Rcpp::as<arma::vec>(a);
    arma::vec by = Rcpp::as<arma::vec>(b);
    if(norm){
        return arma::norm_dot(ay, by);
    }
    return arma::dot(ay, by);

}

//' @name dotmat
//' @title Dot product similarity between a vector and matrix
//' @description Computes the dot product between a vector and a given matrix.
//' The vector returned has a dot product similarity value for each row in the matrix.
//'
//' @param a numeric vector
//' @param b numeric matrix
//' @param norm logical, compute normalised dot product, default=True
//'
//' @return numeric vector containing dot product scores
//' @export
// [[Rcpp::export]]
NumericVector dotmat(NumericVector a, NumericMatrix b, const bool norm=true){

    arma::vec ay = Rcpp::as<arma::vec>(a);
    arma::mat by = Rcpp::as<arma::mat>(b);

    NumericVector result(by.n_rows);

    for(int i = 0; i < by.n_rows; ++i){

        if(norm){
            float vf = norm_dot(ay, by.row(i));
            result[i] = vf;
        } else {
            float vf = dot(ay, by.row(i));
            result[i] = vf;
        }
    }

    return result;

}

// Sorts a vector
// [[Rcpp::export]]
std::vector<double> sorted(NumericVector v){

    arma::vec vect = Rcpp::as<arma::vec>(v);

    // ways to convert a 2d array to 1d array:
    // v1.attr("dim") = R_NilValue;
    // Rcpp::as<std::vector<double> >(wrap(v1));

    return Rcpp::as<std::vector<double> >(wrap(sort(vect)));

}

//' @name sort_index
//' @title sort_index
//' @description For a given vector, return the indexes of the sorted array and
//' not the sorted array itself.
//'
//' @param vec numeric vector
//' @param ascending logical, order to return (ascending or descending), default = True
//'
//' @return numeric vector containing sorted indexes
//' @export
//'
//' @examples
//' v <- c(10,3,1,4)
//' j <- sort_index(v)
//'
// [[Rcpp::export]]
std::vector<int> sort_index(NumericVector vec, const bool ascending=true){

    // buggy function, changes order of input v
    // Rcpp::NumericVector res = Rcpp::clone(v);
    arma::vec vect = Rcpp::as<arma::vec>(vec);
    // add 1 because starts with 0
    if(ascending){
        return Rcpp::as<std::vector<int> >(wrap(stable_sort_index(vect, "ascend") + 1));
    }
    return Rcpp::as<std::vector<int> >(wrap(stable_sort_index(vect, "descend") + 1));

}


//' @name normalise2d
//' @title normalise2d
//' @description Normalises a matrix towards unit p norm row wise or column wise. By default, p = 2 is used.
//' To normalise row wise, use axis=0. To normalise column wise, use axis=1.
//' as the square root of sum of square of values in the given vector.
//'
//' @param mat numeric matrix
//' @param pnorm integer value, default value=2
//' @param axis integer (0 or 1), row wise = 0,  column wise = 1
//'
//' @return normalised numeric matrix
//' @export
//'
//' @examples
//' mat <- matrix(runif(12), 3, 4)
//'
//' ## normalise matrix row wise
//' r <- normalise2d(mat, axis=0)
//'
//' ## normalise matrix column wise
//' r <- normalise2d(mat, axis=1)
//'
// [[Rcpp::export]]
arma::mat normalise2d(NumericMatrix mat, const int pnorm=2, const int axis=1){

    arma::mat mt = Rcpp::as<arma::mat>(mat);

    return arma::normalise(mt, pnorm, axis);

}

//' @name normalise1d
//' @title normalise1d
//' @description Normalises a 1 dimensional vector towards unit p norm. By default, p = 2 is used.
//' For a given vector, eg: c(1,2,3), norm value is calculated as `x / |x|` where `|x|` is calculated
//' as the square root of sum of square of values in the given vector.
//'
//' @param vec vector containing integers or numeric values.
//' @param pnorm integer, default: 2
//'
//' @return a vector containing normalised values
//' @export
//'
//' @examples
//' val <- c(1,10,5,3,8)
//' norm_val <- normalise1d(val)
//'
// [[Rcpp::export]]
std::vector<double> normalise1d(NumericVector vec, const int pnorm=2){

    arma::vec v = Rcpp::as<arma::vec>(vec);

    return Rcpp::as<std::vector<double>>(wrap(arma::normalise(v, pnorm)));

}


// [[Rcpp::export]]
double avg_doc_len(std::vector<std::string> ss){

    int sum = 0;
    int count = 0;

    for(auto str: ss){

        CharacterVector vec = superSplit(str);
        sum += vec.size();
        count += 1;
    }

    return sum/count;

}

// [[Rcpp::export]]
double idf(std::string q, std::vector<std::string> corpus){

    int docCount = corpus.size();
    int f_q = 0;
    for(auto doc: corpus){
        if(doc.find(q) != std::string::npos){
            f_q += 1;
        }
    }
    double v = log(1 + ((docCount - f_q + 0.5)/(f_q + 0.5)));
    return v;
}

// [[Rcpp::export]]
NumericVector sort_vector_with_names(NumericVector x) {
    IntegerVector idx = seq_along(x) - 1;
    //// Ordered indices based on x:
    std::sort(idx.begin(), idx.end(), [&](int i, int j){return x[i] > x[j];});
    //// Get the names of x:
    CharacterVector names_of_x = x.names();
    //// y vector is sorted x
    NumericVector y = x[idx];
    //// Assign sorted names to y vector as names
    y.attr("names") = names_of_x[idx];
    return y;
}

//' @name bm_25
//' @title BM25 Matching
//' @description BM25 stands for Best Matching 25. It is widely using for ranking documents and a preferred method than TF*IDF scores.
//' It is used to find the similar documents from a corpus, given a new document. It is popularly used in information retrieval systems.
//' This implementation is based on c++ functions hence quite optimised as well.
//'
//' @param document a string for which to find similar documents
//' @param corpus a vector of strings against which document is to be matched
//' @param top_n top n similar documents to find
//'
//' @return a vector containing similar documents and their scores
//' @export
//'
//' @examples
//' docs <- c("chimpanzees are found in jungle",
//'           "chimps are jungle animals",
//'           "Mercedes automobiles are best",
//'           "merc is made in germany",
//'           "chimps are intelligent animals")
//'
//' sentence <- "automobiles are"
//' s <- bm_25(document=sentence, corpus=docs, top_n=2)
//'
// [[Rcpp::export]]
NumericVector bm_25(std::string document, std::vector<std::string> corpus, const int top_n){
    CharacterVector vec = superSplit(document);
    double avg_doc = avg_doc_len(corpus);
    double b = 0.75;
    double k1 = 1.20;
    int iter = -1;
    NumericVector scores(corpus.size());

    for(auto corp: corpus){
        iter += 1;
        double global_bm = 0;

        for(auto token: vec){
            std::string token2 = Rcpp::as< std::string >(token);
            regex e = std::regex("\\b" + token2 +  "\\b");
            string m = regex_replace(corp, e, "");

            int x1 = corp.size();
            int new_size = m.size();

            int net = x1 - new_size;
            double f_q_d = net / token2.size();
            double idf_val = idf(token2, corpus);
            int doc_len = std::count(corp.cbegin(), corp.cend(), ' ')+1;
            double local_bm = idf_val * ((f_q_d * (k1 + 1))/(f_q_d + k1 * (1 - b + b * (doc_len/avg_doc))));

            global_bm += local_bm;

        }

        scores[iter] = global_bm;
    }

    scores.attr("names")= corpus;
    return sort_vector_with_names(scores);
}


// Slower than base R table (counter), don't export
NumericVector counter(std::vector<std::string> vec, const bool sort=true, const bool ascending=true){

    //std::vector<std::string> v = {"abc", "cab", "bca", "abc","cab","cab"};
    std::map<std::string , int> mm;
    for (auto str : vec) {
        // cout << str << endl;
        mm[str]++;
    }

    if(!sort){
        return wrap(mm);
    } else {
        CharacterVector names;
        NumericVector vals;

        for(auto imap: mm){
            names.push_back(imap.first);
            vals.push_back(imap.second);
        }
        // std vector to numeric vector
        NumericVector ixs = wrap(sort_index(vals, ascending));
        NumericVector res = vals[ixs];
        res.attr("names") =  names[ixs];

        return res;

    }

}


// buggy - needs fixing
List supersvd(NumericMatrix mat, const int n_components){
    arma::mat mt = Rcpp::as<arma::mat>(mat);

    arma::mat U;
    arma::vec s;
    arma::mat V;

    arma::svd(U,s,V,mt);

    U = U.head_cols(n_components);
    V = V.head_cols(n_components);

    s = s.head(n_components);
    List req;

    arma::mat X_transformed = U.each_row() % s.t();

    arma::mat explained_variance_ratio  = var(X_transformed);

    float full_var = sum(var(mt));
    explained_variance_ratio = explained_variance_ratio / full_var;

    req["components"] = V; // there are the components
    // req["U"] = U;
    req["singular_values"] = Rcpp::as<std::vector<double> >(wrap(s));
    req["explained_variance_ratio"] = Rcpp::as<std::vector<double> >(wrap(explained_variance_ratio));

    return req;

}


std::map<std::string, int> CountOccurences(std::vector<std::string>& vectors)
{
    std::map<std::string, int> result;

    std::for_each(vectors.begin(), vectors.end(), [&result](std::string st) {
        ++result[st];
    });

    return result;
}

// Function to convert a std::map<K,V> to std::multimap<V,K>
template<typename K, typename V>
std::multimap<V,K> invertMap(std::map<K,V> const &map)
{
    std::multimap<V,K> multimap;

    for (auto const &pair: map) {
        multimap.insert(std::make_pair(pair.second, pair.first));
    }

    return multimap;
}

// [[Rcpp::export]]
std::vector<std::string> SortOccurence(std::vector<std::string>& vectors){

    std::map<std::string, int> result;
    result = CountOccurences(vectors);

    std::multimap<int,std::string> multimap = invertMap(result);

    std::vector<std::string> vints;
    for ( auto pair: multimap){
        vints.push_back(pair.second);
    }

    std::reverse(vints.begin(), vints.end());

    return vints;

}
