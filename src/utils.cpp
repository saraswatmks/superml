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
        elems.push_back(std::move(item));
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

// Computes dot product between two vectors
// [[Rcpp::export]]
float dot(NumericVector a, NumericVector b, bool norm=true){

    arma::vec ay = Rcpp::as<arma::vec>(a);
    arma::vec by = Rcpp::as<arma::vec>(b);
    if(norm){
        return arma::norm_dot(ay, by);
    }
    return arma::dot(ay, by);

}

// Computes dot product between a vector & matrix
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
std::vector<double> sorted(NumericVector v, const bool indexes=true){

    arma::vec vect = Rcpp::as<arma::vec>(v);

    // ways to convert a 2d array to 1d array:
    // v1.attr("dim") = R_NilValue;
    // Rcpp::as<std::vector<double> >(wrap(v1));

    return Rcpp::as<std::vector<double> >(wrap(sort(vect)));

}

// Returns the indexes of a sorted array
// [[Rcpp::export]]
std::vector<int> sort_index(NumericVector v, const bool ascending=true){

    // buggy function, changes order of input v
    // Rcpp::NumericVector res = Rcpp::clone(v);
    arma::vec vect = Rcpp::as<arma::vec>(v);
    // add 1 because starts with 0
    if(ascending){
        return Rcpp::as<std::vector<int> >(wrap(stable_sort_index(vect, "ascend") + 1));
    }
    return Rcpp::as<std::vector<int> >(wrap(stable_sort_index(vect, "descend") + 1));

}


// [[Rcpp::export]]
arma::mat normalize2d(NumericMatrix x, int pnorm=2, int axis=0){

    arma::mat mt = Rcpp::as<arma::mat>(x);

    return arma::normalise(mt, pnorm, axis);

}

// [[Rcpp::export]]
std::vector<double> normalize1d(NumericVector x){

    arma::vec v = Rcpp::as<arma::vec>(x);

    return Rcpp::as<std::vector<double>>(wrap(arma::normalise(v)));

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

