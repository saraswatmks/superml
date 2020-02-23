// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/algorithm/string/join.hpp>
#include <boost/tokenizer.hpp>
#include <regex>
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

