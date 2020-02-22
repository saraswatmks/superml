// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/algorithm/string/join.hpp>
#include<boost/tokenizer.hpp>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
CharacterVector superSplit(const std::string &str, char sep) {
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
std::vector<std::string> superNgrams(std::string str, NumericVector ngram_range, char sep){

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
CharacterVector superTokenizer(std::vector<std::string> string){

    CharacterVector output;

    for(auto i: string){

        boost::tokenizer<> tok(i);
        for(boost::tokenizer<>::iterator beg=tok.begin(); beg!=tok.end(); ++beg){
            output.push_back(*beg);
        }

    }

    return output;

}


