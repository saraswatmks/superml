context("text functions")

test_that("calculate bm25 ranking", {

    example <- c("white audi 2.5 car",
                 "black shoes from office",
                 "new mobile iphone 7",
                 "audi tyres audi a3",
                 "nice audi bmw toyota corolla")

    get_bm <- bm25$new(example,n_cores = 2)
    input_document <- c("white toyota corolla")
    result <- get_bm$most_similar(document = input_document, topn = 4)
    expect_equal(result, c("nice audi bmw toyota corolla",
                           "white audi 2.5 car",
                           "black shoes from office",
                           "new mobile iphone 7"))
})


test_that("create word count dictionary", {
    d <- list(c("i","am","bad"),c("you","are","also","bad"))
    counts <- Counter(d, sort=T, decreasing=T)
    expect_equal(counts, list(bad=2, also=1, am=1, are=1, i=1, you=1))
})
