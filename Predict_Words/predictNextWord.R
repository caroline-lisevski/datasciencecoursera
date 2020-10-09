# R script: word prediction

library(tm)
library(dplyr)

unigram <- readRDS("unigram.RDS")
bigram <- readRDS("bigram.RDS")
trigram <- readRDS("trigram.RDS")
quadgram <- readRDS("quadgram.RDS")

lasts <- function(s) {
  word <- strsplit(s, "\\s+")
  sapply(word, function(l) tail(l, 1))
}

combine <- function(word) {
  paste0(word, collapse = ", ")
}

NGramSearch <- function(w, d, num.predictions) {
  search.string <- paste0(w, collapse = " ")
  results <- grep(paste0("^", search.string, " ", collapse = ""), d$name,
                  perl = TRUE, value = TRUE)
  if (length(results) > 0) {
    return(lasts(head(results, num.predictions)))
  } else {
    NULL
  }
}

Uno <- function(w, n) {
  bigram.r <- NGramSearch(w, bigram, n)
  if (length(bigram.r) == n) {
    bigram.r
  } else if (length(bigram.r) < n) {
    # Use unigram results t fill up
    c(bigram.r, head(unigram$name, n - length(bigram.r)))
  } else {
    head(bigram.r, n)
  }
}

Duo <- function(w, n) {
  trigram.r <- NGramSearch(w, trigram, n)
  if (length(trigram.r) == n) {
    trigram.r
  } else if (length(trigram.r) < n) {
    # Combined trigram and bigram predictions, starting with trigrams
    c(trigram.r, Uno(tail(w, 1), n - length(trigram.r)))
  } else {
    head(trigram.r, n)
  }
}

Tre <- function(w, n) {
  quadgram.r <- NGramSearch(w, quadgram, n)
  if (length(quadgram.r) == n) {
    quadgram.results
  } else if (length(quadgram.r) < n) {
    # Combined quadgram and trigram predictions, with quadgram ones at the top
    c(quadgram.r, Duo(tail(w, 2), n - length(quadgram.r)))
  } else {
    head(quadgram.r, n)
  }
}

predictWord <- function(text, num.predictions) {
  word <- text %>% strsplit("\\s+") %>% unlist %>%
    removePunctuation %>% tolower %>% removeNumbers
  # If no input, return unigram results
  if (length(word) == 0) {
    combine(head(unigram$name, num.predictions))
  } else if (length(word) == 1) {
    combine(Uno(word, num.predictions))
  } else if (length(word) == 2) {
    combine(Duo(tail(word, 2), num.predictions))
  } else {
    combine(Tre(tail(word, 3), num.predictions))
  }
}