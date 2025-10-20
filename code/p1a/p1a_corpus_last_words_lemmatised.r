#if I ever come back to this, this shows that the rates for the p1 corpus are wrong


#count stanzas per text which contain at least one instance of a word ending in -ment
p1a_names <- c(
    "01-GdA",
    "24-BibleNostreDame",
    "26-PoinesEnfer",
    "29-VieMonde",
    "44-Mais",
    "54-RobertDeable",
    "56-Menage"
)

last_words_p1a <- last_words_bnf[p1a_names]

count_stanzas_ment_p1a <- data.frame(
    matrix(NA, nrow = length(last_words_p1a), ncol = 3)
)

rownames(count_stanzas_ment_p1a) <- names(last_words_p1a)
colnames(count_stanzas_ment_p1a) <- c("count", "total", "rate")

regex_sequence_ment <- "an[tsz]$|ment$"

for (i in 1:length(last_words_p1a)) {
  text_i <- last_words_p1a[[i]]
  text_name <- names(last_words[i])
  stanzas_containing_ment <- vector()
  for (j in 1:length(text_i)) {
    stanza_j <- text_i[[j]]
    stanza_ends_ment <- FALSE
    for(k in 1:length(stanza_j)) {
      last_word_k <- stanza_j[k]
      if (grepl(regex_sequence_ment, last_word_k)) {
        stanza_ends_ment <- TRUE
      }
    }
    stanzas_containing_ment[j] <- stanza_ends_ment
  }
  count_stanzas_ment_p1a[i, 1] <- sum(stanzas_containing_ment)
  count_stanzas_ment_p1a[i, 2] <- length(stanzas_containing_ment)
  count_stanzas_ment_p1a[i, 3] <- sum(stanzas_containing_ment) / length(stanzas_containing_ment)
}

count_stanzas_ment_p1a
