#clear variables and directories from memory
rm(list = ls())

#libraries
library(stringr)

#directories
rdir <- "my_data/r_corpus"
script_name <- "first_words_lemmatised"
wdir <- file.path("my_data/r_corpus/outputs", script_name)
dir.create(wdir, showWarnings = FALSE)

#load data
r_corpus_c <- readRDS(file.path(rdir, "r_corpus_c.rds"))
r_corpus_s <- readRDS(file.path(rdir, "r_corpus_s.rds"))
r_corpus_w <- readRDS(file.path(rdir, "r_corpus_w.rds"))
r_corpus_lc <- readRDS(file.path(rdir, "r_corpus_lc.rds"))
r_corpus_ls <- readRDS(file.path(rdir, "r_corpus_ls.rds"))
r_corpus_lw <- readRDS(file.path(rdir, "r_corpus_lw.rds"))

#remove line numbers from _r_corpus_c and _lc and convert list items to vectors
for (i in seq_along(r_corpus_c)) {
    text_i <- vector()
    for (j in 1:nrow(r_corpus_c[[i]])) {
        text_i[j] <- r_corpus_c[[i]][j, 2]
    }
    r_corpus_c[[i]] <- text_i
}

for (i in seq_along(r_corpus_lc)) {
    text_i <- vector()
    for (j in 1:nrow(r_corpus_lc[[i]])) {
        text_i[j] <- r_corpus_lc[[i]][j, 2]
    }
    r_corpus_lc[[i]] <- text_i
}


#––––––––––––FOR WORDS––––––––––––#
#find unlemmatised first words
first_words <- list()

for (i in seq_along(r_corpus_w)) {
    words <- character()
    for (j in unique(r_corpus_w[[i]][, 3])) {
        words[j] <- r_corpus_w[[i]][, 1][r_corpus_w[[i]][, 3] == j][1]
    }
    first_words[[i]] <- words
}

names(first_words) <- names(r_corpus_w)

#calculate frequencies and rates for all texts
all_first <- unlist(first_words)
all_first_freq <- table(all_first)[order(table(all_first), decreasing = TRUE)]
all_firsts_head <- head(all_first_freq)

all_first_rate <- vector()
firsts_freq_all <- length(all_first)

for (i in seq_along(all_first_freq)) {
  token <- names(all_first_freq[i])
  freq <- all_first_freq[i]
  rate <- freq / firsts_freq_all
  all_first_rate[token] <- rate
}


#calculate frequencies and rates for each text
list_firsts_freq <- list()
list_firsts_rate <- list()

for (i in 1:length(first_words)) {
  text_i <- first_words[[i]]
  text_name <- names(first_words[i])
  firsts_freq <- table(unlist(text_i))
  firsts_freq_decr <- firsts_freq[order(firsts_freq, decreasing = TRUE)]
  list_firsts_freq[[text_name]] <- firsts_freq_decr

  count_firsts <- length(text_i)

  for (j in 1:length(firsts_freq_decr)) {
    token <- names(firsts_freq_decr[j])
    freq <- firsts_freq_decr[j]
    rate <- freq / count_firsts
    list_firsts_rate[[text_name]][token] <- rate
  }
}


#select head tokens per text
list_firsts_head <- data.frame(matrix(nrow = 0, ncol = 6))

for (i in 1:length(list_firsts_freq)) {
  text_i <- list_firsts_freq[[i]]
  text_name <- names(list_firsts_freq[i])
  head_tokens <- head(names(text_i))
  list_firsts_head[text_name, ] <- head_tokens
}


#calculate p-values
#includes the target text once

#define function for p-value test
p_value_rand <- function(
    observed_tar = integer(),
    observed_ref = integer(),
    total_tar = integer(),
    total_ref = integer(),
    sample_size = 100000) {

    sum_observed <- observed_tar + observed_ref #sum of observed instances
    sum_total <- total_tar + total_ref #sum of total instances

    #null rate: expected rate if there is no difference between a and b
    null_rate <- sum_observed / sum_total

    #observed difference between probabilities
    observed_prob_dif <- (observed_ref / total_ref) - (observed_tar / total_tar)

    #generate random samples, counting observed instances
    ref_rand <- rbinom(sample_size, total_ref, null_rate)
    tar_rand <- rbinom(sample_size, total_tar, null_rate)

    #work out differences in probability between two samples
    rand_prob_difs <- (ref_rand / total_ref) - (tar_rand / total_tar)

    #make differences in probability absolute
    abs_rand_prob_difs <- abs(rand_prob_difs)
    abs_observed_prob_dif <- abs(observed_prob_dif)

    #calculate the p value (the proportion of the random sample for which
        #the difference in probability exceeds the observed probability)
    p_value <- mean(abs_rand_prob_difs >= abs_observed_prob_dif)

    p_value
}

#define function to apply p-value test to single text
p_value_tabled_text <- function(
    text_name = character(),
    list_firsts_freq = list(),
    all_first_freq = list(),
    sample_size = integer(),
    tokens = character()) {

    #calculate p-values for each token in list
    p_values <- vector()
    for (i in 1:length(tokens)) {
        token <- tokens[i]
        #get observed frequencies
        observed_tar <- list_firsts_freq[[text_name]][token]
        observed_ref <- all_first_freq[token] - observed_tar

        #get total frequencies
        total_tar <- sum(unlist(list_firsts_freq[[text_name]]))
        total_ref <- sum(unlist(all_first_freq)) - total_tar

        p_value <- p_value_rand(
            observed_tar = observed_tar[token],
            observed_ref = observed_ref[token],
            total_tar = total_tar,
            total_ref = total_ref,
            sample_size = sample_size
        )
        p_values[token] <- p_value
    }

    p_values
}

#apply p-value test to all texts
list_compared_firsts_p <- list()
compare_tokens <- 6
total_n <- 100000
tokens <- names(all_first_freq)[1:compare_tokens]
text_names <- names(list_firsts_freq)

list_compared_firsts_p <- pblapply(
    text_names,
    p_value_tabled_text,
    list_firsts_freq = list_firsts_freq,
    all_first_freq = all_first_freq,
    sample_size = total_n,
    tokens = tokens
)

names(list_compared_firsts_p) <- text_names

#combine into single data.frame with rownames as text names
compared_firsts_p_df <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(compared_firsts_p_df) <- tokens
for (i in 1:length(list_compared_firsts_p)) {
  text_i <- list_compared_firsts_p[[i]]
  text_name <- names(list_compared_firsts_p[i])
  compared_firsts_p_df[text_name, ] <- text_i
}

#for compared_firsts_p_df, if value is less than 0.05, then TRUE, else FALSE
compared_firsts_p_test_df <- compared_firsts_p_df < 0.05

#create df for each token in tokens
list_tokens_texts <- pblapply(tokens, function(
  token,
  all_first_rate,
  list_firsts_freq,
  list_firsts_rate,
  compared_firsts_p_df
) {
    df <- data.frame(matrix(nrow = length(text_names), ncol = 4))
    colnames(df) <- c("text", "obs_freq", "exp_freq", "p_value")
    for (i in 1:length(text_names)) {
        df[i, "text"] <- text_names[i]
        df[i, "obs_freq"] <- as.numeric(list_firsts_freq[[i]][token])
        df[i, "exp_freq"] <- as.numeric(all_first_rate[token] * sum(unlist(list_firsts_freq[[i]])))
        df[i, "p_value"] <- as.numeric(compared_firsts_p_df[i, token])
    }
    df
},
all_first_rate,
list_firsts_freq,
list_firsts_rate,
compared_firsts_p_df
)

names(list_tokens_texts) <- tokens


#write to csv
write.csv(as.data.frame(head(all_first_freq)), file.path(wdir, "head_first_freq.csv"))


first_per_text <- data.frame(matrix(nrow = length(text_names), ncol = 7))
first_per_text$X1 <- text_names
for (i in 1:length(text_names)) {
  first_per_text[i, 2:7] <- as.character(as.data.frame(list_firsts_freq[[i]])[1:6, 1])
}
write.csv(first_per_text, file.path(wdir, "first_per_text.csv"))

write.csv(compared_firsts_p_df, file.path(wdir, "compared_firsts_p_df.csv"))

for (i in 1:length(list_tokens_texts)) {
  token_i <- list_tokens_texts[[i]]
  token_name <- names(list_tokens_texts[i])
  write.csv(token_i, file.path(wdir, paste0("list_tokens_texts_", i, "_", token_name, ".csv")))
}



#––––––––––––FOR LEMMATA––––––––––––#
#find first lemmata
first_lemmata <- list()

for (i in seq_along(r_corpus_lw)) {
    lemmata <- character()
    for (j in unique(r_corpus_lw[[i]][, 5])) {
        lemmata[j] <- r_corpus_lw[[i]][, 2][r_corpus_lw[[i]][, 5] == j][1]
    }
    first_lemmata[[i]] <- lemmata
}

names(first_lemmata) <- names(r_corpus_lw)

#calculate frequencies and rates for all texts
all_first_lemmata <- unlist(first_lemmata)
all_first_lemmata_freq <- table(all_first_lemmata)[order(table(all_first_lemmata), decreasing = TRUE)]
all_first_lemmata_head <- head(all_first_lemmata_freq)

all_first_lemmata_rate <- all_first_lemmata_freq / sum(all_first_lemmata_freq)


#save all_first_lemmata_freq as csv
write.csv(as.data.frame(all_first_lemmata_freq), file.path(wdir, "all_first_lemmata_freq.csv"))


#calculate frequencies and rates for each text
list_first_lemmata_freq <- list()
list_first_lemmata_rate <- list()

for (i in 1:length(first_lemmata)) {
  text_i <- first_lemmata[[i]]
  text_name <- names(first_lemmata[i])
  list_first_lemmata_freq[[text_name]] <- table(unlist(text_i))[order(table(unlist(text_i)), decreasing = TRUE)]
  list_first_lemmata_rate[[text_name]] <- list_first_lemmata_freq[[text_name]] / sum(list_first_lemmata_freq[[text_name]])
}


#calculate p-values for each text
compare_tokens <- 6
total_n <- 100000
tokens_lemmata <- names(all_first_lemmata_head)[1:compare_tokens]
text_names <- names(list_first_lemmata_freq)

list_compared_first_lemmata_p <- pblapply(
  text_names,
  p_value_tabled_text,
  list_firsts_freq = list_first_lemmata_freq,
  all_first_freq = all_first_lemmata_freq,
  sample_size = total_n,
  tokens = tokens_lemmata
)

names(list_compared_first_lemmata_p) <- text_names


#combine into single data.frame with rownames as text names
compared_first_lemmata_p_df <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(compared_first_lemmata_p_df) <- tokens_lemmata
for (i in 1:length(text_names)) {
  compared_first_lemmata_p_df[i, ] <- as.numeric(list_compared_first_lemmata_p[[i]])
}

rownames(compared_first_lemmata_p_df) <- text_names

#for compared_first_lemmata_p_df, if value is less than 0.05, then PASS, else FAIL
compared_first_lemmata_p_test_df <- compared_first_lemmata_p_df < 0.05


#create df for each token in tokens
list_lemma_tokens_texts <- pblapply(tokens_lemmata, function(
  token,
  all_first_rate = all_first_lemmata_rate,
  list_firsts_freq = list_first_lemmata_freq,
  list_firsts_rate = list_first_lemmata_rate,
  compared_firsts_p_df = compared_first_lemmata_p_df
) {
  df <- data.frame(matrix(nrow = length(text_names), ncol = 4))
    colnames(df) <- c("text", "obs_freq", "exp_freq", "p_value")
    for (i in 1:length(text_names)) {
        df[i, "text"] <- text_names[i]
        df[i, "obs_freq"] <- as.numeric(list_firsts_freq[[i]][token])
        df[i, "exp_freq"] <- as.numeric(all_first_rate[token] * sum(unlist(list_firsts_freq[[i]])))
        df[i, "p_value"] <- as.numeric(compared_firsts_p_df[i, token])
    }
    df
}
)

names(list_lemma_tokens_texts) <- tokens_lemmata


#write to csv
write.csv(as.data.frame(head(all_first_lemmata_freq)), file.path(wdir, "head_first_lemmata_freq.csv"))

first_lemmata_per_text <- data.frame(matrix(nrow = length(text_names), ncol = 7))
first_lemmata_per_text$X1 <- text_names
for (i in 1:length(text_names)) {
  first_lemmata_per_text[i, 2:7] <- as.character(as.data.frame(list_first_lemmata_freq[[i]])[1:6, 1])
}
write.csv(first_lemmata_per_text, file.path(wdir, "first_lemmata_per_text.csv"))

write.csv(compared_first_lemmata_p_df, file.path(wdir, "compared_first_lemmata_p_df.csv"))

for (i in 1:length(list_lemma_tokens_texts)) {
  token_i <- list_lemma_tokens_texts[[i]]
  token_name <- names(list_lemma_tokens_texts[i])
  write.csv(token_i, file.path(wdir, paste0("list_lemma_tokens_texts_", i, "_", token_name, ".csv")))
}