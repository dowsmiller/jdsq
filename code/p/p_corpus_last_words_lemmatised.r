#clear variables and directories from memory
rm(list = ls())

#libraries
library(stringr)
library(tidyr)
library(dplyr)
library(pbapply)

#directories
rdir <- "my_data/p0_corpus"
script_name <- "last_words_lemmatised"
wdir <- file.path("my_data/p0_corpus/outputs", script_name)
dir.create(wdir, showWarnings = FALSE)

#load data
p0_corpus_c <- readRDS(file.path(rdir, "p0_corpus_c.rds"))
p0_corpus_s <- readRDS(file.path(rdir, "p0_corpus_s.rds"))
p0_corpus_w <- readRDS(file.path(rdir, "p0_corpus_w.rds"))
p0_corpus_lc <- readRDS(file.path(rdir, "p0_corpus_lc.rds"))
p0_corpus_ls <- readRDS(file.path(rdir, "p0_corpus_ls.rds"))
p0_corpus_lw <- readRDS(file.path(rdir, "p0_corpus_lw.rds"))

#remove line numbers from p0_corpus_c and _lc and convert list items to vectors

for (i in seq_along(p0_corpus_lc)) {
    text_i <- vector()
    for (j in 1:nrow(p0_corpus_lc[[i]])) {
        text_i[j] <- p0_corpus_lc[[i]][j, 2]
    }
    p0_corpus_lc[[i]] <- text_i
}

# –––––––––– WORDS –––––––––– #

#find unlemmatised last words
last_words <- list()

for (i in seq_along(p0_corpus_w)) {
    words <- character()
    for (j in unique(p0_corpus_w[[i]][, 3])) {
        words[j] <- p0_corpus_w[[i]][, 1][p0_corpus_w[[i]][, 3] == j][length(p0_corpus_w[[i]][, 1][p0_corpus_w[[i]][, 3] == j])]
    }
    last_words[[i]] <- words
}

names(last_words) <- names(p0_corpus_w)

#table last words per text
table_last_words_per_text <- list()

for (i in seq_along(last_words)) {
  text_i <- last_words[[i]]
  text_name <- names(last_words[i])
  table_last_words_per_text[[text_name]] <- table(text_i)[order(-table(text_i))]
}

#table last words for all texts
table_last_words_all <- list()

all_last_words <- vector()

for (i in 1:length(last_words)) {
    all_last_words <- c(all_last_words, unlist(last_words[[i]]))
}

table_last_words_all <- table(all_last_words)[order(-table(all_last_words))]


# –––––––––– LEMMATA –––––––––– #

#find last lemmata
last_lemmata <- list()

for (i in seq_along(p0_corpus_lw)) {
    lemmata <- character()
    for (j in unique(p0_corpus_lw[[i]][, 5])) {
        lemmata[j] <- p0_corpus_lw[[i]][, 2][p0_corpus_lw[[i]][, 5] == j][length(p0_corpus_lw[[i]][, 2][p0_corpus_lw[[i]][, 5] == j])]
    }
    last_lemmata[[i]] <- lemmata
}

names(last_lemmata) <- names(p0_corpus_lw)


#table last lemmata per text
table_last_lemmata_per_text <- list()

for (i in 1:length(last_lemmata)) {
  text_i <- last_lemmata[[i]]
  text_name <- names(last_lemmata[i])
  table_last_lemmata_per_text[[text_name]] <- table(unlist(text_i))[order(-table(unlist(text_i)))]
}


#table last lemmata for all texts
table_last_lemmata_all <- list()

all_last_lemmata <- vector()

for (i in 1:length(last_lemmata)) {
    all_last_lemmata <- c(all_last_lemmata, last_lemmata[[i]])
}

table_last_lemmata_all <- table(all_last_lemmata)[order(-table(all_last_lemmata))]


#combine last lemmata and last words
last_lemmata_words <- list()

for (i in 1:length(last_lemmata)) {
  text_i <- last_lemmata[[i]]
  text_name <- names(last_lemmata[i])
  last_lemmata_words[[text_name]] <- data.frame(lemma = NA, word = NA)
  for (j in 1:length(text_i)) {
    last_lemmata_words[[text_name]][j, 1] <- text_i[j]
    last_lemmata_words[[text_name]][j, 2] <- last_words[[i]][j]
  }
}


#create vector of all last words per lemma
last_words_per_lemma_df <- list()
last_lemmata_words_df <- do.call(rbind, last_lemmata_words)
unique_lemmata <- unique(unlist(last_lemmata))

link_last_words_to_lemma <- function(
    unique_lemma,
    words_and_lemmata_df
) {
    lemma <- unique_lemma
    word_vector <- vector()

    for (i in 1:nrow(words_and_lemmata_df)) {
        if (words_and_lemmata_df[i, 1] == lemma) {
            word_vector <- c(word_vector, words_and_lemmata_df[i, 2])
        }
    }

    return(word_vector)
}

last_lemmata_words_list <- pblapply(
    unique_lemmata,
    link_last_words_to_lemma,
    words_and_lemmata_df = last_lemmata_words_df
)

names(last_lemmata_words_list) <- unique_lemmata

last_lemmata_words_list


#reorder list based on length of vector
last_lemmata_words_list_ord <- last_lemmata_words_list[order(sapply(last_lemmata_words_list, length), decreasing = TRUE)]


#table last words per lemma
table_last_words_per_lemma <- list()

for (i in 1:length(last_lemmata_words_list_ord)) {
  lemma_i <- last_lemmata_words_list_ord[[i]]
  lemma_name <- names(last_lemmata_words_list_ord[i])
  table_last_words_per_lemma[[lemma_name]] <- table(unlist(lemma_i))[order(-table(unlist(lemma_i)))]
}

table_last_words_per_lemma[1:10]


# –––––––––– -ment –––––––––– #

#count lines per text which contain at least one instance of a word ending in -ment
count_lines_ment <- data.frame(
    matrix(NA, nrow = length(last_words), ncol = 3)
)

rownames(count_lines_ment) <- names(last_words)
colnames(count_lines_ment) <- c("count", "total", "rate")

regex_sequence_ment <- "an[tsz]$|ment$"

for (i in 1:length(last_words)) {
  text_i <- last_words[[i]]
  text_name <- names(last_words[i])
  lines_containing_ment <- vector()
  lines <- unlist(text_i)
  for (j in 1:length(lines)) {
    line_j <- lines[[j]]
    line_ends_ment <- FALSE
    if (grepl(regex_sequence_ment, line_j)) {
        line_ends_ment <- TRUE
    }
    lines_containing_ment <- c(lines_containing_ment, line_ends_ment)
  }
  count_lines_ment[text_name, 1] <- sum(lines_containing_ment)
  count_lines_ment[text_name, 2] <- length(lines_containing_ment)
  count_lines_ment[text_name, 3] <- sum(lines_containing_ment) / length(lines_containing_ment)
}


#a few stats
mean_ment <- sum(count_lines_ment$count) / sum(count_lines_ment$total)
quartiles_ment <- quantile(count_lines_ment$rate)


#plots
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)

text_names <- names(p0_corpus_lw)

df_ment <- data.frame(
    text = text_names,
    rate = count_lines_ment$rate,
    count = count_lines_ment$coun
)

palette <- c("#003f5c")

ggplot(
    df_ment,
    aes(
        x = factor(text, levels = rev(text_names)),
        y = rate,
        label = count,
        fill = palette
    )
) +
coord_flip() +
geom_col(show.legend = FALSE) +
scale_fill_manual(values = rev(palette)) +
scale_y_continuous(labels = scales::percent) +
xlab("") +
ylab("Rate of Use of 'Easy' Rhymes (%)") +
geom_text(
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "white"
) +
theme_minimal() +
theme(
    axis.text = element_text(size = 15, face = "italic"),
    axis.title = element_text(size = 15),
)

ggsave(
    filename = file.path(wdir, "ment_rates.png"),
    device = 'png',
    dpi = 700
)


##—————————————————— FEMININE RHYMES ——————————————————##
#count lines per text which contain end in -e

count_lines_fem <- data.frame(
    matrix(NA, nrow = length(last_words), ncol = 3)
)

rownames(count_lines_fem) <- names(last_words)
colnames(count_lines_fem) <- c("count", "total", "rate")

regex_sequence_fem <- "es?$|[a-zéäëïöü]{2,}ent$"
regex_exclude_fem <- "ment$|ant$|^[a-z]i?ent$|é$"

for (i in 1:length(last_words)) {
  text_i <- last_words[[i]]
  text_name <- names(last_words[i])
  lines_containing_fem <- vector()
  lines <- unlist(text_i)
  for (j in 1:length(lines)) {
    line_j <- lines[[j]]
    line_ends_fem <- FALSE
    if (grepl(regex_exclude_fem, line_j)) {
        line_ends_fem <- FALSE
    } else if (grepl(regex_sequence_fem, line_j)) {
        line_ends_fem <- TRUE
    }
    lines_containing_fem <- c(lines_containing_fem, line_ends_fem)
  }
  count_lines_fem[text_name, 1] <- sum(lines_containing_fem)
  count_lines_fem[text_name, 2] <- length(lines_containing_fem)
  count_lines_fem[text_name, 3] <- sum(lines_containing_fem) / length(lines_containing_fem)
}

count_lines_fem <- count_lines_fem [
    !(rownames(count_lines_fem) == "52-LaiAmour1" |
    rownames(count_lines_fem) == "60-TMTV2"),
]

#a few stats
quartiles_fem <- quantile(count_lines_fem$rate)

#plots
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)

#stacked column of rates of feminine rhymes
df_fem <- data.frame(
    text = text_names,
    rate = count_lines_fem$rate,
    count = count_lines_fem$coun
)

palette <- c("#ef5675")

ggplot(
    df_fem,
    aes(
        x = factor(text, levels = rev(text_names)),
        y = rate,
        label = count,
        fill = palette
    )
) +
coord_flip() +
geom_col(show.legend = FALSE) +
scale_fill_manual(values = rev(palette)) +
scale_y_continuous(labels = scales::percent) +
xlab("") +
ylab("Rate of Feminine Rhymes in Jehan's Texts (%)") +
geom_text(
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "white"
) +
theme_minimal() +
theme(
    axis.text = element_text(size = 15, face = "italic"),
    axis.title = element_text(size = 15),
)

ggsave(filename = file.path(wdir, "fem_rates.png"), device = 'png', dpi = 700)


#—————————————————— FEM vs. -ment ——————————————————#
#work out correlation coefficient between rate of feminine rhyme and rate of -ment

count_lines_fem_ment <- cbind(count_lines_fem$rate, count_lines_ment$rate)
colnames(count_lines_fem_ment) <- c("fem_rate", "ment_rate")
rownames(count_lines_fem_ment) <- rownames(count_lines_fem)
count_lines_fem_ment <- as.data.frame(count_lines_fem_ment)

cor.test(count_lines_fem_ment$fem_rate, count_lines_fem_ment$ment_rate)


#——————————————————— ASSOCIATED RHYMES ———————————————————#
#find all lines ending ment
last_words_lines <- list()
for (i in 1:length(last_words)) {
    text_i <- last_words[[i]]
    text_name <- names(last_words[i])
    for (j in 1:length(text_i)) {
        last_words_lines[[text_name]] <- unlist(text_i)
    }
}

ment_lines <- list()
ment_lines_n <- data.frame(matrix(
    NA,
    nrow = 0,
    ncol = 2
))
n_count <- 1

colnames(ment_lines_n) <- c("text", "line")

for (i in 1:length(last_words)) {
    text_i <- last_words_lines[[i]]
    text_name <- names(last_words_lines[i])
    ment_lines[[text_name]] <- vector()
    for (j in 1:length(text_i)) {
        line_j <- text_i[j]
        if (grepl(regex_sequence_ment, line_j) == TRUE) {
            ment_lines[[text_name]][[length(ment_lines[[text_name]]) + 1]] <- line_j
            ment_lines_n[n_count, 1] <- text_name
            ment_lines_n[n_count, 2] <- j
            n_count <- n_count + 1
        }
    }
}


#create vector of all last lemmata
last_lemmata_lines <- list()
for (i in 1:length(last_lemmata)) {
    text_i <- last_lemmata[[i]]
    text_name <- names(last_lemmata[i])
    last_lemmata_lines[[text_name]] <- vector()
    for (j in 1:length(text_i)) {
        stanza_j <- text_i[j]
        last_lemmata_lines[[text_name]] <- c(last_lemmata_lines[[text_name]], unlist(stanza_j))
    }
}


#find lemmata for all lines ending ment
ment_lines_lemmata <- list()
ment_lines_unique_texts <- unique(ment_lines_n$text)

for (i in 1:length(ment_lines_unique_texts)) {
    text_name <- ment_lines_unique_texts[i]
    ment_lines_lemmata[[text_name]] <- vector()
    for (j in 1:nrow(ment_lines_n)) {
        if (ment_lines_n$text[j] == text_name) {
            line_j <- ment_lines_n$line[j]
            ment_lines_lemmata[[text_name]][[length(ment_lines_lemmata[[text_name]]) + 1]] <- last_lemmata_lines[[text_name]][line_j]
        }
    }
}


#create vector of words for all lines ending ment across all texts
ment_lines_vct_all <- vector()

for (i in 1:length(ment_lines_lemmata)) {
    ment_lines_vct_all <- c(ment_lines_vct_all, ment_lines_lemmata[[i]])
}


#table ment_lines_vct_all
ment_lines_vct_all_tbl <- table(ment_lines_vct_all)
ment_lines_vct_all_tbl <- ment_lines_vct_all_tbl[order(ment_lines_vct_all_tbl, decreasing = TRUE)]

#convert ment_lines_vc_all into dataframe with rates
ment_lines_vct_all_df <- data.frame(ment_lines_vct_all_tbl)
colnames(ment_lines_vct_all_df) <- c("lemma", "count")
ment_lines_vct_all_df$rate <- ment_lines_vct_all_df$count / sum(ment_lines_vct_all_df$count)
ment_lines_vct_all_df <- ment_lines_vct_all_df[order(ment_lines_vct_all_df$rate, decreasing = TRUE), ]

#find all lemmata in all texts
all_lemmata <- vector()
for (i in seq_along(p0_corpus_lw)) {
    all_lemmata <- c(all_lemmata, p0_corpus_lw[[i]][, 2])
}

#calculate rates of use compared to full texts
ment_lines_lemmata_all_df_top <- ment_lines_vct_all_df
lemmata_to_find <- as.character(ment_lines_lemmata_all_df_top$lemma)

count_lemmata <- length(all_lemmata)
count_final_lemmata <- length(unlist(last_lemmata))

ment_lines_lemmata_all_df_top$count_all <- rep(NA, length(lemmata_to_find))
ment_lines_lemmata_all_df_top$rate_all <- rep(NA, length(lemmata_to_find))
ment_lines_lemmata_all_df_top$vs_all <- rep(count_lemmata, length(lemmata_to_find))
ment_lines_lemmata_all_df_top$rate_not_final <- rep(NA, length(lemmata_to_find))
ment_lines_lemmata_all_df_top$vs_not_final <- rep(count_lemmata - count_final_lemmata, length(lemmata_to_find))

for (i in 1:length(lemmata_to_find)) {
    lemma_i <- lemmata_to_find[i]
    count_i <- length(all_lemmata[all_lemmata == lemma_i])
    rate_all_i <- count_i / count_lemmata
    rate_not_final_i <- (count_i - ment_lines_lemmata_all_df_top[i, "count"]) / (count_lemmata - count_final_lemmata)
    ment_lines_lemmata_all_df_top$count_all[i] <- count_i
    ment_lines_lemmata_all_df_top$rate_all[i] <- rate_all_i
    ment_lines_lemmata_all_df_top$rate_not_final[i] <- rate_not_final_i
}

ment_lines_lemmata_all_df_top$diff_rates <- ment_lines_lemmata_all_df_top$rate / ment_lines_lemmata_all_df_top$rate_not_final

ment_lines_lemmata_all_df_top <- ment_lines_lemmata_all_df_top[order(ment_lines_lemmata_all_df_top$diff_rates, decreasing = TRUE), ]

#limit to rows with count_all >= 10
ment_lines_lemmata_all_df_top <- ment_lines_lemmata_all_df_top[ment_lines_lemmata_all_df_top$count_all >= 10, ]

#save data
write.csv(ment_lines_lemmata_all_df_top, file.path(wdir, "ment_lines_lemmata_all_df_top.csv"), row.names = FALSE)


#——————————— UNIQUE LEMMATA ———————————
all_last_lemmata_tbl <- table(all_last_lemmata)[order(table(all_last_lemmata), decreasing = TRUE)]
all_last_lemmata_top <- all_last_lemmata_tbl
lemmata_to_find_all <- names(all_last_lemmata_top)

all_last_lemmata_df <- data.frame(all_last_lemmata_top)
colnames(all_last_lemmata_df) <- c("lemma", "count_final")
all_last_lemmata_df$rate_final <- all_last_lemmata_df$count_final / count_final_lemmata

all_last_lemmata_df$count_all <- rep(NA, length(lemmata_to_find_all))
all_last_lemmata_df$rate_all <- rep(NA, length(lemmata_to_find_all))
all_last_lemmata_df$vs_all <- rep(count_lemmata, length(lemmata_to_find_all))
all_last_lemmata_df$count_not_final <- rep(NA, length(lemmata_to_find_all))
all_last_lemmata_df$rate_not_final <- rep(NA, length(lemmata_to_find_all))
all_last_lemmata_df$vs_not_final <- rep(count_lemmata - count_final_lemmata, length(lemmata_to_find_all))

for (i in 1:length(lemmata_to_find_all)) {
    lemma_i <- lemmata_to_find_all[i]
    count_i <- length(all_lemmata[all_lemmata == lemma_i])
    rate_all_i <- count_i / count_lemmata
    rate_not_final_i <- (count_i - all_last_lemmata_df[i, "count_final"]) / (count_lemmata - count_final_lemmata)
    all_last_lemmata_df$count_all[i] <- count_i
    all_last_lemmata_df$rate_all[i] <- rate_all_i
    all_last_lemmata_df$count_not_final[i] <- count_i - all_last_lemmata_df[i, "count_final"]
    all_last_lemmata_df$rate_not_final[i] <- rate_not_final_i
}

all_last_lemmata_df$diff_rates <- all_last_lemmata_df$rate_final / all_last_lemmata_df$rate_not_final
all_last_lemmata_df <- all_last_lemmata_df[order(all_last_lemmata_df$diff_rates, decreasing = TRUE), ]

#limit to rows with count_all >= 10
all_last_lemmata_df <- all_last_lemmata_df[all_last_lemmata_df$count_all >= 10, ]


#save data
write.csv(all_last_lemmata_df, file.path(wdir, "all_last_lemmata_df.csv"), row.names = FALSE)
