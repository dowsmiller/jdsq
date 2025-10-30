#clear variables and directories from memory
rm(list = ls())

#install the relevant packages
# install.packages(c("readxl", "stringr", "pbapply"))

#load the relevant packages
library(readxl)
library(stringr)
library(pbapply)

#set path
r_corpus_path <- "rdat/r/xslx"

#get files in directory
r_corpus_files <- list.files(r_corpus_path, pattern = "*.xlsx", full.names = TRUE)

#read in files
r_corpus <- pblapply(r_corpus_files, read_excel)

#convert to tables
r_corpus <- pblapply(r_corpus, as.data.frame)

#set df name to the value in row 2 column 2 for each
df_names <- vector("character", length(r_corpus))

for (i in 1:length(r_corpus)) {
  df_names[i] <- paste(sprintf("%02d", 1:length(r_corpus))[i], r_corpus[[i]][2, 2])
}

names(r_corpus) <- df_names

#set col1 name to 'line' and col2 name to 'text' for each
for (i in 1:length(r_corpus)) {
  colnames(r_corpus[[i]]) <- c("line", "text")
}

#clear rownames
for (i in 1:length(r_corpus)) {
  rownames(r_corpus[[i]]) <- NULL
}

#remove first three rows
r_corpus <- pblapply(r_corpus, function(x) x[-c(1:3), ])

#empty cells containing a combination of only IVXL
for (i in 1:length(r_corpus)) {
  r_corpus[[i]][, 2] <- str_replace_all(r_corpus[[i]][, 2], "^[IVXL]+$", "")
}

#remove trailing spaces
for (i in 1:length(r_corpus)) {
  r_corpus[[i]][, 2] <- str_trim(r_corpus[[i]][, 2])
}

#remove square brackets and replace quotation marks
for (i in seq_along(r_corpus)) {
    r_corpus[[i]][, 2] <- str_replace_all(r_corpus[[i]][, 2], "\\[", "")
    r_corpus[[i]][, 2] <- str_replace_all(r_corpus[[i]][, 2], "\\]", "")
    r_corpus[[i]][, 2] <- str_replace_all(r_corpus[[i]][, 2], "[^A-zÀ-ÿ0-9\\(\\)\\.,?!\";:—”“]»", "”")
    r_corpus[[i]][, 2] <- str_replace_all(r_corpus[[i]][, 2], "«[^A-zÀ-ÿ0-9\\(\\)\\.,?!\";:—”“]", "“")
    r_corpus[[i]][, 2] <- str_replace_all(r_corpus[[i]][, 2], "^—[^A-zÀ-ÿ0-9\\(\\)\\.,?!\";:—”“]", "“")
}

#deal with empty cells
for (i in 1:length(r_corpus)) {
  for (j in 1:nrow(r_corpus[[i]])) {
    for (k in 1:ncol(r_corpus[[i]])) {
      if (is.na(r_corpus[[i]][j, k]) || r_corpus[[i]][j, k] == "NA" || r_corpus[[i]][j, k] == "") {
        r_corpus[[i]][j, k] <- NA
      }
    }
  }
}

#remove rows for which first column is NA and second contains string
for (i in 1:length(r_corpus)) {
  rows_to_exclude <- which(is.na(r_corpus[[i]][, 1]) == TRUE & !is.na(r_corpus[[i]][, 2]) == TRUE)
  r_corpus[[i]] <- r_corpus[[i]][-rows_to_exclude, ]
}

#remove last row if first column is NA
for (i in 1:length(r_corpus)) {
  if (is.na(r_corpus[[i]][nrow(r_corpus[[i]]), 1]) == TRUE) {
    r_corpus[[i]] <- r_corpus[[i]][-nrow(r_corpus[[i]]), ]
  }
}

#replace NA with ""
for (i in 1:length(r_corpus)) {
  for (j in 1:nrow(r_corpus[[i]])) {
    for (k in 1:ncol(r_corpus[[i]])) {
      if (is.na(r_corpus[[i]][j, k]) == TRUE) {
        r_corpus[[i]][j, k] <- ""
      }
    }
  }
}

#save the second column of r_corpus as individual .txt files
for (i in 1:length(r_corpus)) {
  file_name <- paste0(gsub("[’ ]", "_", gsub("\\.", "", names(r_corpus)[i])))
  file_path <- paste0("rdat/r/txt/", file_name, ".txt")
  write.table(r_corpus[[i]][, 2], file_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}


# —————————— r_corpus_c = all lines concatenated ——————————
r_corpus_c <- r_corpus
for (i in 1:length(r_corpus_c)) {
  rows_to_exclude <- which(r_corpus_c[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    r_corpus_c[[i]] <- r_corpus_c[[i]][-rows_to_exclude, ]
  }
}

#save r_corpus_c as .rds
dir.create("wdat/r", showWarnings = FALSE)
saveRDS(r_corpus_c, "wdat/r/r_corpus_c.rds")


# —————————— r_corpus_s = separated into stanzas ——————————
#add column with stanza number
r_corpus_s <- r_corpus

for (i in 1:length(r_corpus_s)) {
    r_corpus_s[[i]]$stanza <- NA
    stanza <- 1
    for (j in 1:nrow(r_corpus_s[[i]])) {
        if (r_corpus_s[[i]][j, 1] == "") {
            stanza <- stanza + 1
        } else {
            r_corpus_s[[i]][j, 3] <- stanza
        }
    }
}

#remove empty rows
for (i in 1:length(r_corpus_s)) {
  rows_to_exclude <- which(r_corpus_s[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    r_corpus_s[[i]] <- r_corpus_s[[i]][-rows_to_exclude, ]
  }
}

#group into dfs according to stanza number
r_corpus_s_temp <- list()

for (i in 1:length(r_corpus_s)) {
  r_corpus_s_temp[[i]] <- split(r_corpus_s[[i]], r_corpus_s[[i]]$stanza)
}

r_corpus_s <- r_corpus_s_temp

#remove stanza column
for (i in 1:length(r_corpus_s)) {
  for (j in 1:length(r_corpus_s[[i]])) {
    r_corpus_s[[i]][[j]] <- r_corpus_s[[i]][[j]][, -3]
  }
}

#rename lists as in r_corpus
names(r_corpus_s) <- names(r_corpus)

#save r_corpus_s as .rds
saveRDS(r_corpus_s, "wdat/r/r_corpus_s.rds")


# —————————— r_corpus_w = separated into words with stanza and line number ——————————
r_corpus_w <- r_corpus_s

#add column with stanza number
for (i in 1:length(r_corpus_w)) {
  for (j in 1:length(r_corpus_w[[i]])) {
    r_corpus_w[[i]][[j]]$stanza <- j
  }
}

#modify text
for (i in 1:length(r_corpus_w)) {
  for (j in 1:length(r_corpus_w[[i]])) {
    #remove all punctuation except apostrophes
    r_corpus_w[[i]][[j]]$text <- str_replace_all(r_corpus_w[[i]][[j]]$text, "[^A-zÀ-ÿ'’ ]", "")

    #replace apostrophes with space
    r_corpus_w[[i]][[j]]$text <- str_replace_all(r_corpus_w[[i]][[j]]$text, "['’]", " ")

    #remove trailing spaces
    r_corpus_w[[i]][[j]]$text <- str_trim(r_corpus_w[[i]][[j]]$text)

    #convert to lowercase
    r_corpus_w[[i]][[j]]$text <- str_to_lower(r_corpus_w[[i]][[j]]$text)
  }
}

#separate text into words
r_corpus_w_temp <- list()

for (i in 1:length(r_corpus_w)) {
    words <- vector()
    stanzas <- vector()
    lines <- vector()

    for (j in 1:length(r_corpus_w[[i]])) {
        for (k in 1:nrow(r_corpus_w[[i]][[j]])) {
            words <- c(words, unlist(str_split(r_corpus_w[[i]][[j]][k, 2], " ")))
            n_words <- length(unlist(str_split(r_corpus_w[[i]][[j]][k, 2], " ")))
            stanzas <- c(stanzas, rep(r_corpus_w[[i]][[j]][k, 3], n_words))
            lines <- c(lines, rep(r_corpus_w[[i]][[j]][k, 1], n_words))
        }
    }
    r_corpus_w_temp[[i]] <- data.frame(word = words, stanza = stanzas, line = lines, stringsAsFactors = FALSE)
}

r_corpus_w <- r_corpus_w_temp

#delete rows with empty word
for (i in 1:length(r_corpus_w)) {
  rows_to_exclude <- which(r_corpus_w[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    r_corpus_w[[i]] <- r_corpus_w[[i]][-rows_to_exclude, ]
  }
}

#rename lists as in r_corpus
names(r_corpus_w) <- names(r_corpus)

#reset rownames
for (i in 1:length(r_corpus_w)) {
  rownames(r_corpus_w[[i]]) <- NULL
}

#save r_corpus_w as .rds
saveRDS(r_corpus_w, "wdat/r/r_corpus_w.rds")


# –––––––––– r_corpus_lw = lemmatised list aligned with words ––––––––––
#load lemmatised files
r_corpus_lw <- list.files("rdat/r/lemmatised/", pattern = "*.txt", full.names = TRUE)

r_corpus_lw <- pblapply(r_corpus_lw, read.delim, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("word", "pos", "lemma"))

#modify words
r_corpus_lw <- pblapply(r_corpus_lw, function(x) {
  for (i in 1:nrow(x)) {
    x[i, "word"] <- tolower(str_replace_all(x[i, "word"], "[^A-zÀ-ÿ]", ""))
  }
  x
})

#remove rows with empty words
for (i in 1:length(r_corpus_lw)) {
  rows_to_exclude <- which(r_corpus_lw[[i]][ ,1] == "")
  if (length(rows_to_exclude) > 0) {
    r_corpus_lw[[i]] <- r_corpus_lw[[i]][-rows_to_exclude, ]
  }
}

#reset row names
for (i in 1:length(r_corpus_lw)) {
  rownames(r_corpus_lw[[i]]) <- NULL
}

#combine into dataframe of words and lemmata with stanza and line numbers
combine_lw <- function(index, word_list, lemma_list) {

    word_df <- word_list[[index]]
    lemma_df <- lemma_list[[index]]

    df <- data.frame(
        word = NA,
        lemma = NA,
        lemma_assc = NA,
        stanza = NA,
        line = NA
    )

    words <- word_df$word
    lemmata <- lemma_df$lemma
    lemmata_assc <- lemma_df$word
    stanzas <- word_df$stanza
    lines <- word_df$line

    if (length(words) == length(lemmata)) {
        df <- data.frame(
            word = words,
            lemma = lemmata,
            lemma_assc = lemmata_assc,
            stanza = stanzas,
            line = lines
        )
    } else if (length(words) > length(lemmata)) {
        offset <- 0

        for (i in 1:length(lemmata)) {
            word <- words[i + offset]
            lemma <- lemmata[i]
            lemma_assc <- lemmata_assc[i]
            stanza <- stanzas[i + offset]
            df[i, "lemma"] <- lemma
            df[i, "lemma_assc"] <- lemma_assc

            if (word == lemma_assc) {
                df[i, "word"] <- word
                df[i, "stanza"] <- stanza
                df[i, "line"] <- lines[i + offset]
            } else if (word != lemma_assc) {
              possible_word <- paste0(word, words[i + offset + 1])

              if (possible_word == lemma_assc) {
                df[i, "word"] <- possible_word
                df[i, "stanza"] <- stanzas[i + offset]
                df[i, "line"] <- lines[i + offset]
                offset <- offset + 1
              } else if (possible_word != lemma_assc) {
                warning("possible_word not equal to lemma_assc")
              }
            }
        }
    } else if (length(words) < length(lemmata)) {
      warning("lemmata list longer than words")
    }

    df
}

r_corpus_lw <- pblapply(seq_along(r_corpus_w), combine_lw, r_corpus_w, r_corpus_lw)

#rename lists as in r_corpus
names(r_corpus_lw) <- names(r_corpus)

#save r_corpus_lw as .rds
saveRDS(r_corpus_lw, "wdat/r/r_corpus_lw.rds")


# –––––––––– r_corpus_ls = lemmatised in stanzas ––––––––––
#recombine lines and stanzas for lemmata

r_corpus_ls <- list()

for (i in seq_along(r_corpus_lw)) {
    r_corpus_ls[[i]] <- list()
    for (j in unique(r_corpus_lw[[i]]$stanza)) {
        stanza <- r_corpus_lw[[i]][r_corpus_lw[[i]]$stanza == j, ]
        r_corpus_ls[[i]][[j]] <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(r_corpus_ls[[i]][[j]]) <- c("line", "text")
        for (k in 1:length(unique(stanza$line))) {
            line <- paste0(stanza[stanza$line == unique(stanza$line)[k], 2], collapse = " ")
            r_corpus_ls[[i]][[j]][k, 1] <- unique(stanza$line)[k]
            r_corpus_ls[[i]][[j]][k, 2] <- line
        }
    }
}

#rename lists as in r_corpus
names(r_corpus_ls) <- names(r_corpus)

#save r_corpus_ls as .rds
saveRDS(r_corpus_ls, "wdat/r/r_corpus_ls.rds")


# –––––––––– r_corpus_lc = lemmatised and concatenated ––––––––––

r_corpus_lc <- list()

for (i in seq_along(r_corpus_ls)) {
    r_corpus_lc[[i]] <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(r_corpus_lc[[i]]) <- c("line", "text")
    for (j in seq_along(r_corpus_ls[[i]])) {
        for (k in 1:nrow(r_corpus_ls[[i]][[j]])) {
            r_corpus_lc[[i]][nrow(r_corpus_lc[[i]]) + 1, ] <- c(
                r_corpus_ls[[i]][[j]][k, 1],
                r_corpus_ls[[i]][[j]][k, 2]
            )
        }
    }
}

#rename lists as in r_corpus
names(r_corpus_lc) <- names(r_corpus)

#save r_corpus_lc as .rds
saveRDS(r_corpus_lc, "wdat/r/r_corpus_lc.rds")
