#clear variables and directories from memory
rm(list = ls())

#install the relevant packages
install.packages(c("stringr", "pbapply"))

#load the relevant packages
library(stringr)
library(pbapply)

#set path
j_corpus_path <- "my_data/j_corpus/txt_raw"

#get files in directory
j_corpus_files <- list.files(j_corpus_path, pattern = "*.txt", full.names = TRUE)

#read in files
j_corpus <- pblapply(j_corpus_files, readLines)

#set df name to the file name after character 26 for each
    #with ".txt" removed and "_" replaced with " "
df_names <- vector("character", length(j_corpus))

for (i in 1:length(j_corpus)) {
  df_names[i] <- str_replace_all(str_sub(j_corpus_files[i], 26), ".txt", "")
  df_names[i] <- str_replace_all(df_names[i], "_", " ")
}

names(j_corpus) <- df_names

#delete trailing spaces
for (i in 1:length(j_corpus)) {
  j_corpus[[i]] <- str_trim(j_corpus[[i]])
}

#delete empty strings from vector if preceded by another empty string
for (i in 1:length(j_corpus)) {
    for (j in 1:length(j_corpus[[i]])) {
        if (j > 1) {
            if (j_corpus[[i]][j] == "" && (j_corpus[[i]][j - 1] == "" || is.na(j_corpus[[i]][j - 1]))) {
                j_corpus[[i]][j] <- NA
            }
        }
    }

    j_corpus[[i]] <- j_corpus[[i]][!sapply(j_corpus[[i]], is.na)]
}

#delete final lines matching regex ^[AMENamen. ]+$
for (i in 1:length(j_corpus)) {
    for (j in 1:length(j_corpus[[i]])) {
        if (j == length(j_corpus[[i]])) {
            if (str_detect(j_corpus[[i]][j], "^[AMENamen. ]+$")) {
                j_corpus[[i]][j] <- NA
            }
        }
    }

    j_corpus[[i]] <- j_corpus[[i]][!sapply(j_corpus[[i]], is.na)]
}

#delete empty lines in first and final position
for (i in 1:length(j_corpus)) {
    if (j_corpus[[i]][1] == "") {
        j_corpus[[i]][1] <- NA
    }

    if (j_corpus[[i]][length(j_corpus[[i]])] == "") {
        j_corpus[[i]][length(j_corpus[[i]])] <- NA
    }

    j_corpus[[i]] <- j_corpus[[i]][!sapply(j_corpus[[i]], is.na)]
}

#save j_corpus as individual text files
dir.create("my_data/j_corpus/txt", showWarnings = FALSE)

for (i in 1:length(j_corpus)) {
    file_name <- paste0(gsub("[’ ]", "_", gsub("\\.", "", names(j_corpus)[i])))
    file_path <- paste0("my_data/j_corpus/txt/", file_name, ".txt")
    write.table(j_corpus[[i]], file_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}

# –––––––––– j_corpus_c = all lines concatenated ––––––––––
j_corpus_c <- j_corpus

for (i in 1:length(j_corpus_c)) {
    for (j in 1:length(j_corpus_c[[i]])) {
        if (j_corpus_c[[i]][j] == "") {
            j_corpus_c[[i]][j] <- NA
        }
    }

    j_corpus_c[[i]] <- j_corpus_c[[i]][!sapply(j_corpus_c[[i]], is.na)]
}

#save j_corpus_c as .rds
saveRDS(j_corpus_c, "my_data/j_corpus/j_corpus_c.rds")


# –––––––––– j_corpus_s = separated into stanzas ––––––––––
#add column with stanza number
j_corpus_s <- j_corpus

for (i in 1:length(j_corpus_s)) {
    j_corpus_s[[i]] <- data.frame(text = j_corpus_s[[i]], stanza = NA, stringsAsFactors = FALSE)
    stanza <- 1
    for (j in 1:nrow(j_corpus_s[[i]])) {
        if (j_corpus_s[[i]][j, 1] == "") {
            stanza <- stanza + 1
        } else {
            j_corpus_s[[i]][j, 2] <- stanza
        }
    }
}

#remove empty rows
for (i in 1:length(j_corpus_s)) {
  rows_to_exclude <- which(j_corpus_s[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    j_corpus_s[[i]] <- j_corpus_s[[i]][-rows_to_exclude, ]
  }
}

#group into dfs according to stanza number
j_corpus_s_temp <- list()

for (i in 1:length(j_corpus_s)) {
    j_corpus_s_temp[[i]] <- split(j_corpus_s[[i]], j_corpus_s[[i]]$stanza)
}

j_corpus_s <- j_corpus_s_temp

#remove stanza column
for (i in 1:length(j_corpus_s)) {
    for (j in 1:length(j_corpus_s[[i]])) {
        j_corpus_s[[i]][[j]] <- j_corpus_s[[i]][[j]][, 1]
    }
}

#add line number column before text
for (i in 1:length(j_corpus_s)) {
    line <- 1
    for (j in 1:length(j_corpus_s[[i]])) {
        df <- data.frame(line = NA, text = j_corpus_s[[i]][[j]], stringsAsFactors = FALSE)
        for (k in 1:nrow(df)) {
            df[k, 1] <- line
            line <- line + 1
        }
        j_corpus_s[[i]][[j]] <- df
    }
}

#rename lists as in j_corpus
names(j_corpus_s) <- names(j_corpus)

#save j_corpus_s as .rds
saveRDS(j_corpus_s, "my_data/j_corpus/j_corpus_s.rds")


# –––––––––– j_corpus_w = separated into words with stanza and line number ––––––––––
j_corpus_w <- j_corpus_s

#add column with stanza number
for (i in 1:length(j_corpus_w)) {
  for (j in 1:length(j_corpus_w[[i]])) {
    j_corpus_w[[i]][[j]]$stanza <- j
  }
}

#modify text
for (i in 1:length(j_corpus_w)) {
  for (j in 1:length(j_corpus_w[[i]])) {
    #remove all punctuation except apostrophes
    j_corpus_w[[i]][[j]]$text <- str_replace_all(j_corpus_w[[i]][[j]]$text, "[^A-zÀ-ÿ'’ ]", "")

    #replace apostrophes with space
    j_corpus_w[[i]][[j]]$text <- str_replace_all(j_corpus_w[[i]][[j]]$text, "['’]", " ")

    #remove trailing spaces
    j_corpus_w[[i]][[j]]$text <- str_trim(j_corpus_w[[i]][[j]]$text)

    #convert to lowercase
    j_corpus_w[[i]][[j]]$text <- str_to_lower(j_corpus_w[[i]][[j]]$text)
  }
}

#separate text into words
j_corpus_w_temp <- list()

for (i in 1:length(j_corpus_w)) {
    words <- vector()
    stanzas <- vector()
    lines <- vector()

    for (j in 1:length(j_corpus_w[[i]])) {
        for (k in 1:nrow(j_corpus_w[[i]][[j]])) {
            words <- c(words, unlist(str_split(j_corpus_w[[i]][[j]][k, 2], " ")))
            n_words <- length(unlist(str_split(j_corpus_w[[i]][[j]][k, 2], " ")))
            stanzas <- c(stanzas, rep(j_corpus_w[[i]][[j]][k, 3], n_words))
            lines <- c(lines, rep(j_corpus_w[[i]][[j]][k, 1], n_words))
        }
    }
    j_corpus_w_temp[[i]] <- data.frame(word = words, stanza = stanzas, line = lines, stringsAsFactors = FALSE)
}

j_corpus_w <- j_corpus_w_temp

#delete rows with empty word
for (i in 1:length(j_corpus_w)) {
  rows_to_exclude <- which(j_corpus_w[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    j_corpus_w[[i]] <- j_corpus_w[[i]][-rows_to_exclude, ]
  }
}

#rename lists as in j_corpus
names(j_corpus_w) <- names(j_corpus)

#reset rownames
for (i in 1:length(j_corpus_w)) {
  rownames(j_corpus_w[[i]]) <- NULL
}

#save j_corpus_w as .rds
saveRDS(j_corpus_w, "my_data/j_corpus/j_corpus_w.rds")


# –––––––––– j_corpus_lw = lemmatised list aligned with words ––––––––––
#load lemmatised files
j_corpus_lw <- list.files("my_data/j_corpus/lemmatised/", pattern = "*.txt", full.names = TRUE)

j_corpus_lw <- pblapply(j_corpus_lw, read.delim, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("word", "pos", "lemma"))

#modify words
j_corpus_lw <- pblapply(j_corpus_lw, function(x) {
  for (i in 1:nrow(x)) {
    x[i, "word"] <- tolower(str_replace_all(x[i, "word"], "[^A-zÀ-ÿ]", ""))
  }
  x
})

#remove rows with empty words
for (i in 1:length(j_corpus_lw)) {
  rows_to_exclude <- which(j_corpus_lw[[i]][ ,1] == "")
  if (length(rows_to_exclude) > 0) {
    j_corpus_lw[[i]] <- j_corpus_lw[[i]][-rows_to_exclude, ]
  }
}

#reset row names
for (i in 1:length(j_corpus_lw)) {
  rownames(j_corpus_lw[[i]]) <- NULL
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

j_corpus_lw <- pblapply(seq_along(j_corpus_w), combine_lw, j_corpus_w, j_corpus_lw)

#rename lists as in j_corpus
names(j_corpus_lw) <- names(j_corpus)

#save j_corpus_lw as .rds
saveRDS(j_corpus_lw, "my_data/j_corpus/j_corpus_lw.rds")


# –––––––––– j_corpus_ls = lemmatised in stanzas ––––––––––
#recombine lines and stanzas for lemmata

j_corpus_ls <- list()

for (i in seq_along(j_corpus_lw)) {
    j_corpus_ls[[i]] <- list()
    for (j in unique(j_corpus_lw[[i]]$stanza)) {
        stanza <- j_corpus_lw[[i]][j_corpus_lw[[i]]$stanza == j, ]
        j_corpus_ls[[i]][[j]] <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(j_corpus_ls[[i]][[j]]) <- c("line", "text")
        for (k in 1:length(unique(stanza$line))) {
            line <- paste0(stanza[stanza$line == unique(stanza$line)[k], 2], collapse = " ")
            j_corpus_ls[[i]][[j]][k, 1] <- unique(stanza$line)[k]
            j_corpus_ls[[i]][[j]][k, 2] <- line
        }
    }
}

#rename lists as in j_corpus
names(j_corpus_ls) <- names(j_corpus)

#save j_corpus_ls as .rds
saveRDS(j_corpus_ls, "my_data/j_corpus/j_corpus_ls.rds")


# –––––––––– j_corpus_lc = lemmatised and concatenated ––––––––––

j_corpus_lc <- list()

for (i in seq_along(j_corpus_ls)) {
    j_corpus_lc[[i]] <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(j_corpus_lc[[i]]) <- c("line", "text")
    for (j in seq_along(j_corpus_ls[[i]])) {
        for (k in 1:nrow(j_corpus_ls[[i]][[j]])) {
            j_corpus_lc[[i]][nrow(j_corpus_lc[[i]]) + 1, ] <- c(
                j_corpus_ls[[i]][[j]][k, 1],
                j_corpus_ls[[i]][[j]][k, 2]
            )
        }
    }
}

#rename lists as in j_corpus
names(j_corpus_lc) <- names(j_corpus)

#save j_corpus_lc as .rds
saveRDS(j_corpus_lc, "my_data/j_corpus/j_corpus_lc.rds")
