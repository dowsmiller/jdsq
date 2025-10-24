#clear variables and directories from memory
rm(list = ls())

#install the relevant packages
# install.packages(c("stringr", "pbapply"))

#load the relevant packages
library(stringr)
library(pbapply)

#set path
a_corpus_path <- "rdat/a/txt_raw"

#get files in directory
a_corpus_files <- list.files(a_corpus_path, pattern = "*.txt", full.names = TRUE)

#read in files
a_corpus <- pblapply(a_corpus_files, readLines)

#set df name to the file name with ".txt" removed and "_" replaced with " "
df_names <- vector("character", length(a_corpus))

for (i in 1:length(a_corpus)) {
  df_names[i] <- str_replace_all(list.files(a_corpus_path, pattern = "*.txt")[i], ".txt", "")
  df_names[i] <- str_replace_all(df_names[i], "_", " ")
}

names(a_corpus) <- df_names

#delete trailing spaces
for (i in 1:length(a_corpus)) {
  a_corpus[[i]] <- str_trim(a_corpus[[i]])
}

#delete empty strings from vector if preceded by another empty string
for (i in 1:length(a_corpus)) {
    for (j in 1:length(a_corpus[[i]])) {
        if (j > 1) {
            if (a_corpus[[i]][j] == "" && (a_corpus[[i]][j - 1] == "" || is.na(a_corpus[[i]][j - 1]))) {
                a_corpus[[i]][j] <- NA
            }
        }
    }

    a_corpus[[i]] <- a_corpus[[i]][!sapply(a_corpus[[i]], is.na)]
}

#delete final lines matching regex ^[AMENamen. ]+$
for (i in 1:length(a_corpus)) {
    for (j in 1:length(a_corpus[[i]])) {
        if (j == length(a_corpus[[i]])) {
            if (str_detect(a_corpus[[i]][j], "^[AMENamen. ]+$")) {
                a_corpus[[i]][j] <- NA
            }
        }
    }

    a_corpus[[i]] <- a_corpus[[i]][!sapply(a_corpus[[i]], is.na)]
}

#delete empty lines in first and final position
for (i in 1:length(a_corpus)) {
    if (a_corpus[[i]][1] == "") {
        a_corpus[[i]][1] <- NA
    }

    if (a_corpus[[i]][length(a_corpus[[i]])] == "") {
        a_corpus[[i]][length(a_corpus[[i]])] <- NA
    }

    a_corpus[[i]] <- a_corpus[[i]][!sapply(a_corpus[[i]], is.na)]
}

#save a_corpus as individual text files
dir.create("rdat/a/txt", showWarnings = FALSE)

for (i in 1:length(a_corpus)) {
    file_name <- paste0(gsub("[’ ]", "_", gsub("\\.", "", names(a_corpus)[i])))
    file_path <- paste0("rdat/a/txt/", file_name, ".txt")
    write.table(a_corpus[[i]], file_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}

# –––––––––– a_corpus_c = all lines concatenated ––––––––––
a_corpus_c <- a_corpus

for (i in 1:length(a_corpus_c)) {
    for (j in 1:length(a_corpus_c[[i]])) {
        if (a_corpus_c[[i]][j] == "") {
            a_corpus_c[[i]][j] <- NA
        }
    }

    a_corpus_c[[i]] <- a_corpus_c[[i]][!sapply(a_corpus_c[[i]], is.na)]
}

#save a_corpus_c as .rds
dir.create("wdat/a", showWarnings = FALSE)
saveRDS(a_corpus_c, "wdat/a/a_corpus_c.rds")


# –––––––––– a_corpus_s = separated into stanzas ––––––––––
#add column with stanza number
a_corpus_s <- a_corpus

for (i in 1:length(a_corpus_s)) {
    a_corpus_s[[i]] <- data.frame(text = a_corpus_s[[i]], stanza = NA, stringsAsFactors = FALSE)
    stanza <- 1
    for (j in 1:nrow(a_corpus_s[[i]])) {
        if (a_corpus_s[[i]][j, 1] == "") {
            stanza <- stanza + 1
        } else {
            a_corpus_s[[i]][j, 2] <- stanza
        }
    }
}

#remove empty rows
for (i in 1:length(a_corpus_s)) {
  rows_to_exclude <- which(a_corpus_s[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    a_corpus_s[[i]] <- a_corpus_s[[i]][-rows_to_exclude, ]
  }
}

#group into dfs according to stanza number
a_corpus_s_temp <- list()

for (i in 1:length(a_corpus_s)) {
    a_corpus_s_temp[[i]] <- split(a_corpus_s[[i]], a_corpus_s[[i]]$stanza)
}

a_corpus_s <- a_corpus_s_temp

#remove stanza column
for (i in 1:length(a_corpus_s)) {
    for (j in 1:length(a_corpus_s[[i]])) {
        a_corpus_s[[i]][[j]] <- a_corpus_s[[i]][[j]][, 1]
    }
}

#add line number column before text
for (i in 1:length(a_corpus_s)) {
    line <- 1
    for (j in 1:length(a_corpus_s[[i]])) {
        df <- data.frame(line = NA, text = a_corpus_s[[i]][[j]], stringsAsFactors = FALSE)
        for (k in 1:nrow(df)) {
            df[k, 1] <- line
            line <- line + 1
        }
        a_corpus_s[[i]][[j]] <- df
    }
}

#rename lists as in a_corpus
names(a_corpus_s) <- names(a_corpus)

#save a_corpus_s as .rds
saveRDS(a_corpus_s, "wdat/a/a_corpus_s.rds")


# –––––––––– a_corpus_w = separated into words with stanza and line number ––––––––––
a_corpus_w <- a_corpus_s

#add column with stanza number
for (i in 1:length(a_corpus_w)) {
  for (j in 1:length(a_corpus_w[[i]])) {
    a_corpus_w[[i]][[j]]$stanza <- j
  }
}

#modify text
for (i in 1:length(a_corpus_w)) {
  for (j in 1:length(a_corpus_w[[i]])) {
    #remove all punctuation except apostrophes
    a_corpus_w[[i]][[j]]$text <- str_replace_all(a_corpus_w[[i]][[j]]$text, "[^A-zÀ-ÿ'’ ]", "")

    #replace apostrophes with space
    a_corpus_w[[i]][[j]]$text <- str_replace_all(a_corpus_w[[i]][[j]]$text, "['’]", " ")

    #remove trailing spaces
    a_corpus_w[[i]][[j]]$text <- str_trim(a_corpus_w[[i]][[j]]$text)

    #convert to lowercase
    a_corpus_w[[i]][[j]]$text <- str_to_lower(a_corpus_w[[i]][[j]]$text)
  }
}

#separate text into words
a_corpus_w_temp <- list()

for (i in 1:length(a_corpus_w)) {
    words <- vector()
    stanzas <- vector()
    lines <- vector()

    for (j in 1:length(a_corpus_w[[i]])) {
        for (k in 1:nrow(a_corpus_w[[i]][[j]])) {
            words <- c(words, unlist(str_split(a_corpus_w[[i]][[j]][k, 2], " ")))
            n_words <- length(unlist(str_split(a_corpus_w[[i]][[j]][k, 2], " ")))
            stanzas <- c(stanzas, rep(a_corpus_w[[i]][[j]][k, 3], n_words))
            lines <- c(lines, rep(a_corpus_w[[i]][[j]][k, 1], n_words))
        }
    }
    a_corpus_w_temp[[i]] <- data.frame(word = words, stanza = stanzas, line = lines, stringsAsFactors = FALSE)
}

a_corpus_w <- a_corpus_w_temp

#delete rows with empty word
for (i in 1:length(a_corpus_w)) {
  rows_to_exclude <- which(a_corpus_w[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    a_corpus_w[[i]] <- a_corpus_w[[i]][-rows_to_exclude, ]
  }
}

#rename lists as in a_corpus
names(a_corpus_w) <- names(a_corpus)

#reset rownames
for (i in 1:length(a_corpus_w)) {
  rownames(a_corpus_w[[i]]) <- NULL
}

#save a_corpus_w as .rds
saveRDS(a_corpus_w, "wdat/a/a_corpus_w.rds")


# –––––––––– a_corpus_lw = lemmatised list aligned with words ––––––––––
#load lemmatised files
a_corpus_lw <- list.files("rdat/a/lemmatised/", pattern = "*.txt", full.names = TRUE)

a_corpus_lw <- pblapply(a_corpus_lw, read.delim, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("word", "pos", "lemma"))

#modify words
a_corpus_lw <- pblapply(a_corpus_lw, function(x) {
  for (i in 1:nrow(x)) {
    x[i, "word"] <- tolower(str_replace_all(x[i, "word"], "[^A-zÀ-ÿ]", ""))
  }
  x
})

#remove rows with empty words
for (i in 1:length(a_corpus_lw)) {
  rows_to_exclude <- which(a_corpus_lw[[i]][ ,1] == "")
  if (length(rows_to_exclude) > 0) {
    a_corpus_lw[[i]] <- a_corpus_lw[[i]][-rows_to_exclude, ]
  }
}

#reset row names
for (i in 1:length(a_corpus_lw)) {
  rownames(a_corpus_lw[[i]]) <- NULL
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

a_corpus_lw <- pblapply(seq_along(a_corpus_w), combine_lw, a_corpus_w, a_corpus_lw)

#rename lists as in a_corpus
names(a_corpus_lw) <- names(a_corpus)

#save a_corpus_lw as .rds
saveRDS(a_corpus_lw, "wdat/a/a_corpus_lw.rds")


# –––––––––– a_corpus_ls = lemmatised in stanzas ––––––––––
#recombine lines and stanzas for lemmata

a_corpus_ls <- list()

for (i in seq_along(a_corpus_lw)) {
    a_corpus_ls[[i]] <- list()
    for (j in unique(a_corpus_lw[[i]]$stanza)) {
        stanza <- a_corpus_lw[[i]][a_corpus_lw[[i]]$stanza == j, ]
        a_corpus_ls[[i]][[j]] <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(a_corpus_ls[[i]][[j]]) <- c("line", "text")
        for (k in 1:length(unique(stanza$line))) {
            line <- paste0(stanza[stanza$line == unique(stanza$line)[k], 2], collapse = " ")
            a_corpus_ls[[i]][[j]][k, 1] <- unique(stanza$line)[k]
            a_corpus_ls[[i]][[j]][k, 2] <- line
        }
    }
}

#rename lists as in a_corpus
names(a_corpus_ls) <- names(a_corpus)

#save a_corpus_ls as .rds
saveRDS(a_corpus_ls, "wdat/a/a_corpus_ls.rds")


# –––––––––– a_corpus_lc = lemmatised and concatenated ––––––––––

a_corpus_lc <- list()

for (i in seq_along(a_corpus_ls)) {
    a_corpus_lc[[i]] <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(a_corpus_lc[[i]]) <- c("line", "text")
    for (j in seq_along(a_corpus_ls[[i]])) {
        for (k in 1:nrow(a_corpus_ls[[i]][[j]])) {
            a_corpus_lc[[i]][nrow(a_corpus_lc[[i]]) + 1, ] <- c(
                a_corpus_ls[[i]][[j]][k, 1],
                a_corpus_ls[[i]][[j]][k, 2]
            )
        }
    }
}

#rename lists as in a_corpus
names(a_corpus_lc) <- names(a_corpus)

#save a_corpus_lc as .rds
saveRDS(a_corpus_lc, "wdat/a/a_corpus_lc.rds")
