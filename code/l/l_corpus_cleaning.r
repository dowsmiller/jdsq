#clear variables and directories from memory
rm(list = ls())

#install the relevant packages
install.packages(c("readxl", "stringr", "pbapply"))

#load the relevant packages
library(readxl)
library(stringr)
library(pbapply)

#set path
l_corpus_path <- "my_data/l_corpus/txt_raw"

#get files in directory
l_corpus_files <- list.files(l_corpus_path, pattern = "*.txt", full.names = TRUE)

#read in files
l_corpus <- pblapply(l_corpus_files, readLines)

#set df name to the file name after character 26 for each
    #with ".txt" removed and "_" replaced with " "
df_names <- vector("character", length(l_corpus))

for (i in 1:length(l_corpus)) {
  df_names[i] <- str_replace_all(str_sub(l_corpus_files[i], 26), ".txt", "")
  df_names[i] <- str_replace_all(df_names[i], "_", " ")
}

names(l_corpus) <- df_names

#delete trailing spaces
for (i in 1:length(l_corpus)) {
  l_corpus[[i]] <- str_trim(l_corpus[[i]])
}

#delete empty strings from vector if preceded by another empty string
for (i in 1:length(l_corpus)) {
    for (j in 1:length(l_corpus[[i]])) {
        if (j > 1) {
            if (l_corpus[[i]][j] == "" && l_corpus[[i]][j - 1] == "") {
                l_corpus[[i]][j] <- NA
            }
        }
    }

    l_corpus[[i]] <- l_corpus[[i]][!sapply(l_corpus[[i]], is.na)]
}

#delete [*]
for (i in 1:length(l_corpus)) {
    l_corpus[[i]] <- str_replace_all(l_corpus[[i]], "\\[[A-Za-z0-9\\. ]+\\] ", "")
}

#standardise quotation marks
for (i in 1:length(l_corpus)) {
    l_corpus[[i]] <- str_replace_all(l_corpus[[i]], "[^A-zÀ-ÿ0-9\\.,?!\";:—”“]’$", "”")
    l_corpus[[i]] <- str_replace_all(l_corpus[[i]], " ’ ", "” ")
    l_corpus[[i]] <- str_replace_all(l_corpus[[i]], "([,\\.\\)\\(;!?])’", "\\1”")
    l_corpus[[i]] <- str_replace_all(l_corpus[[i]], "’([,\\.\\)\\(;!?])", "”\\1")
    l_corpus[[i]] <- str_replace_all(l_corpus[[i]], "^’", "“")
    l_corpus[[i]] <- str_replace_all(l_corpus[[i]], " ’([A-Za-z])", " “\\1")
}

#save l_corpus as individual text files
dir.create("my_data/l_corpus/txt", showWarnings = FALSE)

for (i in 1:length(l_corpus)) {
    file_name <- paste0(gsub("[’ ]", "_", gsub("\\.", "", names(l_corpus)[i])))
    file_path <- paste0("my_data/l_corpus/txt/", file_name, ".txt")
    write.table(l_corpus[[i]], file_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}

# –––––––––– l_corpus_c = all lines concatenated ––––––––––
l_corpus_c <- l_corpus

for (i in 1:length(l_corpus_c)) {
    for (j in 1:length(l_corpus_c[[i]])) {
        if (l_corpus_c[[i]][j] == "") {
            l_corpus_c[[i]][j] <- NA
        }
    }

    l_corpus_c[[i]] <- l_corpus_c[[i]][!sapply(l_corpus_c[[i]], is.na)]
}

#save l_corpus_c as .rds
saveRDS(l_corpus_c, "my_data/l_corpus/l_corpus_c.rds")


# –––––––––– l_corpus_s = separated into stanzas ––––––––––
#add column with stanza number
l_corpus_s <- l_corpus

for (i in 1:length(l_corpus_s)) {
    l_corpus_s[[i]] <- data.frame(text = l_corpus_s[[i]], stanza = NA, stringsAsFactors = FALSE)
    stanza <- 1
    for (j in 1:nrow(l_corpus_s[[i]])) {
        if (l_corpus_s[[i]][j, 1] == "") {
            stanza <- stanza + 1
        } else {
            l_corpus_s[[i]][j, 2] <- stanza
        }
    }
}

#remove empty rows
for (i in 1:length(l_corpus_s)) {
  rows_to_exclude <- which(l_corpus_s[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    l_corpus_s[[i]] <- l_corpus_s[[i]][-rows_to_exclude, ]
  }
}

#group into dfs according to stanza number
l_corpus_s_temp <- list()

for (i in 1:length(l_corpus_s)) {
    l_corpus_s_temp[[i]] <- split(l_corpus_s[[i]], l_corpus_s[[i]]$stanza)
}

l_corpus_s <- l_corpus_s_temp

#remove stanza column
for (i in 1:length(l_corpus_s)) {
    for (j in 1:length(l_corpus_s[[i]])) {
        l_corpus_s[[i]][[j]] <- l_corpus_s[[i]][[j]][, 1]
    }
}

#add line number column before text
for (i in 1:length(l_corpus_s)) {
    line <- 1
    for (j in 1:length(l_corpus_s[[i]])) {
        df <- data.frame(line = NA, text = l_corpus_s[[i]][[j]], stringsAsFactors = FALSE)
        for (k in 1:nrow(df)) {
            df[k, 1] <- line
            line <- line + 1
        }
        l_corpus_s[[i]][[j]] <- df
    }
}

#rename lists as in l_corpus
names(l_corpus_s) <- names(l_corpus)

#save l_corpus_s as .rds
saveRDS(l_corpus_s, "my_data/l_corpus/l_corpus_s.rds")


# –––––––––– l_corpus_w = separated into words with stanza and line number ––––––––––
l_corpus_w <- l_corpus_s

#add column with stanza number
for (i in 1:length(l_corpus_w)) {
  for (j in 1:length(l_corpus_w[[i]])) {
    l_corpus_w[[i]][[j]]$stanza <- j
  }
}

#modify text
for (i in 1:length(l_corpus_w)) {
  for (j in 1:length(l_corpus_w[[i]])) {
    #remove all punctuation except apostrophes
    l_corpus_w[[i]][[j]]$text <- str_replace_all(l_corpus_w[[i]][[j]]$text, "[^A-zÀ-ÿ'’ ]", "")

    #replace apostrophes with space
    l_corpus_w[[i]][[j]]$text <- str_replace_all(l_corpus_w[[i]][[j]]$text, "['’]", " ")

    #remove trailing spaces
    l_corpus_w[[i]][[j]]$text <- str_trim(l_corpus_w[[i]][[j]]$text)

    #convert to lowercase
    l_corpus_w[[i]][[j]]$text <- str_to_lower(l_corpus_w[[i]][[j]]$text)
  }
}

#separate text into words
l_corpus_w_temp <- list()

for (i in 1:length(l_corpus_w)) {
    words <- vector()
    stanzas <- vector()
    lines <- vector()

    for (j in 1:length(l_corpus_w[[i]])) {
        for (k in 1:nrow(l_corpus_w[[i]][[j]])) {
            words <- c(words, unlist(str_split(l_corpus_w[[i]][[j]][k, 2], " ")))
            n_words <- length(unlist(str_split(l_corpus_w[[i]][[j]][k, 2], " ")))
            stanzas <- c(stanzas, rep(l_corpus_w[[i]][[j]][k, 3], n_words))
            lines <- c(lines, rep(l_corpus_w[[i]][[j]][k, 1], n_words))
        }
    }
    l_corpus_w_temp[[i]] <- data.frame(word = words, stanza = stanzas, line = lines, stringsAsFactors = FALSE)
}

l_corpus_w <- l_corpus_w_temp

#delete rows with empty word
for (i in 1:length(l_corpus_w)) {
  rows_to_exclude <- which(l_corpus_w[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    l_corpus_w[[i]] <- l_corpus_w[[i]][-rows_to_exclude, ]
  }
}

#rename lists as in l_corpus
names(l_corpus_w) <- names(l_corpus)

#reset rownames
for (i in 1:length(l_corpus_w)) {
  rownames(l_corpus_w[[i]]) <- NULL
}

#save l_corpus_w as .rds
saveRDS(l_corpus_w, "my_data/l_corpus/l_corpus_w.rds")


# –––––––––– l_corpus_lw = lemmatised list aligned with words ––––––––––
#load lemmatised files
l_corpus_lw <- list.files("my_data/l_corpus/lemmatised/", pattern = "*.txt", full.names = TRUE)

l_corpus_lw <- pblapply(l_corpus_lw, read.delim, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("word", "pos", "lemma"))

#modify words
l_corpus_lw <- pblapply(l_corpus_lw, function(x) {
  for (i in 1:nrow(x)) {
    x[i, "word"] <- tolower(str_replace_all(x[i, "word"], "[^A-zÀ-ÿ]", ""))
  }
  x
})

#remove rows with empty words
for (i in 1:length(l_corpus_lw)) {
  rows_to_exclude <- which(l_corpus_lw[[i]][ ,1] == "")
  if (length(rows_to_exclude) > 0) {
    l_corpus_lw[[i]] <- l_corpus_lw[[i]][-rows_to_exclude, ]
  }
}

#reset row names
for (i in 1:length(l_corpus_lw)) {
  rownames(l_corpus_lw[[i]]) <- NULL
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

l_corpus_lw <- pblapply(seq_along(l_corpus_w), combine_lw, l_corpus_w, l_corpus_lw)

#rename lists as in l_corpus
names(l_corpus_lw) <- names(l_corpus)

#save l_corpus_lw as .rds
saveRDS(l_corpus_lw, "my_data/l_corpus/l_corpus_lw.rds")


# –––––––––– l_corpus_ls = lemmatised in stanzas ––––––––––
#recombine lines and stanzas for lemmata

l_corpus_ls <- list()

for (i in seq_along(l_corpus_lw)) {
    l_corpus_ls[[i]] <- list()
    for (j in unique(l_corpus_lw[[i]]$stanza)) {
        stanza <- l_corpus_lw[[i]][l_corpus_lw[[i]]$stanza == j, ]
        l_corpus_ls[[i]][[j]] <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(l_corpus_ls[[i]][[j]]) <- c("line", "text")
        for (k in 1:length(unique(stanza$line))) {
            line <- paste0(stanza[stanza$line == unique(stanza$line)[k], 2], collapse = " ")
            l_corpus_ls[[i]][[j]][k, 1] <- unique(stanza$line)[k]
            l_corpus_ls[[i]][[j]][k, 2] <- line
        }
    }
}

#rename lists as in l_corpus
names(l_corpus_ls) <- names(l_corpus)

#save l_corpus_ls as .rds
saveRDS(l_corpus_ls, "my_data/l_corpus/l_corpus_ls.rds")


# –––––––––– l_corpus_lc = lemmatised and concatenated ––––––––––

l_corpus_lc <- list()

for (i in seq_along(l_corpus_ls)) {
    l_corpus_lc[[i]] <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(l_corpus_lc[[i]]) <- c("line", "text")
    for (j in seq_along(l_corpus_ls[[i]])) {
        for (k in 1:nrow(l_corpus_ls[[i]][[j]])) {
            l_corpus_lc[[i]][nrow(l_corpus_lc[[i]]) + 1, ] <- c(
                l_corpus_ls[[i]][[j]][k, 1],
                l_corpus_ls[[i]][[j]][k, 2]
            )
        }
    }
}

#rename lists as in l_corpus
names(l_corpus_lc) <- names(l_corpus)

#save l_corpus_lc as .rds
saveRDS(l_corpus_lc, "my_data/l_corpus/l_corpus_lc.rds")
