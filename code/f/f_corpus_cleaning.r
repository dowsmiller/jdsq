#clear variables and directories from memory
rm(list = ls())

#install the relevant packages
install.packages(c("stringr", "pbapply"))

#load the relevant packages
library(stringr)
library(pbapply)

#set path
f_corpus_path <- "rdat/f/txt_raw"

#get files in directory
f_corpus_files <- list.files(f_corpus_path, pattern = "*.txt", full.names = TRUE)

#read in files
f_corpus <- pblapply(f_corpus_files, readLines)
names(f_corpus) <- list.files(f_corpus_path, pattern = "*.txt")
names(f_corpus) <- str_replace_all(names(f_corpus), "\\.txt", "")
names(f_corpus) <- paste(sprintf("F%03d", 1:length(f_corpus))[1:length(f_corpus)], names(f_corpus), sep = "_")

#remove lines containing … or three . or 0-9
for (i in seq_along(f_corpus)) {
    for (j in seq_along(f_corpus[[i]])) {
        line <- f_corpus[[i]][j]
        if (!is.na(line) && (str_detect(line, "[0-9]") || str_detect(line, "\\.\\.\\.") || str_detect(line, "…"))) {
            f_corpus[[i]][j] <- NA
        }
    }
}

f_corpus <- lapply(f_corpus, function(x) x[!sapply(x, is.na)])

#remove lines which contain [Ee]xplicit
for (i in 1:length(f_corpus)) {
    f_corpus[[i]] <- f_corpus[[i]][!str_detect(f_corpus[[i]], "[Ee]xplicit")]
}

#delete empty lines at beginning/end or with string in line before or after
for (i in 1:length(f_corpus)) {
    for (j in 1:length(f_corpus[[i]])) {
        line <- f_corpus[[i]][j]
        if (j > 1 && j < length(f_corpus[[i]]) && !is.na(f_corpus[[i]][j - 1])) {
            if (line == "" && (f_corpus[[i]][j - 1] != "" || f_corpus[[i]][i + 1] != "")) {
                line <- NA
            }
        }
        else if (j == 1 && line == "") {
            line <- NA
        }
        else if (j == length(f_corpus[[i]]) && line == "") {
            line <- NA
        }
        f_corpus[[i]][j] <- line
    }
}

f_corpus <- lapply(f_corpus, function(x) x[!sapply(x, is.na)])

#text changes
for (i in 1:length(f_corpus)) {
    #remove trailing spaces
    f_corpus[[i]] <- str_trim(f_corpus[[i]])

    #replace "^- " with "“"
    f_corpus[[i]] <- str_replace_all(f_corpus[[i]], "^[—-] ", "“")

    #replace " - " with "” “"
    f_corpus[[i]] <- str_replace_all(f_corpus[[i]], " [—-] ", "” “")

    #replace "« " with "“"
    f_corpus[[i]] <- str_replace_all(f_corpus[[i]], "[«»][^A-zÀ-ÿ0-9\\(\\)\\.,?!\";:—”“]", "“")

    #replace " »" with "”"
    f_corpus[[i]] <- str_replace_all(f_corpus[[i]], "[^A-zÀ-ÿ0-9\\(\\)\\.,?!\";:—”“]»", "”")

    #unwrap square brackets
    f_corpus[[i]] <- str_replace_all(f_corpus[[i]], "\\[", "")
    f_corpus[[i]] <- str_replace_all(f_corpus[[i]], "\\]", "")
}

#save f_corpus as individual text files
dir.create("rdat/f/txt", showWarnings = FALSE)

for (i in 1:length(f_corpus)) {
    file_name <- paste0(gsub("[’ ]", "_", gsub("\\.", "", names(f_corpus)[i])))
    file_path <- paste0("rdat/f/txt/", file_name, ".txt")
    write.table(f_corpus[[i]], file_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}


# –––––––––– f_corpus_c = all lines concatenated ––––––––––
f_corpus_c <- f_corpus

for (i in 1:length(f_corpus_c)) {
    for (j in 1:length(f_corpus_c[[i]])) {
        if (f_corpus_c[[i]][j] == "") {
            f_corpus_c[[i]][j] <- NA
        }
    }

    f_corpus_c[[i]] <- f_corpus_c[[i]][!sapply(f_corpus_c[[i]], is.na)]
}

#save f_corpus_c as .rds
dir.create("wdat/f", showWarnings = FALSE)
saveRDS(f_corpus_c, "wdat/f/f_corpus_c.rds")


# –––––––––– f_corpus_s = separated into stanzas ––––––––––
#add column with stanza number
f_corpus_s <- f_corpus

for (i in 1:length(f_corpus_s)) {
    f_corpus_s[[i]] <- data.frame(text = f_corpus_s[[i]], stanza = NA, stringsAsFactors = FALSE)
    stanza <- 1
    for (j in 1:nrow(f_corpus_s[[i]])) {
        if (f_corpus_s[[i]][j, 1] == "") {
            stanza <- stanza + 1
        } else {
            f_corpus_s[[i]][j, 2] <- stanza
        }
    }
}

#remove empty rows
for (i in 1:length(f_corpus_s)) {
  rows_to_exclude <- which(f_corpus_s[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    f_corpus_s[[i]] <- f_corpus_s[[i]][-rows_to_exclude, ]
  }
}

#group into dfs according to stanza number
f_corpus_s_temp <- list()

for (i in 1:length(f_corpus_s)) {
    f_corpus_s_temp[[i]] <- split(f_corpus_s[[i]], f_corpus_s[[i]]$stanza)
}

f_corpus_s <- f_corpus_s_temp

#remove stanza column
for (i in 1:length(f_corpus_s)) {
    for (j in 1:length(f_corpus_s[[i]])) {
        f_corpus_s[[i]][[j]] <- f_corpus_s[[i]][[j]][, -2]
    }
}

#add line number column before text
for (i in 1:length(f_corpus_s)) {
    line <- 1
    for (j in 1:length(f_corpus_s[[i]])) {
        df <- data.frame(line = NA, text = f_corpus_s[[i]][[j]], stringsAsFactors = FALSE)
        for (k in 1:nrow(df)) {
            df[k, 1] <- line
            line <- line + 1
        }
        f_corpus_s[[i]][[j]] <- df
    }
}

#rename lists as in f_corpus
names(f_corpus_s) <- names(f_corpus)

#save f_corpus_s as .rds
saveRDS(f_corpus_s, "wdat/f/f_corpus_s.rds")


# –––––––––– f_corpus_w = separated into words with stanza and line number ––––––––––
f_corpus_w <- f_corpus_s

#add column with stanza number
for (i in 1:length(f_corpus_w)) {
  for (j in 1:length(f_corpus_w[[i]])) {
    f_corpus_w[[i]][[j]]$stanza <- j
  }
}

#modify text
for (i in 1:length(f_corpus_w)) {
  for (j in 1:length(f_corpus_w[[i]])) {
    #remove all punctuation except apostrophes
    f_corpus_w[[i]][[j]]$text <- str_replace_all(f_corpus_w[[i]][[j]]$text, "[^A-zÀ-ÿ'’\\s]", "")

    #replace apostrophes with space
    f_corpus_w[[i]][[j]]$text <- str_replace_all(f_corpus_w[[i]][[j]]$text, "['’]", " ")

    #remove trailing spaces
    f_corpus_w[[i]][[j]]$text <- str_trim(f_corpus_w[[i]][[j]]$text)

    #remove double spaces
    f_corpus_w[[i]][[j]]$text <- str_replace_all(f_corpus_w[[i]][[j]]$text, "[\\s]+", " ")

    #convert to lowercase
    f_corpus_w[[i]][[j]]$text <- str_to_lower(f_corpus_w[[i]][[j]]$text)
  }
}

#separate text into words
f_corpus_w_temp <- list()

for (i in 1:length(f_corpus_w)) {
    words <- vector()
    stanzas <- vector()
    lines <- vector()

    for (j in 1:length(f_corpus_w[[i]])) {
        for (k in 1:nrow(f_corpus_w[[i]][[j]])) {
            words <- c(words, unlist(str_split(f_corpus_w[[i]][[j]][k, 2], " ")))
            n_words <- length(unlist(str_split(f_corpus_w[[i]][[j]][k, 2], " ")))
            stanzas <- c(stanzas, rep(f_corpus_w[[i]][[j]][k, 3], n_words))
            lines <- c(lines, rep(f_corpus_w[[i]][[j]][k, 1], n_words))
        }
    }
    f_corpus_w_temp[[i]] <- data.frame(word = words, stanza = stanzas, line = lines, stringsAsFactors = FALSE)
}

f_corpus_w <- f_corpus_w_temp

#delete rows with empty word
for (i in 1:length(f_corpus_w)) {
  rows_to_exclude <- which(f_corpus_w[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    f_corpus_w[[i]] <- f_corpus_w[[i]][-rows_to_exclude, ]
  }
}

#rename lists as in f_corpus
names(f_corpus_w) <- names(f_corpus)

#reset rownames
for (i in 1:length(f_corpus_w)) {
  rownames(f_corpus_w[[i]]) <- NULL
}

#save f_corpus_w as .rds
saveRDS(f_corpus_w, "wdat/f/f_corpus_w.rds")


# –––––––––– f_corpus_lw = lemmatised list aligned with words ––––––––––
#load lemmatised files
f_corpus_lw <- list.files("rdat/f/lemmatised/", pattern = "*.txt", full.names = TRUE)

f_corpus_lw <- pblapply(f_corpus_lw, read.delim, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("word", "pos", "lemma"))

#modify words
f_corpus_lw <- pblapply(f_corpus_lw, function(x) {
  for (i in 1:nrow(x)) {
    x[i, "word"] <- tolower(str_replace_all(x[i, "word"], "[^A-zÀ-ÿ]", ""))
  }
  x
})

#remove rows with empty words
for (i in 1:length(f_corpus_lw)) {
  rows_to_exclude <- which(f_corpus_lw[[i]][ ,1] == "")
  if (length(rows_to_exclude) > 0) {
    f_corpus_lw[[i]] <- f_corpus_lw[[i]][-rows_to_exclude, ]
  }
}

#reset row names
for (i in 1:length(f_corpus_lw)) {
  rownames(f_corpus_lw[[i]]) <- NULL
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

f_corpus_lw <- pblapply(seq_along(f_corpus_w), combine_lw, f_corpus_w, f_corpus_lw)

#rename lists as in f_corpus
names(f_corpus_lw) <- names(f_corpus)

#save f_corpus_lw as .rds
saveRDS(f_corpus_lw, "wdat/f/f_corpus_lw.rds")


# –––––––––– f_corpus_ls = lemmatised in stanzas ––––––––––
#recombine lines and stanzas for lemmata

f_corpus_ls <- list()

for (i in seq_along(f_corpus_lw)) {
    f_corpus_ls[[i]] <- list()
    for (j in unique(f_corpus_lw[[i]]$stanza)) {
        stanza <- f_corpus_lw[[i]][f_corpus_lw[[i]]$stanza == j, ]
        f_corpus_ls[[i]][[j]] <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(f_corpus_ls[[i]][[j]]) <- c("line", "text")
        for (k in 1:length(unique(stanza$line))) {
            line <- paste0(stanza[stanza$line == unique(stanza$line)[k], 2], collapse = " ")
            f_corpus_ls[[i]][[j]][k, 1] <- unique(stanza$line)[k]
            f_corpus_ls[[i]][[j]][k, 2] <- line
        }
    }
}

#rename lists as in f_corpus
names(f_corpus_ls) <- names(f_corpus)

#save f_corpus_ls as .rds
saveRDS(f_corpus_ls, "wdat/f/f_corpus_ls.rds")


# –––––––––– f_corpus_lc = lemmatised and concatenated ––––––––––

f_corpus_lc <- list()

for (i in seq_along(f_corpus_ls)) {
    f_corpus_lc[[i]] <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(f_corpus_lc[[i]]) <- c("line", "text")
    for (j in seq_along(f_corpus_ls[[i]])) {
        for (k in 1:nrow(f_corpus_ls[[i]][[j]])) {
            f_corpus_lc[[i]][nrow(f_corpus_lc[[i]]) + 1, ] <- c(
                f_corpus_ls[[i]][[j]][k, 1],
                f_corpus_ls[[i]][[j]][k, 2]
            )
        }
    }
}

#rename lists as in f_corpus
names(f_corpus_lc) <- names(f_corpus)

#save f_corpus_lc as .rds
saveRDS(f_corpus_lc, "wdat/f/f_corpus_lc.rds")
