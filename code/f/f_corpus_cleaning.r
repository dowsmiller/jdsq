#clear variables and directories from memory
rm(list = ls())

#install the relevant packages
install.packages(c("stringr", "pbapply"))

#load the relevant packages
library(stringr)
library(pbapply)

#set path
f_corpus_path <- "my_data/f_corpus/txt_raw"

#get files in directory
f_corpus_files <- list.files(f_corpus_path, pattern = "*.txt", full.names = TRUE)

#read in files
f_corpus <- readLines(f_corpus_files)

#delete empty lines with string in line before or after
for (i in 1:length(f_corpus)) {
    if (i > 1 && i < length(f_corpus) && !is.na(f_corpus[i - 1])) {
        if (f_corpus[i] == "" && (f_corpus[i - 1] != "" || f_corpus[i + 1] != "")) {
            f_corpus[i] <- NA
        }
    }
}

f_corpus <- f_corpus[!sapply(f_corpus, is.na)]

#remove lines containing … or multiple . or 0-9
for (i in 1:length(f_corpus)) {
    if (str_detect(f_corpus[i], "[0-9]") || str_detect(f_corpus[i], "\\.\\.") || str_detect(f_corpus[i], "…")) {
        f_corpus[i] <- NA
    }
}

f_corpus <- f_corpus[!sapply(f_corpus, is.na)]

#attach single character with immediately following character to beginning of next line
for (i in 1:length(f_corpus)) {
    if (i < length(f_corpus)) {
        if (str_detect(f_corpus[i], "^[A-Z]$") && str_detect(f_corpus[i + 1], "^[A-Za-zÀ-ÿ’]")) {
            f_corpus[i + 1] <- paste0(f_corpus[i], f_corpus[i + 1])
            f_corpus[i] <- NA
        }
    }
}

f_corpus <- f_corpus[!sapply(f_corpus, is.na)]

#determine numbers, starts, and ends
text_nos <- vector()
text_starts <- vector()
text_ends <- vector()

for (i in 1:length(f_corpus)) {
    if (str_detect(f_corpus[i], "^[IVXLivxl]+$")) {
        text_nos <- c(text_nos, as.numeric(as.roman(f_corpus[i])))
        text_starts <- c(text_starts, i + 2)
        if (length(text_starts) > 1) {
            text_ends <- c(text_ends, i - 1)
        }
    }
}

text_ends <- c(text_ends, length(f_corpus))

#separate into individual texts
f_corpus_temp <- list()

for (i in 1:length(text_starts)) {
    f_corpus_temp[[i]] <- f_corpus[text_starts[i]:text_ends[i]]
}

f_corpus <- f_corpus_temp

#determine titles
text_titles <- vector()

for (i in 1:length(text_nos)) {
    for (j in 1:length(f_corpus[[i]])) {
        if (str_detect(f_corpus[[i]][j], "^[A-ZÀ-ÿ’ \\.]+$")) {
            if (is.na(text_titles[i])) {
                text_titles <- c(text_titles, tolower(f_corpus[[i]][j]))
            } else {
                text_titles[i] <- paste(text_titles[i], tolower(f_corpus[[i]][j]))
            }
        }
    }
}

text_titles <- str_replace_all(text_titles, "\\.", "")

for (i in 1:length(text_titles)) {
    text_titles[i] <- paste(sprintf("%02d", 1:length(text_titles))[i], text_titles[i])
}

names(f_corpus) <- text_titles

#remove titles from texts
for (i in 1:length(f_corpus)) {
    f_corpus[[i]] <- f_corpus[[i]][!str_detect(f_corpus[[i]], "^[A-ZÀ-ÿ’ \\.]+$")]
}

#remove lines which contain [Ee]xplicit
for (i in 1:length(f_corpus)) {
    f_corpus[[i]] <- f_corpus[[i]][!str_detect(f_corpus[[i]], "[Ee]xplicit")]
}

#remove all lines apprearing after (and including) a line containing "Notes et variantes"
for (i in 1:length(f_corpus)) {
    if (any(str_detect(f_corpus[[i]], "Notes et variantes"))) {
        f_corpus[[i]] <- f_corpus[[i]][1:which(str_detect(f_corpus[[i]], "Notes et variantes"))[1] - 1]
    }
}

#remove empty lines with no preceding lines containing string
for (i in 1:length(f_corpus)) {
    for (j in length(f_corpus[[i]]):1) {
        if (j == 1 && f_corpus[[i]][j] == "") {
            f_corpus[[i]][j] <- NA
        } else if (f_corpus[[i]][j] == "" && paste0(f_corpus[[i]][1:(j - 1)], collapse = "") == "") {
            f_corpus[[i]][j] <- NA
        }
    }
    f_corpus[[i]] <- f_corpus[[i]][!sapply(f_corpus[[i]], is.na)]
}

#remove empty lines with no following lines containing string
for (i in 1:length(f_corpus)) {
    for (j in 1:length(f_corpus[[i]])) {
        if (j == length(f_corpus[[i]]) && f_corpus[[i]][j] == "") {
            f_corpus[[i]][j] <- NA
        } else if (f_corpus[[i]][j] == "" && paste0(f_corpus[[i]][(j + 1):length(f_corpus[[i]])], collapse = "") == "") {
            f_corpus[[i]][j] <- NA
        }
    }
    f_corpus[[i]] <- f_corpus[[i]][!sapply(f_corpus[[i]], is.na)]
}

#remove empty lines if preceding line is also empty
for (i in 1:length(f_corpus)) {
    for (j in length(f_corpus[[i]]):1) {
        if (f_corpus[[i]][j] == "" && f_corpus[[i]][j - 1] == "") {
            f_corpus[[i]][j] <- NA
        }
    }
    f_corpus[[i]] <- f_corpus[[i]][!sapply(f_corpus[[i]], is.na)]
}

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
dir.create("my_data/f_corpus/txt", showWarnings = FALSE)

for (i in 1:length(f_corpus)) {
    file_name <- paste0(gsub("[’ ]", "_", gsub("\\.", "", names(f_corpus)[i])))
    file_path <- paste0("my_data/f_corpus/txt/", file_name, ".txt")
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
saveRDS(f_corpus_c, "my_data/f_corpus/f_corpus_c.rds")


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
saveRDS(f_corpus_s, "my_data/f_corpus/f_corpus_s.rds")


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
    f_corpus_w[[i]][[j]]$text <- str_replace_all(f_corpus_w[[i]][[j]]$text, "[^A-zÀ-ÿ'’ ]", "")

    #replace apostrophes with space
    f_corpus_w[[i]][[j]]$text <- str_replace_all(f_corpus_w[[i]][[j]]$text, "['’]", " ")

    #remove trailing spaces
    f_corpus_w[[i]][[j]]$text <- str_trim(f_corpus_w[[i]][[j]]$text)

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
saveRDS(f_corpus_w, "my_data/f_corpus/f_corpus_w.rds")


# –––––––––– f_corpus_lw = lemmatised list aligned with words ––––––––––
#load lemmatised files
f_corpus_lw <- list.files("my_data/f_corpus/lemmatised/", pattern = "*.txt", full.names = TRUE)

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
saveRDS(f_corpus_lw, "my_data/f_corpus/f_corpus_lw.rds")


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
saveRDS(f_corpus_ls, "my_data/f_corpus/f_corpus_ls.rds")


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
saveRDS(f_corpus_lc, "my_data/f_corpus/f_corpus_lc.rds")
