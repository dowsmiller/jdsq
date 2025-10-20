#clear variables and directories from memory
rm(list = ls())

#install the relevant packages
install.packages(c("stringr", "pbapply"))

#load the relevant packages
library(stringr)
library(pbapply)

#set path
b_corpus_path <- "my_data/b_corpus/txt_raw"

#get files in directory
b_corpus_files <- list.files(b_corpus_path, pattern = "*.txt", full.names = TRUE)

#read in files
b_corpus <- pblapply(b_corpus_files, readLines)

#set df name to the file name after character 26
    #with ".txt" removed and text removed after " "
    #adding two-digit number at beginning
df_names <- vector("character", length(b_corpus))

for (i in 1:length(b_corpus)) {
    df_names[i] <- substr(b_corpus_files[i], 26, nchar(b_corpus_files[i]))
    df_names[i] <- str_replace_all(df_names[i], "\\.txt", "")
    df_names[i] <- str_replace_all(df_names[i], "_.*$", "")
    df_names[i] <- paste0(sprintf("%03d", i),"_", df_names[i])
}

names(b_corpus) <- df_names

#delete trailing spaces
for (i in 1:length(b_corpus)) {
  b_corpus[[i]] <- str_trim(b_corpus[[i]])
}

#delete lines containing [Ee]xplicit
for (i in 1:length(b_corpus)) {
    b_corpus[[i]] <- b_corpus[[i]][!str_detect(b_corpus[[i]], "[Ee]xplicit")]
}

#delete anything in square brackets
for (i in 1:length(b_corpus)) {
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "\\[.*\\]", "")
}

#delete empty lines with no following lines containing string
for (i in 1:length(b_corpus)) {
    for (j in 1:length(b_corpus[[i]])) {
        if (j == length(b_corpus[[i]]) && b_corpus[[i]][j] == "") {
            b_corpus[[i]][j] <- NA
        } else if (b_corpus[[i]][j] == "" && paste0(b_corpus[[i]][(j + 1):length(b_corpus[[i]])], collapse = "") == "") {
            b_corpus[[i]][j] <- NA
        }
    }
    b_corpus[[i]] <- b_corpus[[i]][!sapply(b_corpus[[i]], is.na)]
}

#text changes
for (i in 1:length(b_corpus)) {
    #remove trailing spaces
    b_corpus[[i]] <- str_trim(b_corpus[[i]])

    #if a line starts with "- " and ends with " -", replace with "(" and ")"
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "^[—-] (.*) [—-]$", "\\(\\1\\)")

    #delete <
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "<", "")

    #replace "^- " with "“"
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "^[—-] ", "“")

    #replace " - " with "” “"
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], " [—-] ", "” “")

    #replace "« " with "“"
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "«[^A-zÀ-ÿ0-9\\(\\)\\.,?!\";:—-”“]", "“")

    #replace " »" with "”"
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "[^A-zÀ-ÿ0-9\\(\\)\\.,?!\";:—-”“]»", "”")

    #replace "«" with "“"
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "«", "“")

    #replace "»" with "”"
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "»", "”")

    #remove all but a limited selection of characters
    b_corpus[[i]] <- str_replace_all(b_corpus[[i]], "[^A-zÀ-ÿ\\(\\)\\.,?!\";:—-”“'’ ]", "")
}


#save b_corpus as individual text files
dir.create("my_data/b_corpus/txt", showWarnings = FALSE)

for (i in 1:length(b_corpus)) {
    file_name <- paste0(gsub("[’ ]", "_", gsub("\\.", "", names(b_corpus)[i])))
    file_path <- paste0("my_data/b_corpus/txt/", file_name, ".txt")
    write.table(b_corpus[[i]], file_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}


# –––––––––– b_corpus_c = all lines concatenated ––––––––––
b_corpus_c <- b_corpus

for (i in 1:length(b_corpus_c)) {
    for (j in 1:length(b_corpus_c[[i]])) {
        if (b_corpus_c[[i]][j] == "") {
            b_corpus_c[[i]][j] <- NA
        }
    }

    b_corpus_c[[i]] <- b_corpus_c[[i]][!sapply(b_corpus_c[[i]], is.na)]
}

#save b_corpus_c as .rds
saveRDS(b_corpus_c, "my_data/b_corpus/b_corpus_c.rds")


# –––––––––– b_corpus_s = separated into stanzas ––––––––––
#add column with stanza number
b_corpus_s <- b_corpus

for (i in 1:length(b_corpus_s)) {
    b_corpus_s[[i]] <- data.frame(text = b_corpus_s[[i]], stanza = NA, stringsAsFactors = FALSE)
    stanza <- 1
    for (j in 1:nrow(b_corpus_s[[i]])) {
        if (b_corpus_s[[i]][j, 1] == "") {
            stanza <- stanza + 1
        } else {
            b_corpus_s[[i]][j, 2] <- stanza
        }
    }
}

#remove empty rows
for (i in 1:length(b_corpus_s)) {
  rows_to_exclude <- which(b_corpus_s[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    b_corpus_s[[i]] <- b_corpus_s[[i]][-rows_to_exclude, ]
  }
}

#group into dfs according to stanza number
b_corpus_s_temp <- list()

for (i in 1:length(b_corpus_s)) {
    b_corpus_s_temp[[i]] <- split(b_corpus_s[[i]], b_corpus_s[[i]]$stanza)
}

b_corpus_s <- b_corpus_s_temp

#remove stanza column
for (i in 1:length(b_corpus_s)) {
    for (j in 1:length(b_corpus_s[[i]])) {
        b_corpus_s[[i]][[j]] <- b_corpus_s[[i]][[j]][, -2]
    }
}

#add line number column before text
for (i in 1:length(b_corpus_s)) {
    line <- 1
    for (j in 1:length(b_corpus_s[[i]])) {
        df <- data.frame(line = NA, text = b_corpus_s[[i]][[j]], stringsAsFactors = FALSE)
        for (k in 1:nrow(df)) {
            df[k, 1] <- line
            line <- line + 1
        }
        b_corpus_s[[i]][[j]] <- df
    }
}

#rename lists as in b_corpus
names(b_corpus_s) <- names(b_corpus)

#save b_corpus_s as .rds
saveRDS(b_corpus_s, "my_data/b_corpus/b_corpus_s.rds")


# –––––––––– b_corpus_w = separated into words with stanza and line number ––––––––––
b_corpus_w <- b_corpus_s

#add column with stanza number
for (i in 1:length(b_corpus_w)) {
  for (j in 1:length(b_corpus_w[[i]])) {
    b_corpus_w[[i]][[j]]$stanza <- j
  }
}

#modify text
for (i in 1:length(b_corpus_w)) {
  for (j in 1:length(b_corpus_w[[i]])) {

    #remove all punctuation except apostrophes
    b_corpus_w[[i]][[j]]$text <- str_replace_all(b_corpus_w[[i]][[j]]$text, "[^A-zÀ-ÿ'’ ]", "")

    #replace apostrophes with space
    b_corpus_w[[i]][[j]]$text <- str_replace_all(b_corpus_w[[i]][[j]]$text, "['’]", " ")

    #remove trailing spaces
    b_corpus_w[[i]][[j]]$text <- str_trim(b_corpus_w[[i]][[j]]$text)

    #convert to lowercase
    b_corpus_w[[i]][[j]]$text <- str_to_lower(b_corpus_w[[i]][[j]]$text)
  }
}

#separate text into words
b_corpus_w_temp <- list()

b_corpus_w_temp <- pblapply(b_corpus_w, function(x){
    words <- vector()
    stanzas <- vector()
    lines <- vector()

    for (j in 1:length(x)) {
        for (k in 1:nrow(x[[j]])) {
            words <- c(words, unlist(str_split(x[[j]][k, 2], " ")))
            n_words <- length(unlist(str_split(x[[j]][k, 2], " ")))
            stanzas <- c(stanzas, rep(x[[j]][k, 3], n_words))
            lines <- c(lines, rep(x[[j]][k, 1], n_words))
        }
    }
    df <- data.frame(word = words, stanza = stanzas, line = lines, stringsAsFactors = FALSE)
    df
})

# for (i in 1:length(b_corpus_w)) {
#     words <- vector()
#     stanzas <- vector()
#     lines <- vector()

#     for (j in 1:length(b_corpus_w[[i]])) {
#         for (k in 1:nrow(b_corpus_w[[i]][[j]])) {
#             words <- c(words, unlist(str_split(b_corpus_w[[i]][[j]][k, 2], " ")))
#             n_words <- length(unlist(str_split(b_corpus_w[[i]][[j]][k, 2], " ")))
#             stanzas <- c(stanzas, rep(b_corpus_w[[i]][[j]][k, 3], n_words))
#             lines <- c(lines, rep(b_corpus_w[[i]][[j]][k, 1], n_words))
#         }
#     }
#     b_corpus_w_temp[[i]] <- data.frame(word = words, stanza = stanzas, line = lines, stringsAsFactors = FALSE)
# }

b_corpus_w <- b_corpus_w_temp

#delete rows with empty word
for (i in 1:length(b_corpus_w)) {
  rows_to_exclude <- which(b_corpus_w[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    b_corpus_w[[i]] <- b_corpus_w[[i]][-rows_to_exclude, ]
  }
}

#rename lists as in b_corpus
names(b_corpus_w) <- names(b_corpus)

#reset rownames
for (i in 1:length(b_corpus_w)) {
  rownames(b_corpus_w[[i]]) <- NULL
}

#save b_corpus_w as .rds
saveRDS(b_corpus_w, "my_data/b_corpus/b_corpus_w.rds")


# –––––––––– b_corpus_lw = lemmatised list aligned with words ––––––––––
#load lemmatised files
b_corpus_lw <- list.files("my_data/b_corpus/lemmatised/", pattern = "*.txt", full.names = TRUE)

b_corpus_lw <- pblapply(b_corpus_lw, read.delim, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("word", "pos", "lemma"))

#modify words
b_corpus_lw <- pblapply(b_corpus_lw, function(x) {
  for (i in 1:nrow(x)) {
    x[i, "word"] <- tolower(str_replace_all(x[i, "word"], "[^A-zÀ-ÿ]", ""))
  }
  x
})

#remove rows with empty words
for (i in 1:length(b_corpus_lw)) {
  rows_to_exclude <- which(b_corpus_lw[[i]][ ,1] == "")
  if (length(rows_to_exclude) > 0) {
    b_corpus_lw[[i]] <- b_corpus_lw[[i]][-rows_to_exclude, ]
  }
}

#reset row names
for (i in 1:length(b_corpus_lw)) {
  rownames(b_corpus_lw[[i]]) <- NULL
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

b_corpus_lw <- pblapply(seq_along(b_corpus_w), combine_lw, b_corpus_w, b_corpus_lw)

#rename lists as in b_corpus
names(b_corpus_lw) <- names(b_corpus)

#save b_corpus_lw as .rds
saveRDS(b_corpus_lw, "my_data/b_corpus/b_corpus_lw.rds")


# –––––––––– b_corpus_ls = lemmatised in stanzas ––––––––––
#recombine lines and stanzas for lemmata

b_corpus_ls <- list()

for (i in seq_along(b_corpus_lw)) {
    b_corpus_ls[[i]] <- list()
    for (j in unique(b_corpus_lw[[i]]$stanza)) {
        stanza <- b_corpus_lw[[i]][b_corpus_lw[[i]]$stanza == j, ]
        b_corpus_ls[[i]][[j]] <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(b_corpus_ls[[i]][[j]]) <- c("line", "text")
        for (k in 1:length(unique(stanza$line))) {
            line <- paste0(stanza[stanza$line == unique(stanza$line)[k], 2], collapse = " ")
            b_corpus_ls[[i]][[j]][k, 1] <- unique(stanza$line)[k]
            b_corpus_ls[[i]][[j]][k, 2] <- line
        }
    }
}

#rename lists as in b_corpus
names(b_corpus_ls) <- names(b_corpus)

#save b_corpus_ls as .rds
saveRDS(b_corpus_ls, "my_data/b_corpus/b_corpus_ls.rds")


# –––––––––– b_corpus_lc = lemmatised and concatenated ––––––––––

b_corpus_lc <- list()

for (i in seq_along(b_corpus_ls)) {
    b_corpus_lc[[i]] <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(b_corpus_lc[[i]]) <- c("line", "text")
    for (j in seq_along(b_corpus_ls[[i]])) {
        for (k in 1:nrow(b_corpus_ls[[i]][[j]])) {
            b_corpus_lc[[i]][nrow(b_corpus_lc[[i]]) + 1, ] <- c(
                b_corpus_ls[[i]][[j]][k, 1],
                b_corpus_ls[[i]][[j]][k, 2]
            )
        }
    }
}

#rename lists as in b_corpus
names(b_corpus_lc) <- names(b_corpus)

#save b_corpus_lc as .rds
saveRDS(b_corpus_lc, "my_data/b_corpus/b_corpus_lc.rds")
