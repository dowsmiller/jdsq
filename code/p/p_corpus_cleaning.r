#clear variables and directories from memory
rm(list = ls())

#install the relevant packages
install.packages(c("stringr", "pbapply"))

#load the relevant packages
library(stringr)
library(pbapply)

#set path
p_corpus_path <- "my_data/p_corpus/txt_raw"

#get files in directory
p_corpus_files <- list.files(p_corpus_path, pattern = "*.txt", full.names = TRUE)

#read in files
p_corpus <- pblapply(p_corpus_files, readLines)

#set df name to the file name after character 26 for each
    #with ".txt" removed and "_" replaced with " "
df_names <- vector("character", length(p_corpus))

for (i in 1:length(p_corpus)) {
  df_names[i] <- str_replace_all(str_sub(p_corpus_files[i], 26), ".txt", "")
  df_names[i] <- str_replace_all(df_names[i], "_", " ")
}

names(p_corpus) <- df_names

#define texts to exclude
jdsq <- c("03", "11", "20", "28", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "53", "55", "57", "61", "74")

#define function to exclude texts whose name begins with a number in the above vectors
exclude <- function(x) {if (str_sub(x, 1, 2) %in% jdsq) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#delete texts assigned to JdSQ
p_corpus <- p_corpus[!sapply(names(p_corpus), exclude)]

#modify text
for (i in 1:length(p_corpus)) {
    #delete trailing spaces
    p_corpus[[i]] <- str_trim(p_corpus[[i]])

    #remove all but a limited selection of characters
    p_corpus[[i]] <- str_replace_all(p_corpus[[i]], "[^A-zÀ-ÿ\\(\\)\\.,?!\";:—-”“'’ ]", "")
}

#delete empty strings from vector if preceded by another empty string
for (i in 1:length(p_corpus)) {
    for (j in 1:length(p_corpus[[i]])) {
        if (j > 1) {
            if (p_corpus[[i]][j] == "" && (p_corpus[[i]][j - 1] == "" || is.na(p_corpus[[i]][j - 1]))) {
                p_corpus[[i]][j] <- NA
            }
        }
    }

    p_corpus[[i]] <- p_corpus[[i]][!sapply(p_corpus[[i]], is.na)]
}

#delete final lines matching regex ^[AMENamen. ]+$
for (i in 1:length(p_corpus)) {
    for (j in 1:length(p_corpus[[i]])) {
        if (j == length(p_corpus[[i]])) {
            if (str_detect(p_corpus[[i]][j], "^[AMENamen. ]+$")) {
                p_corpus[[i]][j] <- NA
            }
        }
    }

    p_corpus[[i]] <- p_corpus[[i]][!sapply(p_corpus[[i]], is.na)]
}

#delete empty lines in first and final position
for (i in 1:length(p_corpus)) {
    if (p_corpus[[i]][1] == "") {
        p_corpus[[i]][1] <- NA
    }

    if (p_corpus[[i]][length(p_corpus[[i]])] == "") {
        p_corpus[[i]][length(p_corpus[[i]])] <- NA
    }

    p_corpus[[i]] <- p_corpus[[i]][!sapply(p_corpus[[i]], is.na)]
}

#save p_corpus as individual text files
dir.create("my_data/p_corpus/txt", showWarnings = FALSE)

for (i in 1:length(p_corpus)) {
    file_name <- paste0(gsub("[’ ]", "_", gsub("\\.", "", names(p_corpus)[i])))
    file_path <- paste0("my_data/p_corpus/txt/", file_name, ".txt")
    write.table(p_corpus[[i]], file_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}

# –––––––––– p_corpus_c = all lines concatenated ––––––––––
p_corpus_c <- p_corpus

for (i in 1:length(p_corpus_c)) {
    for (j in 1:length(p_corpus_c[[i]])) {
        if (p_corpus_c[[i]][j] == "") {
            p_corpus_c[[i]][j] <- NA
        }
    }

    p_corpus_c[[i]] <- p_corpus_c[[i]][!sapply(p_corpus_c[[i]], is.na)]
}

#save p_corpus_c as .rds
saveRDS(p_corpus_c, "my_data/p_corpus/p_corpus_c.rds")


# –––––––––– p_corpus_s = separated into stanzas ––––––––––
#add column with stanza number
p_corpus_s <- p_corpus

for (i in 1:length(p_corpus_s)) {
    p_corpus_s[[i]] <- data.frame(text = p_corpus_s[[i]], stanza = NA, stringsAsFactors = FALSE)
    stanza <- 1
    for (j in 1:nrow(p_corpus_s[[i]])) {
        if (p_corpus_s[[i]][j, 1] == "") {
            stanza <- stanza + 1
        } else {
            p_corpus_s[[i]][j, 2] <- stanza
        }
    }
}

#remove empty rows
for (i in 1:length(p_corpus_s)) {
  rows_to_exclude <- which(p_corpus_s[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    p_corpus_s[[i]] <- p_corpus_s[[i]][-rows_to_exclude, ]
  }
}

#group into dfs according to stanza number
p_corpus_s_temp <- list()

for (i in 1:length(p_corpus_s)) {
    p_corpus_s_temp[[i]] <- split(p_corpus_s[[i]], p_corpus_s[[i]]$stanza)
}

p_corpus_s <- p_corpus_s_temp

#remove stanza column
for (i in 1:length(p_corpus_s)) {
    for (j in 1:length(p_corpus_s[[i]])) {
        p_corpus_s[[i]][[j]] <- p_corpus_s[[i]][[j]][, 1]
    }
}

#add line number column before text
for (i in 1:length(p_corpus_s)) {
    line <- 1
    for (j in 1:length(p_corpus_s[[i]])) {
        df <- data.frame(line = NA, text = p_corpus_s[[i]][[j]], stringsAsFactors = FALSE)
        for (k in 1:nrow(df)) {
            df[k, 1] <- line
            line <- line + 1
        }
        p_corpus_s[[i]][[j]] <- df
    }
}

#rename lists as in p_corpus
names(p_corpus_s) <- names(p_corpus)

#save p_corpus_s as .rds
saveRDS(p_corpus_s, "my_data/p_corpus/p_corpus_s.rds")


# –––––––––– p_corpus_w = separated into words with stanza and line number ––––––––––
p_corpus_w <- p_corpus_s

#add column with stanza number
for (i in 1:length(p_corpus_w)) {
  for (j in 1:length(p_corpus_w[[i]])) {
    p_corpus_w[[i]][[j]]$stanza <- j
  }
}

#modify text
for (i in 1:length(p_corpus_w)) {
  for (j in 1:length(p_corpus_w[[i]])) {
    #remove all punctuation except apostrophes
    p_corpus_w[[i]][[j]]$text <- str_replace_all(p_corpus_w[[i]][[j]]$text, "[^A-zÀ-ÿ'’ ]", "")

    #replace apostrophes with space
    p_corpus_w[[i]][[j]]$text <- str_replace_all(p_corpus_w[[i]][[j]]$text, "['’]", " ")

    #remove trailing spaces
    p_corpus_w[[i]][[j]]$text <- str_trim(p_corpus_w[[i]][[j]]$text)

    #convert to lowercase
    p_corpus_w[[i]][[j]]$text <- str_to_lower(p_corpus_w[[i]][[j]]$text)
  }
}

#separate text into words
p_corpus_w_temp <- list()

for (i in 1:length(p_corpus_w)) {
    words <- vector()
    stanzas <- vector()
    lines <- vector()

    for (j in 1:length(p_corpus_w[[i]])) {
        for (k in 1:nrow(p_corpus_w[[i]][[j]])) {
            words <- c(words, unlist(str_split(p_corpus_w[[i]][[j]][k, 2], " ")))
            n_words <- length(unlist(str_split(p_corpus_w[[i]][[j]][k, 2], " ")))
            stanzas <- c(stanzas, rep(p_corpus_w[[i]][[j]][k, 3], n_words))
            lines <- c(lines, rep(p_corpus_w[[i]][[j]][k, 1], n_words))
        }
    }
    p_corpus_w_temp[[i]] <- data.frame(word = words, stanza = stanzas, line = lines, stringsAsFactors = FALSE)
}

p_corpus_w <- p_corpus_w_temp

#delete rows with empty word
for (i in 1:length(p_corpus_w)) {
  rows_to_exclude <- which(p_corpus_w[[i]][, 1] == "")
  if (length(rows_to_exclude) > 0) {
    p_corpus_w[[i]] <- p_corpus_w[[i]][-rows_to_exclude, ]
  }
}

#rename lists as in p_corpus
names(p_corpus_w) <- names(p_corpus)

#reset rownames
for (i in 1:length(p_corpus_w)) {
  rownames(p_corpus_w[[i]]) <- NULL
}

#save p_corpus_w as .rds
saveRDS(p_corpus_w, "my_data/p_corpus/p_corpus_w.rds")


# –––––––––– p_corpus_lw = lemmatised list aligned with words ––––––––––
#load lemmatised files
p_corpus_lw <- list.files("my_data/p_corpus/lemmatised/", pattern = "*.txt", full.names = TRUE)

p_corpus_lw <- pblapply(p_corpus_lw, read.delim, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("word", "pos", "lemma"))

#modify words
p_corpus_lw <- pblapply(p_corpus_lw, function(x) {
  for (i in 1:nrow(x)) {
    x[i, "word"] <- tolower(str_replace_all(x[i, "word"], "[^A-zÀ-ÿ]", ""))
  }
  x
})

#remove rows with empty words
for (i in 1:length(p_corpus_lw)) {
  rows_to_exclude <- which(p_corpus_lw[[i]][ ,1] == "")
  if (length(rows_to_exclude) > 0) {
    p_corpus_lw[[i]] <- p_corpus_lw[[i]][-rows_to_exclude, ]
  }
}

#reset row names
for (i in 1:length(p_corpus_lw)) {
  rownames(p_corpus_lw[[i]]) <- NULL
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

p_corpus_lw <- pblapply(seq_along(p_corpus_w), combine_lw, p_corpus_w, p_corpus_lw)

#rename lists as in p_corpus
names(p_corpus_lw) <- names(p_corpus)

#save p_corpus_lw as .rds
saveRDS(p_corpus_lw, "my_data/p_corpus/p_corpus_lw.rds")


# –––––––––– p_corpus_ls = lemmatised in stanzas ––––––––––
#recombine lines and stanzas for lemmata

p_corpus_ls <- list()

for (i in seq_along(p_corpus_lw)) {
    p_corpus_ls[[i]] <- list()
    for (j in unique(p_corpus_lw[[i]]$stanza)) {
        stanza <- p_corpus_lw[[i]][p_corpus_lw[[i]]$stanza == j, ]
        p_corpus_ls[[i]][[j]] <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(p_corpus_ls[[i]][[j]]) <- c("line", "text")
        for (k in 1:length(unique(stanza$line))) {
            line <- paste0(stanza[stanza$line == unique(stanza$line)[k], 2], collapse = " ")
            p_corpus_ls[[i]][[j]][k, 1] <- unique(stanza$line)[k]
            p_corpus_ls[[i]][[j]][k, 2] <- line
        }
    }
}

#rename lists as in p_corpus
names(p_corpus_ls) <- names(p_corpus)

#delete empty list items
for (i in 1:length(p_corpus_ls)) {
    p_corpus_ls[[i]] <- p_corpus_ls[[i]][!sapply(p_corpus_ls[[i]], function(x) nrow(x) == 0)]
}

#save p_corpus_ls as .rds
saveRDS(p_corpus_ls, "my_data/p_corpus/p_corpus_ls.rds")


# –––––––––– p_corpus_lc = lemmatised and concatenated ––––––––––

p_corpus_lc <- list()

for (i in seq_along(p_corpus_ls)) {
    p_corpus_lc[[i]] <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(p_corpus_lc[[i]]) <- c("line", "text")
    if (class(p_corpus_ls[[i]]) == "list") {
        for (j in seq_along(p_corpus_ls[[i]])) {
            for (k in 1:nrow(p_corpus_ls[[i]][[j]])) {
                p_corpus_lc[[i]][nrow(p_corpus_lc[[i]]) + 1, ] <- c(
                    p_corpus_ls[[i]][[j]][k, 1],
                    p_corpus_ls[[i]][[j]][k, 2]
                )
            }
        }
    } else {
        for (j in 1:nrow(p_corpus_ls[[i]])) {
            p_corpus_lc[[i]][nrow(p_corpus_lc[[i]]) + 1, ] <- c(
                p_corpus_ls[[i]][j, 1],
                p_corpus_ls[[i]][j, 2]
            )
        }
    }
}

#rename lists as in p_corpus
names(p_corpus_lc) <- names(p_corpus)

#save p_corpus_lc as .rds
saveRDS(p_corpus_lc, "my_data/p_corpus/p_corpus_lc.rds")






# –––––––––– p0_corpus: all texts except fragments and those assigned to JdSQ ––––––––––
#set path
p0_path <- "my_data/p0_corpus/"

#define texts to exclude
fragments <- c("16", "52", "60")

#define function to exclude texts whose name begins with a number in the above vectors
exclude <- function(x) {if (str_sub(x, 1, 2) %in% fragments) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#p0_corpus_c = all lines concatenated
p0_corpus_c <- p_corpus_c[!sapply(names(p_corpus_c), exclude)]
saveRDS(p0_corpus_c, "my_data/p0_corpus/p0_corpus_c.rds")

#p0_corpus_s = separated into stanzas
p0_corpus_s <- p_corpus_s[!sapply(names(p_corpus_s), exclude)]
saveRDS(p0_corpus_s, "my_data/p0_corpus/p0_corpus_s.rds")

#p0_corpus_w = separated into words with stanza and line number
p0_corpus_w <- p_corpus_w[!sapply(names(p_corpus_w), exclude)]
saveRDS(p0_corpus_w, "my_data/p0_corpus/p0_corpus_w.rds")

#p0_corpus_lw = lemmatised list aligned with words
p0_corpus_lw <- p_corpus_lw[!sapply(names(p_corpus_lw), exclude)]
saveRDS(p0_corpus_lw, "my_data/p0_corpus/p0_corpus_lw.rds")

#p0_corpus_ls = lemmatised in stanzas
p0_corpus_ls <- p_corpus_ls[!sapply(names(p_corpus_ls), exclude)]
saveRDS(p0_corpus_ls, "my_data/p0_corpus/p0_corpus_ls.rds")

#p0_corpus_lc = lemmatised and concatenated
p0_corpus_lc <- p_corpus_lc[!sapply(names(p_corpus_lc), exclude)]
saveRDS(p0_corpus_lc, "my_data/p0_corpus/p0_corpus_lc.rds")



# –––––––––– p1_corpus: all poetic texts except fragments and those assigned to JdSQ ––––––––––
#set path
p1_path <- "my_data/p1_corpus/"

#define texts to exclude
prose <- c("21", "25", "27", "71", "77", "78", "89", "90")

#define function to exclude texts whose name begins with a number in the above vectors
exclude <- function(x) {
    if (str_sub(x, 1, 2) %in% fragments) {
        return(TRUE)
    } else if (str_sub(x, 1, 2) %in% prose) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#p1_corpus_c = all lines concatenated
p1_corpus_c <- p_corpus_c[!sapply(names(p_corpus_c), exclude)]
saveRDS(p1_corpus_c, "my_data/p1_corpus/p1_corpus_c.rds")

#p1_corpus_s = separated into stanzas
p1_corpus_s <- p_corpus_s[!sapply(names(p_corpus_s), exclude)]
saveRDS(p1_corpus_s, "my_data/p1_corpus/p1_corpus_s.rds")

#p1_corpus_w = separated into words with stanza and line number
p1_corpus_w <- p_corpus_w[!sapply(names(p_corpus_w), exclude)]
saveRDS(p1_corpus_w, "my_data/p1_corpus/p1_corpus_w.rds")

#p1_corpus_lw = lemmatised list aligned with words
p1_corpus_lw <- p_corpus_lw[!sapply(names(p_corpus_lw), exclude)]
saveRDS(p1_corpus_lw, "my_data/p1_corpus/p1_corpus_lw.rds")

#p1_corpus_ls = lemmatised in stanzas
p1_corpus_ls <- p_corpus_ls[!sapply(names(p_corpus_ls), exclude)]
saveRDS(p1_corpus_ls, "my_data/p1_corpus/p1_corpus_ls.rds")

#p1_corpus_lc = lemmatised and concatenated
p1_corpus_lc <- p_corpus_lc[!sapply(names(p_corpus_lc), exclude)]
saveRDS(p1_corpus_lc, "my_data/p1_corpus/p1_corpus_lc.rds")


# –––––––––– p2_corpus: all prose texts ––––––––––
#set path
p2_path <- "my_data/p2_corpus/"

#define function to include texts whose name begins with a number in prose
include <- function(x) {
    if (str_sub(x, 1, 2) %in% prose) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#p2_corpus_c = all lines concatenated
p2_corpus_c <- p_corpus_c[sapply(names(p_corpus_c), include)]
saveRDS(p2_corpus_c, "my_data/p2_corpus/p2_corpus_c.rds")

#p2_corpus_s = separated into stanzas
p2_corpus_s <- p_corpus_s[sapply(names(p_corpus_s), include)]
saveRDS(p2_corpus_s, "my_data/p2_corpus/p2_corpus_s.rds")

#p2_corpus_w = separated into words with stanza and line number
p2_corpus_w <- p_corpus_w[sapply(names(p_corpus_w), include)]
saveRDS(p2_corpus_w, "my_data/p2_corpus/p2_corpus_w.rds")

#p2_corpus_lw = lemmatised list aligned with words
p2_corpus_lw <- p_corpus_lw[sapply(names(p_corpus_lw), include)]
saveRDS(p2_corpus_lw, "my_data/p2_corpus/p2_corpus_lw.rds")

#p2_corpus_ls = lemmatised in stanzas
p2_corpus_ls <- p_corpus_ls[sapply(names(p_corpus_ls), include)]
saveRDS(p2_corpus_ls, "my_data/p2_corpus/p2_corpus_ls.rds")

#p2_corpus_lc = lemmatised and concatenated
p2_corpus_lc <- p_corpus_lc[sapply(names(p_corpus_lc), include)]
saveRDS(p2_corpus_lc, "my_data/p2_corpus/p2_corpus_lc.rds")


# –––––––––– p3_corpus: all fragments ––––––––––
#set path
p3_path <- "my_data/p3_corpus/"

#define function to include texts whose name begins with a number in fragments
include <- function(x) {
    if (str_sub(x, 1, 2) %in% fragments) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#p3_corpus_c = all lines concatenated
p3_corpus_c <- p_corpus_c[sapply(names(p_corpus_c), include)]
saveRDS(p3_corpus_c, "my_data/p3_corpus/p3_corpus_c.rds")

#p3_corpus_s = separated into stanzas
p3_corpus_s <- p_corpus_s[sapply(names(p_corpus_s), include)]
saveRDS(p3_corpus_s, "my_data/p3_corpus/p3_corpus_s.rds")

#p3_corpus_w = separated into words with stanza and line number
p3_corpus_w <- p_corpus_w[sapply(names(p_corpus_w), include)]
saveRDS(p3_corpus_w, "my_data/p3_corpus/p3_corpus_w.rds")

#p3_corpus_lw = lemmatised list aligned with words
p3_corpus_lw <- p_corpus_lw[sapply(names(p_corpus_lw), include)]
saveRDS(p3_corpus_lw, "my_data/p3_corpus/p3_corpus_lw.rds")

#p3_corpus_ls = lemmatised in stanzas
p3_corpus_ls <- p_corpus_ls[sapply(names(p_corpus_ls), include)]
saveRDS(p3_corpus_ls, "my_data/p3_corpus/p3_corpus_ls.rds")

#p3_corpus_lc = lemmatised and concatenated
p3_corpus_lc <- p_corpus_lc[sapply(names(p_corpus_lc), include)]
saveRDS(p3_corpus_lc, "my_data/p3_corpus/p3_corpus_lc.rds")