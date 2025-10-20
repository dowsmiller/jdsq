#clear variables and directories from memory
rm(list = ls())

#libraries
library(stringr)

#directories
rdir <- "my_data/r_corpus"
script_name <- "form"
wdir <- file.path("my_data/r_corpus/outputs", script_name)
dir.create(wdir, showWarnings = FALSE)

#load data
r_corpus_c <- readRDS(file.path(rdir, "r_corpus_c.rds"))
r_corpus_s <- readRDS(file.path(rdir, "r_corpus_s.rds"))
r_corpus_w <- readRDS(file.path(rdir, "r_corpus_w.rds"))
r_corpus_lc <- readRDS(file.path(rdir, "r_corpus_lc.rds"))
r_corpus_ls <- readRDS(file.path(rdir, "r_corpus_ls.rds"))
r_corpus_lw <- readRDS(file.path(rdir, "r_corpus_lw.rds"))

#remove line numbers from r_corpus_c and _lc and convert list items to vectors
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

#get lengths of texts
text_lengths_r <- data.frame(
    text_name = character(),
    length = integer(),
    stringsAsFactors = FALSE
)

for (i in seq_along(r_corpus_c)) {
    text_name <- names(r_corpus_c)[i]
    text_length <- length(r_corpus_c[[i]])
    text_lengths_r[i, ] <- c(text_name, text_length)
}

#plots
library(tidyr)
library(dplyr)
library(ggplot2)

#add text names as row names, then remove text_name column
rownames(text_lengths_r) <- text_lengths_r$text_name
text_lengths_r$text_name <- NULL

palette <- "#006493"

ggplot(
    text_lengths_r,
    aes(y = as.numeric(length))
) +
geom_boxplot(
    fill = palette,
    outlier.size = 3
) +
scale_fill_manual(values = palette) +
scale_x_discrete(labels = "R Corpus") +
ylab("Text Length (lines)") +
theme_minimal() +
theme(
    axis.text = element_text(size = 15, face = "italic"),
    axis.title = element_text(size = 15),
)

ggsave(
    filename = file.path(wdir, "text_length_boxplot.png"),
    device = "png",
    dpi = 700,
    width = 5,
    height = 5
)
