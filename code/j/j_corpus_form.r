#clear variables and directories from memory
rm(list = ls())

#libraries
library(stringr)

#directories
rdir <- "my_data/j_corpus"
script_name <- "form"
wdir <- file.path("my_data/j_corpus/outputs", script_name)
dir.create(wdir, showWarnings = FALSE)

#load data
j_corpus_c <- readRDS(file.path(rdir, "j_corpus_c.rds"))
j_corpus_s <- readRDS(file.path(rdir, "j_corpus_s.rds"))
j_corpus_w <- readRDS(file.path(rdir, "j_corpus_w.rds"))
j_corpus_lc <- readRDS(file.path(rdir, "j_corpus_lc.rds"))
j_corpus_ls <- readRDS(file.path(rdir, "j_corpus_ls.rds"))
j_corpus_lw <- readRDS(file.path(rdir, "j_corpus_lw.rds"))

#remove line numbers from j_corpus_lc and and convert list items to vectors
for (i in seq_along(j_corpus_lc)) {
    text_i <- vector()
    for (j in 1:nrow(j_corpus_lc[[i]])) {
        text_i[j] <- j_corpus_lc[[i]][j, 2]
    }
    j_corpus_lc[[i]] <- text_i
}

#get lengths of texts
text_lengths_j <- data.frame(
    text_name = character(),
    length = integer(),
    stringsAsFactors = FALSE
)

for (i in seq_along(j_corpus_c)) {
    text_name <- names(j_corpus_c)[i]
    text_length <- length(j_corpus_c[[i]])
    text_lengths_j[i, ] <- c(text_name, text_length)
}

#get manual text lengths
text_lengths_j <- read.csv("my_data/text_lengths_jdsq_manual.csv", stringsAsFactors = FALSE)[, 1:2]
colnames(text_lengths_j) <- c("text_name", "length")

#plots
library(tidyr)
library(dplyr)
library(ggplot2)

#add text names as row names, then remove text_name column
rownames(text_lengths_j) <- text_lengths_j$text_name
text_lengths_j$text_name <- NULL

palette <- "#006493"

ggplot(
    text_lengths_j,
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
