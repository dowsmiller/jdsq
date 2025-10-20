#clear variables and directories from memory
rm(list = ls())

#libraries
library(stringr)

#directories
rdir <- "my_data/l_corpus"
script_name <- "form"
wdir <- file.path("my_data/l_corpus/outputs", script_name)
dir.create(wdir, showWarnings = FALSE)

#load data
l_corpus_c <- readRDS(file.path(rdir, "l_corpus_c.rds"))
l_corpus_s <- readRDS(file.path(rdir, "l_corpus_s.rds"))
l_corpus_w <- readRDS(file.path(rdir, "l_corpus_w.rds"))
l_corpus_lc <- readRDS(file.path(rdir, "l_corpus_lc.rds"))
l_corpus_ls <- readRDS(file.path(rdir, "l_corpus_ls.rds"))
l_corpus_lw <- readRDS(file.path(rdir, "l_corpus_lw.rds"))

#remove line numbers from l_corpus_lc and and convert list items to vectors
for (i in seq_along(l_corpus_lc)) {
    text_i <- vector()
    for (j in 1:nrow(l_corpus_lc[[i]])) {
        text_i[j] <- l_corpus_lc[[i]][j, 2]
    }
    l_corpus_lc[[i]] <- text_i
}

#get lengths of texts
text_lengths_l <- data.frame(
    text_name = character(),
    length = integer(),
    stringsAsFactors = FALSE
)

for (i in seq_along(l_corpus_c)) {
    text_name <- names(l_corpus_c)[i]
    text_length <- length(l_corpus_c[[i]])
    text_lengths_l[i, ] <- c(text_name, text_length)
}

#plots
library(tidyr)
library(dplyr)
library(ggplot2)

#add text names as row names, then remove text_name column
rownames(text_lengths_l) <- text_lengths_l$text_name
text_lengths_l$text_name <- NULL

palette <- "#006493"

ggplot(
    text_lengths_l,
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
