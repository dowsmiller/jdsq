#clear variables and directories from memory
rm(list = ls())

#libraries
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)

#directories
rdir <- "wdat/a"
script_name <- "form"
dir.create("outputs/a", showWarnings = FALSE)
odir <- file.path("outputs/a", script_name)
dir.create(odir, showWarnings = FALSE)

#load data
a_corpus_c <- readRDS(file.path(rdir, "a_corpus_c.rds"))
a_corpus_s <- readRDS(file.path(rdir, "a_corpus_s.rds"))
a_corpus_w <- readRDS(file.path(rdir, "a_corpus_w.rds"))
a_corpus_lc <- readRDS(file.path(rdir, "a_corpus_lc.rds"))
a_corpus_ls <- readRDS(file.path(rdir, "a_corpus_ls.rds"))
a_corpus_lw <- readRDS(file.path(rdir, "a_corpus_lw.rds"))

#remove line numbers from a_corpus_lc and and convert list items to vectors
for (i in seq_along(a_corpus_lc)) {
    text_i <- vector()
    for (j in 1:nrow(a_corpus_lc[[i]])) {
        text_i[j] <- a_corpus_lc[[i]][j, 2]
    }
    a_corpus_lc[[i]] <- text_i
}

#get lengths of texts
text_lengths <- data.frame(
    text_name = character(),
    length = integer(),
    stringsAsFactors = FALSE
)

for (i in seq_along(a_corpus_c)) {
    text_name <- names(a_corpus_c)[i]
    text_length <- length(a_corpus_c[[i]])
    text_lengths[i, ] <- c(text_name, text_length)
}

#plots

#add text names as row names, then remove text_name column
rownames(text_lengths) <- text_lengths$text_name
text_lengths$text_name <- NULL

palette <- "gray"

ggplot(
    text_lengths,
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
    filename = file.path(odir, "text_length_boxplot.png"),
    device = "png",
    dpi = 700,
    width = 5,
    height = 5
)
