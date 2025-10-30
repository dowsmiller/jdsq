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

#exclude fragmentary texts (rows 16, 22, 25)
text_lengths <- text_lengths[-c(16, 22, 25),]

#add text names as row names, then remove text_name column
rownames(text_lengths) <- text_lengths$text_name
text_lengths$text_name <- NULL

#plots
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


# prologues and epilogues
text_lengths$length <- as.numeric(text_lengths$length)
text_lengths$prologue <- c(4,8,28,35,5,9,168,8,26,8,32,4,0,57,4,36,19,16,12,0,20,0,4)
text_lengths$epilogue <- c(6,4,17,2,1,1,8,4,25,20,9,8,3,6,0,1,1,3,4,4,16,4,25)

#save as csv
write.csv(
    text_lengths,
    file = file.path(odir, "text_lengths_a_corpus.csv"),
    row.names = TRUE
)

# some stats
cor.test(text_lengths$length, text_lengths$prologue)
cor.test(text_lengths$length, text_lengths$epilogue)
cor.test(text_lengths$prologue, text_lengths$epilogue)

sd(text_lengths$prologue)
sd(text_lengths$prologue) / mean(text_lengths$prologue)

sd(text_lengths$epilogue)
sd(text_lengths$epilogue) / mean(text_lengths$epilogue)

sd(text_lengths$length)
sd(text_lengths$length) / mean(text_lengths$length)



i <- 25
needle <- "diz"
data.frame(
    n = which(str_detect(a_corpus_c[[i]], needle)),
    line = a_corpus_c[[i]][which(str_detect(a_corpus_c[[i]], needle))]
)
