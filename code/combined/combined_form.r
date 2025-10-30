#clear variables and directories from memory
rm(list = ls())

#directories
rdir <- "rdat/combined"
script_name <- "form"
dir.create("outputs/combined", showWarnings = FALSE)
wdir <- file.path("outputs/combined", script_name)
dir.create(wdir, showWarnings = FALSE)

j <- read.csv("outputs/j/form/text_lengths_j_corpus.csv")[,1:2]
a <- read.csv("outputs/a/form/text_lengths_a_corpus.csv")[,1:2]
f <- read.csv("outputs/f/form/text_lengths_f_corpus.csv")[,1:2]
r <- read.csv("outputs/r/form/text_lengths_r_corpus.csv")[,1:2]

#add extra column with corpus name
j$corpus <- "j"
a$corpus <- "a"
f$corpus <- "f"
r$corpus <- "r"


#combine text lengths
text_lengths <- rbind(j, a, f, r)

text_lengths$corpus <- factor(text_lengths$corpus, levels = c("j", "a", "f", "r"))
text_lengths$length <- as.numeric(text_lengths$length)

#plot
palette <- c(
    "#a4a4a4",
    "#999999",
    "#777777",
    "#555555"
)

ggplot(
    text_lengths,
    aes(
        x = factor(corpus, levels = c("j", "a", "f", "r")),
        y = length,
        fill = factor(corpus, levels = c("j", "a", "f", "r"))
    )
) +
geom_boxplot(
    fill = palette,
    outlier.size = 3,
    outlier.fill = palette[2]
) +
scale_fill_manual(values = palette) +
scale_y_continuous(
    breaks = seq(0, 3000, 500),
    minor_breaks = seq(0, 3000, 100),
) +
scale_x_discrete(labels = c("J-corpus", "A-corpus", "F-corpus", "R-corpus")) +
xlab("Corpus") +
ylab("Text Length (lines)") +
theme_minimal() +
theme(
    axis.text = element_text(size = 15, face = "italic"),
    axis.title = element_text(size = 15),
)

ggsave(
    filename = file.path(wdir, "text_length_boxplot.png"),
    device = 'png',
    dpi = 700,
    width = 10,
    height = 5
)


#some stats
median(j$length)
min(j$length)
q1_j <- quantile(j$length, probs = 0.25)
q3_j <- quantile(j$length, probs = 0.75)
iqr_j <- as.numeric(q3_j - q1_j)

a$length <- as.numeric(a$length)
median(a$length)
quantile(a$length, probs = 0.75) - quantile(a$length, probs = 0.25)

f$length <- as.numeric(f$length)
median(f$length)
quantile(f$length, probs = 0.75) - quantile(f$length, probs = 0.25)

r$length <- as.numeric(r$length)
median(r$length)
quantile(r$length, probs = 0.75) - quantile(r$length, probs = 0.25)

sort(r, decreasing = TRUE, by = "length")

a[which(a$length >= (q1_j - 1.5 * iqr_j) & a$length <= (q3_j + 1.5 * iqr_j) & a$length > min(j$length)),]





p1a <- data.frame(
    text_name = c("Guillaume", "Bible","Vision",  "Vie du Monde", "Mais", "Robert", "Menage"),
    prologue = c(36, 4, 8, 0, 4, 15, 2),
    epilogue = c(1, 4, 5, 4, 4, 1, 1),
    length = c(948, 316, 569, 176, 315, 1016, 327)
)

p1a


cor.test(p1a$length, p1a$prologue)
cor.test(p1a$length, p1a$epilogue)
cor.test(p1a$prologue, p1a$epilogue)

sd(p1a$length) / mean(p1a$length)
sd(p1a$prologue) / mean(p1a$prologue)
sd(p1a$epilogue) / mean(p1a$epilogue)
