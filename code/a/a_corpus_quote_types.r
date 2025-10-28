#clear variables and directories from memory
rm(list = ls())

#libraries
library(stringr)
library(writexl)

#directories
rdir <- "wdat/a"
script_name <- "quote_types"
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

#extract lines containing “ or ” with line number
list_q <- list()

for (i in 1:length(a_corpus_c)) {
    q_n <- 1
    text_name <- names(a_corpus_c[i])
    list_q[[text_name]] <- data.frame(matrix(ncol = 2, nrow = 0))
    for (j in 1:length(a_corpus_c[[text_name]])) {
        line_chars <- a_corpus_c[[text_name]][j]
        if (str_detect(line_chars, "[“”]") == TRUE) {
            list_q[[text_name]][q_n, 1] <- j
            list_q[[text_name]][q_n, 2] <- line_chars
            q_n <- q_n + 1
        }
    }
}

#regex search strings
{
    #TYPE A: begins with “ and ends possible ”
    needle_a_1 <- "^“[^“”]+[”]*$"
    needle_a_2 <- "^[“]*[^“”]+”[^“”]{2,}[“]*[^“”]+[”]*$"
    needle_a_3 <- "”$"

    #TYPE B: begins possible “, contains one ” and alpha, possible “, possible ”
    needle_b <- "^[“]*[^“”]+”[^“”]{2,}[“]*[^“”]+[”]*$"

    #TYPE C: begins with at least one alphanumeric, then “ and ends possible ”
    needle_c <- "^[^“”]+“[^“”]+[”]*$"

    #POSSIBLE A: if ends with ” and no other “”, affffix next line (if exists)
    needle_pa_1 <- "^[^“”]+”$"
    needle_pa_2 <- "^“"

    #POSSIBLE B: contains ” and no other “”
    needle_pb <- "^[^“”]+”[^“”]+$"

    #POSSIBLE C: begins possible “ then ” then space then “ then possible ”
    needle_pc <- "^[“]*[^“”]+”[ ]+“[^“”]+[”]*$"

    needle_pd <- "”$"
}

#TYPE A: begins with “ and ends possible ”, and prev line isn't type B or ends with ”
list_q_a <- list()

for (i in 1:length(list_q)) {
    if(nrow(list_q[[i]]) == 0) {
        next
    } else {
        q_n <- 1
        text_name <- names(list_q[i])
        list_q_a[[text_name]] <- data.frame(matrix(ncol = 4, nrow = 0))
        names(list_q_a[[text_name]]) <- c("line_n", "prev_line", "line", "next_line")
        for (j in 1:nrow(list_q[[text_name]])) {
            line_n <- list_q[[text_name]][j, 1]
            line_chars <- list_q[[text_name]][j, 2]
            prev_line <- a_corpus_c[[text_name]][line_n - 1]
            next_line <- a_corpus_c[[text_name]][line_n + 1]

            test_1 <- str_detect(line_chars, needle_a_1)
            test_2 <- str_detect(prev_line, needle_a_2)
            test_3 <- str_detect(prev_line, needle_a_3)
            if (test_1 == TRUE && test_2 == FALSE && test_3 == FALSE) {
                list_q_a[[text_name]][q_n, 1] <- line_n
                list_q_a[[text_name]][q_n, 2] <- prev_line
                list_q_a[[text_name]][q_n, 3] <- line_chars
                list_q_a[[text_name]][q_n, 4] <- next_line
                q_n <- q_n + 1
            }
        }
    }
}

#TYPE B: begins possible “, contains one ” and alpha, possible “, possible ”
list_q_b <- list()

for (i in 1:length(list_q)) {
    if(nrow(list_q[[i]]) == 0) {
        next
    } else {
        q_n <- 1
        text_name <- names(list_q[i])
        list_q_b[[text_name]] <- data.frame(matrix(ncol = 4, nrow = 0))
        names(list_q_b[[text_name]]) <- c("line_n", "prev_line", "line", "next_line")
        for (j in 1:nrow(list_q[[text_name]])) {
            line_n <- list_q[[text_name]][j, 1]
            line_chars <- list_q[[text_name]][j, 2]
            prev_line <- a_corpus_c[[text_name]][line_n - 1]
            next_line <- a_corpus_c[[text_name]][line_n + 1]
            if (str_detect(line_chars, needle_b) == TRUE) {
                list_q_b[[text_name]][q_n, 1] <- line_n
                list_q_b[[text_name]][q_n, 2] <- prev_line
                list_q_b[[text_name]][q_n, 3] <- line_chars
                list_q_b[[text_name]][q_n, 4] <- next_line
                q_n <- q_n + 1
            }
        }
    }
}

#TYPE C: begins with at least one alphanumeric, then “ and ends possible ”
list_q_c <- list()

for (i in 1:length(list_q)) {
    if(nrow(list_q[[i]]) == 0) {
        next
    } else {
        q_n <- 1
        text_name <- names(list_q[i])
        list_q_c[[text_name]] <- data.frame(matrix(ncol = 4, nrow = 0))
        names(list_q_c[[text_name]]) <- c("line_n", "prev_line", "line", "next_line")
        for (j in 1:nrow(list_q[[text_name]])) {
            line_n <- list_q[[text_name]][j, 1]
            line_chars <- list_q[[text_name]][j, 2]
            prev_line <- a_corpus_c[[text_name]][line_n - 1]
            next_line <- a_corpus_c[[text_name]][line_n + 1]
            if (str_detect(line_chars, needle_c) == TRUE) {
                list_q_c[[text_name]][q_n, 1] <- line_n
                list_q_c[[text_name]][q_n, 2] <- prev_line
                list_q_c[[text_name]][q_n, 3] <- line_chars
                list_q_c[[text_name]][q_n, 4] <- next_line
                q_n <- q_n + 1
            }
        }
    }
}

#POSSIBLE A: ends with ” and no other “”, next line doesn't start with “
#speech ends with text in new line – speaker given in new line (unlikely)
#most likely just end of quote
#possibly variant of TYPE B
list_q_pa <- list()

for (i in 1:length(list_q)) {
    if(nrow(list_q[[i]]) == 0) {
        next
    } else {
        q_n <- 1
        text_name <- names(list_q[i])
        list_q_pa[[text_name]] <- data.frame(matrix(ncol = 4, nrow = 0))
        names(list_q_pa[[text_name]]) <- c("line_n", "prev_line", "line", "next_line")
        for (j in 1:nrow(list_q[[text_name]])) {
            line_n <- list_q[[text_name]][j, 1]
            line_chars <- list_q[[text_name]][j, 2]
            prev_line <- a_corpus_c[[text_name]][line_n - 1]
            next_line <- a_corpus_c[[text_name]][line_n + 1]
            
            test_1 <- str_detect(line_chars, needle_pa_1)
            if (is.na(next_line) == TRUE) {
                test_2 <- FALSE
            } else {
                test_2 <- str_detect(next_line, needle_pa_2)
            }
            if (test_1 == TRUE && test_2 == FALSE) {
                list_q_pa[[text_name]][q_n, 1] <- line_n
                list_q_pa[[text_name]][q_n, 2] <- prev_line
                list_q_pa[[text_name]][q_n, 3] <- line_chars
                list_q_pa[[text_name]][q_n, 4] <- next_line
                q_n <- q_n + 1
            }
        }
    }
}

#POSSIBLE B: if contains ” and no other “”
#speech followed by non-speech in same line
#possibly just end of quote
#possibly variant of TYPE A (do line numbers match to be prev line?)
#possibly variant of TYPE B
list_q_pb <- list()

for (i in 1:length(list_q)) {
    if(nrow(list_q[[i]]) == 0) {
        next
    } else {
        q_n <- 1
        text_name <- names(list_q[i])
        list_q_pb[[text_name]] <- data.frame(matrix(ncol = 4, nrow = 0))
        names(list_q_pb[[text_name]]) <- c("line_n", "prev_line", "line", "next_line")
        for (j in 1:nrow(list_q[[text_name]])) {
            line_n <- list_q[[text_name]][j, 1]
            line_chars <- list_q[[text_name]][j, 2]
            prev_line <- a_corpus_c[[text_name]][line_n - 1]
            next_line <- a_corpus_c[[text_name]][line_n + 1]
            if (str_detect(line_chars, needle_pb) == TRUE) {
                list_q_pb[[text_name]][q_n, 1] <- line_n
                list_q_pb[[text_name]][q_n, 2] <- prev_line
                list_q_pb[[text_name]][q_n, 3] <- line_chars
                list_q_pb[[text_name]][q_n, 4] <- next_line
                q_n <- q_n + 1
            }
        }
    }
}

#POSSIBLE C: begins possible “ then ” then space then “ then possible ”
#speech in new hemistich with no indication of speaker
list_q_pc <- list()

for (i in 1:length(list_q)) {
    if(nrow(list_q[[i]]) == 0) {
        next
    } else {
        q_n <- 1
        text_name <- names(list_q[i])
        list_q_pc[[text_name]] <- data.frame(matrix(ncol = 4, nrow = 0))
        names(list_q_pc[[text_name]]) <- c("line_n", "prev_line", "line", "next_line")
        for (j in 1:nrow(list_q[[text_name]])) {
            line_n <- list_q[[text_name]][j, 1]
            line_chars <- list_q[[text_name]][j, 2]
            prev_line <- a_corpus_c[[text_name]][line_n - 1]
            next_line <- a_corpus_c[[text_name]][line_n + 1]
            if (str_detect(line_chars, needle_pc) == TRUE) {
                list_q_pc[[text_name]][q_n, 1] <- line_n
                list_q_pc[[text_name]][q_n, 2] <- prev_line
                list_q_pc[[text_name]][q_n, 3] <- line_chars
                list_q_pc[[text_name]][q_n, 4] <- next_line
                q_n <- q_n + 1
            }
        }
    }
}

#POSSIBLE D: begins with “ and ends possible ”, and prev line ends with ”
#speech on new line with no indication of speaker
list_q_pd <- list()

for (i in 1:length(list_q)) {
    if(nrow(list_q[[i]]) == 0) {
        next
    } else {
        q_n <- 1
        text_name <- names(list_q[i])
        list_q_pd[[text_name]] <- data.frame(matrix(ncol = 4, nrow = 0))
        names(list_q_pd[[text_name]]) <- c("line_n", "prev_line", "line", "next_line")
        for (j in 1:nrow(list_q[[text_name]])) {
            line_n <- list_q[[text_name]][j, 1]
            line_chars <- list_q[[text_name]][j, 2]
            prev_line <- a_corpus_c[[text_name]][line_n - 1]
            next_line <- a_corpus_c[[text_name]][line_n + 1]

            test_1 <- str_detect(line_chars, needle_a_1)
            test_2 <- str_detect(prev_line, needle_pd)
            if (test_1 == TRUE && test_2 == TRUE) {
                list_q_pd[[text_name]][q_n, 1] <- line_n
                list_q_pd[[text_name]][q_n, 2] <- prev_line
                list_q_pd[[text_name]][q_n, 3] <- line_chars
                list_q_pd[[text_name]][q_n, 4] <- next_line
                q_n <- q_n + 1
            }
        }
    }
}

#POSSIBLE E: is none of the above
#should be empty
list_q_pe <- list()

for (i in 1:length(list_q)) {
    if(nrow(list_q[[i]]) == 0) {
        next
    } else {
        q_n <- 1
        text_name <- names(list_q[i])
        list_q_pe[[text_name]] <- data.frame(matrix(ncol = 4, nrow = 0))
        names(list_q_pa[[text_name]]) <- c("line_n", "prev_line", "line", "next_line")
        for (j in 1:nrow(list_q[[text_name]])) {
            line_n <- list_q[[text_name]][j, 1]
            line_chars <- list_q[[text_name]][j, 2]
            prev_line <- a_corpus_c[[text_name]][line_n - 1]
            next_line <- a_corpus_c[[text_name]][line_n + 1]
            if (str_detect(line_chars, needle_a_1) == TRUE) {
            } else if (str_detect(line_chars, needle_b) == TRUE) {
            } else if (str_detect(line_chars, needle_c) == TRUE) {
            } else if (str_detect(line_chars, needle_pa_1) == TRUE) {
            } else if (str_detect(line_chars, needle_pb) == TRUE) {
            } else if (str_detect(line_chars, needle_pc) == TRUE) {
            } else {
                list_q_pe[[text_name]][q_n, 1] <- line_n
                list_q_pe[[text_name]][q_n, 2] <- prev_line
                list_q_pe[[text_name]][q_n, 3] <- line_chars
                list_q_pe[[text_name]][q_n, 4] <- next_line
                q_n <- q_n + 1
            }
        }
    }
}

#list of lists of dfs
lists_q <- list(
    list_q_a = list_q_a,
    list_q_b = list_q_b,
    list_q_c = list_q_c,
    list_q_pa = list_q_pa,
    list_q_pb = list_q_pb,
    list_q_pc = list_q_pc,
    list_q_pd = list_q_pd
)

#write excel with file per type, sheet per text
for (i in 1:length(lists_q)) {
    list <- lists_q[[i]]
    file_name <- names(lists_q)[i]
    file_name_ext <- paste0(file_name, ".xlsx")
    file_path <- file.path(odir, file_name_ext)
    write_xlsx(list, path = file_path)
}


#import manual data
counts_q <- read.csv("outputs/a/quote_types/manually_edited/a-corpus_quote_types.csv", row.names = 1)

#set NA to zero
counts_q[is.na(counts_q)] <- 0

#combine "B" and "B1" columns
counts_q[, "B"] <- counts_q[, "B", ] + counts_q[, "B1"]
counts_q <- counts_q[, -c(2)]

#rename columns "A", "B"", "C", "D" to "type_a", "type_b", "type_c", "type_d"
colnames(counts_q) <- c("type_a", "type_b", "type_c", "type_d")

#add columns "total", "obs_rate_a", "obs_rate_b", "obs_rate_c", "obs_rate_d", "p_value_a", "p_value_b", "p_value_c", "p_value_d"
counts_q[, "total"] <- rowSums(counts_q)
counts_q[, "obs_rate_a"] <- counts_q[, "type_a"] / counts_q[, "total"]
counts_q[, "obs_rate_b"] <- counts_q[, "type_b"] / counts_q[, "total"]
counts_q[, "obs_rate_c"] <- counts_q[, "type_c"] / counts_q[, "total"]
counts_q[, "obs_rate_d"] <- counts_q[, "type_d"] / counts_q[, "total"]
counts_q[, "p_value_type_a"] <- NA
counts_q[, "p_value_type_b"] <- NA
counts_q[, "p_value_type_c"] <- NA
counts_q[, "p_value_type_d"] <- NA

quote_types <- c("type_a", "type_b", "type_c", "type_d")

#total counts and expected rates
totals_q <- data.frame(matrix(ncol = 1, nrow = 4))
colnames(totals_q) <- c(
    "total_counts"
)
rownames(totals_q) <- c(
    "type_a",
    "type_b",
    "type_c",
    "type_d"
)

all_q <- sum(counts_q[, "total"])

for (i in 1:length(quote_types)) {
    total_q <- sum(counts_q[, i])
    totals_q[i, ] <- total_q
}


#randomised p_value tests for target vs reference corpus
p_value_rand <- function(
    observed_tar = integer(),
    observed_ref = integer(),
    total_tar = integer(),
    total_ref = integer(),
    sample_size = 100000) {

    sum_observed <- observed_tar + observed_ref #sum of observed instances
    sum_total <- total_tar + total_ref #sum of total instances

    #null rate: expected rate if there is no difference between a and b
    null_rate <- sum_observed / sum_total

    #observed difference between probabilities
    observed_prob_dif <- (observed_ref / total_ref) - (observed_tar / total_tar)

    #generate random samples, counting observed instances
    ref_rand <- rbinom(sample_size, total_ref, null_rate)
    tar_rand <- rbinom(sample_size, total_tar, null_rate)

    #work out differences in probability between two samples
    rand_prob_difs <- (ref_rand / total_ref) - (tar_rand / total_tar)

    #make differences in probability absolute
    abs_rand_prob_difs <- abs(rand_prob_difs)
    abs_observed_prob_dif <- abs(observed_prob_dif)

    #calculate the p value (the proportion of the random sample for which
        #the difference in probability exceeds the observed probability)
    p_value <- mean(abs_rand_prob_difs >= abs_observed_prob_dif)

    p_value
}

#p-values
for (i in 1:nrow(counts_q)) {
    for (j in 1:length(quote_types)) {
        observed_tar <- counts_q[i, quote_types[j]]
        total_tar <- counts_q[i, "total"]
        observed_ref <- totals_q[quote_types[j], ] - observed_tar
        total_ref <- all_q - total_tar
        p_value <- p_value_rand(
            observed_tar = observed_tar,
            observed_ref = observed_ref,
            total_tar = total_tar,
            total_ref = total_ref
        )
        counts_q[i, paste0("p_value_", quote_types[j])] <- p_value
    }
}

write.csv(counts_q, file.path(odir, "counts_q.csv"))


#plots
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggpattern)

#stacked column of rates
text_names <- rownames(counts_q)

quote_type_names <- c(
    "Type A",
    "Type B",
    "Type C",
    "Type D"
)

df <- cbind(text = text_names, counts_q[, 6:9])
rownames(df) <- NULL

#exclude texts with fewer than 3 quotes
df <- df[counts_q$total >= 3, ]

df_pivot <- pivot_longer(df, cols = 2:5, names_to = "variable", values_to = "value")
df_pivot$variable <- factor(df_pivot$variable, levels = c("obs_rate_a", "obs_rate_b", "obs_rate_c", "obs_rate_d"), labels = quote_type_names)

pattern_vals <- c("stripe", "circle", "crosshatch", "pch")

ggplot(df_pivot, aes(
    y = factor(text, levels = rev(text_names)),
    x = value,
    pattern = forcats::fct_rev(variable)
)) +
  geom_bar_pattern(
    stat = "identity",
    position = "fill",
    fill = "grey95",
    colour = "grey30",
    size = 0.2,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_angle = 45,
    pattern_density = 0.3,
    pattern_spacing = 0.03,
    pattern_key_scale_factor = 1
  ) +
  scale_pattern_manual(values = rev(pattern_vals)) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Rate of Use", y = NULL, pattern = "Onset of Discourse") +
  guides(pattern = guide_legend(
    reverse = TRUE,
    title.position = "top",
    title.hjust = 0.5,
    ncol = 1,
    byrow = TRUE,
    direction = "vertical",
    label.vjust = 0.5,
    override.aes = list(
      pattern_key_scale_factor = 1.2,
      height = unit(1.5, "lines")
    )
  )) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
    axis.text.y = element_text(face = "italic", size = 14),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    legend.text = element_text(size = 13),
    legend.box.just = "top",
    legend.spacing.y = unit(0.8, "lines"),
    plot.margin = margin(10, 20, 10, 20)
  )

ggsave(filename = file.path(odir, "q_types_rates.png"), device="png", dpi=700, width = 10, height = 7.5, units = "in")
