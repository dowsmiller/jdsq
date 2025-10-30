#import manual data
counts_q_gr <- data.frame(
    type_a = c(13, 11, 13, 21, 20),
    type_b = c(9, 3, 23, 3, 9),
    type_c = c(50, 44, 17, 82, 68),
    type_d = c(0, 1, 1, 0, 2)
)

rownames(counts_q_gr) <- c("Florence", "Anelés", "Beuf", "Guillaume", "Robert")


#add columns "total", "obs_rate_a", "obs_rate_b", "obs_rate_c", "obs_rate_d"
counts_q_gr[, "total"] <- rowSums(counts_q_gr)
counts_q_gr[, "obs_rate_a"] <- counts_q_gr[, "type_a"] / counts_q_gr[, "total"]
counts_q_gr[, "obs_rate_b"] <- counts_q_gr[, "type_b"] / counts_q_gr[, "total"]
counts_q_gr[, "obs_rate_c"] <- counts_q_gr[, "type_c"] / counts_q_gr[, "total"]
counts_q_gr[, "obs_rate_d"] <- counts_q_gr[, "type_d"] / counts_q_gr[, "total"]

quote_types <- c("type_a", "type_b", "type_c", "type_d")


#plots
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)

#stacked column of rates
text_names <- rownames(counts_q_gr)

quote_type_names <- c(
    "Type A",
    "Type B",
    "Type C",
    "Type D"
)

df <- cbind(text = text_names, counts_q_gr[, 6:9])
rownames(df) <- NULL

df_pivot <- pivot_longer(df, cols = 2:5, names_to = "variable", values_to = "value")
df_pivot$variable <- factor(df_pivot$variable, levels = c("obs_rate_a", "obs_rate_b", "obs_rate_c", "obs_rate_d"), labels = quote_type_names)

palette <- c("#003f5c", "#7a5195", "#ef5675", "#ffa600")

ggplot(
    df_pivot,
    aes(
        y = factor(text, levels = rev(text_names)),
        x = value,
        fill = forcats::fct_rev(variable)
    )
) +
geom_bar(stat = "identity", position = "fill") +
scale_fill_manual(values = rev(palette)) +
scale_x_continuous(labels = scales::percent) +
xlab("Rate of Use") +
ylab("") +
guides(
    fill = guide_legend(
        reverse = TRUE,
        label = TRUE,
        title = "Onset of Discourse"
    ),
) +
theme_minimal() +
theme(
    axis.text = element_text(size = 15, face = "italic"),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
)

ggsave(filename = file.path("my_data/combined_corpora/outputs/q_types_rates.png"), device="png", dpi=700, width = 10, height = 3, units = "in")
