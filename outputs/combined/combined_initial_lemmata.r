data <- read.csv("my_data/combined_corpora/combined_initial_lemma_counts.csv")

# set col1 to rownames and delete col1
rownames(data) <- data[,1]
data <- data[,-1]

corpora <- c("f", "l", "r", "gda", "rob")

for (i in 1:length(corpora)) {
    for (j in 1:nrow(data)) {
        data[j, paste0("p_", corpora[i])] <- p_value_rand(
            observed_tar = data[j, "observed_tar"],
            observed_ref = data[j, paste0("observed_", corpora[i])],
            total_tar = data[j, "total_tar"],
            total_ref = data[j, paste0("total_", corpora[i])]
        )
    }
}

data
