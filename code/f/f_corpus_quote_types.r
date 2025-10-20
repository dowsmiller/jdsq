#clear variables and directories from memory
rm(list = ls())

#libraries
library(stringr)
library(writexl)

#directories
rdir <- "my_data/f_corpus"
script_name <- "quote_types"
wdir <- file.path("my_data/f_corpus/outputs", script_name)
dir.create(wdir, showWarnings = FALSE)

#load data
f_corpus_c <- readRDS(file.path(rdir, "f_corpus_c.rds"))
f_corpus_s <- readRDS(file.path(rdir, "f_corpus_s.rds"))
f_corpus_w <- readRDS(file.path(rdir, "f_corpus_w.rds"))
f_corpus_lc <- readRDS(file.path(rdir, "f_corpus_lc.rds"))
f_corpus_ls <- readRDS(file.path(rdir, "f_corpus_ls.rds"))
f_corpus_lw <- readRDS(file.path(rdir, "f_corpus_lw.rds"))

#extract lines containing “ or ” with line number
list_q <- list()

for (i in 1:length(f_corpus_c)) {
    q_n <- 1
    text_name <- names(f_corpus_c[i])
    list_q[[text_name]] <- data.frame(matrix(ncol = 2, nrow = 0))
    for (j in 1:length(f_corpus_c[[text_name]])) {
        line_chars <- f_corpus_c[[text_name]][j]
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
            prev_line <- f_corpus_c[[text_name]][line_n - 1]
            next_line <- f_corpus_c[[text_name]][line_n + 1]

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
            prev_line <- f_corpus_c[[text_name]][line_n - 1]
            next_line <- f_corpus_c[[text_name]][line_n + 1]
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
            prev_line <- f_corpus_c[[text_name]][line_n - 1]
            next_line <- f_corpus_c[[text_name]][line_n + 1]
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
            prev_line <- f_corpus_c[[text_name]][line_n - 1]
            next_line <- f_corpus_c[[text_name]][line_n + 1]
            
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
            prev_line <- f_corpus_c[[text_name]][line_n - 1]
            next_line <- f_corpus_c[[text_name]][line_n + 1]
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
            prev_line <- f_corpus_c[[text_name]][line_n - 1]
            next_line <- f_corpus_c[[text_name]][line_n + 1]
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
            prev_line <- f_corpus_c[[text_name]][line_n - 1]
            next_line <- f_corpus_c[[text_name]][line_n + 1]

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
            prev_line <- f_corpus_c[[text_name]][line_n - 1]
            next_line <- f_corpus_c[[text_name]][line_n + 1]
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
    file_path <- file.path(wdir, file_name_ext)
    write_xlsx(list, path = file_path)
}


#Not yet complete