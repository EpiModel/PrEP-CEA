### This file is used to aggregate hyak runs into single dataset

library(stringr)
library(dplyr)


file_arrange <- function(mypath) {
  fileList <- list.files(paste0(mypath, "/"))
  fileList <- fileList[grep("sim.n", fileList)]

  tmp_table <- str_match(fileList, "^[^0-9.]*([0-9.]*)[^0-9.]*([0-9.]*)[^0-9.]*$")[, 3]
  tmp_table <- do.call(rbind, lapply(tmp_table, function(x) { strsplit(x, "\\.")[[1]] }))
  tmp_table <- data.frame(tmp_table)
  colnames(tmp_table) <- c("lv1_id", "lv2_id")
  tmp_table <- tmp_table %>%
    mutate(lv1_id = as.numeric(as.character(lv1_id)),
           lv2_id = as.numeric(as.character(lv2_id))) %>%
    arrange(lv1_id, lv2_id)

  ix_lv1 <- unique(tmp_table[, 1])

  ix_ls <- lapply(ix_lv1, function(i) {
    tmp <- tmp_table %>% filter(lv1_id == i) %>% select(lv2_id)
    tmp$lv2_id
  })


  subtr_fact <- min(ix_lv1) - 1
  batch_no <- min(ix_lv1) / 1000

  for (i in c(min(ix_lv1):max(ix_lv1))) {
    print(i)
    run_ix <- i - subtr_fact
    ix_lv2 <- ix_ls[[run_ix]]
    simlist <- lapply(ix_lv2,
                      function(x) {
                        tmp_data <- get(load(paste0(mypath, "/sim.n", i, ".", x, ".rda")))
                        inputs <- tmp_data$inputs
                        epi <- tmp_data$epi
                        return(list(inputs = inputs, epi = epi))
                      })

    # Check input parameters
    check_inputs <- do.call(rbind, lapply(simlist, `[[`, 1))

    if (!all(apply(check_inputs, 2, max) == apply(check_inputs, 2, min))) {
      stop("inputs parameters are not the same")
    }

    # Bind all epi data.frames
    epi <- do.call(rbind, lapply(simlist, `[[`, 2))
    epi$simno <- c(1:nrow(epi))

    res <- list(inputs = check_inputs[1, ],
                epi = epi)
    saveRDS(res, paste0(mypath, "_cleaned/simulation_output_par", run_ix, ".rds"))
  }
}


# Run funciton
file_arrange(mypath = "simdata_basecase")

mypath <- "simdata_basecase_cleaned"
fileList <- list.files(paste0(mypath, "/"))
df <- data.frame()
for (i in 1:length(fileList)) {
  set <- readRDS(paste0("simdata_basecase_cleaned/", fileList[i]))
  epi <- set[[2]] %>%
    select(-simno) %>%
    summarize_all(.funs = list(mean = mean, sd = sd))
  inputs <- set[[1]]
  row <- cbind(inputs, epi)
  df <- rbind(df, row)
  if (i %% 100 == 0) {
    print(i)
  }
}

df <- df %>% arrange(par.id) %>% filter(par.id %in% which(table(df$par.id) == 8))
saveRDS(df, "basecase_output_cleaned.rds")
