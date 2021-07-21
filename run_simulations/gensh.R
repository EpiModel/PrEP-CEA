library(tidyr)

strat_grid <- expand.grid(INPUT4 = c(TRUE, FALSE), INPUT5 = c(TRUE, FALSE), INPUT6 = c(TRUE, FALSE))
input_params <- data.frame(PARID = c(1, 2, 3),
                           INPUT1 = c(.01, .02, .03),
                           INPUT2 = c(0.15, 0.3, 0.6),
                           INPUT3 = c(0.2, 0.4, 0.8))
inputs <- tidyr::crossing(input_params, strat_grid)
names(inputs) <- c("PARID", paste0("INPUT", c(1:(ncol(inputs) - 1))))

# simno <- 1000:1007
simno <- 1008:1015
# simno <- 1016:1023

sbatch_master(vars = inputs[simno - 999, ],
              expand.vars = FALSE,
              master.file = "hyaksim_bc/master.sh",
              runsim.file = "hyaksim_bc/runsim.sh",
              simno.start = min(simno),
              append = FALSE,
              ckpt = TRUE,
              nsims = 10080,
              ncores = 28,
              narray = 120,
              walltime = "00:58:00",
              mem = "100G")
