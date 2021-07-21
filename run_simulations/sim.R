## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))
library("compiler")
library("data.table")


reallocate_prop <- function(in.pcp = c(0.165, 0.235, 0.60), shift_proportion) {

  out.pcp <- rep(NA, 3)
  out.pcp[1:2] <- in.pcp[1:2] * (1 - shift_proportion)
  out.pcp[3] <- 1 - sum(out.pcp[1:2])

  return(out.pcp)
}

cdf <- function(p, k) {
  -((k*log(2))/log(1-p))
}

## Environmental Arguments
pull_env_vars(num.vars = c("PARID", "INPUT1",
              logic.vars = paste0("INPUT", c(2:4))))

inputs <- list(PARID, INPUT1, INPUT2, INPUT3, INPUT4)

names(inputs) <- c("PARID",
                   "prep.cost.ann", "prep.optim.init", "prep.optim.adhr",
                   "prep.optim.retn")

print(ls())
print(fsimno)
print(simno)
print(nsims)

## Parameters
est <- readRDS("est/netest.rda")
netstats <- readRDS("est/netstats.rda")
epistats <- readRDS("est/epistats.rda")
burnin <- readRDS("est/burnin.ATL.3race.FSonly.Prep15.rda")
asmr_new <- readRDS("est/asmr_new.rda")
netstats$demog$asmr <- asmr_new

netstats$main$nodefactor_active <- 0
netstats$casl$nodefactor_active <- 0
netstats$inst$nodefactor_active <- 0



burnin$nwparam[[1]]$formation <- ~ edges + nodematch("age.grp", diff = TRUE) + nodefactor("age.grp",
                                                                                          base = 1) + nodematch("race", diff = FALSE) + nodefactor("race",
                                                                                                                                                   base = 1) + nodefactor("deg.casl", base = 1) + concurrent +
  degrange(from = 3) + nodematch("role.class", diff = TRUE, keep = 1:2) +
  nodefactor("active", base = 2)
burnin$nwparam[[1]]$target.stats[21] <- 0
burnin$nwparam[[1]]$target.stats.names[21] <- "nodefactor.active.0"
burnin$nwparam[[1]]$coef.form[21] <- -Inf
names(burnin$nwparam[[1]]$coef.form.crude[21]) <- "nodefactor.active.0"
burnin$nwparam[[1]]$coef.form.crude[21] <- -Inf
names(burnin$nwparam[[1]]$coef.form.crude[21]) <- "nodefactor.active.0"

burnin$nwparam[[2]]$formation <- ~edges + nodematch("age.grp", diff = TRUE) + nodefactor("age.grp",
                                                                                         base = c(1, 5)) + nodematch("race", diff = FALSE) +
  nodefactor("race", base = 1) + nodefactor("deg.main",
                                            base = 3) + concurrent + degrange(from = 4) + nodematch("role.class", diff = TRUE, keep = 1:2) +
  nodefactor("active", base = 2)
burnin$nwparam[[2]]$target.stats[19] <- 0
burnin$nwparam[[2]]$target.stats.names[19] <- "nodefactor.active.0"
burnin$nwparam[[2]]$coef.form[19] <- -Inf
names(burnin$nwparam[[2]]$coef.form.crude[19]) <- "nodefactor.active.0"
burnin$nwparam[[2]]$coef.form.crude[19] <- -Inf
names(burnin$nwparam[[2]]$coef.form.crude[19]) <- "nodefactor.active.0"


burnin$nwparam[[3]]$formation <- ~edges + nodematch("age.grp", diff = FALSE) + nodefactor("age.grp", base = 1) +
  nodematch("race", diff = FALSE) + nodefactor("race", base = 1) + nodefactor("risk.grp", base = 5) +
  nodefactor("deg.tot", base = 1) + nodematch("role.class", diff = TRUE, keep = 1:2) +
  nodefactor("active", base = 2)
burnin$nwparam[[3]]$target.stats[19] <- 0
burnin$nwparam[[3]]$target.stats.names[19] <- "nodefactor.active.0"
burnin$nwparam[[3]]$coef.form[19] <- -Inf
names(burnin$nwparam[[3]]$coef.form[19]) <- "nodefactor.active.0"
burnin$nwparam[[3]]$coef.form.crude[19] <- -Inf
names(burnin$nwparam[[3]]$coef.form.crude[19]) <- "nodefactor.active.0"


burnin$param$netstats$main$nodefactor_active <- 0
burnin$param$netstats$casl$nodefactor_active <- 0
burnin$param$netstats$inst$nodefactor_active <- 0

burnin$p[[1]][[1]]$model.form$coef.names[21] <- "nodefactor.active.0"
burnin$p[[1]][[1]]$model.form$offset[10] <- FALSE
burnin$p[[1]][[1]]$model.form$terms[[10]] <- list(name = "nodefactor",
                                                  coef.names = "nodefactor.active.0",
                                                  inputs = burnin$p[[1]][[1]]$model.form$terms[[9]]$inputs,
                                                  dependence = FALSE,
                                                  minval = 0,
                                                  pkgname = "ergm")
burnin$p[[1]][[1]]$model.form$etamap$canonical[21] <- 21
burnin$p[[1]][[1]]$model.form$etamap$offsetmap[21] <- FALSE
burnin$p[[1]][[1]]$model.form$etamap$offset[10] <- FALSE
burnin$p[[1]][[1]]$model.form$etamap$offsettheta[21] <- FALSE
burnin$p[[1]][[1]]$model.form$etamap$etalength <- 21
burnin$p[[1]][[1]]$model.form$term.skipped[10] <- FALSE
burnin$p[[1]][[1]]$model.form$minval[21] <- 0
burnin$p[[1]][[1]]$model.form$maxval[21] <- Inf
burnin$p[[1]][[1]]$model.form$duration[10] <- FALSE

burnin$p[[1]][[2]]$model.form$coef.names[19] <- "nodefactor.active.0"
burnin$p[[1]][[2]]$model.form$offset[10] <- FALSE
burnin$p[[1]][[2]]$model.form$terms[[10]] <- list(name = "nodefactor",
                                                  coef.names = "nodefactor.active.0",
                                                  inputs = burnin$p[[1]][[2]]$model.form$terms[[9]]$inputs,
                                                  dependence = FALSE,
                                                  minval = 0,
                                                  pkgname = "ergm")
burnin$p[[1]][[2]]$model.form$etamap$canonical[19] <- 19
burnin$p[[1]][[2]]$model.form$etamap$offsetmap[19] <- FALSE
burnin$p[[1]][[2]]$model.form$etamap$offset[10] <- FALSE
burnin$p[[1]][[2]]$model.form$etamap$offsettheta[19] <- FALSE
burnin$p[[1]][[2]]$model.form$etamap$etalength <- 19
burnin$p[[1]][[2]]$model.form$term.skipped[10] <- FALSE
burnin$p[[1]][[2]]$model.form$minval[19] <- 0
burnin$p[[1]][[2]]$model.form$maxval[19] <- Inf
burnin$p[[1]][[2]]$model.form$duration[10] <- FALSE

burnin$p[[1]][[3]]$model.form$coef.names[19] <- "nodefactor.active.0"
burnin$p[[1]][[3]]$model.form$offset[9] <- FALSE
burnin$p[[1]][[3]]$model.form$terms[[9]] <- list(name = "nodefactor",
                                                 coef.names = "nodefactor.active.0",
                                                 inputs = burnin$p[[1]][[3]]$model.form$terms[[8]]$inputs,
                                                 dependence = FALSE,
                                                 minval = 0,
                                                 pkgname = "ergm")
burnin$p[[1]][[3]]$model.form$etamap$canonical[19] <- 19
burnin$p[[1]][[3]]$model.form$etamap$offsetmap[19] <- FALSE
burnin$p[[1]][[3]]$model.form$etamap$offset[9] <- FALSE
burnin$p[[1]][[3]]$model.form$etamap$offsettheta[19] <- FALSE
burnin$p[[1]][[3]]$model.form$etamap$etalength <- 19
burnin$p[[1]][[3]]$model.form$term.skipped[9] <- FALSE
burnin$p[[1]][[3]]$model.form$minval[19] <- 0
burnin$p[[1]][[3]]$model.form$maxval[19] <- Inf
burnin$p[[1]][[3]]$model.form$duration[9] <- FALSE

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   trans.scale = c(2.21, 0.405, 0.255),

                   prep.require.lnt = TRUE,
                   prep.risk.reassess.method = "year",

                   # Non-intervention parameters
                   prep.start.prob = 0.715,
                   prep.adhr.dist = c(0.165, 0.235, 0.60),
                   prep.discont.rate = 1 - (2^(-1/(224.4/7))),

                   # Intervention capacity/reach parameters
                   prep.optim.init.prob = 0.2,
                   prep.optim.adhr.prob = 0.3,
                   prep.optim.retn.prob = 0.4,

                   prep.optim.adhr.cap = Inf,
                   prep.optim.retn.cap = Inf,

                   # Intervention effectiveness parameters
                   prep.start.prob.optim = 0.01308633,
                   prep.adhr.dist.optim = reallocate_prop(in.pcp = c(0.165, 0.235, 0.60),
                                                          shift_proportion = 0.6775),
                   prep.discont.rate.optim = 1 - (2^(-1/(346.6633/7))),

                   # Timing parameters
                   cea.start = (52*65) + 1,
                   prep.start = (52*60) + 1,
                   riskh.start = 52*59,
                   prep.optim.start = (52*65) + 1,
                   prep.optim.end = (52*75),
                   end.horizon = 52*85,

                   # Intervention toggles
                   prep.optim.init = INPUT2,
                   prep.optim.adhr = INPUT3,
                   prep.optim.retn = INPUT4,

                   # Discount rate
                   d_r = 0.03,

                   # Costs
                   # Initial office visit cost + 3CrCl test + HepB test + syph + ct + gon
                   init.visit.prep.cost = 187.89 + 9.46 + 10.33 + 4.27 + 35.09 + 35.09,
                   # Follow-up office visit cost
                   ret.visit.prep.cost = 163.01,
                   # HIVpos post-test counseling + step1 + step2
                   # not sure how to incorporate step3 into EpiModel framework
                   hiv.test.pos.cost = 191.14 + 13.71 + 22.41,
                   # step1
                   hiv.test.neg.cost = 13.71,
                   # syphillis + chlamydia + gonorrhea + 3CrCl
                   biannual.test.cost = 4.27 + 35.09 + 35.09 + 9.46,
                   prep.cost.ann = INPUT1,
                   hiv.onart.cost.ann = data.table(age.grp = c("18-29", "30-39", "40-49", "50+"),
                                                   cost = c(40365.96, 46502.22, 51207.15, 55180.51),
                                                   key = "age.grp"),
                   hiv.offart.cost.ann = data.table(age.grp = c("18-29", "30-39", "40-49", "50+"),
                                                    cost = c(5066, 5836.15, 6426.64, 6925.30),
                                                    key = "age.grp"),
                   aids.onart.cost.ann = data.table(age.grp = c("18-29", "30-39", "40-49", "50+"),
                                                    cost = c(43546, 50165, 55241, 59527),
                                                    key = "age.grp"),
                   aids.offart.cost.ann = data.table(age.grp = c("18-29", "30-39", "40-49", "50+"),
                                                     cost = c(11790.88, 13583.03, 14957.43, 16118.02),
                                                     key = "age.grp"),
                   hiv.neg.cost.ann = data.table(age.grp = c("18-44", "45-64", "65+"),
                                                 cost = c(2304, 7063, 12762),
                                                 key = "age.grp"),
                   # Intervention costs
                   prep.optim.init.init.pp = 93.28,
                   prep.optim.init.monthly.fixed = 1659.54,
                   prep.optim.adhr.init.pp = 983.39,
                   prep.optim.retn.init.pp = 679.25,
                   prep.optim.retn.monthly.fixed = 1325.24,

                   # Utilities
                   nohiv.prep.qaly = 1,
                   nohiv.noprep.qaly = 1,
                   aids.art.vs.qaly = 0.87,
                   hiv.art.vs.qaly = 0.89,
                   aids.art.novs.qaly = 0.85,
                   hiv.art.novs.qaly = 0.89,
                   aids.stopart.qaly = 0.82,
                   hiv.stopart.qaly = 0.87,
                   aids.neverart.qaly = 0.83,
                   hiv.neverart.qaly = 0.96,
                   age.adj = -0.003)

init <- init_msm(prev.ugc = 0,
                 prev.rct = 0,
                 prev.rgc = 0,
                 prev.uct = 0)

control <- control_msm(simno = fsimno,
                       start = (52*65) + 1,
                       nsteps = 8892,
                       nsims = ncores * 3,
                       ncores = ncores,
                       initialize.FUN = reinit_msm,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE,
                       verbose = F)

## Simulation
start <- Sys.time()
sim <- netsim(burnin, param, init, control)

inputs <- data.frame(prep.cost.ann = INPUT1,
                     prep.optim.init = INPUT2,
                     prep.optim.adhr = INPUT3,
                     prep.optim.retn = INPUT4,
                     par.id = PARID)

sim$inputs <- inputs

sim[c("param", "stats", "attr", "temp", "el", "p", "exdat")] <- NULL


outcomes <- c("incid", "prepElig", "prepCurr", "num.active", "num.alive", "cuml.total.cost",
              "cuml.total.cost.disc", "cuml.total.qaly","cuml.total.qaly.disc",
              "OptimInitUserStarts", "OptimInitUserPrev", "OptimInitUserPop", "OptimInitStarts",
              "OptimInitPrev","PrEPStarts", "PrEPStartsOptim", "OptimAdhrStarts",
              "OptimAdhrPrev", "OptimRetnStarts", "OptimRetnPrev", "PrEPHighAdr", "PrEPStopsRandOptim",
              "hiv.test.cost", "tot.visit.cost", "prep.cost", "int.cost", "cc.cost",
              "hiv.test.cost.disc", "tot.visit.cost.disc", "prep.cost.disc", "int.cost.disc", "cc.cost.disc",
              "cuml.init.int.cost", "cuml.adhr.int.cost", "cuml.retn.int.cost",
              "cuml.init.int.cost.disc", "cuml.adhr.int.cost.disc", "cuml.retn.int.cost.disc",
              "num.nohiv.prep", "num.nohiv.noprep", "num.aids.art.vs",
              "num.hiv.art.vs", "num.aids.art.novs", "num.hiv.art.novs", "num.aids.stopart", "num.hiv.stopart",
              "num.aids.neverart", "num.hiv.neverart" )

epi <- sim$epi[outcomes]

res <- data.frame(cuml.qaly.disc = unlist(epi[["cuml.total.qaly.disc"]][nrow(epi[["cuml.total.qaly.disc"]]), ]),
                  cuml.cost.disc = unlist(epi[["cuml.total.cost.disc"]][nrow(epi[["cuml.total.cost.disc"]]), ]),
                  cuml.qaly = unlist(epi[["cuml.total.qaly"]][nrow(epi[["cuml.total.qaly"]]), ]),
                  cuml.cost = unlist(epi[["cuml.total.cost"]][nrow(epi[["cuml.total.cost"]]), ]),

                  cuml.init.int.cost.disc = unlist(epi[["cuml.init.int.cost.disc"]][nrow(epi[["cuml.init.int.cost.disc"]]), ]),
                  cuml.adhr.int.cost.disc = unlist(epi[["cuml.adhr.int.cost.disc"]][nrow(epi[["cuml.adhr.int.cost.disc"]]), ]),
                  cuml.retn.int.cost.disc = unlist(epi[["cuml.retn.int.cost.disc"]][nrow(epi[["cuml.retn.int.cost.disc"]]), ]),
                  cuml.init.int.cost = unlist(epi[["cuml.init.int.cost"]][nrow(epi[["cuml.init.int.cost"]]), ]),
                  cuml.adhr.int.cost = unlist(epi[["cuml.adhr.int.cost"]][nrow(epi[["cuml.adhr.int.cost"]]), ]),
                  cuml.retn.int.cost = unlist(epi[["cuml.retn.int.cost"]][nrow(epi[["cuml.retn.int.cost"]]), ]),

                  PrEPStarts = apply(epi[["PrEPStarts"]][-c(1:3380), ], 2, sum, na.rm = T),
                  incid = apply(epi[["incid"]][-c(1:3380), ], 2, sum, na.rm = T),
                  num.active =  unlist(colMeans(epi[["num.active"]][(3900 - 105):(3900 - 1), ])),
                  num.alive = unlist(colMeans(epi[["num.alive"]][(3900 - 105):(3900 - 1), ])),

                  PrEPHighAdr = unlist(colMeans(epi[["PrEPHighAdr"]][(3900 - 105):(3900 - 1), ])),

                  hiv.test.cost = apply(epi[["hiv.test.cost"]][-c(1:3380), ], 2, sum, na.rm = T),
                  tot.visit.cost = apply(epi[["tot.visit.cost"]][-c(1:3380), ], 2, sum, na.rm = T),
                  prep.cost = apply(epi[["prep.cost"]][-c(1:3380), ], 2, sum, na.rm = T),
                  int.cost = apply(epi[["int.cost"]][-c(1:3380), ], 2, sum, na.rm = T),
                  cc.cost = apply(epi[["cc.cost"]][-c(1:3380), ], 2, sum, na.rm = T),

                  hiv.test.cost.disc = apply(epi[["hiv.test.cost.disc"]][-c(1:3380), ], 2, sum, na.rm = T),
                  tot.visit.cost.disc = apply(epi[["tot.visit.cost.disc"]][-c(1:3380), ], 2, sum, na.rm = T),
                  prep.cost.disc = apply(epi[["prep.cost.disc"]][-c(1:3380), ], 2, sum, na.rm = T),
                  int.cost.disc = apply(epi[["int.cost.disc"]][-c(1:3380), ], 2, sum, na.rm = T),
                  cc.cost.disc = apply(epi[["cc.cost.disc"]][-c(1:3380), ], 2, sum, na.rm = T),


                  prepElig = unlist(colMeans(epi[["prepElig"]][(3900 - 105):(3900 - 1), ])),
                  prepCurr = unlist(colMeans(epi[["prepCurr"]][(3900 - 105):(3900 - 1), ])),
                  OptimInitUserPrev = unlist(colMeans(epi[["OptimInitUserPrev"]][(3900 - 105):(3900 - 1), ])),
                  OptimInitUserPop = unlist(colMeans(epi[["OptimInitUserPop"]][(3900 - 105):(3900 - 1), ])),
                  OptimInitPrev = unlist(colMeans(epi[["OptimInitPrev"]][(3900 - 105):(3900 - 1), ])),
                  OptimAdhrPrev = unlist(colMeans(epi[["OptimAdhrPrev"]][(3900 - 105):(3900 - 1), ])),
                  OptimRetnPrev = unlist(colMeans(epi[["OptimRetnPrev"]][(3900 - 105):(3900 - 1), ])))

sim$epi <- res

savesim(sim, save.min = F, save.max = T, compress = T, time.stamp = F)

print(Sys.time() - start)

