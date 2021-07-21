set.seed(1207)

library(kableExtra)
library(dplyr)
library(dampack)
library(tidyr)
library(ellipse)

df <- readRDS("analysis/data/basecase_output_cleaned.rds")
df[1, ] <- df[16, ]
df[17, ] <- df[16, ]

dat <- list()
strats <- rep(NA, 8)
long <- data.frame()

for (i in 1:8) {
  dat[[i]] <- readRDS(paste0("analysis/data/simdata_basecase_cleaned/simulation_output_par", i, ".rds"))
  strats[i] <- paste0(ifelse(dat[[i]]$inputs$prep.optim.init == T, "I", "-"),
                        ifelse(dat[[i]]$inputs$prep.optim.adhr == T, "A", "-"),
                        ifelse(dat[[i]]$inputs$prep.optim.retn == T, "P", "-"))
  raw <- dat[[i]]$epi %>%
    select(cuml.init.int.cost.disc, cuml.cost.disc, int.cost.disc, cuml.qaly.disc, num.alive, num.active) %>%
    mutate(Strategy = strats[i])

  fixed <- sum((1 - 0.03) ^ (1:(52*10) / 52) * (1659.54*12 / 52))
  init.adj <- 172.33 / 93.28

  row <- raw %>%
    mutate(cuml.init.int.cost.disc_std = ifelse(cuml.init.int.cost.disc == 0,
                                                0,
                                                (cuml.init.int.cost.disc - fixed) * init.adj + fixed),
           cuml.cost.disc = 10*(cuml.cost.disc - cuml.init.int.cost.disc + cuml.init.int.cost.disc_std),
           int.cost.disc = 10*(int.cost.disc - cuml.init.int.cost.disc + cuml.init.int.cost.disc_std),
           num.alive = 10*num.alive,
           num.active = 10*num.active,
           cuml.qaly.disc = 10*cuml.qaly.disc)


  lm.alive <- lm(cbind(cuml.cost.disc, cuml.qaly.disc) ~ num.alive, data = row)
  lm.active <- lm(cbind(cuml.cost.disc, cuml.qaly.disc) ~ num.active, data = row)

  row$alive.cuml.cost.disc <- predict(lm.alive, newdata = data.frame(num.alive = mean(row$num.alive)))[, 1] + (lm.alive$residuals[, 1])
  row$alive.cuml.qaly.disc <- predict(lm.alive, newdata = data.frame(num.alive = mean(row$num.alive)))[, 2] + (lm.alive$residuals[, 2])

  row$active.cuml.cost.disc <- predict(lm.active, newdata = data.frame(num.active = mean(row$num.active)))[, 1] + (lm.active$residuals[, 1])
  row$active.cuml.qaly.disc <- predict(lm.active, newdata = data.frame(num.active = mean(row$num.active)))[, 2] + (lm.active$residuals[, 2])


  long <- rbind(long, row)
}
test <- long %>% filter(Strategy == "---")
mean(test$cuml.qaly.disc)
sd(test$cuml.qaly.disc)


all_icer <- list()

##########################
B <- 1000
cost_c <- rep(NA, B)
qaly_c <- rep(NA, B)
cost_t <- rep(NA, B)
qaly_t <- rep(NA, B)
r <- rep(NA, B)

# set strategies
strat_c <- "---"
strat_t <- "IA-"

for (b in 1:B) {
  cost_c[b] <- mean(sample(long[long$Strategy == strat_c,]$alive.cuml.cost.disc, length(long[long$Strategy == strat_c,]$alive.cuml.cost.disc), replace = TRUE))
  qaly_c[b] <- mean(sample(long[long$Strategy == strat_c,]$alive.cuml.qaly.disc, length(long[long$Strategy == strat_c,]$alive.cuml.qaly.disc), replace = TRUE))
  cost_t[b] <- mean(sample(long[long$Strategy == strat_t,]$alive.cuml.cost.disc, length(long[long$Strategy == strat_t,]$alive.cuml.cost.disc), replace = TRUE))
  qaly_t[b] <- mean(sample(long[long$Strategy == strat_t,]$alive.cuml.qaly.disc, length(long[long$Strategy == strat_t,]$alive.cuml.qaly.disc), replace = TRUE))
  r[b] <- (cost_t[b] - cost_c[b]) / (qaly_t[b] - qaly_c[b])
}

icer <- data.frame(cost_c = cost_c,
                   cost_t = cost_t,
                   qaly_c = qaly_c,
                   qaly_t = qaly_t,
                   r = r,
                   d_cost = cost_t - cost_c,
                   d_qaly = qaly_t - qaly_c)

plot<- ggplot(icer, aes(y = d_cost, x = d_qaly)) + geom_point()
icer_est <- mean(icer$r)
icer_se <- sqrt((1/(B-1))*sum((icer_est - icer$r)^2))

icer_est
icer_se

icer1 <- list(icer = icer,
              plot = plot,
              icer_est = icer_est,
              icer_se = icer_se)
all_icer[[1]] <- icer1

###################
cost_c <- rep(NA, B)
qaly_c <- rep(NA, B)
cost_t <- rep(NA, B)
qaly_t <- rep(NA, B)
r <- rep(NA, B)

# set strategies
strat_c <- "-A-"
strat_t <- "IA-"

for (b in 1:B) {
  cost_c[b] <- mean(sample(long[long$Strategy == strat_c,]$alive.cuml.cost.disc, length(long[long$Strategy == strat_c,]$alive.cuml.cost.disc), replace = TRUE))
  qaly_c[b] <- mean(sample(long[long$Strategy == strat_c,]$alive.cuml.qaly.disc, length(long[long$Strategy == strat_c,]$alive.cuml.qaly.disc), replace = TRUE))
  cost_t[b] <- mean(sample(long[long$Strategy == strat_t,]$alive.cuml.cost.disc, length(long[long$Strategy == strat_t,]$alive.cuml.cost.disc), replace = TRUE))
  qaly_t[b] <- mean(sample(long[long$Strategy == strat_t,]$alive.cuml.qaly.disc, length(long[long$Strategy == strat_t,]$alive.cuml.qaly.disc), replace = TRUE))
  r[b] <- (cost_t[b] - cost_c[b]) / (qaly_t[b] - qaly_c[b])
}

icer <- data.frame(cost_c = cost_c,
                   cost_t = cost_t,
                   qaly_c = qaly_c,
                   qaly_t = qaly_t,
                   r = r,
                   d_cost = cost_t - cost_c,
                   d_qaly = qaly_t - qaly_c)
plot <- ggplot(icer, aes(y = d_cost, x = d_qaly)) + geom_point()
icer_est <- mean(icer$d_cost)/mean(icer$d_qaly)
icer_se <- sqrt((1/(B-1))*sum((icer_est - icer$r)^2))

icer2 <- list(icer = icer,
              plot = plot,
              icer_est = icer_est,
              icer_se = icer_se)
all_icer[[2]] <- icer2

###################
cost_c <- rep(NA, B)
qaly_c <- rep(NA, B)
cost_t <- rep(NA, B)
qaly_t <- rep(NA, B)
r <- rep(NA, B)

# set strategies
strat_c <- "IA-"
strat_t <- "I-P"

for (b in 1:B) {
  cost_c[b] <- mean(sample(long[long$Strategy == strat_c,]$alive.cuml.cost.disc, length(long[long$Strategy == strat_c,]$alive.cuml.cost.disc), replace = TRUE))
  qaly_c[b] <- mean(sample(long[long$Strategy == strat_c,]$alive.cuml.qaly.disc, length(long[long$Strategy == strat_c,]$alive.cuml.qaly.disc), replace = TRUE))
  cost_t[b] <- mean(sample(long[long$Strategy == strat_t,]$alive.cuml.cost.disc, length(long[long$Strategy == strat_t,]$alive.cuml.cost.disc), replace = TRUE))
  qaly_t[b] <- mean(sample(long[long$Strategy == strat_t,]$alive.cuml.qaly.disc, length(long[long$Strategy == strat_t,]$alive.cuml.qaly.disc), replace = TRUE))
  r[b] <- (cost_t[b] - cost_c[b]) / (qaly_t[b] - qaly_c[b])
}

icer <- data.frame(cost_c = cost_c,
                   cost_t = cost_t,
                   qaly_c = qaly_c,
                   qaly_t = qaly_t,
                   r = r,
                   d_cost = cost_t - cost_c,
                   d_qaly = qaly_t - qaly_c)
plot <- ggplot(icer, aes(y = d_cost, x = d_qaly)) + geom_point()

icer_est <- mean(icer$d_cost)/mean(icer$d_qaly)
icer_se <- sqrt((1/(B-1))*sum((icer_est - icer$r)^2))

icer3 <- list(icer = icer,
              plot = plot,
              icer_est = icer_est,
              icer_se = icer_se)
all_icer[[3]] <- icer3

test <- all_icer[[1]]$icer
(mean(test$cost_t) - mean(test$cost_c)) / (mean(test$qaly_t) - mean(test$qaly_c))
(mean(test$d_cost)) / (mean(test$d_qaly))

test <- test %>%
  mutate(
    quad = case_when(d_qaly > 0 & d_cost > 0 ~ 1,
                     d_qaly < 0 & d_cost > 0 ~ 2,
                     d_qaly < 0 & d_cost < 0 ~ 3,
                     d_qaly > 0 & d_cost < 0 ~ 4)
  )

###########

all_icer[[1]]$plot
all_icer[[1]]$icer_est
all_icer[[1]]$icer_se
quantile(all_icer[[1]]$icer$r, c(0.025, 0.975))

all_icer[[2]]$plot
all_icer[[2]]$icer_est
all_icer[[2]]$icer_se
quantile(all_icer[[2]]$icer$r, c(0.025, 0.975))

all_icer[[3]]$plot
all_icer[[3]]$icer_est
all_icer[[3]]$icer_se
quantile(all_icer[[3]]$icer$r, c(0.025, 0.975))
test <- all_icer[[1]]$icer

cost_c <- rep(NA, B)
qaly_c <- rep(NA, B)
cost_t <- rep(NA, B)
qaly_t <- rep(NA, B)
r <- rep(NA, B)

# set strategies
strat_c <- "---"
strat_t <- "I--"

for (b in 1:B) {
  cost_c[b] <- mean(sample(long[long$Strategy == strat_c,]$cuml.cost.disc, length(long[long$Strategy == strat_c,]$cuml.cost.disc), replace = TRUE))
  qaly_c[b] <- mean(sample(long[long$Strategy == strat_c,]$cuml.qaly.disc, length(long[long$Strategy == strat_c,]$cuml.qaly.disc), replace = TRUE))
  cost_t[b] <- mean(sample(long[long$Strategy == strat_t,]$cuml.cost.disc, length(long[long$Strategy == strat_t,]$cuml.cost.disc), replace = TRUE))
  qaly_t[b] <- mean(sample(long[long$Strategy == strat_t,]$cuml.qaly.disc, length(long[long$Strategy == strat_t,]$cuml.qaly.disc), replace = TRUE))
  r[b] <- (cost_t[b] - cost_c[b]) / (qaly_t[b] - qaly_c[b])
}

icer <- data.frame(cost_c = cost_c,
                   cost_t = cost_t,
                   qaly_c = qaly_c,
                   qaly_t = qaly_t,
                   r = r,
                   d_cost = cost_t - cost_c,
                   d_qaly = qaly_t - qaly_c)

plot <- ggplot(icer, aes(y = d_cost, x = d_qaly)) + geom_point()
icer_est <- mean(icer$r)
icer_se <- sqrt((1/(B-1))*sum((icer_est - icer$r)^2))

plotdat <- all_icer[[1]]$icer
plot <- ggplot(plotdat, aes(y = d_cost/1000000, x = d_qaly)) + geom_point()
plot

els <- as.data.frame(ellipse(cor(plotdat$d_qaly, plotdat$d_cost),
                             scale = c(sd(plotdat$d_qaly), sd(plotdat$d_cost)),
                             centre = c(mean(plotdat$d_qaly), mean(plotdat$d_cost))))
plot + geom_path(data = els,
                 aes(x = x, y = y/1000000),
                 size = 1,
                 linetype = 2,
                 alpha = .75) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ylab("Incremental Costs (Million $)") +
  xlab("Incremental QALYs") +
  theme(
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(color = "black", face = "bold", size = 12),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(color = "black", face = "bold", size = 12),
  ) +
  ggtitle("Bootstrap Means for Incremental Costs and QALYs",
          subtitle = "Comparing Status Quo and Adherence Strategies")

quad1 <- plotdat %>%
  filter(d_cost > 0, d_qaly > 0)
quantile(quad1$r, c(0.025, 0.975))
mean(quad1$d_cost)/mean(quad1$d_qaly)


all_icer[[1]]$plot
all_icer[[1]]$icer_est
all_icer[[1]]$icer_se
quantile(all_icer[[1]]$icer$r, c(0.025, 0.975))

cor(all_icer[[1]]$icer$d_cost, all_icer[[1]]$icer$d_qaly)

quantile(all_icer[[1]]$icer$d_qaly, c(0.025, 0.975))
quantile(all_icer[[1]]$icer$d_cost, c(0.025, 0.975))
quantile(all_icer[[1]]$icer$r, c(0.025, 0.975))


test <- all_icer[[1]]$icer
test <- test %>% mutate(d_qaly_adj = ifelse(d_qaly < 0, 0, d_qaly),
                        r_adj = d_cost / d_qaly_adj)
quantile(test$r_adj, c(0.025, 0.975))
(mean(test$cost_t) - mean(test$cost_c)) / (mean(test$qaly_t) - mean(test$qaly_c))
(mean(test$d_cost)) / (mean(test$d_qaly))

test <- test %>%
  mutate(
    quad = case_when(d_qaly > 0 & d_cost > 0 ~ 1,
                     d_qaly < 0 & d_cost > 0 ~ 2,
                     d_qaly < 0 & d_cost < 0 ~ 3,
                     d_qaly > 0 & d_cost < 0 ~ 4)
  )
###########



bootdat <- data.frame(strat1_cost = all_icer[[1]]$icer$cost_c,
                      strat1_qaly = all_icer[[1]]$icer$qaly_c,
                      strat2_cost = all_icer[[3]]$icer$cost_c,
                      strat2_qaly = all_icer[[3]]$icer$qaly_c,
                      strat3_cost = all_icer[[3]]$icer$cost_t,
                      strat3_qaly = all_icer[[3]]$icer$qaly_t)
res <- data.frame(ICER_strat___ = numeric(),
                  ICER_stratI_P = numeric(),
                  ICER_stratIA_ = numeric(),
                  Status_strat___ = character(),
                  Status_stratI_P = character(),
                  Status_stratIA_ = character())
for (i in 1:1000) {
  set <- data.frame(strategies = c("strat___", "stratIA_", "stratI_P"),
                    cost = c(bootdat[i, 1], bootdat[i, 3], bootdat[i, 5]),
                    effect = c(bootdat[i, 2], bootdat[i, 4], bootdat[i, 6]))
  icer <- calculate_icers(cost = set$cost,
                          effect = set$effect,
                          strategies = set$strategies)
  icer <- icer[, c("Strategy", "ICER", "Status")] %>%
    arrange(Strategy)
  res_row <- pivot_wider(data = icer,
                         names_from = Strategy,
                         values_from = c(ICER, Status))
  names(res_row)
  res[i,] <- res_row
}

table(res$Status_strat___)
table(res$Status_stratIA_)
table(res$Status_stratI_P)




quantile(res$ICER_strat___, c(0.025, .975), na.rm = TRUE)
quantile(res$ICER_stratIA_, c(0.025, .975), na.rm = TRUE)
quantile(res$ICER_stratI_P, c(0.025, .975), na.rm = TRUE)


res_adj <- res %>%
  mutate(ICER_strat_A_adj = ifelse(Status_strat_A_ != "ND", 9999999, ICER_strat_A_))

quantile(res_adj$ICER_strat_A_adj, c(0.025, .975), na.rm = TRUE)

set <- data.frame(strategies = c("---", "-A-", "IA-", "IAP"),
                  cost = bootdat[i, c(1, 3, 5, 7)],
                  effect = bootdat[i, c(2, 4, 6, 8)])
