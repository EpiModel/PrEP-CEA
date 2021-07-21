library(kableExtra)
library(dplyr)
library(tidyverse)
library(dampack)
library(tidyr)
library(devtools)
library(dampack)
library(ggrepel)
library(scales)
library(here)
library(glue)
library(officer)
library(rvg)
library(tidyverse)
# remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
library(RColorBrewer)
library(viridis)

df <- readRDS("analysis/data/basecase_output_cleaned.rds")

coverage <- df %>%
  mutate(Strategy = as.factor(paste0(ifelse(prep.optim.init == T, "I", "-"),
                                     ifelse(prep.optim.adhr == T, "A", "-"),
                                     ifelse(prep.optim.retn == T, "P", "-")))) %>%
  select(-c(prep.optim.init, prep.optim.adhr, prep.optim.retn)) %>%
  select(Strategy, prep.optim.init.prob, prep.optim.adhr.prob, prep.optim.retn.prob,
         prepElig_mean, prepCurr_mean, OptimInitUserPrev_mean, OptimInitUserPop_mean) %>%
  mutate(prepCov = prepCurr_mean/prepElig_mean,
         initCov = OptimInitUserPrev_mean / OptimInitUserPop_mean) %>%
  select(-c(prepElig_mean, prepCurr_mean, OptimInitUserPrev_mean, OptimInitUserPop_mean))

# Adjusting costs based on updated inputs
fixed <- sum((1 - 0.03) ^ (1:(52*10) / 52) * (1659.54*12 / 52))
init.adj <- 172.33 / 93.28

#Base case set of simulations
df_cov <- df[9:16,]
df_strat <- df_cov %>%
  mutate(Strategy = as.factor(paste0(ifelse(prep.optim.init == T, "I", "-"),
                                     ifelse(prep.optim.adhr == T, "A", "-"),
                                     ifelse(prep.optim.retn == T, "P", "-")))) %>%
  select(-c(prep.optim.init, prep.optim.adhr, prep.optim.retn)) %>%
  rowwise() %>%
  mutate(cuml.init.int.cost.disc_std = ifelse(cuml.init.int.cost.disc_mean == 0,
                                              0,
                                              (cuml.init.int.cost.disc_mean - fixed) * init.adj + fixed),
         cuml.cost.disc_mean = cuml.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
         int.cost.disc_mean = int.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std)

plotdat <- df_strat %>% select(Strategy, incid_mean, int.cost.disc_mean,
                               cuml.cost.disc_mean, cuml.qaly.disc_mean)

icer_df <- data.frame(Strategy = unique(df_strat$Strategy))
# Atlanta MSM population is 10x simulated population size
icer_df$cost <- 10*df_strat$cuml.cost.disc_mean
icer_df$qaly <- 10*df_strat$cuml.qaly.disc_mean

### Figure 2

icers <- calculate_icers(cost = icer_df$cost,
                         effect = icer_df$qaly,
                         strategies = icer_df$Strategy)

icersadj <- calculate_icers(cost = icer_df$cost/1000000,
                         effect = icer_df$qaly/1000,
                         strategies = icer_df$Strategy)

icers_lab <- icersadj %>% filter(Strategy %in% c("---", "-A-", "IA-", "IAP") ) %>%
  mutate(midC = (lag(Cost) + Cost) / 2,
         midQ = (lag(Effect) + Effect) / 2,
         ICER = paste0("$", comma(round(ICER*1000,0)), "/QALY")) %>%
  filter(row_number() != 1)

fig2 <- plot(icersadj,
              label = "none",
              txtsize = 15,
              # n_x_ticks = 4,
              n_y_ticks = 4,
              alpha = 2) +
  ylab("Cost (Million $)") +
  xlab("Effectiveness (Thousand QALYs)") +
  scale_x_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13),
        axis.title.x = element_text(color = "black", size = 15),
        axis.title.y = element_text(color = "black", size = 15)) +
  geom_text_repel(data = icers_lab,
                   aes(x = midQ, y = midC, label = ICER),
                   size = 5,
                   nudge_x = c(1, 1, -1.5),
                   nudge_y = c(-50, -30, 0))
fig2

p_dml <- rvg::dml(ggobj = fig2)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, location = officer::ph_location(
  width = 8, height = 5, left = 1, top = 1)) %>%
  # export slide -----
base::print(
  target = here::here(
    "analysis",
    "plots",
    "Fig2.pptx"
  )
)

##################################

### Figure 1

extra_outcomes <- plotdat %>%
  group_by(Strategy) %>%
  summarize(intervention_cost = 10*mean(int.cost.disc_mean),
            cuml_incidence = 10*mean(incid_mean),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(infections_averted = 10*(first(cuml_incidence) - cuml_incidence),
         pct_infections_averted = 100*(infections_averted / (10*first(cuml_incidence))))

icers_exp <- left_join(icers, extra_outcomes, by = "Strategy")

sq <- icers_exp %>% filter(Strategy == "---")

icers_exp <- icers_exp %>%
  mutate(pct_intervention_cost = 100 * (intervention_cost / Cost),
         Cost_sq_diff = Cost - sq$Cost,
         Effect_sq_diff = Effect - sq$Effect)
icers_exp %>%
  select(-c("cuml_incidence", "pct_intervention_cost")) %>%
  kable() %>%
  kable_styling()


im.outcomes <- df_strat %>% select(Strategy,
                                   prep.cost.disc_mean,  cuml.cost.disc_mean,
                                   cuml.qaly.disc_mean,
                                   cuml.init.int.cost.disc_std, cuml.adhr.int.cost.disc_mean,
                                   cuml.retn.int.cost.disc_mean, hiv.test.cost.disc_mean, tot.visit.cost.disc_mean,
                                   int.cost.disc_mean, cc.cost.disc_mean) %>%
  group_by(Strategy) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

cost.long <- im.outcomes %>% select(Strategy, hiv.test.cost.disc_mean,
                                    tot.visit.cost.disc_mean,
                                    int.cost.disc_mean,
                                    cc.cost.disc_mean,
                                    prep.cost.disc_mean) %>%
  mutate(visit_hiv_test_cost = hiv.test.cost.disc_mean + tot.visit.cost.disc_mean) %>%
  select(-c("tot.visit.cost.disc_mean", "hiv.test.cost.disc_mean")) %>%
  group_by(Strategy) %>%
  pivot_longer(cols = 2:5) %>%
  mutate(name = factor(name,
                       levels = c("visit_hiv_test_cost",
                                  "int.cost.disc_mean",
                                  "cc.cost.disc_mean",
                                  "prep.cost.disc_mean"),
                       labels = c("HIV Testing and PrEP Visits Costs",
                                  "Intervention Costs",
                                  "Other Clinical Care/Health Care Costs",
                                  "PrEP Drug Costs")))

sq.cost <- cost.long %>%
  filter(Strategy == "---") %>%
  mutate(sq_val = value) %>%
  ungroup() %>%
  select(-c(value, Strategy))

diff.cost <- cost.long %>%
  filter(Strategy %in% c("I--", "-A-", "--P", "IAP")) %>%
  left_join(sq.cost, by = "name") %>%
  rowwise() %>%
  mutate(diff = 10*(value - sq_val),
         diff.prop = (value - sq_val)/sq_val)
diff.cost$Strategy <- factor(diff.cost$Strategy,
                             levels = c("I--", "-A-", "--P", "IAP"),
                             labels = c("a) Initation", "c) Adherence", "b) Persistence", "d) Initation, Adherence, Persistence"))

levels(diff.cost$name) <- c("HIV Testing and\nPrEP Visit\nCosts", "Intervention\nCosts", "Other Clinical Care/\nHealth Care\nCosts", "PrEP Drug\nCosts")

fig1 <- ggplot(diff.cost, aes(x = name, y = diff, fill = name)) +
  geom_col() +
  facet_wrap(.~Strategy) +
  geom_text(aes(x = name,
                y = diff + sign(diff)*6e7,
                label = paste0(ifelse(sign(diff) == 1, "+", "-"), "$", abs(round(diff/1e6, 1)), "M")),
            size = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black", size = 12, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(color = "black", size = 13, face = "bold"),
        legend.position = "none",
        strip.text.x = element_text(size = 12, color = "black")
  )

p_dml <- rvg::dml(ggobj = fig1)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, location = officer::ph_location(
  width = 12, height = 8, left = 1, top = 1)) %>%
  # export slide -----
base::print(
  target = here::here(
    "analysis",
    "plots",
    "Fig1.pptx"
  )
)

########################

### Figure 3

prep.costs <- seq(from = 3000, to = 25188, length.out = 200)
opt_strat <- rep(NA, 200)
for (i in 1:length(prep.costs)) {
  df_strat <- df_cov %>%
    mutate(Strategy = as.factor(paste0(ifelse(prep.optim.init == T, "I", "-"),
                                       ifelse(prep.optim.adhr == T, "A", "-"),
                                       ifelse(prep.optim.retn == T, "P", "-")))) %>%
    select(-c(prep.optim.init, prep.optim.adhr, prep.optim.retn)) %>%
    rowwise() %>%
    mutate(cuml.init.int.cost.disc_std = ifelse(cuml.init.int.cost.disc_mean == 0,
                                                0,
                                                (cuml.init.int.cost.disc_mean - fixed) * init.adj + fixed),
           cuml.cost.disc_mean = cuml.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std -
             prep.cost.disc_mean + (prep.cost.disc_mean * (prep.costs[i]/16575.96)),
           int.cost.disc_mean = int.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           nmb = 100000*cuml.qaly.disc_mean - cuml.cost.disc_mean) %>%
    group_by(Strategy) %>%
    summarize(nmb_mean = mean(nmb, na.rm = TRUE), .groups = "keep")

  opt_strat[i] <- as.character(df_strat$Strategy[which.max(df_strat$nmb_mean)])
}

opt_df <- data.frame(prep_cost = prep.costs, opt_strat = factor(opt_strat, levels = rev(levels(df_strat$Strategy))))

min <- opt_df %>% group_by(opt_strat) %>% filter(prep_cost == min(prep_cost)) %>% mutate(smin = prep_cost) %>% select(-prep_cost)
max <- opt_df %>% group_by(opt_strat) %>% filter(prep_cost == max(prep_cost)) %>% mutate(smax = prep_cost) %>% select(-prep_cost)
opt_plot_a <- left_join(min, max)

int.cost <- seq(from = 0, to = 5, length.out = 200)
opt_strat_all <- rep(NA, 200)
opt_strat_init <- rep(NA, 200)
opt_strat_adhr <- rep(NA, 200)
opt_strat_retn <- rep(NA, 200)
for (i in 1:length(int.cost)) {
  df_strat <- df_cov %>%
    mutate(Strategy = as.factor(paste0(ifelse(prep.optim.init == T, "I", "-"),
                                       ifelse(prep.optim.adhr == T, "A", "-"),
                                       ifelse(prep.optim.retn == T, "P", "-")))) %>%
    select(-c(prep.optim.init, prep.optim.adhr, prep.optim.retn)) %>%
    rowwise() %>%
    mutate(cuml.init.int.cost.disc_std = ifelse(cuml.init.int.cost.disc_mean == 0,
                                                0,
                                                (cuml.init.int.cost.disc_mean - fixed) * init.adj + fixed),
           int.cost.disc_mean = int.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           cuml.cost.disc_mean = cuml.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           cuml.cost.disc_std = cuml.cost.disc_mean - int.cost.disc_mean + int.cost[i]*int.cost.disc_mean,
           nmb = 100000*cuml.qaly.disc_mean - cuml.cost.disc_std) %>%
    group_by(Strategy) %>%
    summarize(nmb_mean = mean(nmb, na.rm = TRUE), .groups = "keep")

  opt_strat_all[i] <- as.character(df_strat$Strategy[which.max(df_strat$nmb_mean)])
}
for (i in 1:length(int.cost)) {
  df_strat <- df_cov %>%
    mutate(Strategy = as.factor(paste0(ifelse(prep.optim.init == T, "I", "-"),
                                       ifelse(prep.optim.adhr == T, "A", "-"),
                                       ifelse(prep.optim.retn == T, "P", "-")))) %>%
    select(-c(prep.optim.init, prep.optim.adhr, prep.optim.retn)) %>%
    rowwise() %>%
    mutate(cuml.init.int.cost.disc_std = ifelse(cuml.init.int.cost.disc_mean == 0,
                                                0,
                                                (cuml.init.int.cost.disc_mean - fixed) * init.adj + fixed),
           int.cost.disc_mean = int.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           cuml.cost.disc_mean = cuml.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           cuml.cost.disc_std = cuml.cost.disc_mean - cuml.init.int.cost.disc_std + int.cost[i]*cuml.init.int.cost.disc_std,
           nmb = 100000*cuml.qaly.disc_mean - cuml.cost.disc_std) %>%
    group_by(Strategy) %>%
    summarize(nmb_mean = mean(nmb, na.rm = TRUE), .groups = "keep")

  opt_strat_init[i] <- as.character(df_strat$Strategy[which.max(df_strat$nmb_mean)])
}
for (i in 1:length(int.cost)) {
  df_strat <- df_cov %>%
    mutate(Strategy = as.factor(paste0(ifelse(prep.optim.init == T, "I", "-"),
                                       ifelse(prep.optim.adhr == T, "A", "-"),
                                       ifelse(prep.optim.retn == T, "P", "-")))) %>%
    select(-c(prep.optim.init, prep.optim.adhr, prep.optim.retn)) %>%
    rowwise() %>%
    mutate(cuml.init.int.cost.disc_std = ifelse(cuml.init.int.cost.disc_mean == 0,
                                                0,
                                                (cuml.init.int.cost.disc_mean - fixed) * init.adj + fixed),
           int.cost.disc_mean = int.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           cuml.cost.disc_mean = cuml.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           cuml.cost.disc_std = cuml.cost.disc_mean  - cuml.adhr.int.cost.disc_mean + int.cost[i]*cuml.adhr.int.cost.disc_mean,
           nmb = 100000*cuml.qaly.disc_mean - cuml.cost.disc_std) %>%
    group_by(Strategy) %>%
    summarize(nmb_mean = mean(nmb, na.rm = TRUE), .groups = "keep")

  opt_strat_adhr[i] <- as.character(df_strat$Strategy[which.max(df_strat$nmb_mean)])
}
for (i in 1:length(int.cost)) {
  df_strat <- df_cov %>%
    mutate(Strategy = as.factor(paste0(ifelse(prep.optim.init == T, "I", "-"),
                                       ifelse(prep.optim.adhr == T, "A", "-"),
                                       ifelse(prep.optim.retn == T, "P", "-")))) %>%
    select(-c(prep.optim.init, prep.optim.adhr, prep.optim.retn)) %>%
    rowwise() %>%
    mutate(cuml.init.int.cost.disc_std = ifelse(cuml.init.int.cost.disc_mean == 0,
                                                0,
                                                (cuml.init.int.cost.disc_mean - fixed) * init.adj + fixed),
           int.cost.disc_mean = int.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           cuml.cost.disc_mean = cuml.cost.disc_mean - cuml.init.int.cost.disc_mean + cuml.init.int.cost.disc_std,
           cuml.cost.disc_std = cuml.cost.disc_mean - cuml.retn.int.cost.disc_mean + int.cost[i]*cuml.retn.int.cost.disc_mean,
           nmb = 100000*cuml.qaly.disc_mean - cuml.cost.disc_std) %>%
    group_by(Strategy) %>%
    summarize(nmb_mean = mean(nmb, na.rm = TRUE), .groups = "keep")

  opt_strat_retn[i] <- as.character(df_strat$Strategy[which.max(df_strat$nmb_mean)])
}

opt_df <- data.frame(int_cost = int.cost,
                     opt_strat = c(factor(opt_strat_all, levels = levels(df_strat$Strategy)),
                                   factor(opt_strat_init, levels = levels(df_strat$Strategy)),
                                   factor(opt_strat_adhr, levels = levels(df_strat$Strategy)),
                                   factor(opt_strat_retn, levels = levels(df_strat$Strategy))),
                     param = as.factor(c(rep("all", length(int.cost)),
                                         rep("init", length(int.cost)),
                                         rep("adhr", length(int.cost)),
                                         rep("retn", length(int.cost))))) %>%
  mutate(opt_strat = factor(df_strat$Strategy[opt_strat], levels = rev(levels(df_strat$Strategy))))


min <- opt_df %>% group_by(opt_strat, param) %>% filter(int_cost == min(int_cost)) %>% mutate(smin = int_cost) %>% select(-int_cost)
max <- opt_df %>% group_by(opt_strat, param) %>% filter(int_cost == max(int_cost)) %>% mutate(smax = int_cost) %>% select(-int_cost)
opt_plot <- left_join(min, max)
opt_plot$param <- factor(opt_plot$param, levels = c("all", "init", "adhr", "retn"), labels = c("All", "Initiation", "Adherence", "Persistence"))

g <- ggplot(opt_plot_a) +
  geom_rect(data = opt_plot_a, aes(xmin = smin/12, xmax = smax/12,
                                           ymin = 0, ymax = 1,
                                           fill = opt_strat),
                    position = "identity") +
  xlab("Monthly PrEP cost ($)") +
  theme_bw() +
  scale_fill_manual(values = viridis(n = 5)[c(1:3)]) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 14),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12))+
  labs(fill = "Optimal Strategy")

g_all <- ggplot(opt_plot) +
  geom_rect(aes(xmin = smin, xmax = smax,
                        ymin = 0, ymax = 1,
                        fill = opt_strat),
                    position = "identity") +
  xlab("Intervention Cost Scale") +
  theme_bw() +
  scale_fill_manual(values = viridis(n = 5)[c(2,4,3,5)]) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 14),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        strip.text.y = element_text(color = "black", size = 12)) +
  facet_grid(param~.) +
  labs(fill = "Optimal Strategy")

### Figure 3 is ultimately a combination of objects "g" and "g_all". We ultimately decided to make the
### plot in Excel using the data.frames constructed above.

