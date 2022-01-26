library(rstan)
library(brms)
library(ggplot2)
library(gdata)
library(dplyr)
library(parallel)
library(cowplot)


library(magrittr)   # piping
library(psych)      # describe data
library(GGally)     # pairs plots
library(lme4)       # MLM
library(arm)        # extracting information from lme models
library(sjPlot)     # plot models
library("ggpubr")   # ggarrange() multiple plots
library(stargazer)
library(tidyverse)  # data handling
options(dplyr.summarise.inform = FALSE)
#-------------------------------------------------------------------------------
# MLM: Hearings nested in committees: Contrarian witnesses
#-------------------------------------------------------------------------------
setwd("~/Dropbox/Article_Analysis/")

#-------------------------------------------------------------------------------
# (A) Load data

committee_members_matched <- 
  read.csv("Data/07_climatehearings0310_for_modelling_20220103_nocccm.csv") %>% 
  mutate(term =  as.factor(paste(year, halfyear)),
         date = as.Date(date),
         committee = paste(chamber, committee)) %>% 
  mutate(across(per_Carbon.intensive.Industry:per_Other, ~.*100))

committee_members_matched %>% select_if(is.numeric) %>% describe(skew = F)

table(committee_members_matched$year, committee_members_matched$majority)

#-------------------------------------------------------------------------------
# (B) Aggregate data
# >> Group mean models: manifest aggregation of L1 variables
# >> (Becker et al. 2017, p. 236 f.)
#-------------------------------------------------------------------------------

# Function to rescale variables so that 0 represents the min and 1 the max value

grouping_vars <- c(names(committee_members_matched)[1:30], 
                   names(committee_members_matched)[68:71],
                   names(committee_members_matched)[77], 'Party')

# Aggregate data
hearings <- committee_members_matched %>%
  group_by(across(all_of(grouping_vars))) %>%
  summarise(inc_total = sum(inc_total)/1000,
            inc_total_fossil = sum(inc_fossil)/1000,
            inc_total_electric = sum(inc_electric)/1000,
            inc_total_alternate = sum(inc_alternate)/1000,
            # inc_total_CCCM = sum(per_inc_CCCM),
            inc_total_environmental = sum(per_inc_environmental)/1000,
            per_inc_fossil = mean(per_inc_fossil),
            per_inc_electric = mean(per_inc_electric),
            per_inc_alternate = mean(per_inc_alternate),
            # per_inc_CCCM = mean(per_inc_CCCM),
            per_inc_environmental = mean(per_inc_environmental),
            per_emp_fossil = mean(per_emp_fossil)) %>%
  ungroup() %>% 
  pivot_wider(names_from = 'Party', values_from = c(inc_total:per_emp_fossil)) %>% 
  mutate(across(inc_total_D:per_emp_fossil_I, ~replace_na(.,0))) %>% 
  mutate(inc_total_fossilelectric_D = inc_total_fossil_D + inc_total_electric_D,
         inc_total_fossilelectric_R = inc_total_fossil_R + inc_total_electric_R,
         per_inc_fossilelectric_D = per_inc_fossil_D + per_inc_electric_D,
         per_inc_fossilelectric_R = per_inc_fossil_R + per_inc_electric_R) %>%
  group_by(committee) %>%
  # Extract group means
  mutate(across(c(per_inc_fossilelectric_D, per_inc_fossilelectric_R,
                  per_inc_alternate_D, per_inc_alternate_R,
                  per_emp_fossil_D, per_emp_fossil_R), ~ mean(.x, na.rm=T),
                .names = "{.col}_AVE")) %>%
  # Centering within clusters (CWC)
  mutate(across(c(per_inc_fossilelectric_D, per_inc_fossilelectric_R,
                  per_inc_alternate_D, per_inc_alternate_R,
                  per_emp_fossil_D, per_emp_fossil_R), ~ .x - mean(.x, na.rm=T),
                .names = "{.col}_CWC")) %>%
  # Scale CWC variables
  mutate(across(per_inc_fossilelectric_D_CWC:per_emp_fossil_R_CWC,
                scale, .names = "{.col}_SC")) %>%
  mutate(across(per_inc_fossilelectric_D_CWC_SC:per_emp_fossil_R_CWC_SC, ~replace_na(.,0))) %>%
  ungroup() %>% 
  # Scale AVE variables
  mutate(across(per_inc_fossilelectric_D_AVE:per_emp_fossil_R_AVE,
                scale, .names = "{.col}_SC"))

# Check for multicollinearity

hearings %>% dplyr::select(per_inc_fossilelectric_D_CWC_SC:per_emp_fossil_R_CWC_SC) %>% 
  cor() %>% round(2) %>% 
  corrplot(method="number", type = "upper",
           p.mat = 
             cor.mtest(hearings %>% 
                         dplyr::select(per_inc_fossilelectric_D_CWC_SC:per_emp_fossil_R_CWC_SC))$p,
           sig.level = .05
  )


hearings %>% dplyr::select(per_inc_fossilelectric_D_AVE_SC:per_emp_fossil_R_AVE_SC) %>% 
  cor() %>% round(2) %>% 
  corrplot(method="number", type = "upper",
           p.mat = 
             cor.mtest(hearings %>% 
                         dplyr::select(per_inc_fossilelectric_D_AVE_SC:per_emp_fossil_R_AVE_SC))$p,
           sig.level = .05
  )

ggarrange(
  ggarrange(
  ggplot(hearings, aes(per_inc_fossilelectric_D, per_Contrarians)) + 
    geom_point() + geom_smooth(method = 'lm'),
  ggplot(hearings, aes(per_inc_fossilelectric_R, per_Contrarians)) + 
    geom_point() + geom_smooth(method = 'lm'),
  ggplot(hearings, aes(per_inc_fossilelectric_D + per_inc_fossilelectric_R, per_Contrarians)) + 
    geom_point() + geom_smooth(method = 'lm'), nrow = 1),
  
  ggarrange(
    ggplot(hearings, aes(per_inc_alternate_D, per_Contrarians)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_inc_alternate_R, per_Contrarians)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_inc_alternate_D + per_inc_alternate_R, per_Contrarians)) + 
      geom_point() + geom_smooth(method = 'lm'), nrow = 1),
  
  ggarrange(
    ggplot(hearings, aes(per_emp_fossil_D, per_Contrarians)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_emp_fossil_R, per_Contrarians)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_emp_fossil_D + per_emp_fossil_R, per_Contrarians)) + 
      geom_point() + geom_smooth(method = 'lm'), nrow = 1), 
  nrow = 3)

ggarrange(
  ggarrange(
    ggplot(hearings, aes(per_inc_fossilelectric_D, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_inc_fossilelectric_R, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_inc_fossilelectric_D + per_inc_fossilelectric_R, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'), nrow = 1),
  
  ggarrange(
    ggplot(hearings, aes(per_inc_alternate_D, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_inc_alternate_R, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_inc_alternate_D + per_inc_alternate_R, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'), nrow = 1),
  
  ggarrange(
    ggplot(hearings, aes(per_emp_fossil_D, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_emp_fossil_R, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'),
    ggplot(hearings, aes(per_emp_fossil_D + per_emp_fossil_R, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = 'lm'), nrow = 1), 
  nrow = 3)


# hearings %>% select(inc_total_fossil_D:inc_total_fossilelectric_R) %>% 
#   describe(skew = F)

#-------------------------------------------------------------------------------
# CONTRARIAN WITNESSES

# MLM null model
con0 <-
  brm(n_Contrarians | trials(n_witnesses) ~
        1 +
        (1|committee),
  data = hearings,
  family = binomial(link = "logit"),
  sample_prior = TRUE,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE))
performance::icc(con0) # binary: 0.09; proportion: 0.15
con0

# Level-1 controls model

# get_prior(
#   # binary_contrarian ~
#   n_Contrarians | trials(n_witnesses) ~
#             majority +
#             chamber +
#             date_SC +
#             (1|committee),
#           data = hearings,
#           family = binomial(link = "logit"))

con1 <- 
  brm(n_Contrarians | trials(n_witnesses) ~
        majority +
        chamber +
        date +
        (1|committee),
      data = hearings,
      family = binomial(link = "logit"),
      sample_prior = TRUE,
      warmup = 1000, iter = 5000,
      cores = parallel::detectCores(),
      control = list(adapt_delta = 0.95),
      save_pars = save_pars(all = TRUE)
  )
con1
con1 %>% plot(
  combo = c("hist", "trace"), widths = c(1, 1.5),
  theme = theme_bw(base_size = 16))


loo(con0, con1, moment_match = T)
bayes_factor(con1, con0)


# Level-1 controls and predictors model
con2 <- 
  brm(n_Contrarians | trials(n_witnesses) ~
        per_inc_fossilelectric_D_CWC_SC +
        per_inc_alternate_D_CWC_SC +
        per_inc_fossilelectric_R_CWC_SC  +
        per_inc_alternate_R_CWC_SC +
        majority +
        chamber +
        as.factor(congress) +
        (1|committee),
      data = hearings,
      family = binomial(link = "logit"),
      sample_prior = TRUE,
      warmup = 1000, iter = 5000,
      cores = parallel::detectCores(),
      control = list(adapt_delta = 0.95),
      save_pars = save_pars(all = TRUE)
  )
con2
con2 %>% plot(
  combo = c("hist", "trace"), widths = c(1, 1.5),
  theme = theme_bw(base_size = 16))

bayes_factor(con2, con1)
WAIC(con1, con2)


# Level-1 controls and predictors model
con3 <- 
  brm(n_Contrarians | trials(n_witnesses) ~
        per_inc_fossilelectric_D_CWC +
        per_inc_fossilelectric_R_CWC  +
        per_inc_alternate_D_CWC +
        per_inc_alternate_R_CWC +
        per_inc_fossilelectric_D_AVE * majority +
        per_inc_fossilelectric_R_AVE * majority +
        per_inc_alternate_D_AVE * majority +
        per_inc_alternate_R_AVE * majority +
        majority +
        chamber +
        as.factor(congress) +
        (1|committee),
      data = hearings,
      family = binomial(link = "logit"),
      sample_prior = TRUE,
      warmup = 1000, iter = 5000,
      cores = parallel::detectCores(),
      control = list(adapt_delta = 0.95, max_treedepth = 15),
      save_pars = save_pars(all = TRUE)
  )
summary(con3)


con3b %>% plot(
  combo = c("hist", "trace"), widths = c(1, 1.5),
  theme = theme_bw(base_size = 16),
  bin=50)

# Level-1 controls and predictors model
con3b <- 
  brm(n_Contrarians | trials(n_witnesses) ~
        per_inc_fossilelectric_D_CWC_SC +
        per_inc_fossilelectric_R_CWC_SC  +
        per_inc_alternate_D_CWC_SC +
        per_inc_alternate_R_CWC_SC +
        per_inc_fossilelectric_D_AVE_SC * majority +
        per_inc_fossilelectric_R_AVE_SC * majority +
        per_inc_alternate_D_AVE_SC * majority +
        per_inc_alternate_R_AVE_SC * majority +
        majority +
        chamber +
        year +
        (1|committee),
      data = hearings,
      family = binomial(link = "logit"),
      sample_prior = TRUE,
      warmup = 1000, iter = 5000,
      cores = parallel::detectCores(),
      control = list(adapt_delta = 0.95, max_treedepth = 15),
      save_pars = save_pars(all = TRUE)
  )
summary(con3b)

pp_check(con3b)
pp_check(con3b, type = "ecdf_overlay")

con3b %>% plot(
  combo = c("hist", "trace"), 
  widths = c(1, 1.5),
  theme = theme_bw(base_size = 16),
  bin=50)

WAIC(con0, con1, con2, con3)
loo(con0, con1, con2, con3, con3b, moment_match = T)


#-------------------------------------------------------------------------------

# prior2 <- c(
#   prior(normal(0, 10), class = Intercept, coef = ""),
#   prior(cauchy(0, 10), class = sd),
#   prior(normal(0, 10), class = b)
# )
# 
# con2p <- 
#   brm(n_Contrarians | trials(n_witnesses) ~
#   # brm(binary_contrarian ~
#         majority +
#         chamber +
#         date_SC +
#         (1|committee),
#       data = hearings,
#       family = binomial(link = "logit"),
#       prior = prior2,
#       sample_prior = TRUE,
#       warmup = 2000, iter = 1e4,
#       cores = parallel::detectCores(),
#       control = list(adapt_delta = 0.95)
#   )

con2p %>% plot(
  combo = c("hist", "trace"), widths = c(1, 1.5),
  theme = theme_bw(base_size = 16))

bayes_factor(con1, con2)
