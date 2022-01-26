# 2do: 
# explore complete model without intercept
# understand implications of scaling centered variables (before or after centering)
# should committe campaign contributions be separated by party?



library(magrittr)   # piping
library(psych)      # describe data
library(GGally)     # pairs plots
library(lme4)       # MLM
library(arm)        # extracting information from lme models
library(sjPlot)     # plot models
library("ggpubr")   # ggarrange() multiple plots
library(tidyverse)  # data handling
options(dplyr.summarise.inform = FALSE)
#-------------------------------------------------------------------------------
# MLM: Hearings nested in committees: Contrarian witnesses
#-------------------------------------------------------------------------------
setwd("~/Dropbox/Article_Analysis/")

#-------------------------------------------------------------------------------
# (A) Load data

committee_members_matched <- 
  read.csv("Data/07_climatehearings0310_for_modelling_20220103.csv") %>% 
  mutate(term =  as.factor(paste(year, halfyear))) %>% 
  mutate(across(per_Carbon.intensive.Industry:per_Other, ~.*100)) %>% 
  mutate(committee = paste(chamber, committee))

committee_members_matched %>% select_if(is.numeric) %>% describe(skew = F)

table(committee_members_matched$year, committee_members_matched$majority)

#-------------------------------------------------------------------------------
# (B) Aggregate data
# >> Group mean models: manifest aggregation of L1 variables
# >> (Becker et al. 2017, p. 236 f.)
#-------------------------------------------------------------------------------

grouping_vars <- c(names(committee_members_matched)[1:30], 
                   names(committee_members_matched)[68:73],
                   names(committee_members_matched)[79],)

# Aggregate data
hearings <- committee_members_matched %>%
  group_by(across(all_of(grouping_vars))) %>%
  summarise(inc_total = sum(inc_total),
            inc_total_fossil = sum(inc_fossil),
            inc_total_electric = sum(inc_electric),
            inc_total_alternate = sum(inc_alternate),
            inc_total_CCCM = sum(per_inc_CCCM),
            inc_total_environmental = sum(per_inc_environmental),
            per_inc_fossil = mean(per_inc_fossil),
            per_inc_electric = mean(per_inc_electric),
            per_inc_alternate = mean(per_inc_alternate),
            per_per_inc_CCCM = mean(per_per_inc_CCCM),
            per_per_inc_environmental = mean(per_per_inc_environmental),
            per_emp_fossil = mean(per_emp_fossil))
  
names(hearings)
hearings[c(22:23, 44:47, 3, 7)] %>% as.data.frame() %>% ggpairs() 
# Some IVs highly significantly correlated. 
# >> Check models for multicollinearity issues

rm(committee_members_matched)

hearings_agg <- hearings %>% group_by(committee, majority) %>% 
  summarise(per_inc_fossil = mean(per_inc_fossil),
            per_inc_electric = mean(per_inc_electric),
            per_inc_alternate = mean(per_inc_alternate),
            per_per_inc_CCCM = mean(per_per_inc_CCCM),
            per_per_inc_environmental = mean(per_per_inc_environmental),
            per_emp_fossil = mean(per_emp_fossil),
            per_Contrarians = mean(per_Contrarians),
            per_Fossil.Fuel.Industry = mean(per_Fossil.Fuel.Industry),
            lobbying_fossil = mean(lobbying_fossil))

#-------------------------------------------------------------------------------
# (C) Center the IVs

# IVs:
# per_inc_fossil
# per_inc_CCCM 
# per_inc_environmental
# per_inc_alternate 
# per_emp_fossil

# Controls:
# majority
# chamber
# year


hearings <- hearings %>%
  group_by(committee) %>%
  # Centering grand mean (GWM)
  mutate(across(per_inc_fossil:per_emp_fossil, ~ mean(.x, na.rm=T), 
         .names = "{.col}_AVE")) %>% 
  # Centering within clusters (CWC)
  mutate(across(per_inc_fossil:per_emp_fossil, ~ .x - mean(.x, na.rm=T), 
                .names = "{.col}_CWC")) %>% 
  ungroup()

#-------------------------------------------------------------------------------
# (D) Scale the IVs


# Rescale the variable so that 0 represents the minimum and 1 the maximum
scaleFUN <- function(x) {
  (x - min(x)) / (max(x)-min(x))
}

# hearings <- hearings %>%
#   mutate(across(per_per_inc_fossil_AVE:per_per_emp_fossil_CWC, 
#                 scaleFUN, .names = "{.col}_SC"))

hearings %>%
  pivot_longer(per_inc_fossil_AVE:per_emp_fossil_CWC, 
               names_to = "variable", values_to = "income") %>%
  mutate(variable = as.factor(variable)) %>% 
  ggplot(aes(x = income)) + geom_histogram() +
  facet_wrap(vars(variable), ncol = 2, scales="free_x")

# hearings %>%
#   pivot_longer(per_inc_fossil_AVE_SC:per_per_emp_fossil_CWC_SC, 
#                names_to = "variable", values_to = "income") %>% 
#   mutate(variable = as.factor(variable)) %>%
#   ggplot(aes(x = income)) + geom_histogram() +
#   facet_wrap(vars(variable), ncol = 2)


#-------------------------------------------------------------------------------
# (E) Determine if MLM is necessary: 
# (1) empirical, (2) statistical, and (3) theoretical (see Luke 2020, Chapter 3)

# (1) empirical


### CONTRARIANS ###

# Boxplot by committee and chamber
hearings %>%
  ggplot(aes(x=reorder(committee,per_Contrarians), y=per_Contrarians)) +
  geom_dotplot(alpha = 0.1) +
  geom_boxplot(alpha = 0.6) +
  labs(y="Committee", x="Contrarian witnesses (%)", 
       subtitle="Percentage of contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_grid(.~chamber, scales="free_x")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Null model & ICC
contrarian1 <- lmer(per_Contrarians ~ 
                      1 +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(contrarian1)
performance::icc(contrarian1) # ICC 0.54
ranef(contrarian1)

plot_model(contrarian1,
           type = "re", 
           sort.est = 2)

corecommittes <-
  hearings %>% 
  group_by(chamber, committee) %>% 
  summarise(count = n()) %>%  
  arrange(desc(count)) %>% 
  group_by(chamber) %>% 
  slice(1:6) %>% ungroup() %>% pull(committee)


# Contrarian witnesses by average proportion of fossil fuel campaign contributions
hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_inc_fossil, y=per_Contrarians, color = chamber)) +
  # ggplot(aes(x=per_inc_fossil, y=n_Contrarians, color = chamber)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  labs(y="Contrarian witnesses (%)", x="Average fossil fuel industry campaign contributions for committee members", 
       subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_wrap(vars(committee), nrow = 4) +
  # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
  theme_bw()

# Contrarian witnesses by average proportion of electric utilities campaign contributions
hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_inc_electric, y=per_Contrarians, color = chamber)) +
  # ggplot(aes(x=per_inc_fossil, y=n_Contrarians, color = chamber)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  labs(y="Contrarian witnesses (%)", x="Average fossil fuel industry campaign contributions for committee members", 
       subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_wrap(vars(committee), nrow = 4) +
  # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
  theme_bw()

# Contrarian witnesses by average proportion of alternate energy campaign contributions
hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_inc_alternate, y=per_Contrarians, color = chamber)) +
  # ggplot(aes(x=per_inc_alternate, y=n_Contrarians, color = chamber)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  labs(y="Contrarian witnesses (%)", x="Average alternative energy campaign contributions for committee members", 
       subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_wrap(vars(committee), nrow = 4) +
  # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
  theme_bw()

# Contrarian witnesses by average proportion of CCCM campaign contributions
hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_inc_CCCM, y=per_Contrarians, color = chamber)) +
  # ggplot(aes(x=per_inc_CCCM, y=n_Contrarians, color = chamber)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  labs(y="Contrarian witnesses (%)", x="Average CCCM campaign contributions for committee members", 
       subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_wrap(vars(committee), nrow = 4) +
  # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
  theme_bw()

# Contrarian witnesses by average proportion of environmental organisations campaign contributions
hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_inc_environmental, y=per_Contrarians, color = chamber)) +
  # ggplot(aes(x=per_inc_environmental, y=n_Contrarians, color = chamber)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  labs(y="Contrarian witnesses (%)", x="Average environmental organisation campaign contributions for committee members", 
       subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_wrap(vars(committee), nrow = 4) +
  # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
  theme_bw()

# Contrarian witnesses by average fossil fuel employment in committee member constituencies
hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_emp_fossil, y=per_Contrarians, color = chamber)) +
  # ggplot(aes(x=per_emp_fossil, y=n_Contrarians, color = chamber)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  labs(y="Contrarian witnesses (%)", x="Average energy sector in committee members constituencies", 
       subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_wrap(vars(committee), nrow = 4) +
  # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
  theme_bw()

# Group level effects by majority status for the IVs
annotate_figure(
ggarrange(
  
  ggplot(hearings_agg, aes(per_inc_fossil, per_Contrarians, color = majority)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),

  ggplot(hearings_agg, aes(per_inc_electric, per_Contrarians,color = majority)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
  
  ggplot(hearings_agg, aes(per_inc_alternate, per_Contrarians,color = majority)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),

  ggplot(hearings_agg, aes(per_inc_CCCM, per_Contrarians, color = majority)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
  
  ggplot(hearings_agg, aes(per_inc_environmental, per_Contrarians, color = majority)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
  
  ggplot(hearings_agg, aes(per_emp_fossil, per_Contrarians, color = majority)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
  
  legend.grob = get_legend(ggplot(hearings_agg, aes(color = majority))),
  common.legend = T,
  legend = 'bottom'),
 top = text_grob("Committee-level relationships between the proportion of contrarian witnesses at their respective hearings\nand the IVs grouped by majority status"))

# Some level-2 variation present, however rather little (ICC = 0.054). 
# Potential interaction effect between majority status and group-level IVs.

summary(lm(per_Contrarians ~
             per_inc_fossil*majority + 
             per_inc_electric*majority + 
             per_inc_alternate*majority +
             per_inc_CCCM*majority + 
             per_inc_environmental*majority + 
             per_emp_fossil*majority*per_inc_electric,
           hearings_agg))

### FOSSIL FUEL INDUSTRY ###

# Boxplot by committee and chamber
hearings %>%
  ggplot(aes(x=reorder(committee,per_Fossil.Fuel.Industry), y=per_Fossil.Fuel.Industry)) +
  geom_dotplot(alpha = 0.1) +
  geom_boxplot(alpha = 0.6) +
  labs(y="Committee", x="Fossil fuel industry witnesses (%)", 
       subtitle="Percentage of fossil fuel industry witnesses at Congressional hearings on global warming by commitee: 2003 to 2010.") + 
  facet_grid(.~chamber, scales="free_x")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Null model & ICC
fossil1 <- lmer(per_Fossil.Fuel.Industry ~ 
                      1 +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(fossil1)
performance::icc(fossil1) # ICC 0.043

plot_model(fossil1, # model to plot
           type = "re", # random effects 
           sort.est = 2) 

hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_inc_CCCM, y=per_Fossil.Fuel.Industry)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm") +
  labs(y="Fossil fuel industry witnesses (%)", x="Average CCCM campaign contributions for committee members", 
       subtitle="Fossil fuel industry witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_wrap(vars(committee), nrow = 4)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_inc_CCCM, y=n_Fossil.Fuel.Industry)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm") +
  # geom_smooth(method="glm", method.args = list(family = "poisson")) +
  labs(y="Fossil fuel industry witnesses", x="Average CCCM campaign contributions for committee members",
       subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") +
  facet_wrap(vars(committee), nrow = 4)+
  theme_bw()

hearings[hearings$committee %in% corecommittes,] %>%
  ggplot(aes(x=per_inc_fossil, y=per_Fossil.Fuel.Industry)) +
  geom_jitter(alpha = 0.9) +
  geom_smooth(method="lm") +
  # geom_smooth(method="glm", method.args = list(family = "binomial"), se = F) +
  labs(y="Fossil fuel industry witnesses (%)", x="Average fossil fuel campaign contributions for committee members", 
       subtitle="Fossil fuel industry witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
  facet_wrap(vars(committee), nrow = 4)+
  theme_bw()



ggarrange(
    ggplot(hearings_agg, aes(per_inc_CCCM, per_Fossil.Fuel.Industry)) + 
    geom_point() + geom_smooth(method = "lm", se = T) + facet_wrap(vars(majority)),

    ggplot(hearings_agg, aes(per_inc_fossil, per_Fossil.Fuel.Industry)) + 
    geom_point() + geom_smooth(method = "lm", se = T) + facet_wrap(vars(majority)),

    ggplot(hearings_agg, aes(per_inc_environmental, per_Fossil.Fuel.Industry)) + 
    geom_point() + geom_smooth(method = "lm", se = T) + facet_wrap(vars(majority)),

    ggplot(hearings_agg, aes(per_inc_alternate, per_Fossil.Fuel.Industry)) + 
    geom_point() + geom_smooth(method = "lm", se = T) + facet_wrap(vars(majority)),
    
    ggplot(hearings_agg, aes(per_emp_fossil, per_Fossil.Fuel.Industry)) + 
      geom_point() + geom_smooth(method = "lm", se = T) + facet_wrap(vars(majority)))

summary(lm(per_Fossil.Fuel.Industry ~
             per_inc_CCCM*majority + 
             per_inc_fossil*majority + 
             per_inc_environmental*majority + 
             per_inc_alternate*majority +
             per_emp_fossil*majority,
           hearings_agg))

# (2) statistical

# (3) theoretical

#-------------------------------------------------------------------------------
# (?) Contrarian witness models

# Intraclass Correlation Coefficient (ICC):
# "... the proportion of the variance explained by the grouping structure
# in the population" (Hox, Moerbeek, and van de Schoot 2010, 6)


# Null model
contrarian1 <- lmer(per_Contrarians ~ 
                      1 +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(contrarian1)
performance::icc(contrarian1)

plot_model(contrarian1, # model to plot
           type = "re", # random effects 
           sort.est = 2) 


# # Level-1 predictors model
# contrarian2 <- lmer(per_Contrarians ~ 
#                       per_inc_fossil_CWC +
#                       per_inc_CCCM_CWC +
#                       per_inc_environmental_CWC +
#                       per_inc_alternate_CWC +
#                       per_emp_fossil_CWC +
#                       (1|committee),
#                     data = hearings,
#                     control = lmerControl(optimizer = "Nelder_Mead"),
#                     REML = TRUE)
# summary(contrarian2)
# performance::icc(contrarian2)
# 
# plot_model(contrarian2, # model to plot
#            type = "re", # random effects 
#            sort.est = 2)

# Level-1 predictors model
contrarian2 <- lmer(per_Contrarians ~ 
                      per_inc_fossil_CWC +
                      per_inc_CCCM_CWC +
                      per_inc_environmental_CWC +
                      per_inc_alternate_CWC +
                      per_emp_fossil_CWC +
                      majority +
                      year +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(contrarian2)
performance::icc(contrarian2)

plot_model(contrarian2, # model to plot
           type = "re", # random effects 
           sort.est = 2)


# Level-2 intercept model
contrarian3 <- lmer(per_Contrarians ~ 
                      per_inc_fossil_CWC +
                      per_inc_CCCM_CWC +
                      per_inc_environmental_CWC +
                      per_inc_alternate_CWC +
                      per_emp_fossil_CWC +
                      majority +
                      year +
                      chamber +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(contrarian3)
performance::icc(contrarian3)

plot_model(contrarian3, # model to plot
           type = "re", # random effects 
           sort.est = 2)

# Level-2 intercept model including average effects of the IVs

contrarian4 <- lmer(per_Contrarians ~ 
                      per_inc_fossil_CWC +
                      per_inc_CCCM_CWC +
                      per_inc_environmental_CWC +
                      per_inc_alternate_CWC +
                      per_emp_fossil_CWC +
                      per_inc_fossil_AVE * majority +
                      per_inc_CCCM_AVE * majority +
                      per_inc_environmental_AVE * majority +
                      per_inc_alternate_AVE * majority +
                      per_emp_fossil_AVE * majority +
                      majority +
                      year +
                      chamber +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(contrarian4)
performance::icc(contrarian4)

plot_model(contrarian4, # model to plot
           type = "re", # random effects 
           sort.est = 2)

# Level-2 intercept model including average effects of the IVs

contrarian5 <- lmer(per_Contrarians ~ 
                      per_inc_fossil_CWC*majority +
                      per_inc_CCCM_CWC*majority +
                      per_inc_environmental_CWC*majority +
                      per_inc_alternate_CWC*majority +
                      per_emp_fossil_CWC*majority +
                      per_inc_fossil_AVE*majority+
                      per_inc_CCCM_AVE*majority+
                      per_inc_environmental_AVE*majority +
                      per_inc_alternate_AVE*majority +
                      per_emp_fossil_AVE*majority +
                      # majority +
                      year +
                      chamber +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(contrarian5)
performance::icc(contrarian5)

plot_model(contrarian5, # model to plot
           type = "re", # random effects 
           sort.est = 2)

# Note: SIGNIFCANT INTERACTION BUT NOT MAIN EFFECTS
# >> cross-over interaction
# https://www.theanalysisfactor.com/interactions-main-effects-not-significant/

#-------------------------------------------------------------------------------
# (E) Fossil fuel industry witness models

# Intraclass Correlation Coefficient (ICC):
# "... the proportion of the variance explained by the grouping structure
# in the population" (Hox, Moerbeek, and van de Schoot 2010, 6)


# Null model
fossil1 <- lmer(per_Fossil.Fuel.Industry ~ 
                  1 +
                  (1|committee),
                data = hearings,
                control = lmerControl(optimizer = "Nelder_Mead"),
                REML = TRUE)
summary(fossil1)
performance::icc(fossil1)

plot_model(fossil1, # model to plot
           type = "re", # random effects 
           sort.est = 2) 


# Level-1 predictors model
fossil2 <- lmer(per_Fossil.Fuel.Industry ~ 
                  per_inc_fossil_CWC +
                  per_inc_CCCM_CWC +
                  per_inc_environmental_CWC +
                  per_inc_alternate_CWC +
                  per_emp_fossil_CWC +
                  (1|committee),
                data = hearings,
                control = lmerControl(optimizer = "Nelder_Mead"),
                REML = TRUE)
summary(fossil2)
performance::icc(fossil2)

plot_model(fossil2, # model to plot
           type = "re", # random effects 
           sort.est = 2)

# Level-1 predictors model
fossil2 <- lmer(per_Fossil.Fuel.Industry ~ 
                  per_inc_fossil_CWC +
                  per_inc_CCCM_CWC +
                  per_inc_environmental_CWC +
                  per_inc_alternate_CWC +
                  per_emp_fossil_CWC +
                  majority +
                  year +
                  (1|committee),
                data = hearings,
                control = lmerControl(optimizer = "Nelder_Mead"),
                REML = TRUE)
summary(fossil2)
performance::icc(fossil2)

plot_model(fossil2, # model to plot
           type = "re", # random effects 
           sort.est = 2)


# Level-2 intercept model
fossil3 <- lmer(per_Fossil.Fuel.Industry ~ 
                  per_inc_fossil_CWC +
                  per_inc_CCCM_CWC +
                  per_inc_environmental_CWC +
                  per_inc_alternate_CWC +
                  per_emp_fossil_CWC +
                  majority +
                  year +
                  chamber +
                  (1|committee),
                data = hearings,
                control = lmerControl(optimizer = "Nelder_Mead"),
                REML = TRUE)
summary(fossil3)
performance::icc(fossil3)

plot_model(fossil3, # model to plot
           type = "re", # random effects 
           sort.est = 2)

# Level-2 intercept model including average effects of the IVs

fossil4 <- lmer(per_Fossil.Fuel.Industry ~ 
                  per_inc_fossil_CWC +
                  per_inc_CCCM_CWC +
                  per_inc_environmental_CWC +
                  per_inc_alternate_CWC +
                  per_emp_fossil_CWC +
                  per_inc_fossil_AVE +
                  per_inc_CCCM_AVE +
                  per_inc_environmental_AVE +
                  per_inc_alternate_AVE +
                  per_emp_fossil_AVE +
                  majority +
                  year +
                  chamber +
                  (1|committee),
                data = hearings,
                control = lmerControl(optimizer = "Nelder_Mead"),
                REML = TRUE)
summary(fossil4)
performance::icc(fossil4)

plot_model(fossil4, # model to plot
           type = "re", # random effects 
           sort.est = 2)



























# Brockman and Kalla acces to congress aps (money)
