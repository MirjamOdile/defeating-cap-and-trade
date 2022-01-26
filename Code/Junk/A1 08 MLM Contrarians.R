# 2do: 
# explore complete model without intercept
# understand implications of scaling centered variables (before or after centering)
# should committee campaign contributions be separated by party?



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
                   names(committee_members_matched)[68:71],
                   names(committee_members_matched)[77], 'Party')

# Aggregate data
hearings <- committee_members_matched %>%
  group_by(across(all_of(grouping_vars))) %>%
  summarise(inc_total = sum(inc_total),
            inc_total_fossil = sum(inc_fossil),
            inc_total_electric = sum(inc_electric),
            inc_total_alternate = sum(inc_alternate),
            # inc_total_CCCM = sum(per_inc_CCCM),
            inc_total_environmental = sum(per_inc_environmental),
            per_inc_fossil = mean(per_inc_fossil),
            per_inc_electric = mean(per_inc_electric),
            per_inc_alternate = mean(per_inc_alternate),
            # per_inc_CCCM = mean(per_inc_CCCM),
            per_inc_environmental = mean(per_inc_environmental),
            per_emp_fossil = mean(per_emp_fossil)) %>%
  pivot_wider(names_from = 'Party', values_from = c(inc_total:per_emp_fossil))
  
names(hearings)
hearings[c(22:23, 44:47, 3, 7)] %>% as.data.frame() %>% ggpairs() 
# Some IVs highly significantly correlated. 
# >> Check models for multicollinearity issues

# rm(committee_members_matched)

# hearings_agg <- hearings %>% group_by(committee, majority) %>% 
#   summarise(per_inc_fossil = mean(per_inc_fossil),
#             per_inc_electric = mean(per_inc_electric),
#             per_inc_alternate = mean(per_inc_alternate),
#             per_inc_CCCM = mean(per_inc_CCCM),
#             per_inc_environmental = mean(per_inc_environmental),
#             per_emp_fossil = mean(per_emp_fossil),
#             per_Contrarians = mean(per_Contrarians),
#             per_Fossil.Fuel.Industry = mean(per_Fossil.Fuel.Industry),
#             lobbying_fossil = mean(lobbying_fossil))

# Create weighted data

# committee_members_matched <- committee_members_matched %>%
#   mutate(Party = ifelse(Party.Code == 100, "D", ifelse(Party.Code == 200, "R", "I")),
#          majority.party = ifelse(majority == Party, 2, 1),
#          seniority = ifelse(Senior.Party.Member != 0, 2, 1),
#          service = ifelse(Committee.Period.of.Service == 4, .5, ifelse(Committee.Period.of.Service == 5, 1, 0)),
#          weights = majority.party * seniority,
#          weights2 = majority.party * seniority + service)

# Aggregate data
# hearings <- committee_members_matched %>%
#   group_by(across(all_of(grouping_vars))) %>%
#   summarise(inc_total = sum(inc_total),
#             inc_total_fossil = sum(inc_fossil),
#             inc_total_electric = sum(inc_electric),
#             inc_total_alternate = sum(inc_alternate),
#             inc_total_CCCM = sum(per_inc_CCCM),
#             inc_total_environmental = sum(per_inc_environmental),
#             per_inc_fossil = weighted.mean(per_inc_fossil, weights),
#             per_inc_electric = weighted.mean(per_inc_electric, weights),
#             per_inc_alternate = weighted.mean(per_inc_alternate, weights),
#             per_inc_CCCM = weighted.mean(per_inc_CCCM, weights),
#             per_inc_environmental = weighted.mean(per_inc_environmental, weights),
#             per_emp_fossil = weighted.mean(per_emp_fossil, weights))
# 
# hearings[c(22:23, 44:47, 3, 7)] %>% as.data.frame() %>% ggpairs() 


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
  # Extract group means
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
#   mutate(across(per_inc_fossil_AVE:per_emp_fossil_CWC, 
#                 scaleFUN, .names = "{.col}_SC"))

hearings %>%
  pivot_longer(per_inc_fossil_AVE:per_emp_fossil_CWC, 
               names_to = "variable", values_to = "income") %>%
  mutate(variable = as.factor(variable)) %>% 
  ggplot(aes(x = income)) + geom_histogram() +
  facet_wrap(vars(variable), ncol = 2, scales="free_x")

# hearings %>%
#   pivot_longer(per_inc_fossil_AVE_SC:per_emp_fossil_CWC_SC, 
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
  ggplot(aes(x=reorder(committee, per_Contrarians), y=per_Contrarians)) +
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
performance::icc(contrarian1) # ICC 0.054
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

# (2) statistical

# "Whenever there is a nested structure in a data set, there is a good chance 
# that the independence assumption is violated." (Luke 2020, Chapter 3)


# (3) theoretical

# Theoretically one can assume that committees are similar over time as there is
# (at least some) continuity in the committee members over time and across 
# congresses.

# Unofficial source:
# The two main political parties in the House and Senate assign Members to 
# committees, using a three-stage process. 
# * Stage One - Member requests: At the beginning of a new Congress, Members 
# request assignments to the committees they prefer. The incumbent Members 
# (those who are not new) usually keep the committee assignments they have
# because they have expertise and seniority. 
# * Stage Two - Party approval: Each political party uses a committee in charge 
# of committee assignments to recommend assignments. This committee on 
# committees matches the Member requests with available committee seats, 
# prepares and approves an assignment slate for each committee, and submits all 
# slates to the full party for approval. The full party meets to approve the 
# recommendations. 
# * Stage Three - Full Chamber approval: Each committee (now made up of members 
# from each political party) submits its slate to the full Chamber for approval.
# When a committee member resigns or is assigned to another committee, all of 
# Congress is notified.
# https://www.answers.com/Q/How_are_members_of_congress_assigned_to_committees

# Official source:
# https://crsreports.congress.gov/product/pdf/RS/98-367/7


#-------------------------------------------------------------------------------
# (F) Contrarian witness models: MLM

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
                      per_inc_electric_CWC +
                      per_inc_alternate_CWC +
                      per_inc_CCCM_CWC +
                      per_inc_environmental_CWC +
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
                      per_inc_electric_CWC +
                      per_inc_alternate_CWC +
                      per_inc_CCCM_CWC +
                      per_inc_environmental_CWC +
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
                      per_inc_electric_CWC +
                      per_inc_alternate_CWC +
                      per_inc_CCCM_CWC +
                      per_inc_environmental_CWC +
                      per_emp_fossil_CWC +
                      per_inc_fossil_AVE +
                      per_inc_electric_AVE +
                      per_inc_alternate_AVE +
                      per_inc_CCCM_AVE +
                      per_inc_environmental_AVE +
                      per_emp_fossil_AVE +
                      majority +
                      year +
                      chamber +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(contrarian4)
performance::icc(contrarian4)
plot_model(contrarian4)
plot_model(contrarian4, # model to plot
           type = "re", # random effects 
           sort.est = 2)

# Level-2 intercept model including average effects of the IVs and cross level
# interactions
contrarian5 <- lmer(per_Contrarians ~ 
                      per_inc_fossil_CWC +
                      per_inc_electric_CWC +
                      per_inc_alternate_CWC +
                      # per_inc_CCCM_CWC +
                      # per_inc_environmental_CWC +
                      per_emp_fossil_CWC +
                      per_inc_fossil_AVE * majority +
                      per_inc_electric_AVE * majority +
                      per_inc_alternate_AVE * majority +
                      # per_inc_CCCM_AVE * majority +
                      # per_inc_environmental_AVE * majority +
                      per_emp_fossil_AVE * majority +
                      majority +
                      year +
                      chamber +
                      (1|committee),
                    data = hearings,
                    control = lmerControl(optimizer = "Nelder_Mead"),
                    REML = TRUE)
summary(contrarian5)
performance::icc(contrarian5)
plot_model(contrarian5)
plot_model(contrarian5, # model to plot
           type = "re", # random effects 
           sort.est = 2)

anova(contrarian1, contrarian2, contrarian3, contrarian4, contrarian5)

library(stargazer)
stargazer(contrarian5, type = 'text')
-------------------------------------------------------------------------------
# (G) Contrarian witness models: GMLM

# Intraclass Correlation Coefficient (ICC):
# "... the proportion of the variance explained by the grouping structure
# in the population" (Hox, Moerbeek, and van de Schoot 2010, 6)

# Create binary variable for logistic regression
hearings$bi_contrarian <- ifelse(hearings$n_Contrarians==0, 0, 1)

# Null model
contrarian1g <-
  glmer(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
  # glmer(bi_contrarian ~
          1 +
          (1|committee),
        data = hearings,
        family=binomial("logit"),
        control = glmerControl(optimizer = "Nelder_Mead"))
summary(contrarian1g)
performance::icc(contrarian1g)

# ICC so low that this might arguably not be MLM

# Level-1 predictors model
contrarian2g <- 
  # glmer(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
  glmer(bi_contrarian ~
          per_inc_fossil_CWC +
          per_inc_electric_CWC +
          per_inc_alternate_CWC +
          per_inc_CCCM_CWC +
          per_inc_environmental_CWC +
          per_emp_fossil_CWC +
          majority +
          congress +
          (1|committee),
        data = hearings,
        family=binomial("logit"),
        control = glmerControl(optimizer = "Nelder_Mead"))
        # control= glmerControl(optCtrl=list(maxfun=500000000)))
summary(contrarian2g)
performance::icc(contrarian2g)

plot_model(contrarian2g, # model to plot
           type = "re", # random effects 
           sort.est = 2)


# Level-2 intercept model
contrarian3g <- 
  # glmer(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
  glmer(bi_contrarian ~
          per_inc_fossil_CWC +
          per_inc_electric_CWC +
          per_inc_alternate_CWC +
          per_inc_CCCM_CWC +
          per_inc_environmental_CWC +
          per_emp_fossil_CWC +
          majority +
          congress +
          chamber +
        (1|committee),
        data = hearings,
        family=binomial("logit"),
        control = glmerControl(optimizer = "Nelder_Mead"))
# control= glmerControl(optCtrl=list(maxfun=500000000)))
summary(contrarian3g)
performance::icc(contrarian3g)

plot_model(contrarian3g, # model to plot
           type = "re", # random effects 
           sort.est = 2)

# Level-2 intercept model including average effects of the IVs
contrarian4g <- 
  glmer(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
  # glmer(bi_contrarian ~
          per_inc_fossil_CWC +
          per_inc_electric_CWC +
          per_inc_alternate_CWC +
          per_inc_CCCM_CWC +
          per_inc_environmental_CWC +
          per_emp_fossil_CWC +
          per_inc_fossil_AVE +
          per_inc_electric_AVE +
          per_inc_alternate_AVE +
          per_inc_CCCM_AVE +
          per_inc_environmental_AVE +
          per_emp_fossil_AVE +
          majority +
          congress +
          chamber +
          (1|committee),
        data = hearings,
        family=binomial("logit"),
        # control = glmerControl(optimizer = "Nelder_Mead"))
control= glmerControl(optCtrl=list(maxfun=500000000)))
summary(contrarian4)
performance::icc(contrarian4)

plot_model(contrarian4, # model to plot
           type = "re", # random effects 
           sort.est = 2)

# Level-2 intercept model including average effects of the IVs and cross level
# interactions
contrarian5 <- lmer(per_Contrarians ~ 
                      per_inc_fossil_CWC +
                      per_inc_electric_CWC +
                      per_inc_alternate_CWC +
                      per_inc_CCCM_CWC +
                      per_inc_environmental_CWC +
                      per_emp_fossil_CWC +
                      per_inc_fossil_AVE * majority +
                      per_inc_electric_AVE * majority +
                      per_inc_alternate_AVE * majority +
                      per_inc_CCCM_AVE * majority +
                      per_inc_environmental_AVE * majority +
                      per_emp_fossil_AVE * majority +
                      majority +
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

anova(contrarian1, contrarian2, contrarian3, contrarian4, contrarian5)

# 2do reintegrate CCCM funding into fossil fuel?
# split income into income for republicans vs democrats?



# Note: SIGNIFCANT INTERACTION BUT NOT MAIN EFFECTS
# >> cross-over interaction
# https://www.theanalysisfactor.com/interactions-main-effects-not-significant/









# Brockman and Kalla acces to congress aps (money)
