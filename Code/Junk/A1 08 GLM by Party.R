
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
scaleFUN <- function(x) {
  (x - min(x)) / (max(x)-min(x))
}

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
  mutate(across(c(per_inc_fossilelectric_D, per_inc_fossilelectric_R, 
                  per_inc_alternate_D, per_inc_alternate_R,
                  per_emp_fossil_D, per_emp_fossil_R),
                scaleFUN, .names = "{.col}_SC")) %>% 
  mutate(date_SC = scaleFUN(as.numeric(date))) %>% 
  mutate(binary_contrarian = ifelse(n_Contrarians==0, 0, 1),
         binary_fossil = ifelse(n_Fossil.Fuel.Industry==0, 0, 1))


hearings %>% select(inc_total_fossil_D:inc_total_fossilelectric_R) %>% 
  describe(skew = F)
hearings[c(22:23, 68:69, 57:58, 3, 7)] %>% as.data.frame() %>% ggpairs() 

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



#-------------------------------------------------------------------------------
# MODELs

# Intraclass Correlation Coefficient (ICC):
# "... the proportion of the variance explained by the grouping structure
# in the population" (Hox, Moerbeek, and van de Schoot 2010, 6)

#-------------------------------------------------------------------------------
# CONTRARIAN WITNESSES

# MLM null model
con0 <-
  glmer(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
          1 +
          (1|committee),
        data = hearings,
        family=binomial("logit"),
        control = glmerControl(optimizer = "Nelder_Mead"))
performance::icc(con0)

# ICC very low >> MLM not necessary

con1 <-
  glm(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
  # glm(binary_contrarian~
        1,
      data = hearings,
      family=binomial("logit"))
summary(con1)

con2 <-
  glm(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
  # glm(binary_contrarian~
        majority +
        chamber +
        date_SC,
      data = hearings,
      family=binomial("logit"))
summary(con2)
car::vif(con2)

con3 <-
  glm(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
  # glm(binary_contrarian~
        per_inc_fossilelectric_D_SC +
        per_inc_alternate_D_SC +
        per_inc_fossilelectric_R_SC  +
        per_inc_alternate_R_SC +
        majority +
        chamber +
        date_SC,
      data = hearings,
      family=binomial("logit"))
summary(con3)
car::vif(con3)
plot(con3)



con4 <-
  glm(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
  # glm(binary_contrarian~
        per_inc_fossilelectric_D_SC +
        per_inc_alternate_D_SC +
        per_inc_fossilelectric_R_SC  +
        per_inc_alternate_R_SC +
        per_emp_fossil_D_SC +
        per_emp_fossil_R_SC +
        majority +
        chamber +
        date_SC,
      data = hearings,
      family=binomial("logit"))
summary(con4)
car::vif(con4) # Running into multicollinearity

anova(con1, con2, con3, con4, test='LRT')

stargazer(con2, con3, type = 'text', single.row = T, report = "vc*", align = T,
          apply.coef = exp)


#-------------------------------------------------------------------------------
# FOSSIL FUEL WITNESSES

# MLM null model
fos0 <-
  glmer(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
          1 +
          (1|committee),
        data = hearings,
        family=binomial("logit"),
        control = glmerControl(optimizer = "Nelder_Mead"))
summary(fos0)
performance::icc(fos0)

# ICC very low >> MLM not necessary

fos1 <- glm(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
              1,
            data = hearings,
            family=binomial("logit"))
stargazer(fos1, type = 'text', single.row = T, report = "vc*", align = T)

fos2 <- glm(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
        majority +
        chamber +
        date_SC,
      data = hearings,
      family=binomial("logit"))
stargazer(fos2, type = 'text', single.row = T, report = "vc*", align = T)

fos3 <- glm(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
              per_inc_fossilelectric_D_SC +
              per_inc_alternate_D_SC +
              per_inc_fossilelectric_R_SC  +
              per_inc_alternate_R_SC +
              majority +
              chamber +
              date_SC,
            data = hearings,
            family=binomial("logit"))
car::vif(fos3)
stargazer(fos3, type = 'text', single.row = T, report = "vc*", align = T)


fos4 <- glm(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
              per_inc_fossilelectric_D_SC +
              per_inc_alternate_D_SC +
              per_inc_fossilelectric_R_SC  +
              per_inc_alternate_R_SC +
              per_emp_fossil_D_SC +
              per_emp_fossil_R_SC +
              majority +
              chamber +
              date_SC,
            data = hearings,
            family=binomial("logit"))
car::vif(fos4) # Running into multicollinearity
stargazer(fos3, type = 'text', single.row = T, report = "vc*", align = T)

anova(fos1, fos2, fos3, test='LRT')

# 2do reintegrate CCCM funding into fossil fuel?
# split income into income for republicans vs democrats?



# Note: SIGNIFCANT INTERACTION BUT NOT MAIN EFFECTS
# >> cross-over interaction
# https://www.theanalysisfactor.com/interactions-main-effects-not-significant/










# Brockman and Kalla acces to congress aps (money)



#-------------------------------------------------------------------------------
# (C) Determine if MLM is necessary: 

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
MLMnull <- glmer(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~ 
                   1 +
                   (1|committee),
                 data = hearings,
                 control = glmerControl(optimizer = "Nelder_Mead"),
                 family = binomial('logit'))
summary(MLMnull)
performance::icc(MLMnull) # ICC 0.02 (2%)

plot_model(MLMnull,
           type = "re", 
           sort.est = 2)



# hearings <- hearings %>%
#   # Centering grand mean (CGM)
#   mutate(across(per_inc_fossil_D:per_inc_fossilelectric_R, ~ .x - mean(.x, na.rm=T), 
#                 .names = "{.col}_CGM")) %>% 
#   group_by(committee) %>%
#   # Extract group means
#   mutate(across(per_inc_fossil_D:per_inc_fossilelectric_R, ~ mean(.x, na.rm=T), 
#                 .names = "{.col}_AVE")) %>% 
#   # Centering within clusters (CWC)
#   mutate(across(per_inc_fossil_D:per_inc_fossilelectric_R, ~ .x - mean(.x, na.rm=T), 
#                 .names = "{.col}_CWC")) %>% 
#   ungroup()

# 
# corecommittes <-
#   hearings %>% 
#   group_by(chamber, committee) %>% 
#   summarise(count = n()) %>%  
#   arrange(desc(count)) %>% 
#   group_by(chamber) %>% 
#   slice(1:6) %>% ungroup() %>% pull(committee)
# 
# 
# # Contrarian witnesses by average proportion of fossil fuel campaign contributions
# hearings[hearings$committee %in% corecommittes,] %>%
#   ggplot(aes(x=per_inc_fossil, y=per_Contrarians, color = chamber)) +
#   # ggplot(aes(x=per_inc_fossil, y=n_Contrarians, color = chamber)) +
#   geom_jitter(alpha = 0.9) +
#   geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
#   labs(y="Contrarian witnesses (%)", x="Average fossil fuel industry campaign contributions for committee members", 
#        subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
#   facet_wrap(vars(committee), nrow = 4) +
#   # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
#   theme_bw()
# 
# # Contrarian witnesses by average proportion of electric utilities campaign contributions
# hearings[hearings$committee %in% corecommittes,] %>%
#   ggplot(aes(x=per_inc_electric, y=per_Contrarians, color = chamber)) +
#   # ggplot(aes(x=per_inc_fossil, y=n_Contrarians, color = chamber)) +
#   geom_jitter(alpha = 0.9) +
#   geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
#   labs(y="Contrarian witnesses (%)", x="Average fossil fuel industry campaign contributions for committee members", 
#        subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
#   facet_wrap(vars(committee), nrow = 4) +
#   # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
#   theme_bw()
# 
# # Contrarian witnesses by average proportion of alternate energy campaign contributions
# hearings[hearings$committee %in% corecommittes,] %>%
#   ggplot(aes(x=per_inc_alternate, y=per_Contrarians, color = chamber)) +
#   # ggplot(aes(x=per_inc_alternate, y=n_Contrarians, color = chamber)) +
#   geom_jitter(alpha = 0.9) +
#   geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
#   labs(y="Contrarian witnesses (%)", x="Average alternative energy campaign contributions for committee members", 
#        subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
#   facet_wrap(vars(committee), nrow = 4) +
#   # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
#   theme_bw()
# 
# # Contrarian witnesses by average proportion of CCCM campaign contributions
# hearings[hearings$committee %in% corecommittes,] %>%
#   ggplot(aes(x=per_inc_CCCM, y=per_Contrarians, color = chamber)) +
#   # ggplot(aes(x=per_inc_CCCM, y=n_Contrarians, color = chamber)) +
#   geom_jitter(alpha = 0.9) +
#   geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
#   labs(y="Contrarian witnesses (%)", x="Average CCCM campaign contributions for committee members", 
#        subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
#   facet_wrap(vars(committee), nrow = 4) +
#   # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
#   theme_bw()
# 
# # Contrarian witnesses by average proportion of environmental organisations campaign contributions
# hearings[hearings$committee %in% corecommittes,] %>%
#   ggplot(aes(x=per_inc_environmental, y=per_Contrarians, color = chamber)) +
#   # ggplot(aes(x=per_inc_environmental, y=n_Contrarians, color = chamber)) +
#   geom_jitter(alpha = 0.9) +
#   geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
#   labs(y="Contrarian witnesses (%)", x="Average environmental organisation campaign contributions for committee members", 
#        subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
#   facet_wrap(vars(committee), nrow = 4) +
#   # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
#   theme_bw()
# 
# # Contrarian witnesses by average fossil fuel employment in committee member constituencies
# hearings[hearings$committee %in% corecommittes,] %>%
#   ggplot(aes(x=per_emp_fossil, y=per_Contrarians, color = chamber)) +
#   # ggplot(aes(x=per_emp_fossil, y=n_Contrarians, color = chamber)) +
#   geom_jitter(alpha = 0.9) +
#   geom_smooth(method="lm", aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
#   labs(y="Contrarian witnesses (%)", x="Average energy sector in committee members constituencies", 
#        subtitle="Contrarian witnesses at Congressional hearings on global warming by committee: 2003 to 2010.") + 
#   facet_wrap(vars(committee), nrow = 4) +
#   # facet_wrap(vars(committee), nrow = 4, scales = "free_x") +
#   theme_bw()
# 
# # Group level effects by majority status for the IVs
# annotate_figure(
#   ggarrange(
#     
#     ggplot(hearings_agg, aes(per_inc_fossil, per_Contrarians, color = majority)) + 
#       geom_point() + 
#       geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
#     
#     ggplot(hearings_agg, aes(per_inc_electric, per_Contrarians,color = majority)) + 
#       geom_point() + 
#       geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
#     
#     ggplot(hearings_agg, aes(per_inc_alternate, per_Contrarians,color = majority)) + 
#       geom_point() + 
#       geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
#     
#     ggplot(hearings_agg, aes(per_inc_CCCM, per_Contrarians, color = majority)) + 
#       geom_point() + 
#       geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
#     
#     ggplot(hearings_agg, aes(per_inc_environmental, per_Contrarians, color = majority)) + 
#       geom_point() + 
#       geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
#     
#     ggplot(hearings_agg, aes(per_emp_fossil, per_Contrarians, color = majority)) + 
#       geom_point() + 
#       geom_smooth(method = "lm", se = T, aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))),
#     
#     legend.grob = get_legend(ggplot(hearings_agg, aes(color = majority))),
#     common.legend = T,
#     legend = 'bottom'),
#   top = text_grob("Committee-level relationships between the proportion of contrarian witnesses at their respective hearings\nand the IVs grouped by majority status"))
# 
# # Some level-2 variation present, however rather little (ICC = 0.054). 
# # Potential interaction effect between majority status and group-level IVs.
# 
# summary(lm(per_Contrarians ~
#              per_inc_fossil*majority + 
#              per_inc_electric*majority + 
#              per_inc_alternate*majority +
#              per_inc_CCCM*majority + 
#              per_inc_environmental*majority + 
#              per_emp_fossil*majority*per_inc_electric,
#            hearings_agg))
# 
# # (2) statistical
# 
# # "Whenever there is a nested structure in a data set, there is a good chance 
# # that the independence assumption is violated." (Luke 2020, Chapter 3)
# 

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
