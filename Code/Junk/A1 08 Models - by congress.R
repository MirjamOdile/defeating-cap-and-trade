library(readxl)     # excel files
library(tidyverse)  # data handling
options(dplyr.summarise.inform = FALSE)
library(magrittr)   # piping
library(stringr)    # text data handling
library(stringdist) # matching
library(psych)      # describe data
library(beepr)      # play notification sound
library(GGally)     # pairs plots

#-------------------------------------------------------------------------------
# This script matches the 
# ....
#-------------------------------------------------------------------------------
setwd("~/Dropbox/Article_Analysis/")

#-------------------------------------------------------------------------------
# (A) Load ....

committee_members_matched <- read.csv("Data/07_climatehearings0310_for_modelling_20210920.csv")

# Select relevant committee members (exclude all MoCs that joined the committe 
# after the hearing was held)
committee_members_matched <- committee_members_matched %>% 
  filter(not_member_yet == 0)

describe(committee_members_matched, skew = F)

table(committee_members_matched$year, committee_members_matched$majority)

library("lme4")
# summary(lm(per_Contrarians ~ stab*lobbying.fossil.fuel.industry, 
#   data = committee_members_matched))
# 
# summary(lmer(per_Contrarians ~ lobbying.fossil.fuel.industry + (1 | stab), 
#            data = committee_members_matched, REML=FALSE))
# 
# summary(lm(per_Contrarians ~ lobbying.fossil.fuel.industry + as.factor(Party.Code), 
#            data = committee_members_matched))
# 
# summary(lmer(per_Contrarians ~ lobbying.fossil.fuel.industry + as.factor(Party.Code) + (Party.Code | stab), 
#            data = committee_members_matched), REML=FALSE)
# summary(committee_members_matched$lobbying.fossil.fuel.industry)
#-------------------------------------------------------------------------------
# Group mean models: manifest aggregation of L1 variables
# (Becker et al. 2017, p. 236 f.)
#-------------------------------------------------------------------------------

# Aggregate data
hearings <- committee_members_matched %>%
  group_by(hearing.id, congress, majority, chamber, date, year, halfyear, committee,
           n_witnesses,
           n_Alternate.Energy,
           n_Carbon.intensive.Industry,
           n_Contrarians, 
           n_Environmental.Allies,
           n_Fossil.Fuel.Industry,
           n_Government.Officials,
           n_Non.Profits.Unions,
           n_Miscellaneous.Business,
           n_Scientists,
           n_Other,
           per_Alternate.Energy,
           per_Carbon.intensive.Industry,
           per_Contrarians,
           per_Environmental.Allies,
           per_Fossil.Fuel.Industry,
           per_Government.Officials,
           per_Non.Profits.Unions,
           per_Miscellaneous.Business,
           per_Scientists,
           per_Other,
           lobbying.alternate.energy, # are these lobbying contributions by chamber or for the entire congress?
           lobbying.CCCM,
           lobbying.environmental,
           lobbying.fossil.fuel.industry) %>%
  summarise(inc_fossil.fuel = mean(per_inc_fossil.fuel, na.rm = T),
            inc_CCCM = mean(per_inc_CCCM, na.rm = T),
            inc_alternate.energy = mean(per_inc_alternate.energy, na.rm = T),
            inc_environmental = mean(per_inc_environmental, na.rm = T),
            emp_fossil.fuel = mean(per_emp_fossil.fuel, na.rm = T))
names(hearings)
# hearings[c(20:22, 26)] %>% as.data.frame() %>% ggpairs()
# hearings[c(28:32, 3, 4, 6)] %>% as.data.frame() %>% ggpairs()

hist(table(hearings$committee))


summary(lmer(per_Contrarians ~ 1 + (1 | chamber), data = hearings, REML=FALSE))

summary(lmer(per_Contrarians ~ inc_fossil.fuel + majority + (majority| chamber), 
             data = hearings, REML=FALSE))
summary(lmer(per_Contrarians ~ 
               majority*lobbying.fossil.fuel.industry + (majority| chamber), 
             data = hearings, REML=FALSE))

summary(lm(per_Contrarians ~ 
               majority*lobbying.fossil.fuel.industry*chamber, 
             data = hearings))

unique(hearings$committee)
plot(sort(table(hearings$committee)))

unique(hearings$year)

# Run models

summary(A.glm <- glm(cbind(n_Alternate.Energy,
                           n_witnesses-n_Alternate.Energy) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy + 
                       # lobbying.alternate.energy + lobbying.CCCM +
                       # lobbying.environmental + lobbying.fossil.fuel.industry +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))

summary(I.glm <- glm(cbind(n_Fossil.Fuel.Industry,
                           n_witnesses-n_Fossil.Fuel.Industry) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
                       # lobbying.alternate.energy + lobbying.CCCM +
                       # lobbying.environmental + lobbying.fossil.fuel.industry +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))

summary(C.glm <- glm(cbind(n_Contrarians,
                           n_witnesses-n_Contrarians) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
                       # lobbying.alternate.energy + lobbying.CCCM +
                       # lobbying.environmental + lobbying.fossil.fuel.industry +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))

summary(E.glm <- glm(cbind(n_Environmental.Allies,
                           n_witnesses-n_Environmental.Allies) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
                       # lobbying.alternate.energy + lobbying.CCCM +
                       # lobbying.environmental + lobbying.fossil.fuel.industry +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))

summary(S.glm <- glm(cbind(n_Scientists, n_witnesses-n_Scientists) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
                       # lobbying.alternate.energy + lobbying.CCCM +
                       # lobbying.environmental + lobbying.fossil.fuel.industry +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))


#-------------------------------------------------------------------------------
# Group mean models: manifest aggregation of L1 variables (weighted)
# (Becker et al. 2017, p. 236 f.)
#-------------------------------------------------------------------------------

# Create weights

# Seniority                           # Commitee period of service       
# 0   No seniority                    # 1	 Temporary assignment
# 11  Only Chairman                   # 2	 Only period of service
# 12	1st Chairman                    # 3	 First period of service
# 13	2nd Chairman                    # 4	 Second period of service 
# 16  Acting Chairman                 # 5	 Third period of service 
# 21	Only ranking minority member    # 0	 Inapplicable; no committee assignments
# 22	1st ranking minority member 
# 23	2nd ranking minority member

committee_members_matched <- committee_members_matched %>%
  mutate(Party = ifelse(Party.Code == 100, "D", ifelse(Party.Code == 200, "R", "I")),
         majority.party = ifelse(majority == Party, 2, 1),
         seniority = ifelse(Senior.Party.Member != 0, 2, 1),
         service = ifelse(Committee.Period.of.Service == 4, .5, ifelse(Committee.Period.of.Service == 5, 1, 0)),
         weights = majority.party * seniority,
         weights2 = majority.party * seniority + service)

# senior.party.member <- house_members_districts %>%
#   mutate(hearing.id = droplevels.factor(hearing.id, is.na(hearing.id))) %>%
#   group_by(hearing.id) %>% dplyr::select(Senior.Party.Member) %>%
#   filter(Senior.Party.Member != 0) %>%
#   arrange(Senior.Party.Member) %>% table() %>% addmargins(); senior.party.member
# 
# # Period of service
# period.of.service <- house_members_districts %>%
#   mutate(hearing.id = droplevels.factor(hearing.id, is.na(hearing.id))) %>%
#   group_by(hearing.id) %>% dplyr::select(Committee.Period.of.Service) %>%
#   arrange(Committee.Period.of.Service) %>% table(); period.of.service

# house_members_districts %>% filter(hearing.id == "108hhrg87018") %>%
#   mutate(Party = ifelse(Party.Code == 100, "D", ifelse(Party.Code == 200, "R", NA)),
#          majority.party = ifelse(majority == Party, 1.5, 1)) %>%
#   select(Name, Party, majority.party, Senior.Party.Member)
# house_members_districts %>% filter(hearing.id == "108hhrg90165") %>%
#   mutate(Party = ifelse(Party.Code == 100, "D", ifelse(Party.Code == 200, "R", NA)),
#          majority.party = ifelse(majority == Party, 1.5, 1)) %>%
#   select(Name, Party, majority.party, Senior.Party.Member)


# Aggregate data (weighted)
hearings.weighted <- committee_members_matched %>%
  group_by(hearing.id, congress, majority, chamber, 
           committee, date, year, halfyear, 
           n_witnesses,
           n_Alternate.Energy,
           n_Carbon.intensive.Industry,
           n_Contrarians, 
           n_Environmental.Allies,
           n_Fossil.Fuel.Industry,
           n_Government.Officials,
           n_Non.Profits.Unions,
           n_Miscellaneous.Business,
           n_Scientists,
           n_Other,
           per_Alternate.Energy,
           per_Carbon.intensive.Industry,
           per_Contrarians,
           per_Environmental.Allies,
           per_Fossil.Fuel.Industry,
           per_Government.Officials,
           per_Non.Profits.Unions,
           per_Miscellaneous.Business,
           per_Scientists,
           per_Other,
           lobbying.alternate.energy,
           lobbying.CCCM,
           lobbying.environmental,
           lobbying.fossil.fuel.industry) %>%
  summarise(inc_fossil.fuel = weighted.mean(per_inc_fossil.fuel, weights, na.rm = T),
            inc_CCCM = weighted.mean(per_inc_CCCM, weights, na.rm = T),
            inc_alternate.energy = weighted.mean(per_inc_alternate.energy, weights, na.rm = T),
            inc_environmental = weighted.mean(per_inc_environmental, weights, na.rm = T),
            emp_fossil.fuel = weighted.mean(per_emp_fossil.fuel, weights, na.rm = T))

# hearings.weighted[c(20:22, 26)] %>% as.data.frame() %>% ggpairs()
# hearings.weighted[c(28:32, 3, 4, 6)] %>% as.data.frame() %>% ggpairs()


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 
# QCEW employment data investigation
# 
# boxplot(hearings.weighted$emp_fossil.fuel ~ hearings.weighted$chamber)
# 
# # Teams-chat with Trav (2021-07-09) regarding employmenmt data quality:
# # "for most states with multiple congressional district the state level 
# # percentage (Senate) falls in between the percentages of their respective 
# # congressional districts (House). 
# # For single congressional district states the state level is only a bit higher
# # than the district level which is also good. The most problematic states are 
# # imo Oklahoma, West Virginia and Wyoming."
# # >> Potentially compare boxplot for all MoCs for these years with this boxplot.
# # Are the MoCs on the relevant committees in the Senate more heavily from 
# # fossil fuel heavy regions?
# # 2do: investigate!
# 
# describe(hearings.weighted[c(28:32, 3, 4, 6)])
# names(committee_members_matched)
# 
# head(data.plot)
# table(data.plot$area_title)
# data.plot <- committee_members_matched %>% 
#   group_by(stab, area_title, per_emp_fossil.fuel) %>%
#   select(stab, area_title, per_emp_fossil.fuel, chamber, congress) %>% 
#   group_by(stab, area_title, chamber, congress) %>%
#   summarise(per_emp_fossil.fuel = mean(per_emp_fossil.fuel, na.rm = T))
# 
# ggplot(data.plot, aes(stab, per_emp_fossil.fuel, colour = chamber)) + 
#   geom_point() + theme_bw()
# ggplot(committee_members_matched, aes(chamber, per_emp_fossil.fuel, 
#                                       colour = chamber)) + 
#   geom_boxplot() + theme_bw()
# 
# 
# # 2do investigate this further. maybe include "data categorised as 999 
# # 'unknown district'" in signle district states and maybe even distribute
# # evenly/proportionally over multi districts states
#
# t.test(per_emp_fossil.fuel ~ chamber,
#        data = committee_members_matched)
# t.test(per_emp_fossil.fuel ~ chamber,
#        data = committee_members_matched[
#          !committee_members_matched$stab %in% c("AK", "OK", "WV", "WY"),])
# # Difference remains after removing the single district states. Maybe recreate 
# # dotplot from above for all MoCs and not only the slection that is in the
# # committees to check.
# 
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Run models
summary(I.glm.w <- glm(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         # lobbying.alternate.energy + lobbying.CCCM +
                         # lobbying.environmental + lobbying.fossil.fuel.industry +
                         emp_fossil.fuel + majority + chamber + year,
                       data = hearings.weighted, family = binomial(logit)))

summary(C.glm.w <- glm(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         # lobbying.alternate.energy + lobbying.CCCM +
                         # lobbying.environmental + lobbying.fossil.fuel.industry +
                         emp_fossil.fuel + majority + chamber + year,
                       data = hearings.weighted, family = binomial(logit)))

summary(E.glm.w <- glm(cbind(n_Environmental.Allies, n_witnesses-n_Environmental.Allies) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         # lobbying.alternate.energy + lobbying.CCCM +
                         # lobbying.environmental + lobbying.fossil.fuel.industry +
                         emp_fossil.fuel + majority + chamber + year,
                       data = hearings.weighted, family = binomial(logit)))

summary(S.glm.w <- glm(cbind(n_Scientists, n_witnesses-n_Scientists) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         # lobbying.alternate.energy + lobbying.CCCM +
                         # lobbying.environmental + lobbying.fossil.fuel.industry +
                         emp_fossil.fuel + majority + chamber + year,
                       data = hearings.weighted, family = binomial(logit)))

# summary(G.glm.w <- glm(cbind(n_Government.Officials, n_witnesses-n_Government.Officials) ~
#                          inc_environmental + inc_alternate.energy +
#                          inc_fossil.fuel + inc_CCCM +
#                          emp_fossil.fuel + majority + chamber + year,
#                        data = hearings.weighted, family = binomial(logit)))
# 
# summary(Ca.glm.w <- glm(cbind(n_Carbon.intensive.Industry,
#                            n_witnesses-n_Carbon.intensive.Industry) ~
#                           inc_environmental + inc_alternate.energy +
#                           inc_fossil.fuel + inc_CCCM +
#                           emp_fossil.fuel + majority + chamber + year,
#                        data = hearings.weighted, family = binomial(logit)))



#-------------------------------------------------------------------------------
# random effect models
#-------------------------------------------------------------------------------

library(lme4)


# Run models
summary(I.glm.w.f <- glmer(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + (1|congress),
                       data = hearings.weighted, family = binomial(logit)))

summary(C.glm.w.f <- glmer(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + (1|year),
                       data = hearings.weighted, family = binomial(logit)))

summary(E.glm.w.f <- glmer(cbind(n_Environmental.Allies, n_witnesses-n_Environmental.Allies) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + (1|year),
                       data = hearings.weighted, family = binomial(logit)))

summary(S.glm.w.f <- glmer(cbind(n_Scientists, n_witnesses-n_Scientists) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + (1|year),
                       data = hearings.weighted, family = binomial(logit)))

# summary(G.glm.w.f <- glmer(cbind(n_Government.Officials, n_witnesses-n_Government.Officials) ~
#                          inc_environmental + inc_alternate.energy +
#                          inc_fossil.fuel + inc_CCCM +
#                          emp_fossil.fuel + majority + chamber + (1|date),
#                        data = hearings.weighted, family = binomial(logit)))
# 
# summary(Ca.glm.w.f <- glmer(cbind(n_Carbon.intensive.Industry,
#                            n_witnesses-n_Carbon.intensive.Industry) ~
#                           inc_environmental + inc_alternate.energy +
#                           inc_fossil.fuel + inc_CCCM +
#                          emp_fossil.fuel + majority + chamber + (1|date),
#                        data = hearings.weighted, family = binomial(logit)))


#-------------------------------------------------------------------------------
# Hierarchical models
#-------------------------------------------------------------------------------

library(lme4)
table(hearings.weighted$committee, hearings.weighted$chamber)
hearings.weighted %>% 
  ggplot(aes(inc_fossil.fuel, n_witnesses-n_Fossil.Fuel.Industry)) +
  geom_point() + facet_wrap(. ~  chamber)


# Run models
summary(I.glm.w.f <- glmer(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
                        inc_fossil.fuel + inc_CCCM +
                        inc_environmental + inc_alternate.energy +   
                         # lobbying.alternate.energy + lobbying.CCCM +
                         # lobbying.environmental + lobbying.fossil.fuel.industry +
                        emp_fossil.fuel + majority + year + 
                          (1|committee:chamber), # + (1|chamber) + (1|congress),
                       data = hearings.weighted, family = binomial(logit)))
I.glm.w.f@u


summary(C.glm.w.f <- glmer(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + (1|year),
                       data = hearings.weighted, family = binomial(logit)))

summary(E.glm.w.f <- glmer(cbind(n_Environmental.Allies, n_witnesses-n_Environmental.Allies) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + (1|year),
                       data = hearings.weighted, family = binomial(logit)))

summary(S.glm.w.f <- glmer(cbind(n_Scientists, n_witnesses-n_Scientists) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + (1|year),
                       data = hearings.weighted, family = binomial(logit)))


# # #-------------------------------------------------------------------------------
# # # Bayesian glm
# # #-------------------------------------------------------------------------------
# # 
# # library(arm)
# # 
# # summary(I.glm.b <- bayesglm(cbind(n_Fossil.Fuel.Industry,
# #                            n_witnesses-n_Fossil.Fuel.Industry) ~
# #                        emp_mining + emp_utilities +
# #                        inc_environmental +
# #                        inc_fossil.fuel + inc_CCCM +
# #                        majority,
# #                        data = house_hearings, family = binomial(logit)))
# # 
# # summary(C.glm.b <- bayesglm(cbind(n_Contrarians,
# #                            n_witnesses-n_Contrarians) ~
# #                        emp_mining + emp_utilities + 
# #                        inc_environmental +
# #                        inc_fossil.fuel + inc_CCCM + 
# #                        majority,
# #                        data = house_hearings, family = binomial(logit)))
# # 
# # summary(E.glm.b <- bayesglm(cbind(n_Environmental.Allies, 
# #                            n_witnesses-n_Environmental.Allies) ~
# #                        emp_mining + emp_utilities + 
# #                        inc_environmental + inc_alternate.energy +
# #                        inc_fossil.fuel + inc_CCCM + 
# #                        majority,
# #                        data = house_hearings, family = binomial(logit)))
# 
# 

