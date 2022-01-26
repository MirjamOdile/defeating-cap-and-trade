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

table(committee_members$year, committee_members$majority)


#-------------------------------------------------------------------------------
# Group mean models: manifest aggregation of L1 variables
# (Becker et al. 2017, p. 236 f.)
#-------------------------------------------------------------------------------

# Aggregate data
hearings <- committee_members_matched %>%
  group_by(hearing.id, congress, majority, chamber, date, year,
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
           per_Other) %>%
  summarise(inc_fossil.fuel = mean(per_inc_fossil.fuel, na.rm = T),
            inc_CCCM = mean(per_inc_CCCM, na.rm = T),
            inc_alternate.energy = mean(per_inc_alternate.energy, na.rm = T),
            inc_environmental = mean(per_inc_environmental, na.rm = T),
            emp_fossil.fuel = mean(per_emp_fossil.fuel, na.rm = T))
names(hearings)
hearings[c(20:22, 26)] %>% as.data.frame() %>% ggpairs()
hearings[c(28:32, 3, 4, 6)] %>% as.data.frame() %>% ggpairs()

# Run models

summary(A.glm <- glm(cbind(n_Alternate.Energy,
                           n_witnesses-n_Alternate.Energy) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))

summary(I.glm <- glm(cbind(n_Fossil.Fuel.Industry,
                           n_witnesses-n_Fossil.Fuel.Industry) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))

summary(C.glm <- glm(cbind(n_Contrarians,
                           n_witnesses-n_Contrarians) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))

summary(E.glm <- glm(cbind(n_Environmental.Allies,
                           n_witnesses-n_Environmental.Allies) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
                       emp_fossil.fuel + majority + chamber,
                       data = hearings, family = binomial(logit)))

summary(S.glm <- glm(cbind(n_Scientists, n_witnesses-n_Scientists) ~
                       inc_fossil.fuel + inc_CCCM +
                       inc_environmental + inc_alternate.energy +
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
  group_by(hearing.id, congress, majority, chamber, date, year,
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
           per_Other) %>%
  summarise(inc_fossil.fuel = weighted.mean(per_inc_fossil.fuel, weights, na.rm = T),
            inc_CCCM = weighted.mean(per_inc_CCCM, weights, na.rm = T),
            inc_alternate.energy = weighted.mean(per_inc_alternate.energy, weights, na.rm = T),
            inc_environmental = weighted.mean(per_inc_environmental, weights, na.rm = T),
            emp_fossil.fuel = weighted.mean(per_emp_fossil.fuel, weights, na.rm = T))

hearings.weighted[c(20:22, 26)] %>% as.data.frame() %>% ggpairs()
hearings.weighted[c(28:32, 3, 4, 6)] %>% as.data.frame() %>% ggpairs()

plot(hearings.weighted$emp_fossil.fuel, hearings.weighted$chamber)

describe(hearings.weighted[c(28:32, 3, 4, 6)])
names(committee_members_matched)

# head(data.plot)
# table(data.plot$area_title)
# data.plot <- committee_members_matched %>% group_by(stab, area_title, per_emp_fossil.fuel) %>% 
#   select(stab, area_title, per_emp_fossil.fuel, chamber, congress) %>% group_by(stab, area_title, chamber, congress) %>% 
#   summarise(per_emp_fossil.fuel = mean(per_emp_fossil.fuel, na.rm = T))
# ggplot(data.plot, aes(stab,per_emp_fossil.fuel, colour = chamber)) + geom_point() + theme_bw()
# ggplot(committee_members_matched, aes(chamber,per_emp_fossil.fuel, colour = chamber)) + geom_boxplot() + theme_bw()
# 
# t.test(committee_members_matched$per_emp_fossil.fuel ~ committee_members_matched$chamber)
# t.test(committee_members_matched$per_emp_fossil.fuel[!committee_members_matched$stab %in% c("AK", "OK", "WV", "WY")]
#        ~ committee_members_matched$chamber[!committee_members_matched$stab %in% c("AK","OK", "WV", "WY")])

# Run models
summary(I.glm.w <- glm(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + year,
                       data = hearings.weighted, family = binomial(logit)))

summary(C.glm.w <- glm(cbind(n_Contrarians, n_witnesses-n_Contrarians) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + year,
                       data = hearings.weighted, family = binomial(logit)))

summary(E.glm.w <- glm(cbind(n_Environmental.Allies, n_witnesses-n_Environmental.Allies) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel + majority + chamber + year,
                       data = hearings.weighted, family = binomial(logit)))

summary(S.glm.w <- glm(cbind(n_Scientists, n_witnesses-n_Scientists) ~
                         inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
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
                         emp_fossil.fuel + majority + chamber + (1|year),
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


# Run models
summary(I.glm.w.f <- glmer(cbind(n_Fossil.Fuel.Industry, n_witnesses-n_Fossil.Fuel.Industry) ~
                        inc_fossil.fuel + inc_CCCM +
                         inc_environmental + inc_alternate.energy +
                         emp_fossil.fuel +  majority + chamber + (1|year),
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

