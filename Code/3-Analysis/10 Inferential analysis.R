# Packages ---------------------------------------------------------------------

library(tidyverse)                 # data handling
library(broom)                     # data tidying
library(skimr)                     # grouped summary tables
options(dplyr.summarise.inform  
        = FALSE)                   # omit dplyr warning
library(magrittr)                  # advanced piping
library(psych)                     # describe data
library(kableExtra)                # results tables
library(car, include.only = 'vif') # Variance Inflation Factor (VIF) function
library(sandwich)                  # Cluster-robust standard errors (sandwich)
library(lmtest)                    # significance testing
library(gtools)                    # significance stars
library(ggplot2)                   # plotting
library(ggeffects)                 # plotting predicted probabilities
library(ggpubr)                    # ggarrange
library(geepack)                   # Generalized estimating equations (GEE)

# Functions --------------------------------------------------------------------

## Results extraction from the different model objects
extract_results <- function(model, type = 'default') {
  modelname = deparse(substitute(model))
  if(type == 'default'){
    new_data <- data.frame(coef(summary(model))) %>%
      rename("Std. Error" = Std..Error,
             "z value" = z.value,
             "Pr(>|z|)" = Pr...z..) %>%      
      mutate(sig = stars.pval(`Pr(>|z|)`),
             model = modelname) %>%
      rownames_to_column('Var')
    return(new_data)}
  if(type == 'robustSE'){
    new_data <- model %>% 
      mutate(sig = stars.pval(`Pr(>|z|)`),
             model = modelname) %>%
      rownames_to_column('Var')
    return(new_data)
  }
  if(type == 'GEE'){
    new_data <- data.frame(coef(summary(model))) %>% 
      rename(SE = Std.err,
             PrW = Pr...W..) %>%
      mutate(sig = gtools::stars.pval(PrW),
             model = modelname) %>% 
      rownames_to_column('Var')
    return(new_data)
  }
}

results_table <- function(results_object) {
  nm <- setNames(c(1, ncol(results_object)-3, 
                   rep(ncol(results_object)-4, 
                       length(unique(results_object$model))-1)),
                 c(rep(" ", 1), unique(results_object$model)))
  output <- results_object %>% reshape(idvar = 'Var',
                                       timevar = "model",
                                       direction = "wide") %>%
    setNames(gsub("\\.(?!\\s).*", "", names(.), perl = TRUE))
  output <- output[ , -which(names(output) %in% c("Pr(>|z|)"))] 
  output[ , -which(startsWith(names(output), "Estimate."))] %>%
    setNames(gsub("\\.\\d*", "", names(.), perl = TRUE)) %>% 
    kable(format = 'html') %>%
    kable_paper(full_width = F) %>%
    add_header_above(nm)
}

## Text table of model results for model export
results_export <- function(results_object) {
  nm <- setNames(c(1, ncol(results_object)-3, 
                                  rep(ncol(results_object)-4, 
                                      length(unique(results_object$model))-1)),
                 c(rep(" ", 1), unique(results_object$model)))
  output <- results_object %>% reshape(idvar = 'Var',
                                       timevar = "model",
                                       direction = "wide") %>%
    setNames(gsub("\\.(?!\\s).*", "", names(.), perl = TRUE)) %>%
    setNames(gsub("Std. Error", "SE", names(.), perl = TRUE)) %>%
    setNames(gsub("z value", "z", names(.), perl = TRUE)) %>%
    setNames(gsub("Estimate", "b", names(.), perl = TRUE))
  output <- output[ , -which(names(output) %in% c("Pr(>|z|)"))] 
  output[ , -which(startsWith(names(output), "b."))] %>%
    setNames(gsub("\\.\\d*", "", names(.), perl = TRUE)) %>% 
    kable(format = 'latex', booktabs = T) %>%
    kable_paper(full_width = F) %>%
    add_header_above(nm)
}

# 1: Load data -----------------------------------------------------------------
setwd("/Users/mn/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/GitHub/defeating-cap-and-trade/Data")
getwd()

hearings_committee_member_level <- 
  read.csv("Analysis/hearings_committee_member_level.csv") %>% 
  mutate(term =  as.factor(paste(year, halfyear)),
         date = as.Date(date),
         committee = paste(chamber, committee),
         committee_type = factor(committee_type,
                                 levels = c('Other committee',
                                            'Key committee')))

hearings_committee_member_level %>% select_if(is.numeric) %>% describe(skew = F)

# Investigate negative contributions
hearings_committee_member_level %>% 
  ggplot(aes(cc_fossilfuel, committee_short)) + geom_boxplot() +
  facet_wrap(vars(congress))

hearings_committee_member_level %>% 
  filter(cc_fossilfuel<0|cc_total<0) %>%
  select(congress, chamber, CID, name, cc_fossilfuel, cc_total) %>% 
  unique() %>% as.tibble()

# congress  chamber   CID          name                         
# 110       House     N00002630    Norwood, Charles W., Jr.
# Died on  February 13, 2007.  
# 111       House     N00007665    Abercrombie, Neil
# Announced resignation from congress on December 11, 2009.   
# 109       Senate    N00009945    Corzine, Jon Stevens   
# Left congress for role of Governor of New Jersey on January 17, 2006.      
# 110       Senate    N00000616    Lieberman, Joseph I.
# Won the 2006 Senate election, so probably refunding unused funds.          
# 110       Senate    N00006246    Thomas, Craig
# Died on June 4, 2007.                
# 111       Senate    N00003583    Voinovich, George Victor
# Announced that he would not seek re-election in January 2009. 
# 111       Senate    N00005178    Bond, Christopher S.
# Announced that he would not seek re-election on January 8, 2009.     
# 111       Senate    N00005675    Hutchison, Kay Bailey        
# Announced her intention to resign her Senate post in the autumn of 2009. She
# later went back on this, however, a bunch of refunds had already been made.
# 111       Senate    N00026748    Martinez, Melquiades R. (Mel)
# Announced that he would not seek re-election on December 2, 2008.

hearings_committee_member_level <- hearings_committee_member_level %>% 
  filter(cc_fossilfuel>=0&cc_total>=0)

# 2: Aggregate data ------------------------------------------------------------

grouping_vars <- c("hearing_id", "date", "year", "congress", "hearing_title", 
                   "committee", "chamber", "committee_short",
                   "committee_type", "majority", "n2_witnesses",
                   "n2_carbonintensive", "n2_contrarian","n2_denialist",
                   "n2_fossilfuel", "n2_nonprofit", "n2_government", 
                   "n2_science", "n2_environmental", "n2_business_services",
                   "n2_alternative", "n2_other", "per2_carbonintensive", 
                   "per2_contrarian", "per2_fossilfuel", "per2_nonprofit", 
                   "per2_government", "per2_science", "per2_environmental",
                   "per2_business_services", "per2_alternative", "per2_other",
                   "lobbying_total", "lobbying_fossilfuel", 
                   "per_lobbying_fossilfuel", "lobbying_fossilfuel_noUSCAP",
                   "per_lobbying_fossilfuel_noUSCAP", "term", "halfyear")


# Create weights
hearings_committee_member_level <- hearings_committee_member_level %>%
  mutate(Party = ifelse(party_code == 100, "D",
                        ifelse(party_code == 200, "R", "I")),
         majority_party = ifelse(majority == Party, 2, 1),
         seniority = ifelse(senior_party_member != 0, 2, 1),
         weights = majority_party * seniority / 1.666) 
# Dividing the weights by 1.666 ensures that the weighted sums amount to the 
# same as the unweighted sums of campaign contributions

# Aggregate data
hearings <- merge(
  hearings_committee_member_level %>% 
    group_by(across(all_of(grouping_vars))) %>%
    summarise(sum_cc_fossilfuel = sum(cc_fossilfuel)/1000000,
              sum_cc_fossilfuel_w = sum(cc_fossilfuel*weights)/1000000,
              sum_cc_fossilfuel_noUSCAP = sum(cc_fossilfuel_noUSCAP)/1000000) %>%
    ungroup() %>%
    mutate(lobbying_fossilfuel = lobbying_fossilfuel/1000000,
           lobbying_fossilfuel_noUSCAP = lobbying_fossilfuel_noUSCAP/1000000),
  hearings_committee_member_level %>%
    filter(Party != "I") %>% 
    group_by(across(all_of(c(grouping_vars, "Party")))) %>%
    summarise(sum_cc_fossilfuel = sum(cc_fossilfuel)/1000000) %>%
    ungroup() %>% 
    select(hearing_id, Party, sum_cc_fossilfuel) %>% 
    pivot_wider(names_from = Party,
                values_from = c(sum_cc_fossilfuel),
                names_prefix = "sum_cc_fossilfuel_"), 
  by = "hearing_id")

names(hearings) <- str_replace(colnames(hearings), '2_', '_')


hearings %>% select(year, lobbying_fossilfuel) %>% distinct() %>% 
  group_by(year) %>% summarise(sum(lobbying_fossilfuel))

hearings %>%
  dplyr::select(per_contrarian, 
                sum_cc_fossilfuel,
                lobbying_fossilfuel,
                majority, 
                committee_type) %>%
  psych::pairs.panels(scale = T)

hearings %>%
  filter(chamber=="House") %>% 
  dplyr::select(per_contrarian, 
                sum_cc_fossilfuel,
                lobbying_fossilfuel,
                majority, 
                committee_type) %>%
  psych::pairs.panels(scale = T)

hearings %>%
  filter(chamber=="Senate") %>%
  dplyr::select(per_contrarian, 
                sum_cc_fossilfuel,
                lobbying_fossilfuel,
                majority, 
                committee_type) %>%
  psych::pairs.panels(scale = T)

# 3: Summary statistics --------------------------------------------------------
# Congress
hearings %>% 
  mutate(committee_type = ifelse(committee_type == "Key committee", 1, 0),
         majority = ifelse(majority == "R", 1, 0)) %>% 
select(per_contrarian,
       sum_cc_fossilfuel, sum_cc_fossilfuel_w, 
       lobbying_fossilfuel, committee_type, majority) %>%
  describe() %>%
  select(n, mean, sd, min, max)

hearings %>% 
  group_by(committee_type) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)*100)
hearings %>% group_by(majority) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)*100)

# House
hearings %>% 
  filter(chamber=="House") %>% 
  mutate(committee_type = ifelse(committee_type == "Key committee", 1, 0),
         majority = ifelse(majority == "R", 1, 0)) %>% 
  select(per_contrarian,
         sum_cc_fossilfuel, sum_cc_fossilfuel_w, 
         lobbying_fossilfuel, committee_type, majority) %>%
  describe() %>%
  select(n, mean, sd, min, max)

hearings %>%
  filter(chamber=="House") %>% 
  group_by(committee_type) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)*100)
hearings %>% group_by(majority) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)*100)

# Senate
hearings %>% 
  filter(chamber=="Senate") %>%
  mutate(committee_type = ifelse(committee_type == "Key committee", 1, 0),
         majority = ifelse(majority == "R", 1, 0)) %>% 
  select(per_contrarian,
         sum_cc_fossilfuel, sum_cc_fossilfuel_w, 
         lobbying_fossilfuel, committee_type, majority) %>%
  describe() %>%
  select(n, mean, sd, min, max)

hearings %>%
  filter(chamber=="Senate") %>% 
  group_by(committee_type) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)*100)
hearings %>% group_by(majority) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)*100)

# Contrarians over time
hearings %$% 
  describeBy(per_contrarian, congress)

# 4: Models --------------------------------------------------------------------

## H1: T-Tests -----------------------------------------------------------------

# Contrarian witnesses by committee type

hearings %>% 
  ggplot(aes(sum_cc_fossilfuel, committee_type)) + 
  geom_boxplot(alpha = 0.7) +
  labs(x = "FFI campaign contributions (in $1M)",
       y = "Committee type") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 4),
                     labels = scales::number_format(accuracy = 1)) +
  theme_bw()

t.test(sum_cc_fossilfuel ~ 
         factor(committee_type, 
                levels = c("Key committee", "Other committee")),
       data = hearings)
hearings %$% describeBy(sum_cc_fossilfuel, committee_type)


hearings %>% select(sum_cc_fossilfuel_R, sum_cc_fossilfuel_D) %>% 
  pivot_longer(cols = c(sum_cc_fossilfuel_R, sum_cc_fossilfuel_D),
               names_to = "Party",
               values_to = "sum_cc_fossilfuel",
               names_prefix = "sum_cc_fossilfuel_")  %>% 
  ggplot(aes(sum_cc_fossilfuel, fill = Party)) + 
  geom_boxplot(alpha = 0.7) +
  labs(x = "Total FFI campaign contributions (in $1M)",
       y = "Committee type") +
  scale_fill_manual(values=c("#0015BC", "#FF0000"),
                    labels = c("Democratic", "Republican")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_bw()

t.test(hearings$sum_cc_fossilfuel_R, hearings$sum_cc_fossilfuel_D)
hearings %>% select(sum_cc_fossilfuel_R, sum_cc_fossilfuel_D) %>% describe()

campaigncontributions <- hearings %>% 
  select(sum_cc_fossilfuel_R, sum_cc_fossilfuel_D,
         committee_type) %>% 
  pivot_longer(cols = c(sum_cc_fossilfuel_R, sum_cc_fossilfuel_D),
               names_to = "Party",
               values_to = "sum_cc_fossilfuel",
               names_prefix = "sum_cc_fossilfuel_")  %>% 
  ggplot(aes(sum_cc_fossilfuel, committee_type, fill = Party)) + 
  geom_boxplot(alpha = 0.7) +
  labs(x = "FFI campaign contributions (in $1M)",
       y = "Committee type") +
  scale_fill_manual(values=c("#0015BC", "#FF0000"),
                    labels = c("Democratic", "Republican")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_bw(); campaigncontributions

ggsave("../Plots/campaigncontributions.png", campaigncontributions,
       device = "png", dpi = 200, width = 190, height = 60, units = "mm")

# Denialist witnesses as proportion of all scientific testimonies
hearings %>% 
  mutate(SciDen = n_science + n_denialist,
         per_denialist = n_denialist/SciDen*100) %$% 
  describe(per_denialist)

hearings %>% 
  mutate(SciDen = n_science + n_denialist,
         per_denialist = n_denialist/SciDen*100) %$% 
  t.test(per_denialist, mu = 3, alternative = "greater")

# Contrarian witnesses as proportion of all scientific and contrarian witnesses 
# at key hearings
hearings %>% 
  mutate(SciDen = n_science + n_denialist,
         per_denialist = n_denialist/SciDen*100) %$% 
  describeBy(per_denialist, committee_type)

hearings %>% 
  filter(committee_type == "Key committee") %>% 
  mutate(SciDen = n_science + n_denialist,
         per_denialist = n_denialist/SciDen*100) %>% 
  select(n_science, n_denialist, n_contrarian, SciDen, per_denialist) %$% 
  t.test(per_denialist, mu = 3, alternative = "greater")

# Contrarian witnesses as proportion of all scientific and contrarian witnesses 
# at key hearings vs other hearings
t.test(hearings$n_denialist/(hearings$n_denialist+hearings$n_science)*100 ~
         hearings$committee_type)



## H2: Logistic Regression Models ----------------------------------------------

### 1) Congress -------------------------------------------------------------------

# Note: if we ignore the correlation in the clusters, we 
# - underestimate the between-group standard deviation 
# and 
# - overestimate the within-group standard deviation
#  >> standard errors not reliable
# https://www.youtube.com/watch?v=ROdkmShnwoA&list=PLJ71tqAZr197DkSiGT7DD9dMYxkyZX0ti&index=13

nullmod <- glm(
  cbind(n_contrarian, n_witnesses-n_contrarian) ~ 1,
  family = 'binomial',
  data = hearings)

summary(contrarian <- glm(
  cbind(n_contrarian, n_witnesses-n_contrarian) ~
    sum_cc_fossilfuel + # Unweighted
    # sum_cc_fossilfuel_w + # Weighted
    lobbying_fossilfuel  +
    committee_type +
    majority,
  family = binomial(link = "logit"),
  data = hearings))

#### Cluster-robust standard errors---------------------------------------------

# Clustered within committee
vcov_c_c <- vcovCL(contrarian, ~ committee, type = "HC0", fix = TRUE)
contrarian_robustSE_c <- coeftest(contrarian, vcov_c_c)[,] %>% as.data.frame()
coeftest(contrarian, vcov_c_c)

# Clustered within term
vcov_c_t <- vcovCL(contrarian, ~ term, type = "HC0", fix = TRUE)
contrarian_robustSE_t <- coeftest(contrarian, vcov_c_t)[,] %>% as.data.frame()
coeftest(contrarian, vcov_c_t)

# Clustered within committee and term
vcov_c_ct <- vcovCL(contrarian, ~ committee + term, type = "HC0", fix = TRUE)
contrarian_robustSE_ct <- coeftest(contrarian, vcov_c_ct)[,] %>% as.data.frame()
coeftest(contrarian, vcov_c_ct)

#### Model fit -----------------------------------------------------------------
# Squared pearson correlation between observed and predicted outcomes 
corr.test(contrarian$y, contrarian$fitted.values, method="pearson")$r^2

# McFadden R2
1 - (logLik(contrarian)/logLik(nullmod))

# Model better than null model?
1-pchisq(contrarian$null.deviance-contrarian$deviance, 116-112)
# The deviance of the model with the IVs is statistically significantly lower
# compared to the null model

# Goodness of fit
1-pchisq(contrarian$deviance, 112)
# A chi square of 121.65 on 111 degrees of freedom yields a p-value of 0.26.
# The null hypothesis (i.e., the model) is not rejected. The fitted values are 
# not significantly different from the observed values.

# The Hosmer-Lemeshow goodness-of-fit test

# glmtoolbox::hltest(contrarian)

# Group Size Observed Expected
# 1    85        1     3.02
# 2    79        6     4.50
# 3    89        8     5.73
# 4    86        5     6.19
# 5    89        9     9.22
# 6    68       12     7.88
# 7    83        7     9.87
# 8    86       12    10.72
# 9   127       18    19.66
# 10   63       13    14.19
# 
# Statistic =  6.99 
# degrees of freedom =  8 
# p-value =  0.5 

#### Assumption testing --------------------------------------------------------
model.data <- augment(contrarian) %>% 
  mutate(index = 1:n()) 

## Diagnostics plots
par(mfrow = c(3, 2))
plot(contrarian, which = 1:6, id.n = 0, ask = F)
par(mfrow = c(1, 1))

## Assumption 1: Linearity in the log
# Check that interaction term of continuous IVs and their log are
# NOT significant
summary(
  glm(
    cbind(n_contrarian, n_witnesses-n_contrarian) ~
      sum_cc_fossilfuel*log(sum_cc_fossilfuel) +
      lobbying_fossilfuel*log(lobbying_fossilfuel),
    family = 'binomial',
    data = hearings))

ggarrange(
  ggplot(model.data, aes(sum_cc_fossilfuel, .fitted, color = majority)) + 
    geom_point() + geom_smooth(method = "lm") + theme_bw(),
  ggplot(model.data, aes(lobbying_fossilfuel, .fitted,  color = majority)) + 
    geom_point() + geom_smooth(method = "lm") + theme_bw(), 
  ncol = 1)

ggarrange(
  ggplot(model.data, aes(sum_cc_fossilfuel, .std.resid, color = majority)) + 
    geom_point() + geom_smooth(method = "lm") + theme_bw(),
  ggplot(model.data, aes(lobbying_fossilfuel, .std.resid, color = majority)) + 
    geom_point() + geom_smooth(method = "lm") + theme_bw(), 
  ncol = 1)
# There are no linearity issues in our model.

## Assumption 2: No multicollinearity
vif(contrarian)
# There are no multicollinearty issues in our model.

## Assumption 3: No strongly influential outliers
model.data %>% top_n(5, .cooksd) %>% 
  select(.cooksd, .std.resid)
model.data %>% filter(abs(.std.resid) > 3)
# There are no influential observations with absolute standardized residual
# values greater than 3 in our data.

## Assumption 4: Independence
# There are issues with non-independence of observations as the data is
# nested in committees and time. To address this issue, cluster robust
# standard errors are estimated.

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(alpha = .5) +
  theme_bw()

### 2) GLM by chamber -------------------------------------------------------------

#### House of Representatives --------------------------------------------------
nullmodH <- glm(
  cbind(n_contrarian, n_witnesses-n_contrarian) ~ 1,
  family = 'binomial',
  data = hearings[hearings$chamber=="House",])

summary(contrarianH <- glm(
  cbind(n_contrarian, n_witnesses-n_contrarian) ~
    sum_cc_fossilfuel + # Unweighted
    # sum_cc_fossilfuel_w + # Weighted
    lobbying_fossilfuel  +
    committee_type +
    majority,
  family = binomial(link = "logit"),
  data = hearings[hearings$chamber=="House",]))

##### Cluster robust standard errors  ------------------------------------------

# Clustered within committee
vcov_cH_c <- vcovCL(contrarianH, ~ committee, type = "HC0", fix = TRUE)
contrarianH_robustSE_c <- coeftest(contrarianH, vcov_cH_c)[,] %>% 
  as.data.frame()
coeftest(contrarianH, vcov_cH_c)

# Clustered within term
vcov_cH_t <- vcovCL(contrarianH, ~ term, type = "HC0", fix = TRUE)
contrarianH_robustSE_t <- coeftest(contrarianH, vcov_cH_t)[,] %>% 
  as.data.frame()
coeftest(contrarianH, vcov_cH_t)

# Clustered within committee and term
vcov_cH_ct <- vcovCL(contrarianH, ~ committee + term, type = "HC0", fix = TRUE)
contrarianH_robustSE_ct <- coeftest(contrarianH, vcov_cH_ct)[,] %>%
  as.data.frame()
coeftest(contrarianH, vcov_cH_ct)

##### Model fit ----------------------------------------------------------------

# Squared pearson correlation between observed and predicted outcomes 
corr.test(contrarianH$y, contrarianH$fitted.values, method="pearson")$r^2

# McFadden R2
1 - (logLik(contrarianH)/logLik(nullmodH))

# Model better than null model?
1-pchisq(contrarianH$null.deviance-contrarianH$deviance, 57-53)
# The deviance of the model with the IVs is statistically significantly lower
# compared to the null model.

# Goodness of fit
1-pchisq(contrarianH$deviance, 53)
# A chi square of 54.5 on 53 degrees of freedom yields a p-value of 0.41.
# The null hypothesis (i.e., the model) is not rejected. The fitted values are 
# not significantly different from the observed values.

# The Hosmer-Lemeshow goodness-of-fit test

# glmtoolbox::hltest(contrarianH)

# Group Size Observed Expected
# 1   56        2     2.39
# 2   57        2     3.51
# 3   48        6     4.11
# 4   49        4     4.61
# 5   45        7     4.81
# 6   24        2     3.47
# 7  113       18    17.92
# 8   41       11    11.19
# 
# Statistic =  3.64 
# degrees of freedom =  6 
# p-value =  0.7

#### Senate --------------------------------------------------------------------

nullmodS <- glm(
  cbind(n_contrarian, n_witnesses-n_contrarian) ~ 1,
  family = 'binomial',
  data = hearings[hearings$chamber=="Senate",])

summary(contrarianS <- glm(
  cbind(n_contrarian, n_witnesses-n_contrarian) ~
    sum_cc_fossilfuel +
    # sum_cc_fossilfuel_w +
    lobbying_fossilfuel +
    committee_type +
    majority,
  family = binomial(link = "logit"),
  data = hearings[hearings$chamber=="Senate",]))

##### Cluster robust standard errors  ------------------------------------------

# Clustered within committee
vcov_cS_c <- vcovCL(contrarianS, ~ committee, type = "HC0", fix = TRUE)
contrarianS_robustSE_c <- coeftest(contrarianS, vcov_cS_c)[,] %>% 
  as.data.frame()
coeftest(contrarianS, vcov_cS_c)

# Clustered within term
vcov_cS_t <- vcovCL(contrarianS, ~ term, type = "HC0", fix = TRUE)
contrarianS_robustSE_t <- coeftest(contrarianS, vcov_cS_t)[,] %>% 
  as.data.frame()
coeftest(contrarianS, vcov_cS_t)

# Clustered within committee and term
vcov_cS_ct <- vcovCL(contrarianS, ~ committee + term, type = "HC0", fix = TRUE)
contrarianS_robustSE_ct <- coeftest(contrarianS, vcov_cS_ct)[,] %>% 
  as.data.frame()
coeftest(contrarianS, vcov_cS_ct)

##### Model fit ----------------------------------------------------------------

# Squared pearson correlation between observed and predicted outcomes 
corr.test(contrarianS$y, contrarianS$fitted.values, method="pearson")$r^2

# McFadden R2
1 - (logLik(contrarianS)/logLik(nullmodS))

# Model better than null model?
1-pchisq(contrarianS$null.deviance-contrarianS$deviance, 58-54)
# The deviance of the model with the IVs is statistically significantly lower
# compared to the null model.

# Goodness of fit
1-pchisq(contrarianS$deviance, 54)
# A chi square of 57.4 on 54 degrees of freedom yields a p-value of 0.35.
# The null hypothesis (i.e., the model) is not rejected. The fitted values are 
# not significantly different from the observed values.

# The Hosmer-Lemeshow goodness-of-fit test

# glmtoolbox::hltest(contrarianS)

# Group Size Observed Expected
# 1   45        1    0.736
# 2   48        1    1.444
# 3   63        6    4.559
# 4   43        2    3.646
# 5   29        3    2.618
# 6   44        6    4.406
# 7   12        1    1.345
# 8   83        7    9.945
# 9   55       12   10.301
# 
# Statistic =  3.68 
# degrees of freedom =  7 
# p-value =  0.8 

### 3) GLM without USCAP members --------------------------------------------------

contrarian_noUSCAP <- glm(
  cbind(n_contrarian, n_witnesses-n_contrarian) ~
    sum_cc_fossilfuel_noUSCAP +     # Unweighted
    lobbying_fossilfuel_noUSCAP +
    committee_type +
    majority,
  family = 'binomial',
  data = hearings)
summary(contrarian_noUSCAP)

#### Cluster-robust standard errors-------------------------------------

# Clustered within committee
vcov_c_c_noUSCAP <- vcovCL(contrarian_noUSCAP, ~ committee, type = "HC0", fix = TRUE)
contrarian_robustSE_c_noUSCAP <- coeftest(contrarian_noUSCAP, vcov_c_c_noUSCAP)[,] %>% as.data.frame()
coeftest(contrarian_noUSCAP, vcov_c_c_noUSCAP)

# Clustered within term
vcov_c_t_noUSCAP <- vcovCL(contrarian_noUSCAP, ~ term, type = "HC0", fix = TRUE)
contrarian_robustSE_t_noUSCAP <- coeftest(contrarian_noUSCAP, vcov_c_t_noUSCAP)[,] %>% as.data.frame()
coeftest(contrarian_noUSCAP, vcov_c_t_noUSCAP)

# Clustered within committee and term
vcov_c_ct_noUSCAP <- vcovCL(contrarian_noUSCAP, ~ committee + term, type = "HC0", fix = TRUE)
contrarian_robustSE_ct_noUSCAP <- coeftest(contrarian_noUSCAP, vcov_c_ct_noUSCAP)[,] %>% as.data.frame()
coeftest(contrarian_noUSCAP, vcov_c_ct_noUSCAP)


# 5: Model comparison and export -----------------------------------------------

results_intext <- rbind(
  extract_results(contrarian), 
  extract_results(contrarian_robustSE_ct, 'robustSE')) %>% 
  mutate(across(c("Estimate", "Std. Error", "z value", "Pr(>|z|)"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee",
                      "chamberSenate" = "Chamber: Senate"))
results_table(results_intext)
# results_export(results_intext)

results_intext_House <- rbind(
  extract_results(contrarianH), 
  extract_results(contrarianH_robustSE_ct, 'robustSE')) %>% 
  mutate(across(c("Estimate", "Std. Error", "z value", "Pr(>|z|)"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee"))
results_table(results_intext_House)
# results_export(results_intext_House)

results_intext_Senate <- rbind(
  extract_results(contrarianS), 
  extract_results(contrarianS_robustSE_ct, 'robustSE')) %>% 
  mutate(across(c("Estimate", "Std. Error", "z value", "Pr(>|z|)"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee"))
results_table(results_intext_Senate)
# results_export(results_intext_House)

results_appendix <- rbind(
  extract_results(contrarian),
  extract_results(contrarian_robustSE_c, 'robustSE'),
  extract_results(contrarian_robustSE_t, 'robustSE'),
  extract_results(contrarian_robustSE_ct, 'robustSE')) %>%
  mutate(across(c("Estimate", "Std. Error", "z value", "Pr(>|z|)"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee"))
results_table(results_appendix)
results_export(results_appendix)

results_appendix_noUSCAP <- rbind(
  extract_results(contrarian_noUSCAP),
  extract_results(contrarian_robustSE_c_noUSCAP, 'robustSE'),
  extract_results(contrarian_robustSE_t_noUSCAP, 'robustSE'),
  extract_results(contrarian_robustSE_ct_noUSCAP, 'robustSE')) %>%
  mutate(across(c("Estimate", "Std. Error", "z value", "Pr(>|z|)"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel_noUSCAP" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel_noUSCAP" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee"))
results_table(results_appendix_noUSCAP)
results_export(results_appendix_noUSCAP)

results_appendix_House <- rbind(
  extract_results(contrarianH), 
  extract_results(contrarianH_robustSE_c, 'robustSE'),
  extract_results(contrarianH_robustSE_t, 'robustSE'),
  extract_results(contrarianH_robustSE_ct, 'robustSE')) %>% 
  mutate(across(c("Estimate", "Std. Error", "z value", "Pr(>|z|)"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee"))
results_table(results_appendix_House)
results_export(results_appendix_House)

results_appendix_Senate <- rbind(
  extract_results(contrarianS), 
  extract_results(contrarianS_robustSE_c, 'robustSE'),
  extract_results(contrarianS_robustSE_t, 'robustSE'),
  extract_results(contrarianS_robustSE_ct, 'robustSE')) %>% 
  mutate(across(c("Estimate", "Std. Error", "z value", "Pr(>|z|)"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee"))
results_table(results_appendix_Senate)
results_export(results_appendix_Senate)


# 6: Effect plots --------------------------------------------------------------

library(ggeffects)

## Contrarians by lobbying-----------------------------------------------------

hearings %$% describeBy(sum_cc_fossilfuel, majority)
hearings %$% describeBy(lobbying_fossilfuel, majority)

### Effect plots ---------------------------------------------------------------

eff_R <- ggpredict(contrarian, terms = 
                    c("sum_cc_fossilfuel [0.84:4.96 by=.1]",
                      "lobbying_fossilfuel [69.21, 82.16, 101.94]",
                      "majority", "committee_type")) %>% 
  as.data.frame() %>% 
  filter(facet == "R")

eff_D <- ggpredict(contrarian, terms = 
                    c("sum_cc_fossilfuel [0.75:6.54 by=.1]",
                      "lobbying_fossilfuel [93.51, 140.26,  201.85]",
                      "majority", "committee_type")) %>% 
  as.data.frame() %>%
  filter(facet == "D")

model_effects <- ggarrange(
  ggplot(eff_R, aes(x, predicted, color = group)) + geom_line() +
    facet_wrap(~panel, nrow = 2) +
    scale_y_continuous(name = "Contrarian witnesses",
                       limits = c(0,0.31),
                       breaks = seq(0, .3, by = .1),
                       labels = scales::percent(seq(0, .3, by = .1),
                                                scale = 100, accuracy = 1)) +
    scale_x_continuous(name = "FFI campaign contributions (in $1M)") +
    scale_color_viridis_d(name = "FFI Lobbying",
                         labels = c("Min: $69M", "Mean: $82M", "Max: $102M"),
                         option = "B",
                         begin = 0.8,
                         end = 0.3,) +
    labs(title = "Republican majority (2003-2006)") + 
    theme_minimal() +
    theme(legend.position="bottom", 
          plot.title = element_text(hjust = 0.5)) +
    guides(color=guide_legend(title.position="top", title.hjust = 0.5)),
  
  ggplot(eff_D, aes(x, predicted, color = group)) + geom_line() +
    facet_wrap(~panel, nrow = 2) +
    scale_y_continuous(name = "Contrarian witnesses",
                       limits = c(0,0.31),
                       breaks = seq(0, .3, by = .1),
                       labels = scales::percent(seq(0, .3, by = .1),
                                                scale = 100, accuracy = 1)) +
    scale_x_continuous(name = "FFI campaign contributions (in $1M)",
                       breaks = seq(1:6)) +
    scale_color_viridis_d(name = "FFI Lobbying",
                         labels = c("Min: $94M", "Mean: $140M", "Max: $202M"),
                         option = "B",
                         begin = 0.8,
                         end = 0.3,) +
    labs(title = "Democratic majority (2007-2010)") + 
    theme_minimal() +
    theme(legend.position="bottom", 
          plot.title = element_text(hjust = 0.5)) +
    guides(color=guide_legend(title.position="top", title.hjust = 0.5))
  ); model_effects

ggsave("../Plots/model_effects_CC_L.png", model_effects,
       device = "png", dpi = 200, width = 190, height = 120, units = "mm")

### Effect sizes ---------------------------------------------------------------

eff <- rbind(eff_R, eff_D) %>% 
  group_by(group, facet, panel) %>% 
  skim(predicted) %>% as.data.frame() %>% 
  select(facet, group, panel, numeric.p0, numeric.mean, numeric.p100) %>% 
  mutate(group = case_when(
    group %in% c(69.21, 93.51) ~ "Min",
    group %in% c(82.16, 140.26) ~ "Mean",
    TRUE ~ "Max"),
    numeric.p0 = numeric.p0*100, 
    numeric.mean = numeric.mean*100,
    numeric.p100 = numeric.p100*100) %>% 
  rename(Majority = facet,
         FFIlobbying = group, 
         CommitteeType = panel,
         Min = numeric.p0, 
         Mean = numeric.mean,
         Max = numeric.p100) %>% 
pivot_longer(c(Min, Mean, Max), 
             names_to = "FFIcampaigncontributions",
             values_to = "Contrarians"); eff

## Effect of campaign contributions
eff %>%
  pivot_wider(names_from = FFIcampaigncontributions, values_from = Contrarians) %>% 
  mutate(diff = Max - Min) #%>% summary() # 1%-7%

## Effect of lobbying expenditures
eff %>%
  pivot_wider(names_from = FFIlobbying, values_from = Contrarians) %>% 
  mutate(diff = Max - Min) #%>% summary() # 3%-14%

## Effect of committee type
eff %>%
  pivot_wider(names_from = CommitteeType, values_from = Contrarians) %>% 
  mutate(diff = `Key committee` - `Other committee`) #%>% summary() #3%-14%

## Effect of majority
eff %>%
  pivot_wider(names_from = Majority, values_from = Contrarians) %>% 
  mutate(diff = R - D) #%>% summary() # 5%-15%


## Contrarians by campaign contributions ---------------------------------------

hearings %$% describeBy(lobbying_fossilfuel, majority)
hearings %$% describeBy(sum_cc_fossilfuel, majority)

### Effect plots ---------------------------------------------------------------

eff_R <- ggpredict(contrarian, terms = 
                    c("lobbying_fossilfuel [69.21:101.94 by=.1]",
                      "sum_cc_fossilfuel [0.84, 2.1, 4.96]",
                      "majority", "committee_type")) %>% 
  as.data.frame() %>%
  filter(facet == "R")

eff_D <- ggpredict(contrarian, terms = 
                    c("lobbying_fossilfuel [93.51:201.85 by=.1]",
                      "sum_cc_fossilfuel [0.75, 2.96, 6.54]",
                      "majority", "committee_type")) %>% 
  as.data.frame() %>%
  filter(facet == "D")

model_effects <- ggarrange(
  ggplot(eff_R, aes(x, predicted, color = group)) + geom_line() +
    facet_wrap(~panel, nrow = 2) +
    scale_y_continuous(name = "Contrarian witnesses",
                       limits = c(0,0.31),
                       breaks = seq(0, .3, by = .1),
                       labels = scales::percent(seq(0, .3, by = .1),
                                                scale = 100, accuracy = 1)) +
    scale_x_continuous(name = "FFI lobbying expenditures (in $1M)",
                       breaks = pretty(69:102)) +
    scale_color_viridis_d(name = "FFI Campaign Contributions",
                          labels = c("Min", "Mean", "Max"),
                          option = "B",
                          begin = 0.8,
                          end = 0.3,) +
    labs(title = "Republican majority (2003-2006)") + 
    theme_minimal() +
    theme(legend.position="bottom", 
          plot.title = element_text(hjust = 0.5)) +
    guides(color=guide_legend(title.position="left", title.hjust = 0.5)),
  
  ggplot(eff_D, aes(x, predicted, color = group)) + geom_line() +
    facet_wrap(~panel, nrow = 2) +
    scale_y_continuous(name = "Contrarian witnesses",
                       limits = c(0,0.31),
                       breaks = seq(0, .3, by = .1),
                       labels = scales::percent(seq(0, .3, by = .1),
                                                scale = 100, accuracy = 1)) +
    scale_x_continuous(name = "FFI lobbying expenditures (in $1M)",
                       breaks = pretty(94:202)) +
    scale_color_viridis_d(name = "FFI Campaign Contributions",
                          labels = c("Min", "Mean", "Max"),
                          option = "B",
                          begin = 0.8,
                          end = 0.3,) +
    labs(title = "Democratic majority (2007-2010)") + 
    theme_minimal() +
    theme(legend.position="bottom", 
          plot.title = element_text(hjust = 0.5)) +
    guides(color=guide_legend(title.position="left", title.hjust = 0.5)),
  common.legend = T, legend = "bottom"
); model_effects

ggsave("../Plots/model_effects.png", model_effects,
       device = "png", dpi = 200, width = 190, height = 120, units = "mm")

### Effect sizes ---------------------------------------------------------------

eff <- rbind(eff_R, eff_D) %>% 
  group_by(group, facet, panel) %>% 
  skim(predicted) %>% as.data.frame() %>% 
  select(facet, group, panel, numeric.p0, numeric.mean, numeric.p100) %>% 
  mutate(group = case_when(
    group %in% c(0.75, 0.84) ~ "Min",
    group %in% c(2.96, 2.1) ~ "Mean",
    TRUE ~ "Max"),
    numeric.p0 = numeric.p0*100, 
    numeric.mean = numeric.mean*100,
    numeric.p100 = numeric.p100*100) %>% 
  rename(Majority = facet,
         FFIcampaigncontributions= group, 
         CommitteeType = panel,
         Min = numeric.p0, 
         Mean = numeric.mean,
         Max = numeric.p100) %>% 
  pivot_longer(c(Min, Mean, Max), 
               names_to = "FFIlobbying",
               values_to = "Contrarians"); eff

## Effect of campaign contributions
eff %>%
  pivot_wider(names_from = FFIcampaigncontributions, values_from = Contrarians) %>% 
  mutate(diff = Max - Min) #%>% summary() # 1%-7%

## Effect of lobbying expenditures
eff %>%
  pivot_wider(names_from = FFIlobbying, values_from = Contrarians) %>% 
  mutate(diff = Max - Min) #%>% summary() # 3%-14%

## Effect of committee type
eff %>%
  pivot_wider(names_from = CommitteeType, values_from = Contrarians) %>% 
  mutate(diff = `Key committee` - `Other committee`) #%>% summary() #3%-14%

## Effect of majority
eff %>%
  pivot_wider(names_from = Majority, values_from = Contrarians) %>% 
  mutate(diff = R - D) #%>% summary() # 5%-15%

# 7: Model validation: Generalized Estimating Equations (GEE) ------------------

## Congress --------------------------------------------------------------------


hearings <- hearings %>% group_by(hearing_id) %>% 
  mutate(ID = cur_group_id())

contrarian_gee <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                             sum_cc_fossilfuel +
                             lobbying_fossilfuel +
                             committee_type +
                             majority,
                           id = ID,
                           family = binomial,
                           data = hearings)
summary(contrarian_gee)

# Clustered within committee

# Prepare data
hearings_sorted_c <- hearings %>%
  mutate(committee = factor(committee),
         congress = factor(congress),
         date_0 = as.numeric(date) - min(as.numeric(date))) %>%
  arrange(committee, date_0) %>%
  group_by(committee) %>%
  mutate(wave = row_number()) %>%
  ungroup

contrarian_gee_c <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                             sum_cc_fossilfuel +
                             lobbying_fossilfuel +
                             committee_type +
                             majority,
                           id = committee,
                           family = binomial,
                           data = hearings_sorted_c)
summary(contrarian_gee_c)

# Clustered within term
hearings_sorted_t <- hearings %>%
  mutate(committee = factor(committee),
         congress = factor(congress),
         date_0 = as.numeric(date) - min(as.numeric(date))) %>%
  arrange(term, date_0) %>%
  group_by(term) %>%
  mutate(wave = row_number()) %>%
  ungroup

contrarian_gee_t <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                             sum_cc_fossilfuel +
                             lobbying_fossilfuel +
                             committee_type +
                             majority,
                           id = term,
                           family = binomial,
                           data = hearings_sorted_t)
summary(contrarian_gee_t)

QIC(contrarian_gee)
QIC(contrarian_gee_c)
QIC(contrarian_gee_t)

results_gee_c_appendix <- rbind(
  extract_results(contrarian_gee, 'GEE'),
  extract_results(contrarian_gee_c, 'GEE'),
  extract_results(contrarian_gee_t, 'GEE')) %>%
  mutate(across(c("Estimate", "SE", "Wald", "PrW"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee",
                      "chamberSenate" = "Chamber: Senate"))

results_gee_c_appendix
# results_table(results_gee_c_appendix)
# results_export(results_gee_c_appendix)

## House -----------------------------------------------------------------------

hearingsH <- hearings %>% 
  filter(chamber == "House") %>% 
  group_by(hearing_id) %>% 
  mutate(ID = cur_group_id())

contrarianH_gee <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                            sum_cc_fossilfuel +
                            lobbying_fossilfuel +
                            committee_type +
                            majority,
                          id = ID,
                          family = binomial,
                          data = hearingsH)
summary(contrarianH_gee)

# Clustered within committee

# Prepare data
hearingsH_sorted_c <- hearingsH %>%
  mutate(committee = factor(committee),
         congress = factor(congress),
         date_0 = as.numeric(date) - min(as.numeric(date))) %>%
  arrange(committee, date_0) %>%
  group_by(committee) %>%
  mutate(wave = row_number()) %>%
  ungroup

contrarianH_gee_c <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                              sum_cc_fossilfuel +
                              lobbying_fossilfuel +
                              committee_type +
                              majority,
                            id = committee,
                            family = binomial,
                            data = hearingsH_sorted_c)
summary(contrarianH_gee_c)

# Clustered within term
hearingsH_sorted_t <- hearingsH %>%
  mutate(committee = factor(committee),
         congress = factor(congress),
         date_0 = as.numeric(date) - min(as.numeric(date))) %>%
  arrange(term, date_0) %>%
  group_by(term) %>%
  mutate(wave = row_number()) %>%
  ungroup

contrarianH_gee_t <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                              sum_cc_fossilfuel +
                              lobbying_fossilfuel +
                              committee_type +
                              majority,
                            id = term,
                            family = binomial,
                            data = hearingsH_sorted_t)
summary(contrarianH_gee_t)

QIC(contrarianH_gee)
QIC(contrarianH_gee_c)
QIC(contrarianH_gee_t)

results_gee_H_appendix <- rbind(
  extract_results(contrarianH_gee, 'GEE'),
  extract_results(contrarianH_gee_c, 'GEE'),
  extract_results(contrarianH_gee_t, 'GEE')) %>%
  mutate(across(c("Estimate", "SE", "Wald", "PrW"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee",
                      "chamberSenate" = "Chamber: Senate"))

results_gee_H_appendix

## Senate ----------------------------------------------------------------------

hearingsS <- hearings %>% 
  filter(chamber == "Senate") %>% 
  group_by(hearing_id) %>% 
  mutate(ID = cur_group_id())

contrarianS_gee <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                           sum_cc_fossilfuel +
                           lobbying_fossilfuel +
                           committee_type +
                           majority,
                         id = ID,
                         family = binomial,
                         data = hearingsS)
summary(contrarianS_gee)

# Clustered within committee

# Prepare data
hearingsS_sorted_c <- hearingsS %>%
  mutate(committee = factor(committee),
         congress = factor(congress),
         date_0 = as.numeric(date) - min(as.numeric(date))) %>%
  arrange(committee, date_0) %>%
  group_by(committee) %>%
  mutate(wave = row_number()) %>%
  ungroup

contrarianS_gee_c <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                             sum_cc_fossilfuel +
                             lobbying_fossilfuel +
                             committee_type +
                             majority,
                           id = committee,
                           family = binomial,
                           data = hearingsS_sorted_c)
summary(contrarianS_gee_c)

# Clustered within term
hearingsS_sorted_t <- hearingsS %>%
  mutate(committee = factor(committee),
         congress = factor(congress),
         date_0 = as.numeric(date) - min(as.numeric(date))) %>%
  arrange(term, date_0) %>%
  group_by(term) %>%
  mutate(wave = row_number()) %>%
  ungroup

contrarianS_gee_t <- geeglm(cbind(n_contrarian, n_witnesses - n_contrarian) ~
                             sum_cc_fossilfuel +
                             lobbying_fossilfuel +
                             committee_type +
                             majority,
                           id = term,
                           family = binomial,
                           data = hearingsS_sorted_t)
summary(contrarianS_gee_t)

QIC(contrarianS_gee)
QIC(contrarianS_gee_c)
QIC(contrarianS_gee_t)

results_gee_S_appendix <- rbind(
  extract_results(contrarianS_gee, 'GEE'),
  extract_results(contrarianS_gee_c, 'GEE'),
  extract_results(contrarianS_gee_t, 'GEE')) %>%
  mutate(across(c("Estimate", "SE", "Wald", "PrW"), ~ round(., 2)),
         Var = recode(Var, "(Intercept)" = "Intercept",
                      "sum_cc_fossilfuel" = "FFI campaign contributions (in $1M)",
                      "lobbying_fossilfuel" = "FFI lobbying (in $1M)",
                      "majorityR" = "Majority: Republican",
                      "committee_typeKey committee" = "Committee type: Key committee",
                      "chamberSenate" = "Chamber: Senate"))

results_gee_S_appendix
