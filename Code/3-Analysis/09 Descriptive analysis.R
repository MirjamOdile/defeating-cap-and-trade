# Packages & Setup -------------------------------------------------------------
library(magrittr)   # piping
library(psych)      # describe df_category
library(ggpubr)     # ggarrange() multiple plots
library(ggmosaic)   # mosaic plots
library(tidyverse)  # df_category handling
library(dint)       # dates
library(cowplot)
library(RColorBrewer)
library(Hmisc)

# Elsevier plot guidelines:
# Double column 190 mm
# Single column 90 mm

getwd()
setwd("/Users/mn/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/GitHub/defeating-cap-and-trade/Data")
theme_set(theme_minimal())

# Functions --------------------------------------------------------------------

# Rescale second axis
second_axis = function(y1, y2) {
  ylim1 <- range(y1, na.rm = TRUE)
  ylim2 <- range(y2, na.rm = TRUE)
  mult <- (ylim1[2] - ylim1[1]) / (ylim2[2] - ylim2[1])
  miny1 <- ylim1[1]
  miny2 <- ylim2[1]
  cast_to_y1 = function(x) {
    (mult * (x - miny2)) + miny1
  }
  yf <- cast_to_y1(y2)
  labelsyf <- pretty(y2)
  return(
    list(
      yf = yf,
      labels = labelsyf,
      breaks = cast_to_y1(labelsyf),
      zero = cast_to_y1(0)
    ))
}

# 1: Load data -----------------------------------------------------------------

hearings_committee_member_level.csv <- 
  read.csv("Analysis/hearings_committee_member_level.csv") %>% 
  mutate(term =  as.factor(paste(year, halfyear)),
         date = as.Date(date),
         committee = paste(chamber, committee))

hearings_committee_member_level.csv %>%
  select_if(is.numeric) %>% psych::describe(skew = F)

witnesses <- read.csv("Analysis/witnesses.csv", 
                      sep = "\t", stringsAsFactors = F) %>% 
  mutate(date = as.Date(date, format = '%Y-%m-%d'))


# 2: Data aggregation ----------------------------------------------------------

grouping_vars <- c("hearing_id", "date", "year", "congress", "hearing_title",
                   "committee", "chamber", "committee_short",
                   "committee_type", "majority",
                   "n_witnesses", "n_carbonintensive", "n_fossilfuel",
                   "n_government", "n_nonprofit", "n_science",
                   "n_business_services", "n_alternative", "n_other",
                   "per_carbonintensive", "per_fossilfuel", "per_government",
                   "per_nonprofit", "per_science", "per_business_services",
                   "per_alternative", "per_other", "n2_denialist",
                   "n2_witnesses", "n2_carbonintensive", "n2_contrarian",
                   "n2_fossilfuel", "n2_nonprofit", "n2_government",
                   "n2_science", "n2_environmental", "n2_business_services",
                   "n2_alternative", "n2_other", "per2_carbonintensive",
                   "per2_contrarian", "per2_fossilfuel", "per2_nonprofit",
                   "per2_government", "per2_science", "per2_environmental",
                   "per2_business_services", "per2_alternative", "per2_other",
                   "lobbying_total", "lobbying_fossilfuel",
                   "per_lobbying_fossilfuel", "term", "halfyear")

hearings <- hearings_committee_member_level.csv %>%
  group_by(across(all_of(grouping_vars))) %>%
  summarise(cc_total = sum(cc_total),
            cc_fossilfuel = sum(cc_fossilfuel),
            per_cc_fossilfuel = mean(per_cc_fossilfuel)) %>%
  ungroup() %>%
  mutate(majority = factor(majority, levels = c("R", "D")),
         presidency = factor(ifelse(date < "2009-01-20", "R", "D"),
                             levels = c("R", "D")),
         power = case_when(
           majority == "R" & presidency =="R" ~ "1:RR",
           majority == "D" & presidency =="R" ~ "2:DR",
           majority == "D" & presidency =="D" ~ "3:DD"),
         yearmonth = as.Date(cut(date, breaks = "month"))+14, #+14 to move it to mid-month
         yearquarter = as.Date(cut(date, breaks = "quarter"))+45,  #+45 to move it to mid-quarter
         yearquarter_end = last_of_quarter(date))

df_category <-
  hearings[, !(grepl("n2_|per_(?![l|cc])|per2_", 
                     colnames(hearings), perl = TRUE))] %>% 
  pivot_longer(starts_with("n") & ! starts_with("n_wit"),
               names_to = "category", values_to = "N",
               names_prefix = "n_")  %>% 
  mutate(category = factor(category,
                           levels = c("alternative", "business_services",
                                      "carbonintensive", "fossilfuel", 
                                      "nonprofit", "science",
                                      "government", "other"),
                           labels = c("Alternative Energy",
                                      "Business & Services",
                                      "Carbon-intensive Industry",
                                      "Fossil Fuel Industry",
                                      "Non-Profit Organisations",
                                      "Scientists",
                                      "Government Officials",
                                      "Other")),
  witnesses = n_witnesses) %>% 
  select(!starts_with('n_')) %>%
  mutate(per = N/witnesses) %>%
  group_by(term) %>% 
  mutate(yearterm_end = max(yearquarter_end),
         yearterm_end = replace(yearterm_end, yearterm_end == "2008-09-30",
                                "2008-12-31"),
         year_start = first_of_year(year),
         year2 = case_when(
           year %in% c(2003, 2004) ~ "2003/\n2004 ",
           year %in% c(2005, 2006) ~ "2005/\n2006 ",
           TRUE ~ paste0(year, " "))) %>% 
  ungroup()

df_contrarian <-
  hearings[, !(grepl("n_|per_(?![l|cc])|per2_",
                     colnames(hearings), perl = TRUE))] %>% 
    mutate(n2_contrarian_other = n2_contrarian - n2_denialist) %>% 
  pivot_longer(starts_with("n2") & ! starts_with("n2_wit"),
               names_to = "category", values_to = "N",
               names_prefix = "n2_") %>% 
  mutate(category = factor(category,
                           levels = c("alternative", "business_services",
                                      "carbonintensive",
                                      "contrarian", "denialist",
                                      "contrarian_other",
                                      "environmental", "fossilfuel", 
                                      "nonprofit", "science",
                                      "government", "other"),
                           labels = c("Alternative Energy",
                                      "Business & Services",
                                      "Carbon-intensive Industry",
                                      "Contrarian", "Denialist",
                                      "Other contrarian",
                                      "Environmental Non-Profit",
                                      "Fossil Fuel Industry",
                                      "Non-Profit Organisations",
                                      "Scientist",
                                      "Government Officials",
                                      "Other")),
         witnesses = n2_witnesses) %>% 
  select(!starts_with('n2_')) %>%
  mutate(per = N/witnesses) %>%
  group_by(term) %>% 
  mutate(yearterm_end = max(yearquarter_end),
         yearterm_end = replace(yearterm_end, yearterm_end == "2008-09-30",
                                "2008-12-31"),
         year_start = first_of_year(year),
         year2 = case_when(
           year %in% c(2003, 2004) ~ "2003/\n2004 ",
           year %in% c(2005, 2006) ~ "2005/\n2006 ",
           TRUE ~ paste0(year, " "))) %>% 
  ungroup()

# 3: Tables --------------------------------------------------------------------

## Contrarians by category -----------------------------------------------------
witnesses %>% filter(contrarian == "Contrarian") %>% 
  group_by(category) %>% summarise(n = n()) %>% arrange(-n) %>% 
  mutate(per = n/sum(n)*100)

## Contrarians by affiliation --------------------------------------------------
witnesses %>% 
  filter(contrarian == "Contrarian") %>% 
  group_by(category,  affiliation) %>% 
  summarise(n = n()) %>% arrange(category, -n, affiliation) %>%
  print(n=50)

## Hearings by chamber, committee type and committee ---------------------------
df_contrarian %>% select(hearing_id, chamber, committee_type, committee) %>% 
  distinct() %>% 
  group_by(chamber, committee_type, committee) %>% 
  summarise(n = n()) %>% 
  arrange(chamber, committee_type, -n)

## Hearings by chamber, committee type and majority ----------------------------
hearings %>%
  group_by(factor(majority, levels = c("R", "D")), chamber, committee_type) %>%
  get_summary_stats(per2_contrarian, type = "mean_sd") %>% 
  select(-variable)

## Witnesses by category and chamber -------------------------------------------
witnesses %>% 
  group_by(congress) %>% 
  mutate(n_witnesses = n()) %>% 
  ungroup() %>% 
  group_by(congress, n_witnesses, category) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/n_witnesses*100) %>%
  ungroup() %>% 
  select(-n_witnesses) %>% 
  pivot_wider(names_from = congress, values_from = c(count, prop))

## Contrarian witnesses by category and chamber --------------------------------
witnesses %>% 
  group_by(congress) %>% 
  mutate(n_witnesses = n()) %>% 
  ungroup() %>% 
  filter(contrarian == "Contrarian") %>% 
  group_by(congress, n_witnesses, category) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/n_witnesses*100) %>%
  ungroup() %>% 
  select(-n_witnesses) %>% 
  pivot_wider(names_from = congress, values_from = c(count, prop), 
              values_fill = 0)

## Witnesses by subcategory and chamber ----------------------------------------
witnesses %>% 
  group_by(congress) %>% 
  mutate(n_witnesses = n()) %>% 
  ungroup() %>% 
  group_by(congress, n_witnesses, subcategory) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/n_witnesses*100) %>%
  ungroup() %>% 
  select(-n_witnesses) %>% 
  pivot_wider(names_from = congress, values_from = c(count, prop),
              values_fill = 0) %>% 
  print(n=50)

## Witnesses: Non-Profits by subcategory ---------------------------------------
witnesses %>% 
  group_by(congress) %>% 
  mutate(n_witnesses = n()) %>% 
  ungroup() %>%
  filter(category=="Non-Profit Organisations") %>% 
  group_by(congress, n_witnesses) %>%
  mutate(n_ngo = n(),
         prop_ngo = n_ngo/n_witnesses*100) %>% 
  ungroup() %>% 
  group_by(congress, n_witnesses, n_ngo, prop_ngo, subcategory) %>% 
  summarise(count = n()) %>% 
  mutate(subcategory_prop_of_total = count/n_witnesses*100,
         subcategory_prop_of_ngo = count/n_ngo*100) %>% 
  arrange(subcategory, congress)

# 4: Plots ---------------------------------------------------------------------

## Timeline: Number of hearings and proposed legislation over time -------------

legislation <- data.frame(
  id = c("S.139", "H.R.4067", "S.843", "S.150",
                  "H.R.759", "S.342", "S.1151", "S.3698",
                  "S.280", "S.317", "H.R.620","S.485", 
                  "H.R.1590", "S.1201", "S.1766", "S.2191",
                  "H.R.4226", "S.3036", "H.R.1862", "H.R.2454", 
                  "S.1733", "S.2877"),
  date = as.Date(c("01-09-2003", "03-30-2004", "04-09-2003", "01-25-2005",
                   "02-10-2005", "02-10-2005", "05-26-2005", "07-20-2006",
                   "01-12-2007", "01-17-2007", "01-22-2007", "02-01-2007",
                   "03-20-2007", "04-24-2007", "07-11-2007", "10-18-2007",
                   "11-15-2007", "05-20-2008", "04-01-2009", "05-15-2009", 
                   "09-30-2009", "12-11-2009"),
                 format = "%m-%d-%Y",  origin='01-01-1970')) %>% 
  mutate(year = lubridate::year(date)); legislation

legislation_per_year <- legislation %>% 
  group_by(year) %>%
  summarise(count = n()) %>% 
  add_row(year = 2010, count = 0) %>% 
  mutate(yearstart = as.Date(paste0("01-01-", year), format = "%m-%d-%Y"),
         yearend = as.Date(paste0("12-31-", year), format = "%m-%d-%Y")) %>% 
  group_by(year) %>% 
  mutate(date = list(seq(yearstart, yearend, by="1 day"))) %>%
  unnest(cols = c(date))

hearings_per_quarter <- 
  df_category %>%
  select(hearing_id, yearquarter, witnesses) %>%
  group_by(yearquarter) %>%
  distinct() %>%
  summarise(N = n())

p1 <- ggplot() +
  geom_bar(data = hearings_per_quarter,
           aes(yearquarter, N, fill = "Quarterly count"),
           stat = "identity",  alpha = .9) +
  scale_fill_manual(name = "Climate hearings",
                    values = c("Quarterly count" = "darkgray")) +
  geom_line(data = legislation_per_year,linetype = 2,
            aes(date, count, color = "Yearly count"), 
            lwd = .4) +
  scale_color_manual(name = "Proposed cap & trade bills",
                     values = c("Yearly count" = "black")) +
  labs(x = "Time", y = "N", fill = " ", title = " ") +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  scale_y_continuous(limits = c(0,20)) +
  theme(text = element_text(family = 'Helvetica', size = 9),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom"); p1

# ggsave("../Plots/timeline_new.png", p1,
#        device = "png", dpi = 200, width = 190, height = 115, units = "mm")


## Mosaic Plot: Witnesses by category and time ---------------------------------

p2_labels <- df_category %>% 
  group_by(category) %>% 
  summarise(count = sum(N)) %>% 
  mutate(label = paste0(category,' (', count, ')'),
         label = factor(label, levels=label[order(category)]))

p2 <- df_category %>%
  group_by(congress, category, .drop = FALSE) %>%
  summarise(count = sum(N)) %>%
  ungroup() %>%
  uncount(count) %>%
  merge(., p2_labels[, c(1,3)], by = c('category')) %>% 
  mutate(category = fct_rev(category),
         label = fct_rev(label),
         congress = factor(congress)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(label, congress), fill = label), alpha = 0.7) +
  geom_mosaic_text(aes(x = product(label, congress), label = after_stat(.wt)),
                   color = 'white') +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y="Proportion", x="Congress", fill = "Category") +
  theme_mosaic() +
  scale_y_continuous(breaks = scales::breaks_pretty(10),
                     labels = scales::percent_format(accuracy = 5L)) +
  theme(axis.title.x = element_text(margin = margin(t = 10, b = -10))); p2

# ggsave("../Plots/witnesses_mosaic_congress.png", p2,
#        device = "png", dpi = 200, width = 190, height = 140, units = "mm")

## Denialists vs scientists ----------------------------------------------------

df_contrarian %>% filter(category %in% c("Denialist", "Scientist")) %>% 
  group_by(category) %>% 
  summarise(N = sum(N))

denialists_vs_contrarians <- 
  rbind(df_contrarian %>%
          filter(category %in% c("Denialist", "Scientist")) %>%
          mutate(category = factor(category,
                                   levels = c("Denialist", "Scientist")),
                 type = 'All hearings') %>%
          group_by(congress, category, type) %>%
          summarise(count = sum(N)),
        df_contrarian %>%
          filter(category %in% c("Denialist", "Scientist") &
                   committee_type == 'Key committee') %>% 
          mutate(category = factor(category,
                                   levels = c("Denialist", "Scientist")),
                 type = "Hearings in key committees") %>%
          group_by(congress, category, type) %>%
          summarise(count = sum(N))) %>% 
  ungroup() %>% 
  group_by(congress, type) %>% 
  mutate(prop = count/sum(count)) %>% 
  ungroup()
  
p3_count <-
  ggplot(aes(congress, count, fill = category, label = count),
         data = denialists_vs_contrarians) +
  geom_col(position = 'dodge', alpha = 1) +
  
  geom_text(position = position_dodge(1), vjust = -1, size = 2) +
  facet_wrap(~type, ncol = 2) +
  scale_y_continuous(limits = c(0,62)) +
  scale_color_manual(name = " ",
                     values = c("Balance between sides" = "black")) +
  labs(x = "Congress", y = "Number of witnesses", fill = "Witness category") +
  scale_fill_manual(values = brewer.pal(name="Greys",n=3)[c(3,2)]) +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica', size = 9),
        title = element_blank(),
        strip.text = element_text(margin = margin(t = 10)),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position="right",
        legend.box="vertical",
        legend.margin=margin(t = 2),
        axis.title.x = element_text(margin = margin(t = 3, b = -5)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank()); p3_count

p3_prop <-
  ggplot(aes(congress, prop, color = category, fill = category, label = count),
         data = denialists_vs_contrarians) +
  geom_col(position = 'dodge', alpha = 1) +
  geom_text(position = position_dodge(1), vjust = -1, size = 2,
            show.legend = FALSE) +
  labs(x = "Congress", y = "Proportion of total", 
       color = "Witness category", fill = "Witness category") +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = brewer.pal(name="Greys",n=8)[c(8,1)]) +
  scale_y_continuous(labels = scales::percent,
                     # breaks = scales::pretty_breaks(n = 10),
                     limits = c(0,1.04)) +
  facet_wrap(~type, ncol = 2) +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica', size = 9),
        title = element_blank(),
        strip.text = element_text(margin = margin(t = 10, b = 1)),
        plot.margin = margin(t= 2),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position="right",
        legend.box="vertical",
        legend.margin=margin(t = 2),
        axis.title.x = element_text(margin = margin(t = 3)),
        axis.text.x = element_text(margin = margin(t = -5)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank()); p3_prop

# ggsave("../Plots/denialists_vs_scientists.png", p3_prop,
#        device = "png", dpi = 200, width = 160, height = 60, units = "mm")

## Other plots -----------------------------------------------------------------

# Monthly witnesses by category
df_category %>% 
  mutate(category = 
           recode(category,
                  "Alternative Energy" = "Alternative\nEnergy",
                  "Business & Services" = "Business &\nServices",
                  "Carbon-intensive Industry" = "Carbon-intensive\nIndustry",
                  "Fossil Fuel Industry" = "Fossil Fuel\nIndustry",
                  "Non-Profit Organisations" = "Non-Profit\nOrganisations",
                  "Government Officials" = "Government\nOfficials")) %>%
  ggplot(aes(x=yearmonth, y = N, fill = category)) +
  geom_bar(stat = "identity", alpha = 0.8) + 
  labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  facet_grid(vars(category)) +
  theme(text = element_text(family = 'Helvetica', size = 9),
        axis.title.x = element_text(margin = margin(t = 3, b = 0)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Quarterly witnesses by category (in count OR proportion >> change aes())
p2_barplot <-
  df_category %>% 
  mutate(category = 
           recode(category,
                  "Alternative Energy" = "Alternative\nEnergy",
                  "Business & Services" = "Business &\nServices",
                  "Carbon-intensive Industry" = "Carbon-intensive\nIndustry",
                  "Fossil Fuel Industry" = "Fossil Fuel\nIndustry",
                  "Non-Profit Organisations" = "Non-Profit\nOrganisations",
                  "Government Officials" = "Government\nOfficials")) %>%
  group_by(yearquarter_end, category, .drop = FALSE) %>%
  summarise(count = sum(N)) %>%
  mutate(prop = count/sum(count)*100) %>%
  ungroup() %>%
  ggplot(aes(x=yearquarter_end - 45, y = prop, fill = category)) + # Proportion
  geom_bar(stat = "identity") +
  labs(x = "Time", y = "Testimonies per quarter") +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  facet_grid(rows = vars(category)) +
  theme(text = element_text(family = 'Helvetica'),
        title = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position="bottom",
        axis.title.x = element_text(margin = margin(t = 3, b = -10)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 2)); p2_barplot

# Proportion of witnesses by category and congress
p2_lineplot <- df_category %>%
  group_by(congress) %>%
  mutate(congressly_witnesses = sum(N)) %>%
  group_by(congress, category, .drop = FALSE) %>%
  summarise(per = sum(N)/congressly_witnesses) %>%
  distinct() %>%
  ggplot(aes(x = congress, y = per,  colour = category, linetype = category)) +
  geom_line(lwd = 1) +
  scale_x_continuous(breaks = seq(2003, 2010, 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Time", y = "congressly proportion of testimonies per category",
       colour = "Category", linetype = "Category") +
  theme(text = element_text(family = 'Helvetica', size = 9),
        legend.title = element_text(face = "bold"),
        legend.position = 'bottom',
        axis.title.x = element_text(margin = margin(t = 3, b = -10)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank())+
  guides(colour = guide_legend(nrow = 2)); p2_lineplot
