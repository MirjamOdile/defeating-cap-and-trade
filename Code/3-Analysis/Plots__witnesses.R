library(tidyverse)
library(magrittr) # proper piping with %>% 
library(xtable) # export LaTeX tables
library(ggpubr) # join plots with ggarrange()
library(dint)


theme_set(theme_minimal())

# Elsevier plot guidelines:
# Double column 190 mm
# Single column 90 mm

# library(foreign)
# library(car)
# library(gmodels)
# library(labelled)
# library(tidyverse)
# library(ggthemes)
# library(extrafont)
# library(scales)
# library(zoo)
# library(grid)

setwd("~/OneDrive/congress_committees/ArticleOne/Article_Data")

#-------------------------------------------------------------------------------
# Data loading and preparation
#-------------------------------------------------------------------------------

data <- read.csv('06_witnesses_aggregated.csv', 
                 # stringsAsFactors = T,
                 sep = "\t") %>% 
  mutate(date = as.Date(date, format = '%B %d, %Y'),
         yearmonth = as.Date(cut(date, breaks = "month"))+14, #+14 to move it to mid-month
         yearquarter = as.Date(cut(date, breaks = "quarter")),
         yearquarter_end = last_of_quarter(date),
         congress = factor(congress),
         hearing.id = factor(hearing.id),
         category = factor(ifelse(category != "Non-Governmental Organisations",
                                  category, "NGOs"),
                           levels = c("Government Officials", 
                                      "Industry Allies", "Contrarians", 
                                      "NGOs",
                                      "Scientists", "Other")),
         industry = factor(industry),
         sector = factor(sector),
         subcategory.sankey = factor(subcategory),
         subcategory = factor(ifelse(category == 'Contrarians', 
                                     'Contrarians', subcategory)),
         sub.subcategory = factor(sub.subcategory), 
         contrarian = factor(contrarian), 
         chamber = factor(ifelse(grepl('shrg', hearing.id), 'Senate', 
                          ifelse(grepl('hhrg', hearing.id), 'House', 'Joint')),
                          levels = c('House', 'Senate', 'Joint'))
         )

summary(data)
length(levels(data$hearing.id))
#-------------------------------------------------------------------------------
# Descriptives
#-------------------------------------------------------------------------------

descriptives <- data %$% round(cbind(
  N = addmargins(table(category)), 
  propC = addmargins(prop.table(table(category)))*100,
  N.H = addmargins(table(category[chamber == 'House'])),
  prop.HC = addmargins(table(category[chamber == 'House']))/1780*100,
  prop.H = addmargins(prop.table(table(category[chamber == 'House'])))*100,
  N.S = addmargins(table(category[chamber == 'Senate'])),
  prop.SC = addmargins(table(category[chamber == 'Senate']))/1780*100,
  prop.S = addmargins(prop.table(table(category[chamber == 'Senate'])))*100
  #,
  # N.J = addmargins(table(category[chamber == 'Joint'])),
  # prop.JC = addmargins(table(category[chamber == 'Joint']))/1780*100,
  # prop.J = addmargins(prop.table(table(category[chamber == 'Joint'])))*100
  ), 1)
descriptives

# # Export to LaTeX
# descriptives <- xtable(descriptives)
# align(descriptives) <- xalign(descriptives)
# digits(descriptives) <- xdigits(descriptives)
# display(descriptives) <- xdisplay(descriptives)
# descriptives

subs <- c(1,2,5,7,4,8)

descriptives.sub <- data %$% round(cbind(
  N = table(subcategory)[subs], 
  propC = prop.table(table(subcategory))[subs]*100,
  N.H = table(subcategory[chamber == 'House'])[subs],
  prop.HC = table(subcategory[chamber == 'House'])[subs]/1780*100,
  prop.H = prop.table(table(subcategory[chamber == 'House']))[subs]*100,
  N.S = table(subcategory[chamber == 'Senate'])[subs],
  prop.SC = table(subcategory[chamber == 'Senate'])[subs]/1780*100,
  prop.S = prop.table(table(subcategory[chamber == 'Senate']))[subs]*100
  # ,
  # N.J = table(subcategory[chamber == 'Joint'])[subs],
  # prop.JC = table(subcategory[chamber == 'Joint'])[subs]/1780*100,
  # prop.J = prop.table(table(subcategory[chamber == 'Joint']))[subs]*100
  ), 1)
descriptives.sub

# # Export to LaTeX
# descriptives.sub <- xtable(descriptives.sub)
# align(descriptives.sub) <- xalign(descriptives.sub)
# digits(descriptives.sub) <- xdigits(descriptives.sub)
# display(descriptives.sub) <- xdisplay(descriptives.sub)
# descriptives.sub

#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------


# 1) Timeline
#-------------------------------------------------------------------------------
# scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")) +
# scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#A6CEE3", "#1F78B4")) +
# scale_fill_manual(values = c("#bebada", "#fdb462", "#fb8072", "#8dd3c7", "#80b1d3", "#b3de69")) +

dates <- as.numeric(as.Date(c("2003-01-09", "2003-10-30", "2005-05-26",
                              "2005-06-22", "2007-01-12", "2007-10-18",
                              "2008-06-06", "2009-06-26", "2010-07-22")))
category_colours <- c("#bebada", "#fb8072", "#fdb462", 
                      "#80b1d3", "#8dd3c7", "#fdfd96")

p1 <- data %>%
  ggplot(aes(x=yearmonth)) +
  geom_bar(stat = "count", alpha = 1) + 
  geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
  labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  # scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
  theme(text = element_text(family = 'Helvetica', size = 9),
        axis.title.x = element_text(margin = margin(t = 3, b = 0)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank()); p1
  #       plot.margin = unit(c(2,0.36,0.36,0.36), "lines")) +
  # annotation_custom(grob = grid::textGrob(label = "2003 Climate\nStewardship Act",
  #                                         gp = grid::gpar(cex = 0.5), hjust = 0),
  #                   xmin = as.Date("2003-01-01"), xmax = as.Date("2003-01-01"),
  #                   ymin = 105, ymax = 105); p1

p2 <- data %>% 
  group_by(yearquarter_end, category, .drop = FALSE) %>% 
  summarise(n = n()) %>% ungroup() %>% 
  add_row(yearquarter_end = as.Date("2003-01-01"), 
          category = dput(levels(data$category)), n = 0) %>%
  mutate(category = factor(category, 
                           levels = c("Government Officials", "Industry Allies",
                                      "Contrarians","NGOs", "Scientists", 
                                      "Other"))) %>% 
  ggplot(aes(x=yearquarter_end, y = n, fill = category)) +
  geom_area(stat = "identity") +
  scale_fill_manual(values = category_colours) +
  geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
  labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  scale_y_continuous(breaks = seq(0, 250, 50)) +
  theme(text = element_text(family = 'Helvetica', size = 9),
        legend.title = element_text(face = "bold"),
        legend.position="bottom",
        axis.title.x = element_text(margin = margin(t = 3, b = -10)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 1)); p2

ggarrange(p1, p2, ncol = 1, heights = c(.7,2))

ggsave("/home/mirjam/OneDrive/congress_committees/ArticleOne/Plots/timeline_1.pdf", 
       p1, device = "pdf", dpi = 1000, width = 190, height = 30, units = "mm") 
ggsave("/home/mirjam/OneDrive/congress_committees/ArticleOne/Plots/timeline_2.pdf", 
       p2, device = "pdf", dpi = 1000, width = 190, height = 80, units = "mm") 


# 2) Yearly proportions
#-------------------------------------------------------------------------------
# Witnesses per category per hearing (n = count, x = total witnesses per hearing)
grouped <- data %>% count(year, category)
total <-  aggregate(x = grouped$n, FUN = sum, 
                    by = list(Group.date = grouped$year))
grouped <- merge(grouped, total, 
                 by.y = "Group.date", by.x = "year", all.x = TRUE)
# Absolute witnesses by category
# ggplot(grouped, aes(x = year, y = n, colour = as.factor(category))) +
#   geom_line(lwd = 1) +
#   scale_colour_manual(values = category_colours) +
#   theme_minimal()  
# Percentage per category
p3 <- ggplot(grouped, aes(x = year, y = n/x*100, colour = category)) +
  geom_line(lwd = 1) +
  scale_colour_manual(values = category_colours) +
  labs(x = "Year", y = "Proportion (%)", colour = "Category") +
  theme(text = element_text(family = 'Helvetica', size = 9),
        legend.title = element_text(face = "bold"),
        legend.position = 'bottom',
        axis.title.x = element_text(margin = margin(t = 3, b = -10)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank())+
    guides(colour = guide_legend(nrow = 3)); p3

ggsave("/home/mirjam/OneDrive/congress_committees/ArticleOne/Plots/proportion.pdf", 
       p3, device = "pdf", dpi = 1000, width = 90, height = 90, units = "mm")



# 3) Industry witnesses
#-------------------------------------------------------------------------------

# data %>% filter(category == "Industry Allies") %>% 
#   ggplot(aes(x=yearmonth)) +
#   geom_bar(stat = "count", alpha = 1) + 
#   # geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
#   labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
#   scale_x_date(date_breaks = '1 year', date_labels = "%Y",
#                limits = as.Date(c("2003-01-01", "2011-01-01"))) +
#   facet_wrap(vars(subcategory), nrow = 4, strip.position= "top") +
#   # scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
#   theme(text = element_text(family = 'Helvetica', size = 9),
#         axis.title.x = element_text(margin = margin(t = 3, b = 0)),
#         axis.title.y = element_text(margin = margin(l = 0, r = 3)),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(fill = 'gray95', colour = 'white'))


p4 <- data %>% filter(category == "Industry Allies") %>%
  mutate(subcategory = 
           recode_factor(subcategory, 
                         `Alternate Energy Production & Services` = "Alternate\nEnergy",
                         `Carbon-intensive Industry` = "Carbon-intensive\nIndustry",
                         `Fossil Fuel Industry` = "Fossil Fuel\nIndustry",
                         `Miscellaneous Business` = "Miscellaneous\nBusiness")) %>% 
  ggplot(aes(x=yearmonth, fill = subcategory)) +
  geom_bar(stat = "count", alpha = 0.8) + 
  geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
  labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  facet_grid(vars(subcategory), scales = 'free_y', space = 'free') +
  scale_y_continuous(breaks = c(0,5,10,15)) +
  theme(text = element_text(family = 'Helvetica', size = 9),
        axis.title.x = element_text(margin = margin(t = 3, b = 0)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'gray95', colour = 'white'),
        legend.position = "none"); p4

p4.hs <- data %>% filter(category == "Industry Allies") %>%
  mutate(subcategory = 
           recode_factor(subcategory, 
                         `Alternate Energy Production & Services` = "Alternate\nEnergy",
                         `Carbon-intensive Industry` = "Carbon-intensive\nIndustry",
                         `Fossil Fuel Industry` = "Fossil Fuel\nIndustry",
                         `Miscellaneous Business` = "Miscellaneous\nBusiness")) %>% 
  ggplot(aes(x=yearmonth, fill = subcategory)) +
  geom_bar(stat = "count", alpha = 0.8) + 
  # geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
  labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
  scale_x_date(date_breaks = '2 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  facet_grid(vars(chamber), vars(subcategory), scales = 'free_y', space = 'free') +
  scale_y_continuous(breaks = c(0,5,10,15)) +
  theme(text = element_text(family = 'Helvetica', size = 9),
        axis.title.x = element_text(margin = margin(t = 3, b = 0)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'gray95', colour = 'white'),
        legend.position = "none"); p4.hs

# p4.h <- data %>% filter(category == "Industry Allies",
#                         chamber == 'House') %>%
#   mutate(subcategory =
#            recode_factor(subcategory,
#                          `Alternate Energy Production & Services` = "Alternate\nEnergy",
#                          `Carbon-intensive Industry` = "Carbon-intensive\nIndustry",
#                          `Fossil Fuel Industry` = "Fossil Fuel\nIndustry",
#                          `Miscellaneous Business` = "Miscellaneous\nBusiness")) %>%
#   ggplot(aes(x=yearmonth, fill = subcategory)) +
#   geom_bar(stat = "count", alpha = 0.8) +
#   geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
#   labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
#   scale_x_date(date_breaks = '1 year', date_labels = "%Y",
#                limits = as.Date(c("2003-01-01", "2011-01-01"))) +
#   facet_grid(vars(subcategory), scales = 'free_y', space = 'free') +
#   scale_y_continuous(breaks = c(0,5,10,15)) +
#   theme(text = element_text(family = 'Helvetica', size = 9),
#         axis.title.x = element_text(margin = margin(t = 3, b = 0)),
#         axis.title.y = element_text(margin = margin(l = 0, r = 3)),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(fill = 'gray95', colour = 'white'),
#         legend.position = "none")
# 
# p4.s <- data %>% filter(category == "Industry Allies",
#                       chamber == 'Senate') %>%
#   mutate(subcategory =
#            recode_factor(subcategory,
#                          `Alternate Energy Production & Services` = "Alternate\nEnergy",
#                          `Carbon-intensive Industry` = "Carbon-intensive\nIndustry",
#                          `Fossil Fuel Industry` = "Fossil Fuel\nIndustry",
#                          `Miscellaneous Business` = "Miscellaneous\nBusiness")) %>%
#   ggplot(aes(x=yearmonth, fill = subcategory)) +
#   geom_bar(stat = "count", alpha = 0.8) +
#   geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
#   labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
#   scale_x_date(date_breaks = '1 year', date_labels = "%Y",
#                limits = as.Date(c("2003-01-01", "2011-01-01"))) +
#   facet_grid(vars(subcategory), scales = 'free_y', space = 'free') +
#   scale_y_continuous(breaks = c(0,5,10,15)) +
#   theme(text = element_text(family = 'Helvetica', size = 9),
#         axis.title.x = element_text(margin = margin(t = 3, b = 0)),
#         axis.title.y = element_text(margin = margin(l = 0, r = 3)),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(fill = 'gray95', colour = 'white'),
#         legend.position = "none")

ggarrange(p4.h, p4.s, ncol = 1)

# ggsave("/home/mirjam/OneDrive/congress_committees/ArticleOne/Plots/industry.pdf",
#        p4, device = "pdf", dpi = 1000, width = 190, height = 110, units = "mm")

ggsave("/home/mirjam/OneDrive/congress_committees/ArticleOne/Plots/industry_hs.pdf",
       p4.hs, device = "pdf", dpi = 1000, width = 190, height = 110, units = "mm")

# 3) Environmental movement, scientists and sceptics
#-------------------------------------------------------------------------------

# data %>% filter(subcategory %in% c("Contrarians", "Environmental Allies", "Scientists")) %>% 
#   ggplot(aes(x=yearmonth)) +
#   geom_bar(stat = "count", alpha = 1) + 
#   # geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
#   labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
#   scale_x_date(date_breaks = '1 year', date_labels = "%Y",
#                limits = as.Date(c("2003-01-01", "2011-01-01"))) +
#   facet_wrap(vars(subcategory), nrow = 3, strip.position= "top") +
#   # scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
#   theme(text = element_text(family = 'Helvetica', size = 9),
#         axis.title.x = element_text(margin = margin(t = 3, b = 0)),
#         axis.title.y = element_text(margin = margin(l = 0, r = 3)),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(fill = 'gray95', colour = 'white'))

p5 <- data %>% filter(subcategory %in% c("Contrarians", "Environmental Allies",
                                         "Scientists")) %>% 
  ggplot(aes(x=yearmonth, fill = subcategory)) +
  geom_bar(stat = "count", alpha = 0.8) + 
  geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
  labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  # facet_wrap(vars(subcategory), nrow = 3, strip.position = "top",
  #            scales = 'free_y', space = 'free') +
  facet_grid(vars(subcategory), scales = 'free_y', space = 'free') +
  scale_y_continuous(breaks = c(0,5,10,15)) +
  theme(text = element_text(family = 'Helvetica', size = 9),
        axis.title.x = element_text(margin = margin(t = 3, b = 0)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'gray95', colour = 'white'),
        legend.position = "none"); p5

p5.hs <- data %>% filter(subcategory %in% c("Contrarians", "Environmental Allies",
                                         "Scientists")) %>% 
  ggplot(aes(x=yearmonth, fill = subcategory)) +
  geom_bar(stat = "count", alpha = 0.8) + 
  # geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
  labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
  scale_x_date(date_breaks = '2 year', date_labels = "%Y",
               limits = as.Date(c("2003-01-01", "2011-01-01"))) +
  # facet_wrap(vars(subcategory), nrow = 3, strip.position = "top",
  #            scales = 'free_y', space = 'free') +
  facet_grid(vars(chamber), vars(subcategory), scales = 'free_y', space = 'free') +
  scale_y_continuous(breaks = c(0,5,10,15)) +
  theme(text = element_text(family = 'Helvetica', size = 9),
        axis.title.x = element_text(margin = margin(t = 3, b = 0)),
        axis.title.y = element_text(margin = margin(l = 0, r = 3)),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'gray95', colour = 'white'),
        legend.position = "none"); p5.hs

# p5.h <- data %>% filter(subcategory %in% c("Contrarians", "Environmental Allies",
#                                          "Scientists"),
#                       chamber == 'House') %>%
#   ggplot(aes(x=yearmonth, fill = subcategory)) +
#   geom_bar(stat = "count", alpha = 0.8) +
#   geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
#   labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
#   scale_x_date(date_breaks = '1 year', date_labels = "%Y",
#                limits = as.Date(c("2003-01-01", "2011-01-01"))) +
#   # facet_wrap(vars(subcategory), nrow = 3, strip.position = "top",
#   #            scales = 'free_y', space = 'free') +
#   facet_grid(vars(subcategory), scales = 'free_y', space = 'free') +
#   scale_y_continuous(breaks = c(0,5,10,15)) +
#   theme(text = element_text(family = 'Helvetica', size = 9),
#         axis.title.x = element_text(margin = margin(t = 3, b = 0)),
#         axis.title.y = element_text(margin = margin(l = 0, r = 3)),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(fill = 'gray95', colour = 'white'),
#         legend.position = "none"); p5.s
# 
# p5.s <- data %>% filter(subcategory %in% c("Contrarians", "Environmental Allies",
#                                          "Scientists"),
#                       chamber == 'Senate') %>%
#   ggplot(aes(x=yearmonth, fill = subcategory)) +
#   geom_bar(stat = "count", alpha = 0.8) +
#   geom_vline(xintercept = dates, linetype="dotted",  alpha = 0.7) +
#   labs(x = "Date", y = "Frequency", fill = "Category", title = ' ') +
#   scale_x_date(date_breaks = '1 year', date_labels = "%Y",
#                limits = as.Date(c("2003-01-01", "2011-01-01"))) +
#   # facet_wrap(vars(subcategory), nrow = 3, strip.position = "top",
#   #            scales = 'free_y', space = 'free') +
#   facet_grid(vars(subcategory), scales = 'free_y', space = 'free') +
#   scale_y_continuous(breaks = c(0,5,10,15)) +
#   theme(text = element_text(family = 'Helvetica', size = 9),
#         axis.title.x = element_text(margin = margin(t = 3, b = 0)),
#         axis.title.y = element_text(margin = margin(l = 0, r = 3)),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(fill = 'gray95', colour = 'white'),
#         legend.position = "none"); p5.s

ggarrange(p5.h, p5.s, ncol = 1)


# ggsave("/home/mirjam/OneDrive/congress_committees/ArticleOne/Plots/sides.pdf", 
#        p5, device = "pdf", dpi = 1000, width = 190, height = 90, units = "mm")

ggsave("/home/mirjam/OneDrive/congress_committees/ArticleOne/Plots/sides_hs.pdf",
       p5.hs, device = "pdf", dpi = 1000, width = 190, height = 100, units = "mm")

####################################################################################
#################                     Data 2                       #################
####################################################################################

# # Hearings with
#     # All contributions
data = read.csv('hearings_witnesses.csv', stringsAsFactors = T, sep = "\t")
data$date = as.Date(data$date, origin="1970-01-01")

# #     # Only present witnesses giving testimony
# data = read.csv('hearings_testimonies.csv', stringsAsFactors = T, sep = "\t")
# data$date = as.Date(data2$date, origin="1970-01-01")


data$administrator_p = data$administrator / data$sum_witnesses
data$industry_p      = data$industry / data$sum_witnesses
data$npo_p           = data$npo / data$sum_witnesses
data$sceptic_p       = data$sceptic / data$sum_witnesses
data$scientist_p     = data$scientist / data$sum_witnesses



