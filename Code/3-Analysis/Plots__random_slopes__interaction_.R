
## -----------------------------------------------------------------------------
## Investigate random slopes / interaction effects

## -----------------------------------------------------------------------------
## CONTRARIANS

# Level-2 interaction with chamber
ggarrange(
  # fossil fuel campaign contributions 
  df %>%
    group_by(committee) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = per_cc_fossilfuel, y = per_contrarians, color = chamber)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~committee, nrow = 3) +
    theme_bw(base_size = 10),
  df %>%
    group_by(committee) %>% 
    mutate(per_contrarians_AVE = mean(per_contrarians),
           per_cc_fossilfuel_AVE = mean(per_cc_fossilfuel)) %>% 
    ggplot(aes(x = per_cc_fossilfuel_AVE, y = per_contrarians_AVE, color = chamber)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~chamber, nrow = 3) +
    theme_bw(base_size = 10),
  # fossil fuel lobbying 
  df %>%
    group_by(committee) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = lobbying_fossilfuel, y = per_contrarians, color = chamber)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~committee, nrow = 3) +
    theme_bw(base_size = 10),
  df %>%
    group_by(committee) %>% 
    mutate(per_contrarians_AVE = mean(per_contrarians),
           lobbying_fossilfuel_AVE = mean(lobbying_fossilfuel)) %>% 
    ggplot(aes(x = lobbying_fossilfuel_AVE, y = per_contrarians_AVE, color = chamber)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~chamber, nrow = 3) +
    theme_bw(base_size = 10),
  common.legend = TRUE) %>% 
  annotate_figure(top = "Contrarian witnesses: Interaction between chamber and\naverage fossil fuel campaign contributions and lobbying, respectively.")


# Level-2 cross-level interaction with majority
ggarrange(
  # fossil fuel campaign contributions 
  df %>%
    group_by(committee) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = per_cc_fossilfuel, y = per_contrarians, color = majority)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~committee, nrow = 3) +
    theme_bw(base_size = 10),
  df %>%
    group_by(committee) %>% 
    mutate(per_contrarians_AVE = mean(per_contrarians),
           per_cc_fossilfuel_AVE = mean(per_cc_fossilfuel)) %>% 
    ggplot(aes(x = per_cc_fossilfuel_AVE, y = per_contrarians_AVE, color = majority)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~majority, nrow = 3) +
    theme_bw(base_size = 10),
  # fossil fuel lobbying 
  df %>%
    group_by(committee) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = lobbying_fossilfuel, y = per_contrarians, color = majority)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~committee, nrow = 3) +
    theme_bw(base_size = 10),
  df %>%
    group_by(committee) %>% 
    mutate(per_contrarians_AVE = mean(per_contrarians),
           lobbying_fossilfuel_AVE = mean(lobbying_fossilfuel)) %>% 
    ggplot(aes(x = lobbying_fossilfuel_AVE, y = per_contrarians_AVE, color = majority)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~majority, nrow = 3) +
    theme_bw(base_size = 10),
  common.legend = TRUE) %>% 
  annotate_figure(top = "Contrarian witnesses: Cross-level interaction between majority and\naverage fossil fuel campaign contributions and lobbying, respectively.")



## -----------------------------------------------------------------------------
## INDUSTRY

# Level-2 interaction with chamber
ggarrange(
  # fossil fuel campaign contributions 
  df %>%
    group_by(committee) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = per_cc_fossilfuel, y = per_fossilfuel, color = chamber)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~committee, nrow = 3) +
    theme_bw(base_size = 10),
  df %>%
    group_by(committee) %>% 
    mutate(per_fossilfuel_AVE = mean(per_fossilfuel),
           per_cc_fossilfuel_AVE = mean(per_cc_fossilfuel)) %>% 
    ggplot(aes(x = per_cc_fossilfuel_AVE, y = per_fossilfuel_AVE, color = chamber)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~chamber, nrow = 3) +
    theme_bw(base_size = 10),
  # fossil fuel lobbying 
  df %>%
    group_by(committee) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = lobbying_fossilfuel, y = per_fossilfuel, color = chamber)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~committee, nrow = 3) +
    theme_bw(base_size = 10),
  df %>%
    group_by(committee) %>% 
    mutate(per_fossilfuel_AVE = mean(per_fossilfuel),
           lobbying_fossilfuel_AVE = mean(lobbying_fossilfuel)) %>% 
    ggplot(aes(x = lobbying_fossilfuel_AVE, y = per_fossilfuel_AVE, color = chamber)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~chamber, nrow = 3) +
    theme_bw(base_size = 10),
  common.legend = TRUE) %>% 
  annotate_figure(top = "Fossil fuel industry witnesses: Interaction between chamber and\naverage fossil fuel campaign contributions and lobbying, respectively.")


# Level-2 cross-level interaction with majority
ggarrange(
  # fossil fuel campaign contributions 
  df %>%
    group_by(committee) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = per_cc_fossilfuel, y = per_fossilfuel, color = majority)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~committee, nrow = 3) +
    theme_bw(base_size = 10),
  df %>%
    group_by(committee) %>% 
    mutate(per_fossilfuel_AVE = mean(per_fossilfuel),
           per_cc_fossilfuel_AVE = mean(per_cc_fossilfuel)) %>% 
    ggplot(aes(x = per_cc_fossilfuel_AVE, y = per_fossilfuel_AVE, color = majority)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~majority, nrow = 3) +
    theme_bw(base_size = 10),
  # fossil fuel lobbying 
  df %>%
    group_by(committee) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count >= 10) %>% 
    ggplot(aes(x = lobbying_fossilfuel, y = per_fossilfuel, color = majority)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~committee, nrow = 3) +
    theme_bw(base_size = 10),
  df %>%
    group_by(committee) %>% 
    mutate(per_fossilfuel_AVE = mean(per_fossilfuel),
           lobbying_fossilfuel_AVE = mean(lobbying_fossilfuel)) %>% 
    ggplot(aes(x = lobbying_fossilfuel_AVE, y = per_fossilfuel_AVE, color = majority)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~majority, nrow = 3) +
    theme_bw(base_size = 10),
  common.legend = TRUE) %>% 
  annotate_figure(top = "Fossil fuel industry witnesses: Cross-level interaction between majority and\naverage fossil fuel campaign contributions and lobbying, respectively.")


