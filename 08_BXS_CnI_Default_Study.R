#------------------------------
# How do C&I Borrowers Default?
#------------------------------

#------------------------------
# How do C&I Borrowers Default?
#------------------------------

BXS_default_type <- 
  BXS_valid_data %>% 
  filter(Default_Flag == 1) %>% 
  group_by(Portfolio, portfolio_year) %>% 
  count(default_90dpd, default_nac1, default_chargeoff) %>% 
  mutate(default_type1 = ifelse(default_90dpd + default_nac1 + default_chargeoff == 3, "All Default Types", NA),
         default_type2 = ifelse(default_90dpd + default_nac1 == 2, "90dpd & NAC_1", NA),
         default_type3 = ifelse(default_90dpd + default_chargeoff == 2, "90dpd & chargeoff", NA),
         default_type4 = ifelse(default_nac1 + default_chargeoff == 2,"NAC_1 & chargeoff", NA),
         default_type5 = ifelse(default_90dpd == 1, "90dpd", NA),
         default_type6 = ifelse(default_nac1 == 1, "NAC_1", NA),
         default_type7 = ifelse(default_chargeoff == 1, "chargeoff", NA)
  ) %>% 
  mutate(default_type = ifelse(is.na(default_type1) == FALSE, default_type1,
                               ifelse(is.na(default_type2) == FALSE, default_type2,
                                      ifelse(is.na(default_type3) == FALSE, default_type3,
                                             ifelse(is.na(default_type4) == FALSE, default_type4,
                                                    ifelse(is.na(default_type5) == FALSE, default_type5, 
                                                           ifelse(is.na(default_type6) == FALSE, default_type6, default_type7))))))
  ) %>% 
  select(-c(default_type1,
            default_type2,
            default_type3,
            default_type4,
            default_type5,
            default_type6,
            default_type7)) %>% 
  mutate(default_type = factor(default_type,
                               levels = c("chargeoff",
                                          "NAC_1",
                                          "NAC_1 & chargeoff",
                                          "90dpd",
                                          "90dpd & NAC_1",
                                          "90dpd & chargeoff",
                                          "All Default Types"
                               ))) %>% 
  filter(Portfolio %in% c("Consumer",
                          "Small Business",
                          "CRE Income Producing",
                          "CRE Construction and Development",
                          "Commercial & Industrial"))

# By Number
BXS_default_type %>% 
  ggplot(aes(x = portfolio_year,
             y = n,
             fill = default_type)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(Portfolio), scales = "free_y")

# By Proportion
BXS_default_type %>%
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = portfolio_year,
             y = prop,
             fill = default_type)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(Portfolio))

#------------------------------------
# 2. Do borrowers go past due prior to default?
#------------------------------------
  
#Input 2: Past Due in Days
BXS_valid_data %>% 
  filter(Portfolio == "Commercial & Industrial",
         Default_Flag == 1,
         PAST_DUE_IN_DAYS < 90) %>% 
  ggplot(aes(x = PAST_DUE_IN_DAYS)) +geom_histogram() + theme_bw() +
  xlab("Current Days Past Due") +
  ylab("Count")+
  ggtitle("LBXS C&I Scorecard", "CDPD Distribution for accounts which defaulted within 12 months") +
  labs(caption = "Source: each account's last 6/30 snapshot prior to default")

# FICO prior to default among default borrowers
BXS_valid_data %>%
  mutate(JOINT_FICO = ifelse(GUAR_CREDIT_SCORE == 0, CREDIT_SCORE, GUAR_CREDIT_SCORE)) %>% 
  filter(Portfolio == "Commercial & Industrial",
         Default_Flag == 1,
         JOINT_FICO < 1000) %>% 
  ggplot(aes(x = JOINT_FICO)) +geom_histogram(bins = 100) + theme_bw() +
  scale_x_continuous(name = "Joint FICO",
                     labels = scales::number) +
  ylab("Count") +
  ggtitle("LBXS C&I Scorecard", "Joint FICO Distribution for accounts which defaulted within 12 months") +
  labs(caption = "Source: each account's last 6/30 snapshot prior to default")

#------------------------------------
# 3. How often is data refreshed?
#------------------------------------

BXS_cni_data_updates <-
  BXS_valid_data %>%
  filter(Portfolio == "Commercial & Industrial",
         processing_date >= "2009-06-30") %>% 
  select(acct_num, processing_date, TOTAL_ASSETS) %>% 
  pivot_wider(names_from = processing_date, values_from = TOTAL_ASSETS) %>% 
  pivot_longer(-acct_num, names_to = "processing_date", values_to = "TOTAL_ASSETS") %>% 
  mutate(processing_date = as.Date(processing_date),
         next_date = processing_date %m+% years(1))

BXS_cni_data_updates <-
  BXS_cni_data_updates %>% 
  left_join(BXS_cni_data_updates, 
            by = c("acct_num", "next_date" = "processing_date")) %>% 
  mutate(data_refresh = ifelse(TOTAL_ASSETS.x == TOTAL_ASSETS.y, 0, 1)) %>% 
  filter(is.na(data_refresh) == FALSE) %>%
  select(acct_num, next_date, data_refresh) %>% 
  pivot_wider(names_from = next_date, values_from = data_refresh)

BXS_cni_data_updates %>% 
  summarize(across(-acct_num, ~hablar::sum_(.x)/n())) %>% 
  pivot_longer(everything(), names_to = "next_date", values_to = "data_refresh") %>% 
  mutate(next_date = as.Date(next_date)) 
         
  