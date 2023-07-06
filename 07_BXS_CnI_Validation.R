#-----------------------------
# C&I Validation
#-----------------------------

# 1. FICO Rating Distribution
BXS_valid_data %>%
  filter(Portfolio == "Commercial & Industrial") %>% 
  mutate(JOINT_FICO = ifelse(GUAR_CREDIT_SCORE == 0, CREDIT_SCORE, GUAR_CREDIT_SCORE)) %>% 
  filter(JOINT_FICO < 1000) %>% 
  ggplot(aes(x = JOINT_FICO)) +geom_histogram(bins = 100) + theme_bw() +
  scale_x_continuous(name = "Joint FICO",
                     labels = scales::number) +
  ylab("Count") +
  ggtitle("LBXS C&I Scorecard", "Joint FICO Distribution")


# 2. Default Rate by FICO Score
BXS_valid_data %>%
  filter(Portfolio == "Commercial & Industrial") %>% 
  mutate(JOINT_FICO = ifelse(GUAR_CREDIT_SCORE == 0, CREDIT_SCORE, GUAR_CREDIT_SCORE)) %>% 
  group_by(JOINT_FICO1 = cut(JOINT_FICO, breaks= seq(600, 850, by = 25), right = FALSE)) %>% 
  summarize(count = n(),
            default = hablar::sum_(Default_Flag),
            def_rate = default / count,
            rating = hablar::mean_(NEXT_MONTH_RISK_RATING),
            pd = hablar::mean_(NEXT_MONTH_PD),
            min_var = hablar::min_(JOINT_FICO)) %>%
  mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
  filter(is.na(JOINT_FICO1)==FALSE) %>% 
  ggplot(aes(x = min_var, y = def_rate)) +
  geom_point(aes(size = count))+
  geom_step(aes(y = pd), color = "grey70") + 
  geom_abline(intercept = 166/45034, slope = 0, color = "grey40", lty = "dashed")+
  theme_bw() +
  xlab("Joint FICO") +
  scale_y_continuous(name = "Observed Default Rate",
                     labels = scales::percent) +
  ggtitle("LBXS C&I Scorecard", "Default Rate by Joint FICO") 


#----------------------------------
# 3. C&I Rating Distribution

BXS_valid_data %>%
  filter(Portfolio == "Commercial & Industrial") %>% 
  mutate(portfolio_year = as.character(lubridate::year(processing_date))) %>% 
  group_by(portfolio_year, rating = NEXT_MONTH_RISK_RATING, portfolio_year) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = rating, y = count, fill = portfolio_year)) + 
  geom_bar(stat = "identity") + theme_bw() +
  xlab("Risk Rating") +
  scale_fill_grey() +
  ylab("Count")+
  theme(legend.position = "bottom") +
  ggtitle("LBXS C&I Scorecard", "Cumulative Risk Rating Distribution")

#4. C&I Default Rate by Risk Rating
  BXS_valid_data %>%
    filter(Portfolio == "Commercial & Industrial")  %>% 
    group_by(rating = NEXT_MONTH_RISK_RATING) %>% 
    summarize(count = n(),
              default = hablar::sum_(Default_Flag),
              def_rate = default / count,
              pd = hablar::mean_(NEXT_MONTH_PD)) %>% 
    mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
    filter(rating < 15) %>% 
    ggplot(aes(x = rating, y = def_rate)) +
    geom_line()+
    #geom_point(aes(size = count)) +
    geom_step(aes(y = pd), color = "grey70") + 
    ggrepel::geom_label_repel(aes(label = scales::percent(def_rate, accuracy = 0.01)), size = 2.8) +
    ggrepel::geom_label_repel(aes(label = scales::percent(pd, accuracy = 0.01)), size = 2.8, color = "blue") +
    geom_abline(intercept = 297/23631, slope = 0, color = "grey40", lty = "dashed")+
    theme_bw()+
    xlab("Risk Rating") +
    scale_y_continuous(name = "Observed Default Rate",
                       labels = scales::percent)+
    ggtitle("LBXS C&I Scorecard", "Default Rate by Risk Rating")


# C&I Default Rate by Total Points
BXS_valid_data %>%
  filter(Portfolio == "Commercial & Industrial") %>% 
  group_by(points = cut(NEXT_MONTH_TOTAL_POINTS, breaks= seq(0, 100, by = 5), right = FALSE)) %>% 
  summarize(count = n(),
            default = hablar::sum_(Default_Flag),
            def_rate = default / count,
            rating = hablar::mean_(NEXT_MONTH_RISK_RATING),
            pd = hablar::mean_(NEXT_MONTH_PD),
            min_var = hablar::min_(NEXT_MONTH_TOTAL_POINTS)) %>%
  mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
  filter(is.na(points)==FALSE) %>% 
  ggplot(aes(x = min_var, y = def_rate)) +
  geom_point(aes(size = count))+
  geom_step(aes(y = pd), color = "grey70") + 
  geom_abline(intercept = 144/21097, slope = 0, color = "grey40", lty = "dashed")+
  theme_bw() +
  xlab("Total Points") +
  scale_y_continuous(name = "Observed Default Rate",
                     labels = scales::percent) +
  ggtitle("LBXS C&I Scorecard", "Default Rate by Total Points")



#-------------------------------------------------------------
# 6. C&I Default Rate by Days Past Due
BXS_valid_data %>%
  filter(Portfolio == "Commercial & Industrial") %>% 
  filter(PAST_DUE_IN_DAYS >0) %>% 
  group_by(PAST_DUE_IN_DAYS1 = cut(PAST_DUE_IN_DAYS, breaks= seq(0, 90, by = 5), right = FALSE)) %>% 
  summarize(count = n(),
            default = hablar::sum_(Default_Flag),
            def_rate = default / count,
            rating = hablar::mean_(NEXT_MONTH_RISK_RATING),
            pd = hablar::mean_(NEXT_MONTH_PD),
            min_var = hablar::min_(PAST_DUE_IN_DAYS)) %>%
  mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
  filter(is.na(PAST_DUE_IN_DAYS1)==FALSE) %>% 
  ggplot(aes(x = min_var, y = def_rate)) +
  geom_point(aes(size = count))+
  geom_step(aes(y = pd), color = "grey70") + 
  geom_abline(intercept = 144/21097, slope = 0, color = "grey40", lty = "dashed")+
  theme_bw() +
  xlab("Current Days Past Due") +
  scale_y_continuous(name = "Observed Default Rate",
                     labels = scales::percent) +
  ggtitle("LBXS C&I Scorecard", "Default Rate by CDPD")


#--------------------------------------------
