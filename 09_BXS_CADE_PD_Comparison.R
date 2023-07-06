#-------------------
# BXS to CADE Default Rate comparison
#-------------------

# Cade default Rate
source("01_LCADE_Default_Rates v2.R")
source("06_BXS_Validation.R")

CADE_scorecard_byrating <-
  cade_default_data %>% 
  select(facility_id,
         rating = pd,
         pd_rate = pd_rate,
         default_flag,
         year = portfolio_date,
         portfolio = true_scorecard,
         default_date = first_default_date) %>% 
  
  mutate(year = year(year)) %>% 
  mutate(portfolio = tools::toTitleCase(tolower(portfolio)),
         portfolio = gsub("C&i", "Commercial & Industrial", portfolio),
         portfolio = gsub("Cre", "CRE Income Producing", portfolio)) %>%
  filter(portfolio %in% c("Commercial & Industrial",
                          "CRE Income Producing",
                          "Consumer",
                          "Small Business")) %>%
  filter(year < 2022, year > 2017) %>% 
  filter(rating > 0) %>% 
  mutate(default_flag = replace_na(default_flag, 0)) %>% 
  group_by(portfolio, year, rating) %>% 
  summarize(count = n(),
            default = hablar::sum_(default_flag),
            def_rate = default / count,
            pd_rate = hablar::mean_(pd_rate)) %>% 
  mutate(bank = "CADE")

#BXS Default Rates
BXS_scorecard <- 
  BXS_valid_data %>% 
  mutate(portfolio = ifelse(is.na(true_portfolio), Portfolio, true_portfolio)) %>% 
  group_by(year = portfolio_year, portfolio) %>% 
  summarize(count = n(),
    default = hablar::sum_(Default_Flag),
    def_rate = default / count,
    pd_rate = hablar::mean_(NEXT_MONTH_PD)) %>% 
  mutate(bank = "BXS")

BXS_scorecard_byrating <- 
  BXS_valid_data %>% 
  group_by(portfolio = Portfolio, year = portfolio_year,  rating = NEXT_MONTH_RISK_RATING) %>% 
  summarize(count = n(),
            default = hablar::sum_(Default_Flag),
            def_rate = default / count,
            pd_rate = hablar::mean_(NEXT_MONTH_PD)) %>% 
  mutate(bank = "BXS")


CADE_BXS_scorecard <-
  CADE_scorecard %>% 
  mutate(portfolio = tools::toTitleCase(tolower(true_scorecard)),
         portfolio = str_replace(portfolio, "C&i", "Commercial & Industrial"),
         portfolio = str_replace(portfolio, "Cre", "CRE Income Producing")) %>%
  select(-true_scorecard) %>% 
  mutate(year = year + 1) %>% 
  mutate(bank = "CADE") %>% 
  union_all(BXS_scorecard) %>% 
  mutate(portfolio_short = ifelse(portfolio == "Commercial & Industrial", "C&I", portfolio),
         portfolio_short = ifelse(portfolio == "CRE Construction and Development", "Construction", portfolio_short),
         portfolio_short = ifelse(portfolio == "CRE Income Producing", "CRE IP", portfolio_short))


CADE_BXS_scorecard_byrating <-
  CADE_scorecard_byrating %>% 
  mutate(year = year + 1) %>% 
  mutate(bank = "CADE") %>% 
  union_all(BXS_scorecard_byrating) %>% 
  group_by(portfolio, bank, rating) %>% 
  mutate(portfolio_short = ifelse(portfolio == "Commercial & Industrial", "C&I", portfolio),
         portfolio_short = ifelse(portfolio == "CRE Construction and Development", "Construction", portfolio_short),
         portfolio_short = ifelse(portfolio == "CRE Income Producing", "CRE IP", portfolio_short)) 



# First we compare Default Rates
CADE_BXS_scorecard %>%
  filter(def_rate < 1) %>% 
  filter(portfolio %in% c("Commercial & Industrial",
                          "CRE Income Producing",
                          "Consumer",
                          "Small Business")) %>% 
  
  mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
  ggplot(aes(x = year, y = def_rate, fill = bank, color = bank)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  scale_y_continuous(name = "Observed Default Rate",
                     labels = scales::percent,
                     limits = c(0, NA)) +
  facet_grid(rows = vars(portfolio_short)) +
  scale_color_viridis_d() +
  ggtitle("CADE / BXS Comparison: Historical Default Rate")+
  theme(strip.text.y = element_text(size = 8))


# Compare PD and Default Rate (1 of 2)
CADE_BXS_scorecard %>%
  filter(def_rate < 1) %>% 
  filter(portfolio %in% c("Commercial & Industrial",
                          "CRE Income Producing",
                          "Consumer",
                          "Small Business")) %>% 
  mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
  pivot_longer(cols = def_rate:pd_rate) %>% 
  mutate(name = gsub("def_rate", "Default Rate (%)", name),
         name = gsub("pd_rate", "Scorecard PD (%)", name)) %>% 
  ggplot(aes(x = year, y = value, fill = bank, color = bank)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  scale_y_continuous(name =  "PD (%)",
                     labels = scales::percent,
                     limits = c(0, NA)) +
  scale_color_viridis_d() +
  facet_grid(cols = vars(portfolio_short),
             rows = vars(name))+
  theme(strip.text.y = element_text(size = 8))+
  ggtitle("CADE / BXS Comparison: Scorecard Avg. PD")



# Compare PD and Default Rate (2 of 2)
CADE_BXS_scorecard %>%
  filter(def_rate < 1) %>% 
  filter(portfolio %in% c("Commercial & Industrial",
                          "CRE Income Producing",
                          "Consumer",
                          "Small Business")) %>% 
  mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
  pivot_longer(cols = def_rate:pd_rate) %>% 
  mutate(name = gsub("def_rate", "Default Rate (%)", name),
         name = gsub("pd_rate", "Scorecard PD (%)", name)) %>% 
  ggplot(aes(x = year, y = value, fill = name, color = name)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  scale_y_continuous(name =  "PD (%)",
                     labels = scales::percent,
                     limits = c(0, NA)) +
  scale_color_viridis_d() +
  facet_grid(rows = vars(portfolio_short),
             cols = vars(bank))+
  theme(strip.text.y = element_text(size = 8))+
  ggtitle("CADE / BXS Comparison: Scorecard Avg. PD")


# Default Rates by RIsk Rating

CADE_BXS_scorecard_byrating %>%
  filter(def_rate < 1) %>% 
  group_by(portfolio, portfolio_short, bank, rating) %>% 
  summarize(count = sum(count),
            default = sum(default),
            def_rate = default / count,
            pd_rate = hablar::mean_(pd_rate)) %>% 
  filter(portfolio %in% c("Commercial & Industrial",
                          "CRE Income Producing",
                          "Consumer",
                          "Small Business")) %>% 
  mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
  pivot_longer(cols = def_rate:pd_rate) %>% 
  mutate(name = gsub("def_rate", "Default Rate (%)", name),
         name = gsub("pd_rate", "Scorecard PD (%)", name)) %>% 
  mutate(bank = gsub("BXS", "BXS (2009-2022)", bank),
         bank = gsub("CADE", "CADE (2019-2022)", bank)) %>% 
  ggplot(aes(x = rating, y = value, fill = name, color = name)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Rating") +
  scale_y_continuous(name =  "PD (%)",
                     labels = scales::percent,
                     limits = c(0, NA)) +
  scale_color_viridis_d() +
  facet_grid(rows = vars(portfolio_short),
             cols = vars(bank))+
  theme(strip.text.y = element_text(size = 8))+
  ggtitle("CADE / BXS Comparison", "PD Calibration by Rating (Aggregated across all years)")


#-------------------------------


# The incidence rate of each group in your data is just the mean of a sum of 
# independent Bernoulli (0/1) variables - 
# each patient has its own variable receiving a value of 0 or 1, 
# you sum them up and take the mean, which is the incidence rate.
# 
# In large samples (and your sample is large), 
# the mean will be distributed normally, 
# so you can use a simple z-test to test if the two rates are different



# Testing if the indident rate is the same 
# We apply this test using stats::prop.test()
prop_test_results <- 
  CADE_BXS_scorecard %>%
  filter(def_rate < 1) %>% 
  filter(year > 2018) %>% 
    filter(portfolio %in% c("Commercial & Industrial",
                            "CRE Income Producing",
                            "Consumer",
                            "Small Business")) %>% 
    group_by(bank, portfolio) %>% 
    summarize(
                default = hablar::sum_(default),
                count = hablar::sum_(count)) %>% 
    ungroup() %>% 
    select(-bank) %>% 
    group_by(portfolio) %>% 
    nest() %>% 
    mutate(proptest = map(.x = data, 
                          .f = ~prop.test(pull(select(.x, default)),
                                          pull(select(.x, count)),
                                          correct = TRUE
                                          )
                          )
           ) %>% 
    mutate(proptest = map(.x = proptest,broom::tidy)) %>% 
  unnest(proptest) %>% 
  mutate(across(c(estimate1, estimate2, p.value, conf.low, conf.high), 
                ~scales::percent(.x, accuracy = 0.001) ))

    
write.table(prop_test_results %>% select(-data), "clipboard", sep="\t")


