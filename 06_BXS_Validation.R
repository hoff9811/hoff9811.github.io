#--------------------------
# BXS Risk Rating Migration
#--------------------------
library(tidyverse)
library(odbc)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "AMBBOSPSSQL1\\MSSQLSERVER2016",
                 Database = "RH_BXS_2022",
                 trusted_connection = TRUE)

{
BXS_valid_data <- dbGetQuery(con, "

select 
acct_num
	, processing_date
	, default_date
	, default_year
	, portfolio_year
	, Portfolio
	, PAST_DUE_IN_DAYS
	, NEXT_MONTH_RISK_RATING
	, NEXT_MONTH_PD
	, NEXT_MONTH_TOTAL_POINTS
	, CREDIT_SCORE
	, GUAR_CREDIT_SCORE
	, TOTAL_ASSETS
	, default_portfolio
	, true_portfolio
	, b.Year_ScoreCard as year_scorecard
	, defaulted_scorecard
	, prior_scorecard
	, Default_Flag	
	, default_90dpd
	, default_nac1
	, default_chargeoff
from
(select 
	a.acct_num
	, a.processing_date
	, default_date
	, default_year
	, (year(A.processing_date)+1) as portfolio_year
	, b.Portfolio
	, PAST_DUE_IN_DAYS
	, NEXT_MONTH_RISK_RATING
	, NEXT_MONTH_PD
	, CREDIT_SCORE
	, GUAR_CREDIT_SCORE
	, TOTAL_ASSETS
	, NEXT_MONTH_TOTAL_POINTS
	, Dflt.Portfolio as default_portfolio
	, (case when Dflt.portfolio =  'Impaired' then b.portfolio else Dflt.portfolio end) as true_portfolio
	, Dflt.year_scorecard as defaulted_scorecard
	, e.year_scorecard as prior_scorecard
	, (case when Dflt.acct_num is NULL then 0 else 1 end) as Default_Flag
	, default_90dpd
	, default_nac1
	, default_chargeoff

from EriskData_2008to2022 A
left join a_BXS_Scorecard_Map B	
on A.score_card = B.score_card	
--identifies to which scorecard each loan in 'A' belongs

left join zz_exclude_list c
on c.acct_num = a.acct_num and a.processing_date = c.processing_date

left join b_Year_ScoreCard_Map2022 e  
on year(A.processing_date) = e.[year] and B.portfolio = e.[scorecard]	
-- maps loans to scorecard-year	

left join  zz_default_list Dflt
on Dflt.acct_num = a.acct_num and Dflt.default_year = (year(A.processing_date)+1)


where A.processing_date in ('11/30/2008','6/30/2008','6/30/2009','6/30/2010',
'6/30/2011','6/30/2012','6/30/2013','6/30/2014','6/30/2015','6/30/2016',
'6/30/2017','6/30/2018','6/30/2019', '6/30/2020', '6/30/2021')
and (A.current_book_principal+A.cmt_available) >'0'
and c.acct_num is null
--and Dflt.acct_num is not null
) a

left join b_Year_ScoreCard_Map2022 b  
on a.default_year = b.[year] and b.scorecard = a.true_portfolio	

order by processing_date
")
}

#Input 2: Past Due in Days
BXS_valid_data %>% 
  filter(Portfolio == "Consumer",
         Default_Flag == 1,
         PAST_DUE_IN_DAYS < 90) %>% 
ggplot(aes(x = PAST_DUE_IN_DAYS)) +geom_histogram() + theme_bw() +
  xlab("Current Days Past Due") +
  ylab("Count")+
  ggtitle("LBXS Consumer Scorecard", "CDPD Distribution for accounts which defaulted within 12 months") +
  labs(caption = "Source: each account's last 6/30 snapshot prior to default")


#--------------------------------------------
# Gini Coefficient by Year
#-------------------------------------------
allROCR_df <- NULL
for (i_scorecard in c("Consumer",
                      "Small Business",
                      "CRE Income Producing",
                      "CRE Construction and Development",
                      "Commercial & Industrial")) {
  valid_data <-
    BXS_valid_data %>% 
    filter(Portfolio == i_scorecard,
           true_portfolio == i_scorecard | is.na(true_portfolio),
           TOTAL_ASSETS != 9174000) %>%
    select(portfolio_year,
           NEXT_MONTH_PD, 
           NEXT_MONTH_RISK_RATING,
           NEXT_MONTH_TOTAL_POINTS,
           Default_Flag) %>% 
    mutate(default_flag = replace_na(Default_Flag, 0),
           pd = NEXT_MONTH_PD,
           rating = NEXT_MONTH_RISK_RATING,
           points = NEXT_MONTH_TOTAL_POINTS) 
  
  ROCRpred = ROCR::prediction(valid_data$rating, 
                              valid_data$default_flag)
  
  ROCRperf = ROCR::performance(ROCRpred, "tpr", "fpr")
  ROCRauc  = ROCR::performance(ROCRpred, "auc")
  gini     = 2 * ROCRauc@y.values[[1]] - 1
  
  ROCRgg_df <- tibble(portfolio = i_scorecard,
                      year = "Cumulative",
                      alpha = ROCRperf@alpha.values[[1]],
                      FalsePositive=ROCRperf@x.values[[1]],
                      TruePositive=ROCRperf@y.values[[1]],
                      gini = gini) 
  allROCR_df <-
    allROCR_df %>% 
    union_all(ROCRgg_df)
  
  for (i_year in c(2013:2022)) {
    valid_data <-
      BXS_valid_data %>% 
      filter(Portfolio == i_scorecard,
             true_portfolio == i_scorecard | is.na(true_portfolio),
             portfolio_year == i_year,
             TOTAL_ASSETS != 9174000) %>%
      select(portfolio_year,
             NEXT_MONTH_PD, 
             NEXT_MONTH_RISK_RATING,
             NEXT_MONTH_TOTAL_POINTS,
             Default_Flag) %>% 
      mutate(default_flag = replace_na(Default_Flag, 0),
             pd = NEXT_MONTH_PD,
             rating = NEXT_MONTH_RISK_RATING,
             points = NEXT_MONTH_TOTAL_POINTS) 
    
    ROCRpred = ROCR::prediction(valid_data$rating, 
                                valid_data$default_flag)
    
    ROCRperf = ROCR::performance(ROCRpred, "tpr", "fpr")
    ROCRauc  = ROCR::performance(ROCRpred, "auc")
    gini     = 2 * ROCRauc@y.values[[1]] - 1
    
    ROCRgg_df <- tibble(portfolio = i_scorecard,
                        year = as.character(i_year),
                        alpha = ROCRperf@alpha.values[[1]],
                        FalsePositive=ROCRperf@x.values[[1]],
                        TruePositive=ROCRperf@y.values[[1]],
                        gini = gini) 
    allROCR_df <-
      allROCR_df %>% 
      union_all(ROCRgg_df)
  }
}


allROCR_df <- 
  allROCR_df %>% 
  group_by(portfolio, year) %>% 
  nest() %>% 
  mutate(portfolio_short = ifelse(portfolio == "Commercial & Industrial", "C&I", portfolio),
         portfolio_short = ifelse(portfolio == "CRE Construction and Development", "Construction", portfolio_short),
         portfolio_short = ifelse(portfolio == "CRE Income Producing", "CRE IP", portfolio_short))
  

allROCR_df %>% 
  unnest(cols = c(data)) %>% 
  filter(year != "Cumulative") %>% 
  mutate(alpha = ifelse(alpha == Inf, NA, alpha)) %>%
  ggplot(aes(x=FalsePositive, y=TruePositive)) + 
  geom_line()+
  geom_label(data = allROCR_df %>% unnest() %>% filter(year != "Cumulative", alpha == 10),
            aes(label = paste0("gini = ", scales::percent(gini, accuracy = .01))),
             x=0.75,
             y=0.1,
             size = 2.5) +
  scale_x_continuous(name = "Percent of All Loans",
                     limits = c(0, 1),
                     labels = scales::percent,
                     breaks = c(0, 0.5, 1)) +
scale_y_continuous(name = "Percent of All Defaults",
                   limits = c(0, 1),
                   labels = scales::percent,
                   breaks = c(0, 0.5, 1)) +
  geom_abline(slope = 1) +
  theme_bw()+
  facet_grid(rows = vars(portfolio_short),
             cols = vars(year))


#---------------------
# Gini Coefficient: Cumulative
#--------------------
allROCR_df %>% 
  unnest(cols = c(data)) %>% 
  filter(year == "Cumulative") %>% 
  mutate(alpha = ifelse(alpha == Inf, NA, alpha)) %>% 
  ggplot(aes(x=FalsePositive, y=TruePositive)) + 
  geom_line(size = 1)+
  geom_label(data = allROCR_df %>% unnest() %>% filter(year == "Cumulative", alpha == 10),
             aes(label = paste0("gini = ", scales::percent(gini, accuracy = .01))),
             x=0.75,
             y=0.1,
             size = 2.5) +
  scale_x_continuous(name = "Percent of All Loans",
                     limits = c(0, 1),
                     labels = scales::percent,
                     breaks = c(0,.5,1)) +
  scale_y_continuous(name = "Percent of All Defaults",
                     limits = c(0, 1),
                     labels = scales::percent,
                     breaks = c(0,.5,1)) +
  geom_abline(slope = 1) +
  theme_bw()+
  facet_grid(cols = vars(portfolio_short)) +
  ggtitle("LBXS Scorecard Validation", "Cumulative Results")


#---------------
# Cumulative Rating Distributions

for (i_scorecard in c("Consumer",
                      "Small Business",
                      "CRE Income Producing",
                      "CRE Construction and Development",
                      "Commercial & Industrial")) {
  print(
  BXS_valid_data %>%
    filter(Portfolio == i_scorecard,
           portfolio_year == 2022) %>% 
    mutate(healthy_year = as.character(lubridate::year(processing_date))) %>% 
    group_by(healthy_year, rating = NEXT_MONTH_RISK_RATING, portfolio_year) %>% 
    summarize(count = n()) %>% 
    ggplot(aes(x = rating, y = count, fill = healthy_year)) + 
    geom_bar(stat = "identity") + theme_bw() +
    xlab("Risk Rating") +
    scale_fill_grey() +
    ylab("Count")+
    theme(legend.position = "bottom") +
    ggtitle(paste("LBXS", i_scorecard, "Scorecard"),
            "2021 Risk Rating Distribution")
  )
  
  portfolio_pd <- BXS_valid_data %>%
    filter(Portfolio == i_scorecard)  %>% 
    summarize(count = n(),
              default = hablar::sum_(Default_Flag),
              def_rate = default / count)
  
  #4. C&I Default Rate by Risk Rating
  print(
  BXS_valid_data %>%
    filter(Portfolio == i_scorecard,
           portfolio_year == 2022,
           TOTAL_ASSETS != 9174000)  %>% 
    group_by(rating = NEXT_MONTH_RISK_RATING) %>% 
    summarize(count = n(),
              default = hablar::sum_(Default_Flag),
              def_rate = default / count) %>% 
    mutate(across(default:def_rate, ~replace_na(., 0))) %>% 
    ggplot(aes(x = rating, y = def_rate, size = count)) +
    geom_point() +
    geom_abline(intercept = portfolio_pd$def_rate, slope = 0, color = "grey40", lty = "dashed")+
    theme_bw()+
    xlab("Risk Rating") +
    scale_y_continuous(name = "Observed Default Rate",
                       labels = scales::percent)+
    ggtitle(paste("LBXS", i_scorecard, "Scorecard"),
            "Default Rate by Risk Rating: 2022 Defaults")
  )
  
  
}

#-----
# Gini Coefficients by Scorecard
#----

for (i_scorecard in c("Consumer",
                      "Small Business",
                      "CRE Income Producing",
                      "CRE Construction and Development",
                      "Commercial & Industrial")) {
  # Power Curve by Scorecard
  print(
  allROCR_df %>% 
    unnest(cols = c(data)) %>% 
    filter(year %in% c("Cumulative",  2022),
           portfolio == i_scorecard) %>% 
    mutate(alpha = ifelse(alpha == Inf, NA, alpha)) %>% 
    ggplot(aes(x=FalsePositive, y=TruePositive)) + 
    geom_line(size = 1)+
    geom_point()+
    geom_label(data = allROCR_df %>% unnest() %>% filter(year %in% c("Cumulative",  2022), 
                                                         portfolio == i_scorecard,
                                                         alpha != Inf),
               aes(label = alpha),
               size = 2.75)+
    geom_label(data = allROCR_df %>% unnest() %>% filter(year %in% c("Cumulative",  2022), 
                                                         portfolio == i_scorecard,
                                                         alpha == 10),
               aes(label = paste0("gini = ", scales::percent(gini, accuracy = .01))),
               x=0.75,
               y=0.1,
               size = 2.75) +
    scale_x_continuous(name = "Percent of All Loans",
                       limits = c(0, 1),
                       labels = scales::percent,
                       breaks = c(0,.5,1)) +
    scale_y_continuous(name = "Percent of All Defaults",
                       limits = c(0, 1),
                       labels = scales::percent,
                       breaks = c(0,.5,1)) +
    geom_abline(slope = 1) +
    theme_bw()+
    facet_grid(cols = vars(year)) +
    ggtitle(paste("LBXS", i_scorecard, "Scorecard"), "2022 Annual & Cumulative Power Curves")
    )
  }
