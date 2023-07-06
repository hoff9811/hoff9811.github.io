#--------------------------
# BXS One Year Lookback
#--------------------------

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "AMBBOSPSSQL1\\MSSQLSERVER2016",
                 Database = "RH_BXS_2022",
                 trusted_connection = TRUE)

BXS_oneyear_lookback <- dbGetQuery(con, "
 
 select * 
 from(
 select    
 a.acct_num as def_acct_num,
 default_date,
 b.processing_date as healthy_date,
 (case when a.portfolio =  'Impaired' then b.portfolio else a.portfolio end) as true_portfolio,
 case when a.portfolio =  'Impaired' then c.year_scorecard else a.year_scorecard end as year_scorecard,
 a.year_scorecard as defaulted_scorecard,
 c.year_scorecard as prior_scorecard,
 a.portfolio
 
 from zz_default_list a
 inner join zz_snapshot_list b
 on a.acct_num = b.acct_num and a.default_year = b.snapshot_year
 left join b_Year_ScoreCard_Map2022 as c         	
 on a.default_year = c.[Year] and b.portfolio = c.[scorecard]  
 ) as a

left join EriskData_2008to2022 d
on def_acct_num = d.acct_num and  DATEADD(year, -1, a.default_date) = d.processing_date

")

#Input 2: Past Due in Days
 BXS_oneyear_lookback %>% 
  filter(true_portfolio == "Consumer") %>% 
  ggplot(aes(x = PAST_DUE_IN_DAYS)) +geom_histogram() + theme_bw() +
  xlab("Current Days Past Due") +
  ylab("Count")+
  ggtitle("LBXS Consumer Scorecard", "CDPD Distribution for accounts which defaulted within 12 months") +
   labs(caption = "Source: each account's last 6/30 snapshot prior to default")
  
  
  BXS_oneyear_lookback %>%
    filter(SCORE_CARD > 0) %>% 
    group_by(rating = NEXT_MONTH_RISK_RATING) %>% 
    summarize(count = n()) %>% 
    ggplot(aes(x = rating, y = count, fill = year)) + 
    geom_bar(stat = "identity") + theme_bw() +
    xlab("Risk") +
    scale_fill_grey() +
    ylab("Count")+
    theme(legend.position = "bottom") +
    ggtitle("LBXS Consumer Scorecard", "Risk Rating Distribution")
  
  
test <- 
  BXS_valid_data %>% 
    left_join(BXS_oneyear_lookback, by = c("acct_num", "default_date")) %>% 
  mutate(cdpd   = ifelse(Default_Flag == 1, PAST_DUE_IN_DAYS.y, PAST_DUE_IN_DAYS.x),
         rating = ifelse(Default_Flag == 1, NEXT_MONTH_RISK_RATING.y, NEXT_MONTH_RISK_RATING.x),
         pd = ifelse(Default_Flag == 1, NEXT_MONTH_PD.y, NEXT_MONTH_PD.x)) %>% 
  
  select(acct_num,
         Portfolio,
         processing_date = processing_date.x,
         cdpd, 
         rating, 
         pd,
         Default_Flag)

#-----------

i_scorecard = "Commercial & Industrial"
valid_data <-
  test %>% 
  filter(Portfolio == i_scorecard) %>%
  select(pd, Default_Flag) %>% 
  mutate(default_flag = replace_na(Default_Flag, 0)) %>% 
  filter(is.na(pd) == FALSE,
         pd > 0)

ROCRpred = ROCR::prediction(valid_data$pd, 
                            valid_data$default_flag)
ROCRperf = ROCR::performance(ROCRpred, "tpr", "fpr")
ROCRauc = ROCR::performance(ROCRpred, "auc")
gini = 2 * ROCRauc@y.values[[1]] - 1

ROCRgg_df <- data.frame(alpha = ROCRperf@alpha.values[[1]],
                        FalsePositive=ROCRperf@x.values[[1]],
                        TruePositive=ROCRperf@y.values[[1]]
)
ROCRgg_df %>% 
  ggplot(aes(x=FalsePositive, y=TruePositive, color = alpha)) + 
  geom_line()+
  xlim(0,1)+
  ylim(0,1)+
  geom_abline(slope = 1) +
  theme_bw()+
  ggtitle(paste(i_scorecard, "Scorecard Power Curve"), 
          paste("Gini Coefficient =", scales::percent(gini, accuracy = .01)))
  