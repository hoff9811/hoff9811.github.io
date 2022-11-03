# Reshape C&I data into panel data
#library(tidyverse)
library(tidymodels)
library(lubridate)
library(censored)
library(odbc)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "AMBBOSPSSQL1\\MSSQLSERVER2016",
                 Database = "RH_BXS_2022",
                 trusted_connection = TRUE)

{
  BXS_cni_all <- dbGetQuery(con, "
select 
*
from
(select 
	  a.processing_date
	, a.customer_num
	, a.acct_num
	, a.original_trans_purch_date
	, a.NEXT_MONTH_RISK_RATING
	, default_date
	, default_year
	, (year(A.processing_date)+1) as portfolio_year
	, b.Portfolio
	, (case when b.Portfolio =  'Impaired' then Dflt.portfolio else b.Portfolio end) as true_portfolio
	, (case when a.processing_date = default_date then 1 else 0 end) as Default_Flag
	--, default_90dpd
	--, default_nac1
	--, default_chargeoff


from EriskData_2008to2022 A
left join a_BXS_Scorecard_Map B	
on A.score_card = B.score_card	
--identifies to which scorecard each loan in 'A' belongs


left join  (
			select 
			z.acct_num, default_date, x.processing_date, Portfolio, default_year, default_90dpd, default_nac1, default_chargeoff
			from 
			(select acct_num, default_date, default_year, default_90dpd, default_nac1, default_chargeoff from zz_default_list) z --where acct_num = 'ALS_00949000916052'
			left join (select acct_num, processing_date, score_card from EriskData_2008to2022) x 
			on x.acct_num = z.acct_num and x.processing_date = datefromparts(default_year - 1, 6, 30)

			left join a_BXS_Scorecard_Map map
			on x.score_card = map.score_card) Dflt

on Dflt.acct_num = a.acct_num


where 
(A.current_book_principal+A.cmt_available) >'0'
--and Dflt.acct_num is not null
) a


where true_portfolio = 'Commercial & Industrial' and (default_date is null or processing_date <= default_date)
order by processing_date
")
}

# TWo models. 
# 1. for origination. We are going to train a model based on what a customer looked like the first time 
# it appeared in the C&I scorecard

# Model 2: Scorecard Full
# We are going to train a model using annual default data
# We are going to assume independence in our data points. 
# All healthy observations will be taken as of 6/30 and default_flag == 0
# All default observation will use the financials from 1 year prior to default.

# C&I origination
BXS_cni_accounts <-
  BXS_cni_all %>% 
  group_by(acct_num) %>% 
  summarize(
    snapshots = n(),
    start = min(processing_date),
    end = max(processing_date),
    default = sum(Default_Flag)
  ) %>% 
  filter(year(start) > 2008)

# Default is lagged one year
BXS_defaults_test <- 
  BXS_cni_all %>%
  filter(Default_Flag == 1) %>% 
  select(acct_num, Default_Flag, processing_date, default_date) %>% 
  
  # Get rating from one year prior
  mutate(processing_date = processing_date - years(1)) %>% 
  left_join(select(BXS_cni_all, c(acct_num, processing_date, NEXT_MONTH_RISK_RATING, original_trans_purch_date)),
            by = c("acct_num", "processing_date")
            )

BXS_test <-
  BXS_cni_all %>%
  filter(Default_Flag == 0,
         month(processing_date) == 6) %>% 
  select(acct_num, Default_Flag, processing_date, NEXT_MONTH_RISK_RATING, original_trans_purch_date) %>% 
  
  union_all(BXS_defaults_test) %>% 
  
  mutate(age = round(time_length(processing_date - original_trans_purch_date, "months"), 0)) %>%
  filter(is.na(age) == FALSE) %>% 
  select(id = acct_num,
         time1 = age,
         time2 = age,
         default = Default_Flag,
         rating = NEXT_MONTH_RISK_RATING) %>% 
  mutate(time2 = time2 + 12) %>% 
  filter(rating > 0)

BXS_cni_df <-
  BXS_cni_all %>% 
  # left_join(select(BXS_cni_accounts, c(acct_num, start)),
  #           by = "acct_num") %>% 
  mutate(age = round(time_length(processing_date - original_trans_purch_date, "months"), 0)) %>% 
  filter(is.na(age) == FALSE)

library("survival")
library("ggfortify")

BXS_cni_surv_df <- 
  BXS_cni_df %>% 
  select(id = acct_num,
         time1 = age,
         time2 = age,
         default = Default_Flag,
         rating = NEXT_MONTH_RISK_RATING) %>% 
  mutate(time2 = time2 + 1) %>% 
  filter(rating > 0)
  

BXS_surv_fit <- 
  BXS_cni_surv_df %>%
  survfit(Surv(time1, time2, default) ~ 1, data = .)

summary(BXS_surv_fit, time = c(0:10)*12)


BXS_surv_fit %>%
  autoplot() +
  ylab("S(t)") +
  xlab("Time")

# Strata by risk rating
fit <- 
  BXS_test %>% 
  survfit(Surv(time1, time2, default) ~ rating, data = .)

summary(fit, time = c(0:10)*12)
fit %>%
  autoplot() +
  ylab("S(t)") +
  xlab("Time")

cox <- 
  BXS_test %>% 
  coxph(Surv(time1, time2, default) ~ rating, data = .)
summary(cox)


newdata_df <-
  expand_grid(rating = c(1:2), time1 = c(0:150)) %>% 
  mutate(id = rating, time2 = time1 + 1, default = 0)

  cox_fitted <-survfit(cox, newdata_df, id=id)
autoplot(cox_fitted)


BXS_test$fitted <- predict(cox, newdata = BXS_test, type = "survival")

BXS_test %>% 
  roc_curve(as.factor(default), fitted) %>%
  autoplot()

BXS_test %>% 
  roc_auc(as.factor(default), fitted) 


# Uses Nelson-Aalen estimator to first get cumulative hazard, and then predict
# the hazard function from that.
library("muhaz")

BXS_kphaz <- 
  kphaz.fit(BXS_cni_accounts$snapshots, BXS_cni_accounts$default, method = "nelson")

BXS_kphaz %>%
  as.data.frame() %>%
  ggplot(aes(x = time, y = haz)) +
  geom_line() +
  xlab("Time") +
  ylab("h(t)")

