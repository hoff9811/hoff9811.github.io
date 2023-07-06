#---------------
# BXS LGD Charts
#---------------


library(odbc)
library(tidyverse)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "AMBNAPSSQL1\\MSSQLSERVER2016",
                 Database = "RH_BXS_2022",
                 trusted_connection = TRUE)

bxs_lgdanalysis <- dbGetQuery(con, 
                              "Select *
      	      	
From(       	
select C.acct_num, V.year_scorecard, R.processing_date, default_date, portfolio, 
year(lookup_date) as default_year     	
from (            	
      select B.acct_num, min(B.processing_date) as default_date   	
      from (      	
            Select A.acct_num, A.processing_date	
            From EriskData_2008to2022 A	
            left join z_Admin_Defaults2014 as Z	
            on A.acct_num = Z.ACCT_NUM and A.processing_date = Z.PROC_DATE	
            where A.PAST_DUE_IN_DAYS >= 90  and Z.ACCT_NUM is null
            -- 90 days past due defaults
            union all	
            	
            Select acct_num, processing_date	
            From EriskData_2008to2022 A	
            where a.NON_ACCRUAL ='NAC_1' 		
            -- nonaccrual defaults
			
            union all	
            	
            select A.Acct_Num, A.Charge_Off_Date	
            from ChargeOff_2008to2022_withCC A	
            left join EriskData_2008to2022 S
            on A.Acct_Num = S.Acct_num
            -- charge-off defaults	
            ) as B
      group by B.acct_num
      ) as C      	
            	
left join EriskData_2008to2022 as R          	
on C.acct_num = R.acct_num and C.default_Date = R.processing_Date
left join a_BXS_Scorecard_Map as T           	
on T.score_card = R.score_card            	
left join b_Date_Conversion2022 as U            	
on C.default_date = U.[date]        	
left join b_Year_ScoreCard_Map2022 as V         	
on Year(U.[lookUp_Date]) = V.[Year] and T.portfolio = V.[scorecard]
where default_date not in ('11/30/2008')   
) as default_list    

/*left join YTD_ChargeOff_2008to2016 as a		-- join defaults to their charge-offs
on default_list.acct_num = a.Acct_Num */

left join ( select [ACCT_NUM],cast(avg(cast([Charge_Off_Date] as float)) as datetime) 
as avg_charge_off_date,
sum([Charge_Off_Amount]) as charge_off_amount from [dbo].[ChargeOff_2008to2022_withCC] 
where charge_off_date <='6/30/2021' or charge_off_date is null group by [ACCT_NUM]) as a
on default_list.acct_num=a.ACCT_NUM

left join (select acct_num, cast(avg(cast(processing_date as float)) as datetime) as avg_recovery_date, 
sum(recovery_amt) as recovery_amt from Recoveries_2012to2022 
--where processing_Date < '2016-07-01 00:00:00.000' 
group by acct_num) as b
on default_list.acct_num = b.acct_num

left join (select acct_num, processing_date, collateral_code from EriskData_2008to2022) as c		
on default_list.acct_num = c.acct_num and default_list.default_Date = c.processing_Date		

left join b_Prev_Month_Snapshot2022 as d		
on default_list.default_date = d.processing_date

left join 
(select acct_num, processing_date, current_book_principal from EriskData_2008to2022) as e		
on default_list.acct_num = e.acct_num and d.prev_processing_date = e.processing_date	 

left join a_Collateral_Code_Mapping as f		--map to collateral code
on  f.collateral_code = c.collateral_code		
where e.current_book_principal <> 0 and e.current_book_principal is not NULL 
and default_date <= '6/30/2021' "
)

colnames(bxs_lgdanalysis) <-
  c("acct_num",
    "year_scorecard",
    "processing_date",
    "default_date",
    "portfolio",
    "default_year",
    "acct_num2",
    "avg_charge_off_date",
    "charge_off_amount",
    "acct_num3",
    "avg_recovery_date",
    "recovery_amt",
    "acct_num4",
    "processing_date4",
    "collateral_code",
    "processing_date2",
    "prev_processing_date",
    "acct_num5",
    "processing_date3",
    "current_book_principal",
    "Collateral_code",
    "Collateral_type")

bxs_lgdanalysis <-
  bxs_lgdanalysis %>% 
  mutate(bal = ifelse(is.na(current_book_principal), 0, current_book_principal),
         rec = ifelse(is.na(recovery_amt), 0, recovery_amt),
         loss = ifelse(is.na(charge_off_amount), 0, charge_off_amount),
         lgd = (loss - rec) / bal) %>% 
  select(-c("loss", "rec", "bal"))

#----------------------
# LGD Table
BXS_lgd_ccode <-
  bxs_lgdanalysis %>% 
  mutate(co_count = ifelse(is.na(charge_off_amount), 0, 1),
         rec_count = ifelse(is.na(recovery_amt), 0, 1) * co_count) %>% 
  group_by(Collateral_type) %>% 
  summarize(count = n(),
           co_count = hablar::sum_(co_count),
           rec_count = hablar::sum_(rec_count),
           lgl = hablar::sum_(lgd) / co_count,
           lgd = hablar::sum_(lgd) / count)
           

#----------------------
# LGD Histogram
bxs_lgdanalysis %>% 
  filter(Collateral_type %in% c("CRE - Industrial",
                                "CRE - Office",
                                "CRE - Other",
                                "CRE - Residential",
                                "CRE - Retail"
  )) %>% 
  ggplot(aes(x = lgd)) + 
  geom_histogram(fill = "grey40") + theme_bw() +
  theme_bw() +
  xlab("Loss Given Default") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent,
                     limits = c(-1, 1.5)) +
  scale_y_continuous(labels = scales::number) +
  ggtitle("LBXS Loss Given Default", "Observed LGD Distribution")


bxs_lgdanalysis %>% 
  filter(!Collateral_type %in% c("CRE - Industrial",
                                "CRE - Office",
                                "CRE - Other",
                                "CRE - Residential",
                                "CRE - Retail"
  )) %>% 
  filter(is.na(Collateral_type) == FALSE) %>% 
  filter(Collateral_type != "FSA/SBA Guaranteed") %>% 
  ggplot(aes(x = lgd)) + 
  geom_histogram(fill = "grey40") + theme_bw() +
  theme_bw() +
  xlab("Loss Given Default") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent,
                     limits = c(-1, 1.5)) +
  scale_y_continuous(labels = scales::number) +
  ggtitle("LBXS Loss Given Default", "Observed LGD Distribution by Collateral Type") +
  facet_wrap(vars(Collateral_type), scales = "free_y")

#---------------------
BXS_lgd_ccode %>%
  filter(is.na(Collateral_type)==FALSE) %>% 
  mutate(across(count:lgd, ~ifelse(count < 15, NA, .))) %>% # Hide the rows with low counts
  ggplot(aes(x = reorder(Collateral_type, -lgd), y = lgd)) +
  geom_point(aes(size = count), color = "grey40") +
  theme_bw() +
  xlab("Collateral Type") +
  scale_y_continuous(name = "Observed LGD",
                     labels = scales::percent,
                     limits = c(0, NA)) +
  ggtitle("BXS Defaults", "LGD by Collateral Type") +
  theme(axis.text.x = element_text(angle = 90))