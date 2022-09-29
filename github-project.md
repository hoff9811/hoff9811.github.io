Scorecard Validation
================

## Introduction

The purpose of this project is to monitor and test the ongoing
performance of a client’s credit risk rating scorecards. The Bank’s risk
rating scorecards have been in place for over ten years, and the Bank
wants to understand whether the models have weakened over time or
whether the scorecards are still predictive on today’s loan borrowers.

Generally speaking, model performance for a credit risk scorecard
translates to: **does the scorecard correctly classify borrowers who
default on their loan as high risk prior to the loan default?**

To accomplish this goal, there are a couple of questions we have to
answer as we analyze the risk rating data:

-   How do we identify whether a borrower has defaulted?

-   How do we identify which scorecard a borrower belongs to?

-   What metrics should we use to measure the model’s accuracy?

## Data Request

The client provides a SQL database containing monthly snapshots of each
account at a given point in time. The id columns are The monthly
snapshots record both loan-level and borrower-level information. Using
this information, I track how the scorecards perform over time giving
the Bank insight into the efficacy of their model.

The monthly snapshots contain all loans spanning 2014-2022:

``` r
cade_2014to2022 %>% 
  count(portfolio_date) %>% 
  ggplot(aes(x = portfolio_date, y = n))+
  geom_point(stat = "identity")+
  geom_area(fill = "grey70")+
  theme_bw()+
  xlab("Snapshot Date")+
  ylab("Count")
```

![](github-project_files/figure-gfm/portfolio_date-1.png)<!-- -->

The data contains the following fields

``` r
cade_2014to2022 %>% colnames() %>% tibble(colnames = .)
```

    ## # A tibble: 24 x 1
    ##    colnames                   
    ##    <chr>                      
    ##  1 portfolio_date             
    ##  2 facility_id                
    ##  3 Call_Code                  
    ##  4 shadow_balance             
    ##  5 cmt_available              
    ##  6 Past_Due_in_Days           
    ##  7 Accural_Status.Non.Accrual.
    ##  8 Collateral_Code            
    ##  9 Collateral_Value           
    ## 10 scorecard                  
    ## # ... with 14 more rows

Based on discussions with the Bank to understand their data, I’ve
gathered the following information about their dataset:

1.  Default is defined using two trigger criteria: 1) **Current Days
    Past Due \> 90** or 2) **Accrual Status == “NonAccrual”.** If either
    condition is met, the account is considered as in default.

2.  The Default Date is defined as the first month-end snapshot where a
    loan enters default.

3.  A borrowers risk rating is stored in the field, *pd.*

4.  The scorecard validation analysis should be segmented by risk rating
    scorecard using the field, *scorecard.*

While our dataset has many different fields that I use for other
analyses, I can validate the scorecards performance using a subset of
the full dataset:

``` r
cade_2014to2022 %>% 
  select(facility_id, 
         portfolio_date,
         Accural_Status.Non.Accrual., 
         pd,
         Past_Due_in_Days,
         scorecard) %>% 
  glimpse()
```

    ## Rows: 801,425
    ## Columns: 6
    ## $ facility_id                 <chr> "1000539310", "1000539351", "1000539369", ~
    ## $ portfolio_date              <date> 2014-12-31, 2014-12-31, 2014-12-31, 2014-~
    ## $ Accural_Status.Non.Accrual. <chr> "Accruing", "Accruing", "Accruing", "Accru~
    ## $ pd                          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ Past_Due_in_Days            <dbl> 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 0, 0, 0, 0,~
    ## $ scorecard                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

The number of accounts across the month-end snapshot is shown below for
each of the Bank’s portfolio-models:

![](github-project_files/figure-gfm/scorecard%20count%20over%20time-1.png)<!-- -->

## Closed-end PD Analysis

Our data shows the fluid nature of the Bank’s portfolio - each month,
new accounts are added and old account roll off the books. Because of
this dynamic, it’s important to control for portfolio changes over time
when evaluating model performance. A commonly used technique to account
for this is what’s called a *closed-end analysis*:

-   At t= 0, a starting list of accounts is defined. In this analysis,
    the list of accounts consists of **all accounts in the Bank’s
    portfolio, which have never previously entered default.**

-   Over the next twelve monthly snapshots, I track whether any of these
    accounts meet the default criteria. If either default criteria is
    met, that account is flagged as a default observation.

-   The observed default rate for that year is defined as
    ![count(Defaults) / count(Total)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;count%28Defaults%29%20%2F%20count%28Total%29 "count(Defaults) / count(Total)")
    .

Since the Bank provided us data spanning, 2018-2022, we calculated
default rates for 4 years. Our closed end analysis picks a 6/30 annual
snapshot as our starting list, and then I track whether any of those
accounts default over the next 12 months. Each year’s healthy snapshot
date and default measurement window is outlined below:

| Year | Healthy Snapshot Date | Default Measurement Window |
|------|-----------------------|----------------------------|
| 2019 | 2018-06-30            | 2018-07-31 to 2019-06-30   |
| 2020 | 2019-06-30            | 2019-07-31 to 2020-06-30   |
| 2021 | 2020-06-30            | 2020-07-31 to 2021-06-30   |
| 2022 | 2021-06-30            | 2021-07-31 to 2022-06-30   |

Across the four years, we compared the number of defaults to the number
of healthy observations, yielding the annual observed default rate by
scorecard.

Overall, the default rate for the commercial scorecards (C&I and CRE)
were relatively low, while the consumer oriented models (Consumer and
Small Business) had higher observed default rates over the measurement
period.

| Portfolio      | Year | Healthy Count | Default Count | Default Rate (%) |
|----------------|------|---------------|---------------|------------------|
| C&I            | 2019 | 1,946         | 1             | 0.05%            |
| C&I            | 2020 | 1,543         | 17            | 1.12%            |
| C&I            | 2021 | 1,436         | 6             | 0.42%            |
| C&I            | 2022 | 1,264         | 5             | 0.40%            |
| CRE            | 2019 | 653           | 1             | 0.155            |
| CRE            | 2020 | 714           | 11            | 1.54%            |
| CRE            | 2021 | 555           | 5             | 0.90%            |
| CRE            | 2022 | 427           | 1             | 0.23%            |
| Small Business | 2020 | 6,417         | 116           | 1.81%            |
| Small Business | 2021 | 10,350        | 109           | 1.05%            |
| Small Business | 2022 | 6,849         | 128           | 1.87%            |
| Consumer       | 2020 | 19,650        | 265           | 1.35%            |
| Consumer       | 2021 | 17,647        | 123           | 0.70%            |
| Consumer       | 2022 | 15,434        | 103           | 0.67%            |

Tracking the default rates over time, it is clear that the Bank as a
whole saw an uptick in defaults during the 2020 observation. That data
point measures defaults spanning July 2020 - June 2021, which coincides
with the COVID-19 pandemic, thus the elevated default rates is an
expected result.

![](github-project_files/figure-gfm/default_rate-1.png)<!-- -->

The annual default rates are useful in other scorecard related excerises
such as probability of default (PD) calibration, but the scope of this
project is scorecard validation and testing. As such, the default data
can be thought of as the data set on which we will test the scorecard’s
accuracy.

## Scorecard Validation

Using the same data set, I measured the performance of each risk rating
scorecard by looking at the default rate by risk rating. The scorecard
data is segmented further based on the risk rating assigned in the
healthy snapshot.

As shown below, the Bank has a quasi-normal risk rating distribution
spanning 1-10. Most accounts receive a rating between 4-8.

![](github-project_files/figure-gfm/rating_distribution-1.png)<!-- -->

We define model performance based on the classification power the model
has in assigning defaulted borrowers as high risk customers.

![](github-project_files/figure-gfm/default_rate_by_rating-1.png)<!-- -->

As shown above, the higher risk ratings capture a large portion of the
future defaults. This is an initial indication that the models are high
performing. However, to verify this, I used a more formal process, which
quantified each scorecard’s ability to identify defaults as high-risk.

## Gini Coefficients / ROC Curves

The most common metric used to evaluate a risk rating scorecard is a
Gini coefficient, which can be derived from the scorecard’s Receiver
Operating Characteristic (ROC) curve. An ROC curve is a visual
representation of how well a classification model works.

With an ROC curve, the statistic that I am tracking for each scorecard
is the Gini coefficient, which quantifies the area under the ROC curve
(AUROC):

![Gini = 2 \* AUROC - 0.5](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Gini%20%3D%202%20%2A%20AUROC%20-%200.5 "Gini = 2 * AUROC - 0.5")

For this validation, I calculate Gini coefficients for each scorecard on
two levels of granularity:

-   **Annual ROC Curve** (uses the healthy and default observations
    exclusive to that year’s data)

-   **Cumulative ROC Curve** (aggregates the healthy and default
    observations across all years)

Certain portfolios may be affected

![](github-project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
