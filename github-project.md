Scorecard Validation
================

## Scorecard Validation

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
cade_2014to2022 %>% count(portfolio_date)
```

    ##    portfolio_date     n
    ## 1      2014-12-31 20692
    ## 2      2015-03-31 19986
    ## 3      2015-06-30 19138
    ## 4      2015-09-30 19113
    ## 5      2015-12-31 18516
    ## 6      2016-03-31 18193
    ## 7      2016-06-30 17887
    ## 8      2016-09-30 17881
    ## 9      2016-12-31 17743
    ## 10     2017-03-31 17502
    ## 11     2017-06-30 17286
    ## 12     2017-09-30 17637
    ## 13     2017-12-31 17784
    ## 14     2018-03-31 17664
    ## 15     2018-06-30 18008
    ## 16     2018-09-30 19159
    ## 17     2018-12-31 19913
    ## 18     2019-03-31 30240
    ## 19     2019-06-30 28534
    ## 20     2019-09-30 28583
    ## 21     2019-12-31 28097
    ## 22     2020-03-31 27562
    ## 23     2020-06-30 30727
    ## 24     2020-09-30 30485
    ## 25     2020-12-31 29617
    ## 26     2021-03-31 28422
    ## 27     2021-06-30 24799
    ## 28     2021-09-30 24059
    ## 29     2021-11-30 23526
    ## 30     2021-12-31 22346
    ## 31     2022-01-31 22168
    ## 32     2022-02-28 22014
    ## 33     2022-03-31 21823
    ## 34     2022-04-30 21593
    ## 35     2022-05-31 21413
    ## 36     2022-06-30 21315

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
         Past_Due_in_Days,
         scorecard) %>% 
  glimpse()
```

    ## Rows: 801,425
    ## Columns: 5
    ## $ facility_id                 <chr> "1000539310", "1000539351", "1000539369", ~
    ## $ portfolio_date              <date> 2014-12-31, 2014-12-31, 2014-12-31, 2014-~
    ## $ Accural_Status.Non.Accrual. <chr> "Accruing", "Accruing", "Accruing", "Accru~
    ## $ Past_Due_in_Days            <dbl> 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 0, 0, 0, 0,~
    ## $ scorecard                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

The number of accounts across the month-end snapshot is shown below for
each of the Bank’s portfolio-models:

![](github-project_files/figure-gfm/scorecard%20count%20over%20time-1.png)<!-- -->

## Closed-end PD Analysis

Our data shows the fluid nature of the Bank’s portfolio - each month,
new accounts are added and old account roll off the books. Because of
this dynamic, it’s important to control for portfolio changes over time
when evaluating model performance. A commonly used techinque to account
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

-   Since we have data spanning, 2018-2021, we can calculate default
    rates for 4 years

| Year | Healthy Snapshot Date | Default Measurement Window |
|------|-----------------------|----------------------------|
| 2019 | 2018-06-30            | 2018-07-31 to 2019-06-30   |
| 2020 | 2019-06-30            | 2019-07-31 to 2020-06-30   |
| 2021 | 2020-06-30            | 2020-07-31 to 2021-06-30   |
| 2022 | 2021-06-30            | 2021-07-31 to 2022-06-30   |

I performed this closed end analysis for each risk rating scorecard,
yielding the below annual, observed default rates.

    ## `summarise()` has grouped output by 'portfolio'. You can override using the
    ## `.groups` argument.

![](github-project_files/figure-gfm/default_rate-1.png)<!-- -->

The default rates are as follows for each scorecard:

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

    ## `summarise()` has grouped output by 'portfolio', 'year'. You can override using
    ## the `.groups` argument.

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
