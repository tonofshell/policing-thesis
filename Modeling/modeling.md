Modeling
================
Adam Shelton
2/19/2020

## Data Preparation

``` r
analysis_data = merged_crime_data %>% select(-c(id, case_number, description, block_id, location_description, reporting_dept))

partition_data = function(data_set, y, p = 0.75, ...) {
  train_indices = createDataPartition(unlist(data_set[y]), p = p, ...) %>% unlist()
  list("training" = data_set[train_indices, ], "testing" = data_set[-train_indices, ])
}

arrest_data = analysis_data %>% partition_data("arrest")
dept_data = analysis_data %>% partition_data("responding_dept")
```

## Lasso Regression

``` r
glmnet_coefs_table = . %>% coef(s = .$lambda.1se) %>% as.matrix() %>% as_tibble(rownames = "var") %>% filter(`1` != 0) %>% mutate(`1` = round(`1`, 3))
```

``` r
lasso_arrests_mod = arrest_data$training %>% {model.matrix(arrest ~ ., .)} %>% cv.glmnet(arrest_data$training$arrest, alpha = 1, type.measure = "auc", family = "binomial")
arrests_results = glmnet_coefs_table(lasso_arrests_mod)
arrests_results %>% write_csv(here("Modeling", "arrests_results.csv"))
arrests_results %>% kable()
```

| var                                           |        1 |
| :-------------------------------------------- | -------: |
| (Intercept)                                   | \-30.066 |
| responding\_deptucpd                          |    1.002 |
| date                                          |    0.000 |
| primary\_typebattery                          |    0.031 |
| primary\_typeburglary                         |  \-1.434 |
| primary\_typedamage                           |  \-1.189 |
| primary\_typedeceptive practice               |  \-0.677 |
| primary\_typeharassment                       |  \-0.084 |
| primary\_typehomicide                         |    0.700 |
| primary\_typeinterference with public officer |    3.868 |
| primary\_typeliquor law violation             |    3.712 |
| primary\_typenarcotics                        |    5.993 |
| primary\_typeother                            |    0.257 |
| primary\_typepublic peace violation           |    2.541 |
| primary\_typerobbery                          |  \-1.003 |
| primary\_typesex crime                        |    0.966 |
| primary\_typetheft                            |  \-0.868 |
| primary\_typetrespassing                      |    2.111 |
| primary\_typeweapons violation                |    2.638 |
| domesticTRUE                                  |  \-0.106 |
| armedTRUE                                     |  \-0.283 |
| aggravatedTRUE                                |    0.280 |
| avg\_family\_size                             |  \-0.010 |
| avg\_household\_size                          |  \-0.001 |
| total\_housing\_units                         |  \-0.001 |
| hu\_occupied                                  |  \-0.252 |
| hu\_owned                                     |  \-0.381 |
| hu\_owned\_loan                               |  \-0.346 |
| hu\_vacant                                    |  \-0.014 |
| pop\_black                                    |  \-0.018 |
| pop\_family\_hh                               |  \-0.155 |
| pop\_in\_hus                                  |  \-0.081 |
| pop\_latino\_other\_race                      |    0.425 |
| pop\_other\_race                              |    0.048 |
| pop\_two\_plus\_races                         |    0.473 |
| pop\_under\_18                                |  \-0.144 |
| lon\_x                                        |  \-1.612 |
| lat\_y                                        |  \-2.669 |

``` r
rm(lasso_arrests_mod)
```

``` r
lasso_dept_mod = dept_data$training %>% {model.matrix(responding_dept ~ ., .)} %>% cv.glmnet(dept_data$training$responding_dept, alpha = 1, type.measure = "auc", family = "multinomial")
```

    ## Warning: Only deviance, class, mse, mae available as type.measure for
    ## Multinomial models; deviance used instead

    ## Warning in lognet(x, is.sparse, ix, jx, y, weights, offset, alpha, nobs, : one
    ## multinomial or binomial class has fewer than 8 observations; dangerous ground
    
    ## Warning in lognet(x, is.sparse, ix, jx, y, weights, offset, alpha, nobs, : one
    ## multinomial or binomial class has fewer than 8 observations; dangerous ground

    ## Error: cannot allocate vector of size 1.4 Gb

``` r
dept_results = glmnet_coefs_table(lasso_dept_mod)
```

    ## Error in coef(., s = .$lambda.1se): object 'lasso_dept_mod' not found

``` r
dept_results %>% kable()
```

    ## Error in eval(lhs, parent, parent): object 'dept_results' not found

``` r
dept_results %>% write_csv(here("Modeling", "dept_results.csv"))
```

    ## Error in eval(lhs, parent, parent): object 'dept_results' not found

## RandomForest
