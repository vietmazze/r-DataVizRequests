Algae growth
================
Duy Nguyen
August 14, 2017

Reading excel file
------------------

``` r
theurl <- getURL("http://www.aquatext.com/tables/algaegrwth.htm",.opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
n.rows<- unlist(lapply(tables, function(t) dim(t)[1]))
beta<-tables[[which.max(n.rows)]]
```

Tidy data
=========

``` r
col_names<-beta[,1] %>%
  str_replace(" ","_") %>%
  str_replace("\n",'') %>%
  str_replace("    ","") %>%
  str_replace("\\(.*\\)|\\.", "") %>%
  str_replace("\'\'","")
data<-as.tibble(t(beta[,-1]))
colnames(data)<-col_names
data<-data[,-2]
```

Adding temperature data
=======================

``` r
data<-data %>%
  mutate(
    temperature=c(5,5,10,10,25,25,30,30)
  )
```

Mutate and gather variables in correct order
============================================

``` r
data1<- data %>%
  mutate_all(as.numeric) %>%
  gather(2:20,key="Species",value="growth") %>%
  arrange(temperature,`Light_ Intensity `) %>%
  mutate(light_intensity= as.factor(`Light_ Intensity `))
```

    ## Warning in eval(substitute(expr), envir, enclos): NAs introduced by
    ## coercion

``` r
data1<-data1[,c(3,1,2,4)]
data1
```

    ## # A tibble: 152 × 4
    ##                    Species `Light_ Intensity ` temperature growth
    ##                      <chr>               <dbl>       <dbl>  <dbl>
    ## 1       Caloneis_schroderi                2500           5  -0.50
    ## 2     Chaetoceros_gracilis                2500           5  -0.05
    ## 3      Chaetoceros_simplex                2500           5  -0.01
    ## 4    Chlorella_ellipsoidea                2500           5   0.02
    ## 5  Chlorella_stigmatophora                2500           5   0.24
    ## 6     Chlorella_vulgaris                  2500           5   0.00
    ## 7   Dunaniella_tertiolecta                2500           5   0.17
    ## 8     Cyclotella_sp NUFP-9                2500           5   0.08
    ## 9          Hanzchia_marina                2500           5  -0.07
    ## 10      Isochrysis_galbana                2500           5  -0.07
    ## # ... with 142 more rows

\#GRAPH RELATIONSHIP FOR GROWTH BETWEEN TEMPERATURE AND LIGHT INTENSITY
-----------------------------------------------------------------------

``` r
ggplot(data1,aes(x=temperature,y=growth,color=data1$`Light_ Intensity `))+
  geom_smooth(method="lm",alpha=0.5)+
  geom_point()+
  scale_x_continuous(breaks = c(5, 10, 25, 30)) +
  facet_wrap(~ Species, ncol = 4) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Growth rate of algae by species",
       x = "Temperature (degrees celsius)",
       y = "Growth rate (divisions per day)",
       color = "Light intensity (lux)")
```

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](https://github.com/vietmazze/r-DataVizRequests/blob/master/pressure-1.png)
