Essay Graphs for r/Datavizrequest user
================
Duy Nguyen
August 10, 2017

Reading excel file
------------------

``` r
test<-read_excel("Undergrad Essays Written.xlsx",1,col_names=FALSE,skip=1)

head(test)
```

    ## # A tibble: 6 × 3
    ##                          X0         X1                   X2
    ##                       <chr>     <dttm>                <chr>
    ## 1 Freshman Year, Semester 1 2012-09-05  0.91249999999999998
    ## 2                      <NA> 2012-09-25  0.97777777777777775
    ## 3                      <NA> 2012-10-18  0.90902777777777777
    ## 4                      <NA> 2012-10-27   0.9784722222222223
    ## 5                      <NA> 2012-10-29 8.819444444444445E-2
    ## 6                      <NA> 2012-11-02   0.5083333333333333

Fixing the data
---------------

``` r
colnames(test)<-c("Year","Date","Time")

test<-test%>%
  separate(Year,into=c("Year","Semester"),sep=",")

test$Year[[1]]<-("1.0")
test$Year[[10]]<-("1.0")
test$Year[[20]]<-("2.0")
test$Year[[32]]<-("2.0")
test$Year[[49]]<-("3.0")
test$Year[[59]]<-("3.0")
test$Year[[71]]<-("4.0")
test$Year[[90]]<-("4.0")

test$Semester[[1]]<-("1")
test$Semester[[10]]<-("2")
test$Semester[[20]]<-("1")
test$Semester[[32]]<-("2")
test$Semester[[49]]<-("1")
test$Semester[[59]]<-("2")
test$Semester[[71]]<-("1")
test$Semester[[90]]<-("2")

test<-test%>%
  fill(Year)
test<-test%>%
  fill(Semester)
test$Year<-as.numeric(test$Year)
test$Semester<-as.numeric(test$Semester)
test<-as.tibble(test, na.rm=TRUE)
```

``` r
head(test)
```

    ## # A tibble: 6 × 4
    ##    Year Semester       Date                 Time
    ##   <dbl>    <dbl>     <dttm>                <chr>
    ## 1     1        1 2012-09-05  0.91249999999999998
    ## 2     1        1 2012-09-25  0.97777777777777775
    ## 3     1        1 2012-10-18  0.90902777777777777
    ## 4     1        1 2012-10-27   0.9784722222222223
    ## 5     1        1 2012-10-29 8.819444444444445E-2
    ## 6     1        1 2012-11-02   0.5083333333333333

Converting to datetime variable
-------------------------------

``` r
test$Time<-as.numeric(test$Time)
```

    ## Warning: NAs introduced by coercion

``` r
test$Time<-chron::times(test$Time)


test<- test%>% 
  unite(datetime,Date,Time,sep=" ")

test<-test[-21,]
test$datetime<-parse_datetime(test$datetime,"%Y-%m-%d %H:%M:%S")
```

Looking at tidy data
====================

``` r
knitr::kable(
 test, 
 caption = "A knitr kable.")
```

|  Year|  Semester| datetime            |
|-----:|---------:|:--------------------|
|     1|         1| 2012-09-05 21:54:00 |
|     1|         1| 2012-09-25 23:28:00 |
|     1|         1| 2012-10-18 21:49:00 |
|     1|         1| 2012-10-27 23:29:00 |
|     1|         1| 2012-10-29 02:07:00 |
|     1|         1| 2012-11-02 12:12:00 |
|     1|         1| 2012-12-03 21:46:00 |
|     1|         1| 2012-12-06 02:19:00 |
|     1|         1| 2012-12-11 22:13:00 |
|     1|         2| 2013-02-15 20:44:00 |
|     1|         2| 2013-02-26 22:52:00 |
|     1|         2| 2013-02-28 20:36:00 |
|     1|         2| 2013-03-27 20:20:00 |
|     1|         2| 2013-04-09 19:34:00 |
|     1|         2| 2013-04-22 17:38:00 |
|     1|         2| 2013-04-23 21:48:00 |
|     1|         2| 2013-04-25 12:40:00 |
|     1|         2| 2013-05-15 09:05:00 |
|     1|         2| 2013-05-31 22:09:00 |
|     2|         1| 2013-10-06 19:12:00 |
|     2|         1| 2013-11-05 22:06:00 |
|     2|         1| 2013-11-11 17:39:00 |
|     2|         1| 2013-11-25 17:42:00 |
|     2|         1| 2013-12-01 23:50:00 |
|     2|         1| 2013-12-03 20:10:00 |
|     2|         1| 2013-12-05 21:03:00 |
|     2|         1| 2013-12-05 21:23:00 |
|     2|         1| 2013-12-09 16:02:00 |
|     2|         1| 2013-12-12 23:23:00 |
|     2|         1| 2013-12-16 22:27:00 |
|     2|         2| 2014-02-26 22:10:00 |
|     2|         2| 2014-03-04 23:19:00 |
|     2|         2| 2014-03-05 15:04:00 |
|     2|         2| 2014-03-18 01:08:00 |
|     2|         2| 2014-03-26 23:14:00 |
|     2|         2| 2014-04-03 13:59:00 |
|     2|         2| 2014-04-15 22:38:00 |
|     2|         2| 2014-04-21 22:05:00 |
|     2|         2| 2014-04-23 12:53:00 |
|     2|         2| 2014-04-23 15:10:00 |
|     2|         2| 2014-04-23 19:39:00 |
|     2|         2| 2014-04-20 16:33:00 |
|     2|         2| 2014-05-05 15:58:00 |
|     2|         2| 2014-05-06 00:03:00 |
|     2|         2| 2014-05-11 21:39:00 |
|     2|         2| 2014-05-12 20:17:00 |
|     2|         2| 2014-05-13 12:24:00 |
|     3|         1| 2014-09-22 10:14:00 |
|     3|         1| 2014-09-29 00:48:00 |
|     3|         1| 2014-11-03 22:33:00 |
|     3|         1| 2014-11-18 10:52:00 |
|     3|         1| 2014-12-05 23:37:00 |
|     3|         1| 2014-12-07 20:34:00 |
|     3|         1| 2014-12-08 22:22:00 |
|     3|         1| 2014-12-15 12:16:00 |
|     3|         1| 2014-12-16 21:44:00 |
|     3|         1| 2014-12-20 13:33:00 |
|     3|         2| 2015-09-24 22:47:00 |
|     3|         2| 2015-10-06 22:59:00 |
|     3|         2| 2015-10-22 14:14:00 |
|     3|         2| 2015-10-22 15:24:00 |
|     3|         2| 2015-11-11 23:17:00 |
|     3|         2| 2015-11-30 11:48:00 |
|     3|         2| 2015-12-11 14:56:00 |
|     3|         2| 2015-12-14 13:56:00 |
|     3|         2| 2015-12-14 23:55:00 |
|     3|         2| 2015-12-17 00:10:00 |
|     3|         2| 2015-12-17 14:25:00 |
|     3|         2| 2015-12-18 21:08:00 |
|     4|         1| 2016-10-02 15:17:00 |
|     4|         1| 2016-10-03 18:07:00 |
|     4|         1| 2016-10-05 20:51:00 |
|     4|         1| 2016-10-15 12:00:00 |
|     4|         1| 2016-10-27 18:39:00 |
|     4|         1| 2016-10-30 17:45:00 |
|     4|         1| 2016-11-10 11:54:00 |
|     4|         1| 2016-11-16 11:09:00 |
|     4|         1| 2016-11-20 15:33:00 |
|     4|         1| 2016-11-26 19:27:00 |
|     4|         1| 2016-11-30 23:21:00 |
|     4|         1| 2016-12-06 17:08:00 |
|     4|         1| 2016-12-07 11:16:00 |
|     4|         1| 2016-12-09 14:33:00 |
|     4|         1| 2016-12-10 01:14:00 |
|     4|         1| 2016-12-11 12:49:00 |
|     4|         1| 2016-12-11 09:54:00 |
|     4|         1| 2016-12-15 00:14:00 |
|     4|         1| 2016-12-17 12:14:00 |
|     4|         2| 2017-02-06 22:41:00 |
|     4|         2| 2017-02-11 21:09:00 |
|     4|         2| 2017-02-13 15:39:00 |
|     4|         2| 2017-02-19 12:39:00 |
|     4|         2| 2017-02-20 20:40:00 |
|     4|         2| 2017-03-04 11:11:00 |
|     4|         2| 2017-03-06 17:34:00 |
|     4|         2| 2017-03-08 19:17:00 |
|     4|         2| 2017-03-16 12:10:00 |
|     4|         2| 2017-04-03 16:19:00 |
|     4|         2| 2017-04-03 21:31:00 |
|     4|         2| 2017-04-10 17:25:00 |
|     4|         2| 2017-04-18 20:10:00 |
|     4|         2| 2017-04-28 05:01:00 |
|     4|         2| 2017-05-01 14:52:00 |
|     4|         2| 2017-05-04 23:41:00 |
|     4|         2| 2017-05-09 16:21:00 |
|     4|         2| 2017-05-09 22:20:00 |
|     4|         2| 2017-05-10 00:51:00 |
|     4|         2| 2017-05-11 17:03:00 |

Graphing histogram and boxplot for data
=======================================

``` r
ggplot(test,aes(hour(datetime)))+
  geom_histogram(bins=40,color="black",fill="steelblue1")+
  labs(title="Essays", x="Hour",y="Number",caption="created by DQN")+
  facet_grid(paste("year",Year)~paste("semester",Semester))+
  theme_bw()
```

![](https://github.com/vietmazze/r-DataVizRequests/blob/master/graphing-1.png)

``` r
ggplot(test,aes(y=hour(datetime),x=Year,group=Semester))+
  geom_boxplot(color="blue")+
  facet_grid(paste("year",Year)~paste("semester",Semester))+
  labs(title="Summary of time used the most to do essay",x="Year",y="Hour")+
  theme_bw()
```

![](https://github.com/vietmazze/r-DataVizRequests/blob/master/graphing-2.png)
