Peer Assignment 1 
========================================================

Loading and preprocessing the data
--------------------------------------------------------
- Load the data (i.e. read.csv())
- Process/transform the data (if necessary) into a format suitable for your analysis


```r
# 1.1 Read data file into data.frame
activity <- read.csv("activity.csv")
```


What is mean total number of steps taken per day?
--------------------------------------------------------
- Make a histogram of the total number of steps taken each day. (For this part of the assignment, you can ignore the missing values in the dataset.)


```r
# 2.1 Sum steps by date into new
dailyStepSum <- aggregate(steps ~ date, data = activity, sum)

# Plot diagram here
barplot(dailyStepSum$steps, names.arg = dailyStepSum$date, main = "Total number of steps taken each day", 
    ylab = "Number of steps", xlab = "", col = "blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


- Calculate and report the mean and median total number of steps taken per day


```r
# 2.2a calculate mean
dailyStepMean <- mean(dailyStepSum$steps)
message("Mean: ", dailyStepMean)
```

```
## Mean: 10766.1886792453
```

```r

# 2.2b calculate median
dailyStepMedian <- median(dailyStepSum$steps)
message("Median: ", dailyStepMedian)
```

```
## Median: 10765
```


What is the average daily activity pattern?
--------------------------------------------------------
-  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# 3.1 use ggplot2 to plot the time series
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
intervalStepAve <- aggregate(steps ~ interval, data = activity, mean)
a <- ggplot(intervalStepAve, aes(interval, steps)) + geom_line() + xlab("Interval") + 
    ylab("Average Steps")
a
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxrol <- which(intervalStepAve == max(intervalStepAve[2]), arr.ind = TRUE)
maxAveInterval <- intervalStepAve[maxrol[1, 1], 1]
message("5-minute interval, on average across all the days, contains maximum number of steps: ", 
    maxAveInterval)
```

```
## 5-minute interval, on average across all the days, contains maximum number of steps: 835
```


Imputing missing values
--------------------------------------------------------
- Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
naSummary <- colSums(is.na(activity))
message("Total number of missing values in the dataset: ", naSummary[1])
```

```
## Total number of missing values in the dataset: 2304
```


- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## Strategy for filling in all of the missing values: 
## Replace NA with the mean for that 5-minute interval, e.g. any date of interval 0 = NA, repalce with 1.7169811.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newAct <- activity

for (i in 1:nrow(newAct)) {
    if (is.na(newAct[i, 1]) == TRUE) {
        newAct[i, 1] <- intervalStepAve[match(newAct[i, 3], intervalStepAve$interval), 
            2]
    }
}
```


- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
## 4.4.1a Sum steps by date into new

dailyStepSum <- aggregate(steps ~ date, data = newAct, sum)

## 4.4.1b Plot diagram here
barplot(dailyStepSum$steps, names.arg = dailyStepSum$date, main = "Total number of steps taken each day", 
    ylab = "Number of steps", xlab = "", col = "blue")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r

# 4.4.2 Calculate and report the mean and median total number of steps taken
# per day
dailyStepMean <- mean(dailyStepSum$steps)
message("Mean: ", dailyStepMean)
```

```
## Mean: 10766.1886792453
```

```r

dailyStepMedian <- median(dailyStepSum$steps)
message("Median: ", dailyStepMedian)
```

```
## Median: 10766.1886792453
```


Are there differences in activity patterns between weekdays and weekends?
--------------------------------------------------------

- For this part the weekdays() function may be of some help here. Use the dataset with the filled in missing values for this part.

- Create a new factor variable in the dataset with two levels  weekday and weekend indicating whether a given date is a weekday or weekend day.

```r
newAct$day <- as.POSIXlt(as.Date(newAct$date, "%Y-%m-%d"))$wday

newAct$day[newAct$day > 0 & newAct$day < 6] <- "weekday"
newAct$day[newAct$day == 0] <- "weekend"
newAct$day[newAct$day == 6] <- "weekend"
```


- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
## aggregate
newStepAve <- aggregate(steps ~ interval + day, data = newAct, mean)

# option 1 load lattice
require(lattice)
```

```
## Loading required package: lattice
```

```r
attach(newStepAve)
# create factors with value labels
as.factor(newStepAve$day)
```

```
##   [1] weekday weekday weekday weekday weekday weekday weekday weekday
##   [9] weekday weekday weekday weekday weekday weekday weekday weekday
##  [17] weekday weekday weekday weekday weekday weekday weekday weekday
##  [25] weekday weekday weekday weekday weekday weekday weekday weekday
##  [33] weekday weekday weekday weekday weekday weekday weekday weekday
##  [41] weekday weekday weekday weekday weekday weekday weekday weekday
##  [49] weekday weekday weekday weekday weekday weekday weekday weekday
##  [57] weekday weekday weekday weekday weekday weekday weekday weekday
##  [65] weekday weekday weekday weekday weekday weekday weekday weekday
##  [73] weekday weekday weekday weekday weekday weekday weekday weekday
##  [81] weekday weekday weekday weekday weekday weekday weekday weekday
##  [89] weekday weekday weekday weekday weekday weekday weekday weekday
##  [97] weekday weekday weekday weekday weekday weekday weekday weekday
## [105] weekday weekday weekday weekday weekday weekday weekday weekday
## [113] weekday weekday weekday weekday weekday weekday weekday weekday
## [121] weekday weekday weekday weekday weekday weekday weekday weekday
## [129] weekday weekday weekday weekday weekday weekday weekday weekday
## [137] weekday weekday weekday weekday weekday weekday weekday weekday
## [145] weekday weekday weekday weekday weekday weekday weekday weekday
## [153] weekday weekday weekday weekday weekday weekday weekday weekday
## [161] weekday weekday weekday weekday weekday weekday weekday weekday
## [169] weekday weekday weekday weekday weekday weekday weekday weekday
## [177] weekday weekday weekday weekday weekday weekday weekday weekday
## [185] weekday weekday weekday weekday weekday weekday weekday weekday
## [193] weekday weekday weekday weekday weekday weekday weekday weekday
## [201] weekday weekday weekday weekday weekday weekday weekday weekday
## [209] weekday weekday weekday weekday weekday weekday weekday weekday
## [217] weekday weekday weekday weekday weekday weekday weekday weekday
## [225] weekday weekday weekday weekday weekday weekday weekday weekday
## [233] weekday weekday weekday weekday weekday weekday weekday weekday
## [241] weekday weekday weekday weekday weekday weekday weekday weekday
## [249] weekday weekday weekday weekday weekday weekday weekday weekday
## [257] weekday weekday weekday weekday weekday weekday weekday weekday
## [265] weekday weekday weekday weekday weekday weekday weekday weekday
## [273] weekday weekday weekday weekday weekday weekday weekday weekday
## [281] weekday weekday weekday weekday weekday weekday weekday weekday
## [289] weekend weekend weekend weekend weekend weekend weekend weekend
## [297] weekend weekend weekend weekend weekend weekend weekend weekend
## [305] weekend weekend weekend weekend weekend weekend weekend weekend
## [313] weekend weekend weekend weekend weekend weekend weekend weekend
## [321] weekend weekend weekend weekend weekend weekend weekend weekend
## [329] weekend weekend weekend weekend weekend weekend weekend weekend
## [337] weekend weekend weekend weekend weekend weekend weekend weekend
## [345] weekend weekend weekend weekend weekend weekend weekend weekend
## [353] weekend weekend weekend weekend weekend weekend weekend weekend
## [361] weekend weekend weekend weekend weekend weekend weekend weekend
## [369] weekend weekend weekend weekend weekend weekend weekend weekend
## [377] weekend weekend weekend weekend weekend weekend weekend weekend
## [385] weekend weekend weekend weekend weekend weekend weekend weekend
## [393] weekend weekend weekend weekend weekend weekend weekend weekend
## [401] weekend weekend weekend weekend weekend weekend weekend weekend
## [409] weekend weekend weekend weekend weekend weekend weekend weekend
## [417] weekend weekend weekend weekend weekend weekend weekend weekend
## [425] weekend weekend weekend weekend weekend weekend weekend weekend
## [433] weekend weekend weekend weekend weekend weekend weekend weekend
## [441] weekend weekend weekend weekend weekend weekend weekend weekend
## [449] weekend weekend weekend weekend weekend weekend weekend weekend
## [457] weekend weekend weekend weekend weekend weekend weekend weekend
## [465] weekend weekend weekend weekend weekend weekend weekend weekend
## [473] weekend weekend weekend weekend weekend weekend weekend weekend
## [481] weekend weekend weekend weekend weekend weekend weekend weekend
## [489] weekend weekend weekend weekend weekend weekend weekend weekend
## [497] weekend weekend weekend weekend weekend weekend weekend weekend
## [505] weekend weekend weekend weekend weekend weekend weekend weekend
## [513] weekend weekend weekend weekend weekend weekend weekend weekend
## [521] weekend weekend weekend weekend weekend weekend weekend weekend
## [529] weekend weekend weekend weekend weekend weekend weekend weekend
## [537] weekend weekend weekend weekend weekend weekend weekend weekend
## [545] weekend weekend weekend weekend weekend weekend weekend weekend
## [553] weekend weekend weekend weekend weekend weekend weekend weekend
## [561] weekend weekend weekend weekend weekend weekend weekend weekend
## [569] weekend weekend weekend weekend weekend weekend weekend weekend
## Levels: weekday weekend
```

```r
as.factor(newStepAve$interval)
```

```
##   [1] 0    5    10   15   20   25   30   35   40   45   50   55   100  105 
##  [15] 110  115  120  125  130  135  140  145  150  155  200  205  210  215 
##  [29] 220  225  230  235  240  245  250  255  300  305  310  315  320  325 
##  [43] 330  335  340  345  350  355  400  405  410  415  420  425  430  435 
##  [57] 440  445  450  455  500  505  510  515  520  525  530  535  540  545 
##  [71] 550  555  600  605  610  615  620  625  630  635  640  645  650  655 
##  [85] 700  705  710  715  720  725  730  735  740  745  750  755  800  805 
##  [99] 810  815  820  825  830  835  840  845  850  855  900  905  910  915 
## [113] 920  925  930  935  940  945  950  955  1000 1005 1010 1015 1020 1025
## [127] 1030 1035 1040 1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135
## [141] 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245
## [155] 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 1345 1350 1355
## [169] 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 1500 1505
## [183] 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610 1615
## [197] 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725
## [211] 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835
## [225] 1840 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945
## [239] 1950 1955 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055
## [253] 2100 2105 2110 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205
## [267] 2210 2215 2220 2225 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315
## [281] 2320 2325 2330 2335 2340 2345 2350 2355 0    5    10   15   20   25  
## [295] 30   35   40   45   50   55   100  105  110  115  120  125  130  135 
## [309] 140  145  150  155  200  205  210  215  220  225  230  235  240  245 
## [323] 250  255  300  305  310  315  320  325  330  335  340  345  350  355 
## [337] 400  405  410  415  420  425  430  435  440  445  450  455  500  505 
## [351] 510  515  520  525  530  535  540  545  550  555  600  605  610  615 
## [365] 620  625  630  635  640  645  650  655  700  705  710  715  720  725 
## [379] 730  735  740  745  750  755  800  805  810  815  820  825  830  835 
## [393] 840  845  850  855  900  905  910  915  920  925  930  935  940  945 
## [407] 950  955  1000 1005 1010 1015 1020 1025 1030 1035 1040 1045 1050 1055
## [421] 1100 1105 1110 1115 1120 1125 1130 1135 1140 1145 1150 1155 1200 1205
## [435] 1210 1215 1220 1225 1230 1235 1240 1245 1250 1255 1300 1305 1310 1315
## [449] 1320 1325 1330 1335 1340 1345 1350 1355 1400 1405 1410 1415 1420 1425
## [463] 1430 1435 1440 1445 1450 1455 1500 1505 1510 1515 1520 1525 1530 1535
## [477] 1540 1545 1550 1555 1600 1605 1610 1615 1620 1625 1630 1635 1640 1645
## [491] 1650 1655 1700 1705 1710 1715 1720 1725 1730 1735 1740 1745 1750 1755
## [505] 1800 1805 1810 1815 1820 1825 1830 1835 1840 1845 1850 1855 1900 1905
## [519] 1910 1915 1920 1925 1930 1935 1940 1945 1950 1955 2000 2005 2010 2015
## [533] 2020 2025 2030 2035 2040 2045 2050 2055 2100 2105 2110 2115 2120 2125
## [547] 2130 2135 2140 2145 2150 2155 2200 2205 2210 2215 2220 2225 2230 2235
## [561] 2240 2245 2250 2255 2300 2305 2310 2315 2320 2325 2330 2335 2340 2345
## [575] 2350 2355
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

```r

# create the plot with xyplot
a <- xyplot(data = newStepAve, steps ~ interval | day, panel = "panel.lines", 
    layout = c(1, 2))
a
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


