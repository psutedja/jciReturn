#load the library needed for analysis
library(tidyverse)
library(lubridate)
library(anomalize)
library(PerformanceAnalytics)
library(gridExtra)

# Load the JCI daily file since 1983
jci <- read.csv("./rda/JCI_1983.csv",stringsAsFactors = FALSE)
jci <- setNames(jci,c("date","JCI","LQ45"))
jci <- jci %>% mutate(JCI = as.numeric(JCI),LQ45 = as.numeric(LQ45), date = as.Date(date,"%d/%m/%Y")) %>% select(-LQ45) %>% na.omit()

#calculate daily arithmetic/log return, add a field on month and year of the price data for further analysis
jci <- jci %>% 
  mutate(JCI_growth_daily = (JCI / lag(JCI))-1,
         year=as.numeric(format(date,"%Y")),
         month=factor(format(date,"%b"),levels=month.abb),
         day=wday(date,label=TRUE),
         JCI_log_return_daily = log(JCI/lag(JCI))) %>% 
  na.omit()

# Can also sum the daily log return to get the monthly log return
jcimonthly <- jci %>% 
  group_by(year,month) %>% 
  summarize(month = last(month), 
            year = last(year), 
            date = last(date),
            JCI = last(JCI),
            monthly_log_return_calculated = sum(JCI_log_return_daily)) %>% 
  ungroup()

# LT daily return analysis using arithmetic daily return
mean <- mean(jci$JCI_growth_daily,na.rm=TRUE)
sd <- sd(jci$JCI_growth_daily,na.rm=TRUE)

jci %>% 
  mutate(year=format(date,"%Y")) %>% 
  filter(!is.na(JCI_growth_daily)) %>% 
  ggplot() + 
  geom_histogram(aes(x=JCI_log_return_daily,y=..density..),binwidth=0.005) + 
  xlab("JCI daily arithmetic return") + 
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) + 
  stat_function(fun=function(x) dnorm(x,mean,sd),color="red") +
  geom_vline(aes(xintercept=mean),color="grey") + # mean
  geom_vline(aes(xintercept=mean+sd),color="grey",linetype="dashed") + # +1s.d.
  geom_vline(aes(xintercept=mean-sd),color="grey",linetype="dashed")  # -1s.d.

ggsave("./output/01-1 JCI daily arithmetic return since 1980.png") #save the plot

# LT daily return analysis using log daily return
mean <- mean(jci$JCI_log_return_daily,na.rm=TRUE)
sd <- sd(jci$JCI_log_return_daily,na.rm=TRUE)

jci %>% 
  mutate(year=format(date,"%Y")) %>% 
  filter(!is.na(JCI_log_return_daily)) %>% 
  ggplot() + 
  geom_histogram(aes(x=JCI_log_return_daily,y=..density..),binwidth=0.005) +
#  geom_density(aes(x=JCI_log_return_daily),color="green") +
  xlab("JCI daily log return") + 
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) + 
  stat_function(fun=function(x) dnorm(x,mean,sd),color="red") +
  geom_vline(aes(xintercept=mean),color="grey") + # mean
  geom_vline(aes(xintercept=mean+sd),color="grey",linetype="dashed") + # +1s.d.
  geom_vline(aes(xintercept=mean-sd),color="grey",linetype="dashed")  # -1s.d.

ggsave("./output/02-1 JCI daily log return histogram since 1980.png") #save the plot

#Plot the data based on year
jci %>% 
  mutate(year=format(date,"%Y")) %>% 
  filter(!is.na(JCI_log_return_daily)) %>% 
  ggplot() + 
  geom_histogram(aes(x=JCI_log_return_daily,y=..density..),binwidth=0.01) + 
  xlab("JCI daily log return") + 
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) + facet_wrap(~year)

  ggsave("./output/03-1 JCI daily log return histogram for each year.png") #save the plot

# Clean the data, only acccount for > 2004
# Why 2004? Arguably the time where Indonesia finally get back on foot after 1998/99 Asian Financial Crisis, and
# the start of a new era following the first ever direct election in the country
jci <- jci %>% filter(date >= ymd("2004/01/01"))

# LT daily return analysis since 2004 using log daily return
mean <- mean(jci$JCI_log_return_daily,na.rm=TRUE)
sd <- sd(jci$JCI_log_return_daily,na.rm=TRUE)

jci %>% 
  mutate(year=format(date,"%Y")) %>% 
  filter(!is.na(JCI_log_return_daily)) %>% 
  ggplot() + 
  geom_histogram(aes(x=JCI_log_return_daily,y=..density..),binwidth=0.005) + 
  xlab("JCI daily log return") + 
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) + 
  stat_function(fun=function(x) dnorm(x,mean,sd),color="red") + 
  geom_vline(aes(xintercept=mean),color="grey") + # mean
  geom_vline(aes(xintercept=mean+sd),color="grey",linetype="dashed") + # +1s.d.
  geom_vline(aes(xintercept=mean-sd),color="grey",linetype="dashed")  # -1s.d.

ggsave("./output/04-1 JCI daily log return since 2004.png") #save the plot

# Print the mean and standard deviation table
jci %>% 
  mutate(year=as.factor(format(date,"%Y"))) %>% 
  filter(!is.na(JCI_log_return_daily)) %>% 
  group_by(year) %>% 
  summarize(mean = mean(JCI_log_return_daily), 
            sd = sd(JCI_log_return_daily))

# Plot the data based on year
jci %>% 
  mutate(year=format(date,"%Y")) %>% 
  filter(!is.na(JCI_log_return_daily)) %>% 
  ggplot() + 
  geom_histogram(aes(x=JCI_log_return_daily,y=..density..),binwidth=0.005) + 
  xlab("JCI daily log return - split by year") + 
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) + facet_wrap(~year)

ggsave("./output/05-1 JCI daily log return histogram for each year since 2004.png") #save the plot

# Plot the yearly qq line
jci %>% 
  mutate(year=format(date,"%Y")) %>% 
  filter(!is.na(JCI_log_return_daily)) %>% 
  ggplot(aes(sample=JCI_log_return_daily)) + 
  geom_qq(alpha=0.3) + geom_qq_line() + 
  xlab("JCI daily log return Q-Q plot - split by year") + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) + facet_wrap(~year)

ggsave("./output/05-1a JCI daily log return QQ plot for each year since 2004.png") #save the plot

# Plot the yearly boxplot
jci %>% 
  mutate(year=as.factor(format(date,"%Y"))) %>% 
  filter(!is.na(JCI_log_return_daily)) %>% 
  ggplot(aes(x=year,y=JCI_log_return_daily)) + 
  geom_boxplot() +  
  ylab("JCI daily log return") + xlab("Year") +
  scale_y_continuous(labels=scales::percent_format(accuracy=1))

ggsave("./output/06-1 JCI daily log return boxplot for each year since 2004.png") #save the plot

# Print the mean and standard deviation table + median and skewness
yearlystat <- jci %>% 
  mutate(year=as.factor(format(date,"%Y"))) %>% 
  filter(!is.na(JCI_log_return_daily)) %>% 
  group_by(year) %>% 
  summarize(mean = mean(JCI_log_return_daily), 
            median = median(JCI_log_return_daily),
            sd = sd(JCI_log_return_daily),
            skewness = skewness(JCI_log_return_daily),
            kurtosis = kurtosis(JCI_log_return_daily),
            pct_positive_return = mean(JCI_log_return_daily>0))

yearlystat <- yearlystat %>% mutate(mean = scales::percent(mean), median = scales::percent(median), sd = scales::percent(sd), skewness = scales::number(skewness), kurtosis = scales::number(kurtosis),pct_positive_return = scales::percent(pct_positive_return))
yearlystat <- setNames(yearlystat,c("Year","Mean","Median","St.Dev","Skewness","Excess Kurtosis","% of positive return days"))

png("./output/yearlystat.png",height=21*nrow(yearlystat),width=84*ncol(yearlystat))
p <- tableGrob(yearlystat)
grid.arrange(p)
dev.off()

#plot the return boxplot based on day of the week
jci %>% 
  filter(!is.na(JCI_log_return_daily), date >= ymd('2004-01-01')) %>% 
  ggplot(aes(x=day,y=JCI_log_return_daily)) + 
  geom_boxplot() +  
  ylab("JCI daily log return") + xlab("Day") +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) + 
  geom_hline(aes(yintercept=0),color="red",linetype="dashed")
ggsave("./output/07-1 JCI daily log return boxplot for each day of the week.png") #save the plot

#plot the histogram split by day of the week
jci %>% 
  filter(!is.na(JCI_log_return_daily), date >= ymd('2004-01-01')) %>% 
  ggplot() + 
  geom_histogram(aes(x=JCI_log_return_daily,y=..density..),binwidth = 0.01) +  
  xlab("JCI daily log return") + ylab("Density") +
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) + 
  geom_vline(aes(xintercept=0),color="red",linetype="dashed") + facet_wrap(~day)
ggsave("./output/08-1 JCI daily log return histogram for each day of the week.png") #save the plot

# Plot the daily qq line
jci %>% 
  filter(!is.na(JCI_log_return_daily), date >= ymd('2004-01-01')) %>% 
  ggplot(aes(sample=JCI_log_return_daily)) + 
  geom_qq(alpha=0.3) + geom_qq_line() + 
  xlab("JCI daily log return Q-Q plot - based on day of the week") + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) + facet_wrap(~day) 
ggsave("./output/09-1 JCI daily log return qq plot for each day of the week.png") #save the plot

#print the statistics
years <- 2004:2020
sapply(years,function(n) {
  dailystat <- jci %>% 
    filter(!is.na(JCI_log_return_daily), year==n) %>% 
    group_by(day) %>% 
    summarize(mean = mean(JCI_log_return_daily), 
              median = median(JCI_log_return_daily), 
              sd = sd(JCI_log_return_daily),
              skewness = skewness(JCI_log_return_daily), 
              kurtosis = kurtosis(JCI_log_return_daily),
              pct_positive_return = mean(JCI_log_return_daily>0)) 
  
  dailystat <- dailystat %>% mutate(mean = scales::percent(mean), median = scales::percent(median), sd = scales::percent(sd), skewness = scales::number(skewness), kurtosis = scales::number(kurtosis), pct_positive_return = scales::percent(pct_positive_return))
  dailystat <- setNames(dailystat,c("Day","Mean","Median","St.Dev","Skewness","Excess Kurtosis","% of positive return days"))
  
  png(paste("./output/dailystat_",n,".png",sep=""),height=30*nrow(dailystat),width=79*ncol(dailystat))
  p <- tableGrob(dailystat)
  title <- paste("JCI daily log return statistics based on day of the week, year",n)
  grid.arrange(bottom = title,p)
  dev.off()
})

#plot the return boxplot based on day of the month
jci %>% 
  mutate(day_of_month = as.factor(day(date))) %>% 
  filter(!is.na(JCI_log_return_daily), date >= ymd('2011-01-01')) %>% 
  ggplot(aes(x=day_of_month,y=JCI_log_return_daily)) + 
  geom_boxplot() +  
  ylab("JCI daily log return") + xlab("Day") +
  scale_y_continuous(labels=scales::percent_format(accuracy=1))
