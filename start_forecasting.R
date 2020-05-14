
setwd("/Users/kim.larsen/Documents/Code/Start-Forecasting")

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cowplot)

### Helper function to create a survival curve
get_survival_curve <- function(p, length){
  r <- rep(1, length)
  for (i in 2:length){
    r[i] <- r[i-1] * p
  }
  return(r)
}

roundup <- function(x, n){
  return (n * ceiling(x / n))
}

rounddown <- function(x, n){
  return (n * floor(x / n))
}

### Helper Function to create cohorts
create_cohort <- function(id, acquisition, p_survival){ 
  zeros <- rep(0, id-1)
  window <- length(p_survival)
  row <- data.frame(id, t(c(zeros, t(p_survival[1:(window-(id-1))])*acquisition)))
  names(row) <- c("Cohort", paste0("s", seq_along(1:window)))
  return(row)
}

marketing_elasticity=0.3 
engagement <- 1
price <- 60
marketing_allocation <- 0.15
survival_rate <- 0.9
initial_marketing <- 40000
#base <- 200
base <- 250
gm <- 0.4
dates <- seq(as.Date("2018-1-1"), as.Date("2021-12-1"), by = "months")
n <- length(dates)
start_forecast <- as.Date("2020-11-1")
cutoff <- as.Date("2018-12-1")
chart_start <- as.Date("2020-01-1")
start_next_fiscal_year <- as.Date("2021-01-1")
marketing_boost <- 0
desired_growth <- 1.2

  
cohorts <- list()
df <- list()
  
marketing <- initial_marketing

s <- get_survival_curve(survival_rate, n)

sim <- function(){

   ## Loop through the months
   for (i in 1:n){
    
      ## Get the year
      year <- year(dates[i])
      month <- month(dates[i])
    
      ## Calculate acquisition and retention
      Acquisition <- base * (marketing+1)**marketing_elasticity
      marginal_cac <- 1 / (base*( (marketing+2)**marketing_elasticity - (marketing+1)**marketing_elasticity))

      ## Create a cohort for the given level of acquisition and retention
      cohorts[[i]] <- create_cohort(i, Acquisition, s)
      customers <- sum(bind_rows(cohorts)[,i+1])
      revenue <- customers*price*engagement
      df[[i]] <- data.frame(date=dates[i], month, year, marketing_spend=marketing, Acquisition, revenue, CAC=marketing/Acquisition, customers, LTV=sum(s[1:24]*price*engagement*gm), marginal_cac)
    
      ## Calculate marketing spend for the next month
      if (dates[i]>=start_next_fiscal_year){
        marketing <- max((marketing_allocation+marketing_boost)*revenue, initial_marketing)
      } else{
        marketing <- max(marketing_allocation*revenue, initial_marketing)
      }
   }

   ## Create the dataframe by collapsing the rows
   d <- bind_rows(df) %>%
   mutate(yoy_revenue_growth=revenue/lag(revenue,12)-1, 
          Churn=lag(customers)-customers+Acquisition, 
          revenue_change=revenue-lag(revenue),
          churned_revenue=Churn*engagement*price) %>%
   filter(date>cutoff)
   return(d)
}
   
## Generate the numbers. Note that I'm using the model for historical AND future data. So cheating here. 
d <- sim()

## Annual revenue
group_by(d, year) %>%
      summarise(revenue=sum(revenue)) %>%
      ggplot(aes(x=year, y=revenue)) +
      geom_bar(stat="identity") + 
      scale_y_continuous(labels = scales::dollar) 

## Monthly revenue growth
yoy <- mutate(d, yoy_revenue_growth=ifelse(year==2020, yoy_revenue_growth, NA))
g1 <- filter(yoy, date<=start_forecast & year==2020) %>%
  ggplot(aes(x=date, y=revenue)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0,to=roundup(max(d$revenue), 1000000), by=1000000)) + 
  scale_x_date(breaks="1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) +
  geom_text(aes(label=scales::percent(yoy_revenue_growth, 1)), vjust=-1, size=2) +
  ylab("Monthly revenue") + xlab("Date")
g1
ggsave(g1, file="growth.png", device = "png", dpi=72, width=9, height=6)


## YTD revenue growth
ytd <- filter(d, date<=start_forecast & month<10) %>%
  group_by(year) %>%
  summarise(revenue=sum(revenue)) %>%
  ungroup() %>%
  mutate(label=ifelse(row_number()==1, "Jan - Sep 2019", "Jan - Sep 2020"), 
         growth=revenue/lag(revenue)-1) 

g2 <-
  ggplot(data=ytd, aes(x=label, y=revenue)) +
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0,to=roundup(max(ytd$revenue), 10000000), by=10000000)) +
  geom_text(aes(label = scales::percent(growth)), vjust = -1) +
  ylab("Total revenue") + xlab("")
g2  
ggsave(g2, file="ytd.png", device = "png", dpi=72, width=9, height=6)


## Top down forecast
g3 <- mutate(d, 
       revenue=ifelse(date>start_forecast, lag(revenue,12)*desired_growth, revenue), 
       source=ifelse(date>start_forecast, "Top-down plan", "Historical")) %>%
  filter(date>=chart_start) %>%
  ggplot(aes(x=date, y=revenue, fill=factor(source))) + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0,to=roundup(max(td$revenue), 1000000), by=2000000)) + 
  scale_x_date(breaks="3 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) +
  ylab("Monthly revenue") + xlab("Date")
g3
ggsave(g3, file="topdown.png", device = "png", dpi=72, width=9, height=6)


# Cumulative delta between top-down and bottom-up (model)
dd <- mutate(d, 
       `Top-down plan`=ifelse(date>start_forecast, lag(revenue,12)*desired_growth, 0), 
       `Model-based forecast`=ifelse(date<=start_forecast, 0, revenue), 
       difference=ifelse(year<2021, 0, cumsum(`Top-down plan`-`Model-based forecast`)),
       cumulative_forecast = cumsum(ifelse(year==2021, `Model-based forecast`, 0)),
       percent_difference=difference/cumulative_forecast,
       Historical=ifelse(date<=start_forecast, revenue, 0)) %>%
  select(date, year, `Top-down plan`, `Model-based forecast`, Historical, difference, percent_difference, cumulative_forecast) %>%
  filter(date>=chart_start) 

g4 <- filter(dd, year==2021) %>%
  ggplot(aes(x=date, y=difference)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_date(breaks="1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) +
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0,to=roundup(max(dd$difference, na.rm=T), 1000000), by=2000000)) +
  geom_text(aes(label=scales::percent(percent_difference, 0.1)), vjust=-1, size=2) +
  ylab("Cumulative difference")
g4  

ggsave(g4, file="cumulative_delta.png", device = "png", dpi=72, width=11, height=6)

# Plotting top-down and bottom-up (model) side-by-side
long <- pivot_longer(data=dd, cols=c(`Top-down plan`, `Model-based forecast`, "Historical"), names_to = "source", values_to = "y")
gap <- sum(filter(long, source=="Top-down plan" & year(date)==2021)$y)-
  sum(filter(long, source=="Model-based forecast" & year(date)==2021)$y)

g5 <-
  ggplot(data=filter(long, source != "Historical" & year==2021), aes(x=date, y=y, colour=factor(source))) + 
  #geom_bar(stat="identity", position="dodge") + 
  geom_line(size=1.5) + 
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0,to=roundup(max(long$y, na.rm=T), 1000000), by=500000)) + 
  scale_x_date(breaks="1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank(), legend.position="bottom") +
  ylab("Monthly revenue") + xlab("Date")
g5
ggsave(g5, file="compare.png", device = "png", dpi=72, width=11, height=6)


# Why is the model projecting lower numbers? Why is it flattening?
g6 <- select(d, date, Churn, Acquisition) %>%
  pivot_longer(cols=c("Acquisition", "Churn"), names_to = "source", values_to = "y") %>%
  ggplot(aes(x=date, y=y, colour=factor(source))) + 
  geom_line(size=1.2) + 
  scale_y_continuous(labels = scales::comma, breaks=seq(from=0,to=roundup(max(long$y, na.rm=T), 1000), by=1000)) + 
  scale_x_date(breaks="4 months") +
  geom_vline(xintercept=as.Date("2019-10-01"), linetype=2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank(), legend.position="bottom") +
  ylab("") + xlab("Date")
g6
ggsave(g6, file="tyranny.png", device = "png", dpi=72, width=9, height=6)


# Marginal CACs
g7 <- select(d, date, marginal_cac) %>%
  ggplot(aes(x=date, y=marginal_cac, colour="Marginal CAC")) + 
  geom_line(size=1.2) + 
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_date(breaks="2 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank(), legend.position="bottom") +
  ylab("") + xlab("Date")
g7
ggsave(g7, file="marginal.png", device = "png", dpi=72, width=9, height=6)


### Sensitivity
sens <- list()
for (i in 0:20){
  marketing_boost <- i/100
  dd <- sim()
  runrate <- 12*filter(dd, date==as.Date("2021-12-1"))$revenue
  revenue <- sum(filter(dd, year==2021)$revenue)
  acq <- sum(filter(dd, year==2021)$Acquisition)
  spend <- sum(filter(dd, year==2021)$marketing)
  ddd <- data.frame(allocation=marketing_allocation+marketing_boost, run_rate=runrate, acquisition=acq, marketing_spend=spend, revenue)
  sens[[i+1]] <- ddd 
}
sens_df <- bind_rows(sens) %>%
  mutate(allocation_text=as.factor(paste0(round(100*allocation, 0), "%"))) %>%
  ungroup() %>%
  mutate(run_rate_gain=run_rate-min(run_rate), 
         revenue_gain=revenue-min(revenue),
         acquisition_gain=acquisition-min(acquisition), 
         additional_spend=marketing_spend-min(marketing_spend), 
         marginal_cac=additional_spend/acquisition_gain)

g8 <-
  ggplot(data=sens_df, aes(x=additional_spend, y=revenue_gain)) +
  geom_line(size=2) +
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0, to=roundup(max(sens_df$run_rate_gain, na.rm=T), 1000000), by=2000000)) +
  scale_x_continuous(labels = scales::dollar, breaks=seq(from=0, to=roundup(max(sens_df$additional_spend, na.rm=T), 1000000), by=2000000)) +
  ylab("Incremental revenue in 2021") + xlab("Additional marketing spend") +
  geom_text(aes(label=scales::percent(allocation, 1)), vjust=-3, size=2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = gap, linetype='dashed')
g8

ggsave(g8, file="sensitivity.png", device = "png", dpi=72, width=9, height=6)