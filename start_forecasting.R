
setwd("/Users/kim.larsen/Documents/Code/Pretend_Company_Growth/Bad Forecasting")

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
base <- 200
gm <- 0.4
dates <- seq(as.Date("2017-1-1"), as.Date("2020-12-1"), by = "months")
n <- length(dates)
start_forecast <- as.Date("2019-09-1")
cutoff <- as.Date("2017-12-1")
chart_start <- as.Date("2019-01-1")
start_next_fiscal_year <- as.Date("2020-01-1")
marketing_boost <- 1

  
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
      marketing <- max(marketing_allocation*revenue, initial_marketing)
      if (dates[i]>=start_next_fiscal_year){
        marketing <- marketing*marketing_boost  
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
   
d <- sim()

## Annual revenue
group_by(d, year) %>%
      summarise(revenue=sum(revenue)) %>%
      ggplot(aes(x=year, y=revenue)) +
      geom_bar(stat="identity") + 
      scale_y_continuous(labels = scales::dollar) 

## YOY revenue
g1 <- filter(d, date<=start_forecast) %>%
  ggplot(aes(x=month, y=revenue, fill=factor(year))) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0,to=roundup(max(d$revenue), 1000000), by=1000000)) + 
  scale_x_continuous(breaks = seq(1:12)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) +
  ylab("Monthly revenue") + xlab("Month")
g1
ggsave(g1, file="yoy.png", device = "png", dpi=72, width=9, height=6)


## Top down forecast
td <- mutate(d, 
       revenue=ifelse(date>start_forecast, lag(revenue,12)*1.22, revenue), 
       source=ifelse(date>start_forecast, "Topdown plan", "Historical")) %>%
  filter(date>=chart_start)

g2 <-  ggplot(data=td, aes(x=date, y=revenue, fill=factor(source))) + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0,to=roundup(max(td$revenue), 1000000), by=1000000)) + 
  scale_x_date(breaks="3 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) +
  ylab("Monthly revenue") + xlab("Date")
g2
ggsave(g2, file="topdown.png", device = "png", dpi=72, width=9, height=6)


# How aggressive is this?
long <- mutate(d, 
       `Topdown plan`=ifelse(date>start_forecast, lag(revenue,12)*1.22, NA), 
       Model=ifelse(date<=start_forecast, NA, revenue), 
       Historical=ifelse(date<=start_forecast, revenue, NA)) %>%
  select(date, `Topdown plan`, Model, Historical) %>%
  filter(date>=chart_start) %>%
  pivot_longer(cols=c(`Topdown plan`, "Model", "Historical"), names_to = "source", values_to = "y") 

g3 <- 
  ggplot(data=long, aes(x=date, y=y, fill=factor(source))) + 
  geom_bar(stat="identity", position="dodge") + 
  scale_y_continuous(labels = scales::dollar, breaks=seq(from=0,to=roundup(max(long$y, na.rm=T), 1000000), by=1000000)) + 
  scale_x_date(breaks="3 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) +
  ylab("Monthly revenue") + xlab("Date")
g3
ggsave(g3, file="compare_methohds.png", device = "png", dpi=72, width=9, height=6)



# Why is the model negative? Why the flattening?
g4 <- select(d, date, Churn, Acquisition) %>%
  filter(date>start_next_fiscal_year) %>%
  pivot_longer(cols=c("Acquisition", "Churn"), names_to = "source", values_to = "y") %>%
  ggplot(aes(x=date, y=y, colour=factor(source))) + 
  geom_line(size=1.2) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_date(breaks="2 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank(), legend.position="bottom") +
  ylab("") + xlab("Date")
g4
ggsave(g4, file="tyrani.png", device = "png", dpi=72, width=9, height=6)


# blame the CACs
g5 <- select(d, date, marginal_cac) %>%
  filter(date>start_next_fiscal_year) %>%
  ggplot(aes(x=date, y=marginal_cac, colour="Marginal CAC")) + 
  geom_line(size=1.2) + 
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_date(breaks="2 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank(), legend.position="bottom") +
  ylab("") + xlab("Date")
g5
ggsave(g5, file="blame_cacs.png", device = "png", dpi=72, width=9, height=6)


ggsave(plot_grid(g4, g5), file="blame_cacs_and_churn.png", device = "png", dpi=72, width=9, height=6)

### Sensitivity
sens <- list()
for (i in 10:20){
  marketing_boost <- i/10
  dd <- sim()
  runrate <- 12*filter(dd, date==as.Date("2020-12-1"))$revenue
  ddd <- data.frame(allocation=marketing_allocation*marketing_boost, run_rate=runrate)
  sens[[i-9]] <- ddd 
}

g6 <- bind_rows(sens) %>%
  ggplot(aes(x=allocation, y=run_rate)) +
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(labels = scales::percent, breaks=seq(from=0, to=.3, by=0.05)) +
  ylab("Annual run-rate at the end of 2020") + xlab("Marketing allocation (% of revenue)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
ggsave(g6, file="sensitivity.png", device = "png", dpi=72, width=9, height=6)