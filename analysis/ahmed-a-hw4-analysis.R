install.packages('rdrobust')
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, modelsummary, rdrobust)

final.data <- read_rds("data/output/final_data.rds")

#1
final.data2 <- final.data %>% filter(snp == 'No' & partd == 'No' & !(planid %in% 800:899)) 
plan_count<- final.data2 %>% group_by(fips, year, plan_type) %>% count() %>% arrange(-n)

plan.count.plot <- 

plan_count %>% group_by(fips, year) %>% 
  select(fips, year) %>% 
  group_by(fips, year, plan_type) %>%
  summarise(count = count()) %>%
  ggplot(aes(x = count, y = year)) +
  geom_boxplot(fill="blue", colour = "black", alpha=0.9) +
  labs(title = "Distribution of Plan Counts by County", x = "Year", y = "Plan Counts") +
  theme_bw()

plan.count.plot <- plan_count %>% 
  group_by(fips, year) %>%
  select(fips, year, plan_type) %>% 
  group_by(fips, year, plan_type) %>%
  summarise(plan_co= count()) %>%
  ggplot(aes(x = year, y = plan_co)) +
  geom_boxplot(fill="blue", colour = "black", alpha = 0.9) +
  labs(title = "Distribution of Plan Counts by County", x = "Year", y = "Plan Counts") +
  theme_bw()

plan.count.plot

chart_unemployed<- ggplot(df.employment, aes(x=S003, y= D1)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = D1 - CI_1, ymax = D1 + CI_1),
                width = 0.2) +
  labs(title = "Difference in wellbeing (full time and unemployed)",
       x = "Country",
       y = "Difference in Means") +
  theme_bw()
print(chart_unemployed)

#create function for box and whisker plot   
f<- function(x) {
  r<- quantile(x, probs=c(0.10,0.25,0.5,0.75,0.90))  
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
  
}
  
#2
ratings.fig <- final.data2 %>% filter(year == '2009'| year == '2012'| year == '2015') %>%
ggplot(aes(x = as.factor(Star_Rating))) +
  geom_bar(aes(fill = as.factor(year))) +
  scale_fill_grey() + 
  labs(title = "Distribution of Star Ratings", x = "Star Ratings", y = "Count of Plans", fill="Year") +
  theme_bw()
  
ratings.fig

#3
filtered_data <- final.data2 %>% filter(year %in% c(2009:2015))
filtered_data <- filtered_data %>% mutate(avg_bench = mean(ma_rate))

bench.fig<- filtered_data %>% ggplot(aes(x = year, y = ma_rate)) +
  geom_line() +
  labs(title = "Average Benchmark Payments, 2009-2015", x = "Year", y = "Average Benchmark Payments") +
  theme_bw()
bench.fig

#4
filtered_data <- filtered_data %>% mutate(mkt_share = avg_enrollment/avg_eligibles)

share.fig <- filtered_data %>% group_by(fips, year) %>%
  select(fips, year, mkt_share) %>%
  ggplot(aes(x = year, y = mkt_share)) + 
  geom_line() +
  labs(title = "Share of Medicare Advanatge", x = "Year", y = "Market Share") +
  theme_bw()
share.fig

#5





















