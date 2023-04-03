install.packages('rdrobust')
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, modelsummary, rdrobust)

final.data <- read_rds("data/output/final_data.rds")

#1
final.data2 <- final.data %>% filter(snp == 'No' & !is.na(partc_score) & (planid < 800 | planid >= 900)) 
plan_count<- final.data2 %>% group_by(fips, year) %>% summarise(n = n())

plan.count.fig <- plan_count %>%
  ggplot(aes(x = as.factor(year), y = log(n))) +
  geom_boxplot() +
  labs(title = "Distribution of Plan Counts by County", x = "Year", y = "Plan Counts") +
  theme_bw()

plan.count.fig
  
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
  ggplot(aes(x = year, y = mkt_share)) + 
  geom_line() +
  labs(title = "Share of Medicare Advanatge", x = "Year", y = "Market Share") +
  theme_bw()
share.fig

#5
data_09<- final.data2 %>% filter(year == 2009)
data_09<- data_09 %>% mutate(raw_rating=rowMeans(
  cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
        glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
        mental_health,osteo_test,physical_monitor,primaryaccess,
        hospital_followup,depression_followup,nodelays,carequickly,
        overallrating_care,overallrating_plan,calltime,
        doctor_communicate,customer_service,osteo_manage,
        diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
        diabetes_chol,antidepressant,bloodpressure,ra_manage,
        copd_test,betablocker,bladder,falling,appeals_timely,
        appeals_review),
  na.rm=T))

final.data.2009 <- data_09[!is.na(data_09$Star_Rating), ]
final.data.2009$indicator <- ifelse(final.data.2009$Star_Rating > final.data.2009$raw_rating, 1,0)

plans_table <- final.data.2009 %>% group_by(Star_Rating) %>% summarize(avg_ind = mean(indicator))

#6
table_6 <- final.data.2009 %>%
  mutate(score1 = raw_rating - 2.75,
         score2 = raw_rating - 3.25, 
         score3 = raw_rating - 3.75, 
         score4 = raw_rating - 4.25,
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share))

reg_25 <- rdrobust(y=table_6$mkt_share, x=table_6$score1, c=0,
                   h=0.125, p=1, kernel="uniform", vce="hc0",
                   masspoints="off")

reg_3 <- rdrobust(y=table_6$mkt_share, x=table_6$score2, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

reg_4 <- rdrobust(y=table_6$mkt_share, x=table_6$score3, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

reg_45 <-  rdrobust(y=table_6$mkt_share, x=table_6$score4, c=0,
                    h=0.125, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

#7
# bandwidth 0.1
reg_25_2 <- rdrobust(y=table_6$mkt_share, x=table_6$score1, c=0,
                   h=0.1, p=1, kernel="uniform", vce="hc0",
                   masspoints="off")

reg_3_2 <- rdrobust(y=table_6$mkt_share, x=table_6$score2, c=0,
                  h=0.1, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

reg_4_2 <- rdrobust(y=table_6$mkt_share, x=table_6$score3, c=0,
                  h=0.1, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

reg_45_2 <-  rdrobust(y=table_6$mkt_share, x=table_6$score4, c=0,
                    h=0.1, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")
#bandwidth 0.12
reg_25_3 <- rdrobust(y=table_6$mkt_share, x=table_6$score1, c=0,
                     h=0.12, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_3_3 <- rdrobust(y=table_6$mkt_share, x=table_6$score2, c=0,
                    h=0.12, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_4_3 <- rdrobust(y=table_6$mkt_share, x=table_6$score3, c=0,
                    h=0.12, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_45_3 <-  rdrobust(y=table_6$mkt_share, x=table_6$score4, c=0,
                      h=0.12, p=1, kernel="uniform", vce="hc0",
                      masspoints="off")
#bandwidth 0.13
reg_25_4 <- rdrobust(y=table_6$mkt_share, x=table_6$score1, c=0,
                     h=0.13, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_3_4 <- rdrobust(y=table_6$mkt_share, x=table_6$score2, c=0,
                    h=0.13, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_4_4 <- rdrobust(y=table_6$mkt_share, x=table_6$score3, c=0,
                    h=0.13, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_45_4 <-  rdrobust(y=table_6$mkt_share, x=table_6$score4, c=0,
                      h=0.13, p=1, kernel="uniform", vce="hc0",
                      masspoints="off")
#bandwidth 0.14
reg_25_5 <- rdrobust(y=table_6$mkt_share, x=table_6$score1, c=0,
                     h=0.14, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_3_5 <- rdrobust(y=table_6$mkt_share, x=table_6$score2, c=0,
                    h=0.14, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_4_5 <- rdrobust(y=table_6$mkt_share, x=table_6$score3, c=0,
                    h=0.14, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_45_5 <-  rdrobust(y=table_6$mkt_share, x=table_6$score4, c=0,
                      h=0.14, p=1, kernel="uniform", vce="hc0",
                      masspoints="off")
#bandwidth 0.15
reg_25_6 <- rdrobust(y=table_6$mkt_share, x=table_6$score1, c=0,
                     h=0.15, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_3_6 <- rdrobust(y=table_6$mkt_share, x=table_6$score2, c=0,
                    h=0.15, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_4_6 <- rdrobust(y=table_6$mkt_share, x=table_6$score3, c=0,
                    h=0.15, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_45_6 <-  rdrobust(y=table_6$mkt_share, x=table_6$score4, c=0,
                      h=0.15, p=1, kernel="uniform", vce="hc0",
                      masspoints="off")

#4

#5




save.image("homework4_workspace.Rdata")
