install.packages('rdrobust')
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, modelsummary, rdrobust)

final.data <- read_rds("data/output/final_data.rds")

#1
final.data2 <- final.data %>% filter(snp == 'No' & (planid < 800 | planid >= 900)) 
plan_count<- final.data2 %>% group_by(fips, year) %>% summarise(n = n())

plan.count.fig <- plan_count %>%
  ggplot(aes(x = as.factor(year), y = log(n))) +
  geom_boxplot() +
  labs(title = "Distribution of Plan Counts by County", x = "Year", y = "Plan Counts") +
  theme_bw()

plan.count.fig
  
#2
ratings.fig <- final.data2 %>% filter(year == '2009'| year == '2012'| year == '2015' & !is.na(Star_Rating)) %>%
ggplot(aes(x = as.factor(Star_Rating))) +
  geom_bar(aes(fill = as.factor(year)), position = "dodge") +
  scale_fill_grey() + 
  labs(title = "Distribution of Star Ratings", x = "Star Ratings", y = "Count of Plans", fill="Year") +
  theme_bw()

ratings.fig

#3
filtered_data <- final.data %>% filter(year %in% c(2009:2015))

bench.fig<- filtered_data %>% 
  group_by(year) %>%
  summarise(avg_bench = mean(ma_rate , na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(year), y = avg_bench)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Benchmark Payments, 2009-2015", x = "Year", y = "Average Benchmark Payments") +
  theme_bw()
bench.fig

#4
filtered_data2 <- filtered_data %>% group_by(fips, year) %>% 
  summarise(enroll = first(avg_enrolled), medicare=first(avg_eligibles), 
                                             bench=mean(ma_rate, na.rm = TRUE)) %>%
  mutate(mkt_share = enroll/medicare) %>% 
  group_by(year) %>%
  summarise(avg_share = mean(mkt_share, na.rm = TRUE))

share.fig <- filtered_data2 %>% 
  ggplot(aes(x = as.factor(year), y = avg_share, group=1)) + 
  stat_summary(fun.y = "mean", geom = "line", na.rm = TRUE) +
  labs(title = "Share of Medicare Advanatge", x = "Year", y = "Market Share") +
  theme_bw()
share.fig

#5
data_09<- final.data2 %>% filter(!is.na(avg_enrollment & year == 2009))
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

ma.rd1 <- table_6 %>%
  filter(Star_Rating==2.5 | Star_Rating==3)

rd_plot1<- rdplot(y=ma.rd1$mkt_share, x=ma.rd1$score1, binselect="es",
       title="RD Plot: Market Share for 2.5 vs 3 Stars", x.label="Summary Score",
       y.label="Market Share", masspoints="off")

ma.rd2 <- table_6 %>%
  filter(Star_Rating==3 | Star_Rating==3.5)
rd_plot2 <- rdplot(y=ma.rd2$mkt_share, x=ma.rd2$score2, binselect="es",
                   title="RD Plot: Market Share for 3 vs 3.5 Stars", x.label="Summary Score",
                   y.label="Market Share", masspoints="off")

ma.rd3 <- table_6 %>%
  filter(Star_Rating==3.5 | Star_Rating==4)
rd_plot3 <- rdplot(y=ma.rd3$mkt_share, x=ma.rd3$score3, binselect="es",
                   title="RD Plot: Market Share for 3.5 vs 4 Stars", x.label="Summary Score",
                   y.label="Market Share", masspoints="off")

ma.rd4 <- table_6 %>%
  filter(Star_Rating==4 | Star_Rating==4.5)
rd_plot4 <- rdplot(y=ma.rd4$mkt_share, x=ma.rd4$score4, binselect="es",
                   title="RD Plot: Market Share for 4 vs 4.5 Stars", x.label="Summary Score",
                   y.label="Market Share", masspoints="off")

#5



save.image("homework4_workspace.Rdata")
