install.packages('rdrobust')
install.packages('broom')
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest,
               modelsummary, rdrobust, broom)

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
data_09<- final.data %>% filter(!(is.na(avg_enrollment)) & year == 2009)
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

plan_table <- data_09 %>% 
  mutate(rounded_30 = ifelse(raw_rating>=2.75 & raw_rating<3.00 & Star_Rating==3.0,1,0),
         rounded_35 = ifelse(raw_rating>=3.25 & raw_rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40 = ifelse(raw_rating>=3.75 & raw_rating<4.00 & Star_Rating==4.0,1,0),
         rounded_45 = ifelse(raw_rating>=4.25 & raw_rating<4.50 & Star_Rating==4.5,1,0),
         rounded_50 = ifelse(raw_rating>=4.50 & raw_rating<5.00 & Star_Rating==5.0,1,0),
         ) %>%
  group_by(factor(Star_Rating)) %>%
  filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>%
  summarise(count_30=sum(rounded_30),
            count_35=sum(rounded_35),
            count_40=sum(rounded_40),
            count_45=sum(rounded_45),
            count_50=sum(rounded_50))
plan_table

#6

data_09 <- data_09 %>%
  mutate(score = raw_rating - 2.25,
         treat = (score>=0),
         window1 = (score>=-.175 & score<=.175),
         window2 = (score>=-.125 & score<=.125),
         score_treat=score*treat)

star30 <- lm(avg_enrollment ~ treat + score,
               data = (data_09 %>% 
                         filter(raw_rating>=(2.75-0.125),
                                raw_rating<=(2.75+0.125),
                                Star_Rating %in% c(2.5,3.0)) %>%
                         mutate(treat=(Star_Rating==3.0),
                                score=raw_rating-2.75)))
coef.30.1 <- tidy(star30, conf.int=TRUE) %>% mutate(rating=30)

star35 <- lm(avg_enrollment ~ treat + score,
               data = (data_09 %>% 
                         filter(raw_rating>=(3.25-0.125),
                                raw_rating<=(3.25+0.125),
                                Star_Rating %in% c(3.0,3.5)) %>%
                         mutate(treat=(Star_Rating==3.5),
                                score=raw_rating-3.25)))
coef.35.1 <- tidy(star35, conf.int=TRUE) %>% mutate(rating=35)

star40 <- lm(avg_enrollment ~ treat + score,
               data = (data_09 %>% 
                         filter(raw_rating>=(3.75-0.125),
                                raw_rating<=(3.75+0.125),
                                Star_Rating %in% c(3.5,4.0)) %>%
                         mutate(treat=(Star_Rating==4.0),
                                score=raw_rating-3.75)))
coef.40.1 <- tidy(star30, conf.int=TRUE) %>% mutate(rating=40)

star45 <- lm(avg_enrollment ~ treat + score,
               data = (data_09 %>% 
                         filter(raw_rating>=(4.25-0.125),
                                raw_rating<=(4.25+0.125),
                                Star_Rating %in% c(4.0,4.5)) %>%
                         mutate(treat=(Star_Rating==4.5),
                                score=raw_rating-4.25)))
coef.45.1 <- tidy(star45, conf.int=TRUE) %>% mutate(rating=45)


#table.reg<- data.frame(Star_Rating = c("2.5 vs 3", "3.5 vs 4", "4 vs 4.5"),
#Estimate = c(reg_25$Estimate[1], reg_3$coef[1], reg_4$coef[1]))

#7

for (h in seq(0.1, 0.15, 0.01)) {
  star30bw <- lm(avg_enrollment ~ treat + score,
                 data = (data_09 %>% 
                           filter(raw_rating>=(2.75-h),
                                  raw_rating<=(2.75+h),
                                  Star_Rating %in% c(2.5,3.0)) %>%
                           mutate(treat=(Star_Rating==3.0),
                                  score=raw_rating-2.75)))
  coef.30 <- tidy(star30bw, conf.int=TRUE) %>% mutate(rating=30)
  
  star35bw <- lm(avg_enrollment ~ treat + score,
                 data = (data_09 %>% 
                           filter(raw_rating>=(3.25-h),
                                  raw_rating<=(3.25+h),
                                  Star_Rating %in% c(3.0,3.5)) %>%
                           mutate(treat=(Star_Rating==3.5),
                                  score=raw_rating-3.25)))
  coef.35 <- tidy(star35bw, conf.int=TRUE) %>% mutate(rating=35)
  
  star40bw <- lm(avg_enrollment ~ treat + score,
                 data = (data_09 %>% 
                           filter(raw_rating>=(3.75-h),
                                  raw_rating<=(3.75+h),
                                  Star_Rating %in% c(3.5,4.0)) %>%
                           mutate(treat=(Star_Rating==4.0),
                                  score=raw_rating-3.75)))
  coef.40 <- tidy(star30bw, conf.int=TRUE) %>% mutate(rating=40)
  
  star45bw <- lm(avg_enrollment ~ treat + score,
                 data = (data_09 %>% 
                           filter(raw_rating>=(4.25-h),
                                  raw_rating<=(4.25+h),
                                  Star_Rating %in% c(4.0,4.5)) %>%
                           mutate(treat=(Star_Rating==4.5),
                                  score=raw_rating-4.25)))
  coef.45 <- tidy(star45bw, conf.int=TRUE) %>% mutate(rating=45)
}


est.collect <- rbind(coef.30, coef.35, coef.40) %>%
  mutate(bandwidth=h)


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

#8
dens25 <- rddensity(ma.rd1$score1, c=0)
rdplotdensity(dens25, ma.rd1$score1)

dens3 <- rddensity(ma.rd2$score2, c=0)
rdplotdensity(dens3, ma.rd2$score2)

dens4 <- rddensity(ma.rd3$score3, c=0)
rdplotdensity(dens4, ma.rd3$score3)

dens45 <- rddensity(ma.rd4$score4, c=0)
rdplotdensity(dens45, ma.rd4$score4)

#9



save.image("homework4workspace.Rdata")
