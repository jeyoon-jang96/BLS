library(blscrapeR)
library(dplyr)
library(tidyverse)
BLS <- get_bls_county()
head(BLS)
tail(BLS)

bridges <- read.csv("2018AllRecordsDelimitedAllStates.csv")
colnames(bridges)
head(bridges)


bridge <- bridges %>%
  filter(!(STATE_CODE_001 %in% c('G' , '1014', '164.11'))) %>%
  filter(!(is.na(COUNTY_CODE_003)))

summary(bridge$STATE_CODE_001)
summary(bridge$COUNTY_CODE_003)

bridge <- bridge %>%
  rename(fips_state = STATE_CODE_001, fips_county = COUNTY_CODE_003)

samplebridge <-sample_n(bridge,5000)
sampleBLS <- sample_n(BLS, 1000)

finaldata <-merge(bridge,BLS,by=c("fips_state","fips_county"))


finaldata$APPR_WIDTH_MT_032<- as.numeric(finaldata$APPR_WIDTH_MT_032 )
finaldata$ROADWAY_WIDTH_MT_051 <- as.numeric(finaldata$ROADWAY_WIDTH_MT_051)
finaldata$DECK_WIDTH_MT_052 <- as.numeric(finaldata$DECK_WIDTH_MT_052)
finaldata$VERT_CLR_OVER_MT_053 <- as.numeric(finaldata$VERT_CLR_OVER_MT_053)
finaldata$VERT_CLR_UND_054B <- as.numeric(finaldata$VERT_CLR_UND_054B)
finaldata$INVENTORY_RATING_066 <- as.numeric(finaldata$INVENTORY_RATING_066)
finaldata$fips_county <- as.factor(finaldata$fips_county)


finaldata <- finaldata %>%
  select_if(is.numeric)%>%
  drop_na()




#samplefinaldata <- sample_n(finaldata, 5000)

library(glmnet)
library(Matrix)

finaldata1 <-finaldata %>%
  select(-unemployed_rate)

x_var <- model.matrix(unemployed~.,finaldata1)[,-1]
y_var <- finaldata1$unemployed
lambda_seq<- 10^seq(2,-2, by= -.1)

set.seed(12345)
train = sample(1:nrow(x_var), nrow(x_var)/2)
test = (-train)
y_test = y_var[test]

cv_output <-cv.glmnet(x_var[train,], y_var[train], alpha=1, lamda= lamda_seq)

best_lam <- cv_output$lambda.min

lasso_best <- glmnet(x_var[train,], y_var[train], alpha = 1, lambda = best_lam)

pred <- predict(lasso_best, s=best_lam, newx = x_var[test,])

mod <- cbind (y_var[test], pred)

head(mod)
tail(mod)

coef(lasso_best)


finaldata2 <- finaldata %>%
  select(-unemployed)

x_var2 <- model.matrix(unemployed_rate~.,finaldata2)[,-1]
y_var2 <- finaldata2$unemployed
lambda_seq<- 10^seq(2,-2, by= -.1)

set.seed(12345)
train2 = sample(1:nrow(x_var2), nrow(x_var2)/2)
test2 = (-train2)
y_test2 = y_var2[test2]

cv_output2 <-cv.glmnet(x_var2[train,], y_var2[train], alpha=1, lamda= lamda_seq)

best_lam2 <- cv_output2$lambda.min

lasso_best2 <- glmnet(x_var2[train,], y_var2[train], alpha = 1, lambda = best_lam2)

pred2 <- predict(lasso_best2, s=best_lam2, newx = x_var2[test,])

mod2 <- cbind (y_var2[test], pred)

head(mod2)
tail(mod2)

coef(lasso_best2)



November <-get_bls_county(date_mth = "November 2019")

November <- November %>%
  rename( Nov_employed = employed, Nov_unemplyed_rate = unemployed_rate)



Onemorefinal <- merge(finaldata2, November, by =c("fips_state", "fips_county"))






#bridge <- bridge %>%
  as_tibble() %>%
  select(STATE_CODE_001,COUNTY_CODE_003)%>%
  mutate(STATE_CODE_001 = as.character(STATE_CODE_001))%>%
  mutate(COUNTY_CODE_003 = as.character(COUNTY_CODE_003))%>%
  mutate(fips= c(STATE_CODE_001,COUNTY_CODE_003))

#summary(bridges$STATE_CODE_001)
#summary(bridges$COUNTY_CODE_003)


library(dplyr)
rename_all(bridges, recode, STATE_CODE_001 ="fips_state",COUNTY_CODE_003="fips_county")
left_join(bridges, BLS, by = c("fips_state", "fips_county"))

bridges %>%
  as_tibble()%>%
  select(fips_state, fips_county, STRUCTURE_LEN_MT_049, TOTAL_IMP_COST_096)
  
merge(bridges, BLS, all.x=TRUE)
