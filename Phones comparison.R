rm(list = ls())

## load packages
require(dplyr)
require(stringr)
require(foreign)
require(ggplot2)
require(survey)
require(purrr)
require(ggmap)

## load data
filepath.1 <- here::here("data", "mVAM_panel.csv")
dat <- read.csv(filepath.1)
as.tbl(dat)
str(dat)

## create needed variables
# add the FCS
dat <- dat%>%
  mutate(FCS.rank = 
           factor(case_when(
             FCS <= 28 ~ 1,
             FCS > 28 & FCS <= 42 ~ 2,
             FCS > 42 ~ 3),
             levels = c(1, 2, 3),
             labels = c("Poor", "Borderline", "Acceptable")
             )
         )
# create own_phone, factor with 2 levels
dat$own_phone <- dat$Doesyourhouseholdhavemobilephone
dat$own_phone <- factor(dat$own_phone,
                        levels = c(1,2),
                        labels = c("Yes", "No")
                        )
# create survey_type, factor with 5 levels
dat$survey_type <- factor(dat$Type_of_survey,
                          levels = c(1,2,3,4,5),
                          labels = c("Phone", "F2F (HH no phone)", "F2F (HH w/ phone)", "F2F (HH w/ phone not reachable)", "F2F new HH")
                          )

# create round, factor with 5 levels
dat <- dat%>%
  mutate(round = 
           factor(
             case_when(
               Month == "April" ~ 4,
               Month == "August" ~ 5,
               Month == "December" ~ 3,
               Month == "June" ~ 2,
               Month == "November" ~ 1),
             levels = c(1,2,3,4,5)
         )
  )
## cleaning: phone/ survey type.

table(dat$survey_type, dat$own_phone)

test <- dat%>%
  group_by(round, survey_type, own_phone)%>%
  summarise(n())

class(dat$Phonenumber)

# quick summaries

tot.hh.per.psu.per.round <- dat%>%
  group_by(Year, Month, Ward_name)%>%
  summarize(No.households = n())
sample.size.by.round <- table(dat$Year, dat$Month)
levels(dat$rounds)

## Mean FCS comparison across rounds and survey type--naive, without survey design

naive <- dat%>%
  group_by(round, survey_type)%>%
  summarize(FCS = mean(FCS),
            n())

naive.2 <- dat%>%
  group_by(round, own_phone)%>%
  summarize(FCS = mean(FCS),
            n())

# create different dataframes for each round, then extract from list
list.1 <- split(dat, dat$round)

for(i in seq(list.1)){
  assign(paste0("Round.", i), list.1[[i]])
}

# Set survey for each Round
svy.rd.1 <- svydesign(Round.1$Ward_name, weights = Round.1$Weight, strata = Round.1$Strata, data = Round.1)
svy.rd.2 <- svydesign(Round.2$Ward_name, weights = Round.2$Weight, strata = Round.2$Strata, data = Round.2)
svy.rd.3 <- svydesign(Round.3$Ward_name, weights = Round.3$Weight, strata = Round.3$Strata, data = Round.3)
svy.rd.4 <- svydesign(Round.4$Ward_name, weights = Round.4$Weight, strata = Round.4$Strata, data = Round.4)
svy.rd.5 <- svydesign(Round.5$Ward_name, weights = Round.5$Weight, strata = Round.5$Strata, data = Round.5)

# Round 1 phone tables with average FCS
table.phone.rd.1 <- svyby(~FCS, ~own_phone, svymean, design = svy.rd.1)
table.phone.rd.1 <- mutate(table.phone.rd.1, Round = 1)

# Round 2 tables with average FCS
table.type.rd.2 <- svyby(~FCS, ~survey_type, svymean, design = svy.rd.2)
  table.type.rd.2 <- mutate(table.type.rd.2, Round = 2)
table.phone.rd.2 <- svyby(~FCS, ~own_phone, svymean, design = svy.rd.2)
  table.phone.rd.2 <- mutate(table.phone.rd.2, Round = 2)

# Round 4 tables with average FCS
table.type.rd.4 <- svyby(~FCS, ~survey_type, svymean, design = svy.rd.4)
  table.type.rd.4 <- mutate(table.type.rd.4, Round = 4)
table.phone.rd.4 <- svyby(~FCS, ~own_phone, svymean, design = svy.rd.4)
  table.phone.rd.4 <- mutate(table.phone.rd.4, Round = 4)
  
# Round 5 tables with average FCS
table.type.rd.5 <- svyby(~FCS, ~survey_type, svymean, design = svy.rd.5)
  table.type.rd.5 <- mutate(table.type.rd.5, Round = 5)
table.phone.rd.5 <- svyby(~FCS, ~own_phone, svymean, design = svy.rd.5)
  table.phone.rd.5 <- mutate(table.phone.rd.5, Round = 5)

type.table <- bind_rows(table.type.rd.2,table.type.rd.4, table.type.rd.5)
type.table <- mutate(type.table, 
                     ymin = FCS - (1.96 * se),
                     ymax = FCS + (1.96 * se))
phone.table <- bind_rows(table.phone.rd.1, table.phone.rd.2, table.phone.rd.4, table.phone.rd.5)
phone.table <- mutate(phone.table,
                      ymin = FCS - (1.96 * se),
                      ymax = FCS + (1.96* se))

# graphing differences across phone ownership and survey type
ggplot(phone.table,
       aes(x = own_phone,
           y = FCS))+
  geom_errorbar(aes(ymin = ymin,
                    ymax = ymax),
                width = 0.05,
                size = 0.5)+
  geom_point(shape = 15,
             size = 3)+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45))+
  ylab("Food Consumption Score")+
  xlab("Household Phone Ownership")+
  geom_hline(yintercept = 42, color = "red")+
  facet_grid(~Round)

ggplot(type.table,
       aes(x = survey_type,
           y = FCS))+
  geom_errorbar(aes(ymin = ymin,
                    ymax = ymax),
                width = 0.05,
                size = 0.5)+
  geom_point(shape = 15,
             size = 3)+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45))+
  ggtitle("FCS by Round and survey type (2, 4, 5)")+
  ylab("Food Consumption Score")+
  xlab("Survey Type")+
  facet_grid(~Round)