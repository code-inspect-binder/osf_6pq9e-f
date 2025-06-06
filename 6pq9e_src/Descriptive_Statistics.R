# Descriptive statistics using summarytools package
### Clear memory
rm(list = ls())

library(summarytools)
library(marginaleffects)
library(dplyr)
db = read.csv("db_clean_reg.csv",header=TRUE)

# db = read.csv("taptop2.csv",header=TRUE)


# Inputs for Table 1

freq(db[,c("choice2","female","edu","drur")],cumul=FALSE,round.digits=0,report.nas = FALSE)
descr(db[,c("age","inc_cont","nchildren")],
      headings = FALSE,
      stats = c("mean","sd"),
      split.tables=80,
      round.digits=0
)


# Table 2 - including Chi2 test reported in the body text (order changes w.r.t manuscript content)

  ctable(x = db$qual, y = db$choice2,
         chisq = TRUE,
         prop="r",
         totals=FALSE,
         headings = FALSE)
 

db$tap = ifelse(db$choice2==1,1,0)


# Inputs for Table 3
# Only the mean is reported for nitrates and quality perception


db %>% 
  group_by(region) %>% 
  descr(nit_sout_ar,stats = "mean")

db %>% 
  group_by(region) %>% 
  descr(qual,stats = "mean")


db %>% 
  group_by(region) %>% 
  freq(tap,report.nas = FALSE,cumul = FALSE)

# Or for tap==1 only

db %>% 
  group_by(region) %>% 
  freq(tap,rows=2,report.nas = FALSE,cumul = FALSE)


# Other syntax / with stby from summarytools

(db_stats_by_regions <- stby(data      = db[,c("nit_sout_ar","qual")], 
                             INDICES   = db$region, 
                             FUN       = descr, 
                             stats     = "mean"))


# Table 4

freq(db[,c("cost_m3_na","prix_diff","price_tap")],cumul=FALSE,headings=FALSE,report.nas = FALSE)

# additional inputs - December 2023 second round of revision

freq(db[,c("qual","qual_f")],cumul=FALSE,headings=FALSE,report.nas = FALSE)

# additional results - second round of revision

# price_tap 0=DK, 1=Affordable 2=Costly

db$price_tap<-as.factor(db$price_tap)

# cost_m3_nA 1=K 2=DK

db$cost_m3_na<-as.factor(db$cost_m3_na)

# recoding for clarity purpose
# dk_cost_m3=1 for respondent who answer don't know the cubic meter price

db$dk_cost_m3<-as.numeric(db$cost_m3_na=="2")

freq(db[,c("price_tap","dk_cost_m3")],cumul=FALSE,round.digits=0,report.nas = FALSE)

# chi-square test

ctable(x = db$dk_cost_m3, y = db$price_tap,
       chisq = TRUE,
       prop="r",
       totals=FALSE,
       headings = FALSE)


# Association between DK m3 and DK price_tap
# with affordable as base category for price_tap

db<-within(db,price_tap <- relevel(price_tap, ref = "1"))

mod1<-glm(dk_cost_m3~price_tap,family=binomial(link="logit"), data=db)
summary(mod1)
summary(marginaleffects(mod1))

# with costly as base category for price_tap

db<-within(db,price_tap <- relevel(price_tap, ref = "2"))

mod2<-glm(dk_cost_m3~price_tap,family=binomial(link="logit"), data=db)
summary(mod2)
summary(marginaleffects(mod2))

# correlation between pestices and nitrates at the "département" level
# rho=0.8544 p<0.000

cor.test(db$pesti_esout_rd_mod,db$nitrates_esout_rd_mod)

