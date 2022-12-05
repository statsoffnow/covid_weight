#workDir
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research/Wave3_covid")
setwd("~/Documents/Research/Wave3_covid")
options(scipen = 999)
options(digits = 3)

# libraries -----------
library(psych)
library(dplyr)
library(mlogit)
library(ggplot2)
library(readxl)
covid <- read_excel("covid_masa1.xlsx")

# to factor ------------
covid$plec01 <- factor(covid$plec01,
                       levels = c(0,1),
                       labels = c("K", "M"))

covid$edu3gr <- factor(covid$edu3gr,
                       levels = c(1,2,3),
                       labels = c("High", "Mid", "Low"))

covid$edu_high[covid$edu3gr=="Low"] <- 0
covid$edu_high[covid$edu3gr=="Mid"|covid$edu3gr=="High"] <- 1


covid$stancyw01 <- factor(covid$stancyw01,
                          levels = c(0,1),
                          labels = c("single", "cohab"))

covid$sytzaw01 <- factor(covid$sytzaw01,
                         levels = c(0,1),
                         labels = c("non_empl", "working"))

covid$BMI3gr <- factor(covid$BMI3gr,
                       levels = c(1,2,3),
                       labels = c("UW+N", "OW", "OB"))

covid$Zmiana_masy012 <- factor(covid$Zmiana_masy012,
                               levels = c(0,1,2),
                               labels = c("no change", "more", "less"))

covid$pali01 <- factor(covid$pali01,
                       levels = c(0,1),
                       labels = c("no", "smoke"))

covid$samooc3gr <- factor(covid$samooc3gr,
                          levels = c(1,2,3),
                          labels = c("good", "med", "poor"))

covid$health[covid$samooc3gr=="good"] <- 0
covid$health[covid$samooc3gr=="med"|covid$samooc3gr=="poor"] <- 1


covid$dieta3 <- factor(covid$dieta3,
                       levels = c(0,1,2),
                       labels = c("no change", "better", "worse"))

covid$diet_change[covid$dieta3=="no change"] <- 0
covid$diet_change[covid$dieta3=="better"|covid$dieta3=="worse"] <- 1

covid$Kwarantanna01 <- factor(covid$Kwarantanna01,
                              levels = c(0,1),
                              labels = c("no", "yes"))

covid$covtest <- factor(covid$covtest,
                        levels = c(1,2),
                        labels = c("done", "never"))

covid$covtest_rekod <- factor(covid$covtest_rekod,
                              levels = c(0,1),
                              labels = c("no", "positive"))

covid$covidchor_taknie <- factor(covid$covidchor_taknie,
                                 levels = c(0,1),
                                 labels = c("no", "yes"))

covid$hosp01z289covtak <- factor(covid$hosp01z289covtak,
                                 levels = c(0,1),
                                 labels = c("no", "yes"))

covid$covlekitest <- factor(covid$covlekitest,
                            levels = c(0,1),
                            labels = c("no", "yes"))


covid$hosp01z217covtak <- factor(covid$hosp01z217covtak,
                                 levels = c(0,1),
                                 labels = c("no", "yes"))

covid$hosp01test_plus <- factor(covid$hosp01test_plus,
                                levels = c(0,1),
                                labels = c("no", "yes"))

covid$cov_szczep2gr <- factor(covid$cov_szczep2gr,
                              levels = c(1,2),
                              labels = c("yes", "no"))

covid$negative_healthy[covid$covidchor_taknie=="yes" & covid$covtest_rekod=="no"] <- 1
covid$negative_healthy[covid$covidchor_taknie=="no"] <- 0

covid %>% 
  select(negative_healthy, covtest_rekod, covidchor_taknie) %>% 
  print(n=3000)

#covtest_posneg
covid$covtest_posneg[covid$covtest=="done" & covid$covtest_rekod=="positive"] <- 1
covid$covtest_posneg[covid$covtest=="done" & covid$covtest_rekod=="no"] <- 0

covid %>% 
  count(covtest, covtest_rekod, covtest_posneg)

#covid w wywiadzie
covid$covid_wywiad[covid$covd_chory == 1 |
                     covid$covd_chory == 2|
                     covid$covd_chory ==3] <- 1
covid$covid_wywiad[covid$covd_chory == 4] <- 0

covid$covid_wywiad <- factor(covid$covid_wywiad,
                             levels = c(0,1),
                             labels = c("no", "yes"))


covid$wywiad_hosp[covid$covid_wywiad == "yes" & covid$hosp01z289covtak == "yes"] <- 2
covid$wywiad_hosp[covid$covid_wywiad == "yes" & covid$hosp01z289covtak == "no"] <- 1
covid$wywiad_hosp[covid$covid_wywiad == "no"] <- 0

covid$wywiad_hosp <- factor(covid$wywiad_hosp,
                            levels = c(0,1,2),
                            labels = c("healthy", "no hosp", "hosp"))


covid %>% 
  count(covlekitest)
covid$hosp01z217covtak

covid$lek_hosp[covid$covlekitest == "yes" & covid$hosp01z217covtak == "yes"] <- 2
covid$lek_hosp[covid$covlekitest == "yes" & covid$hosp01z217covtak == "no"] <- 1
covid$lek_hosp[covid$covlekitest == "no"] <- 0

covid$lek_hosp <- factor(covid$lek_hosp,
                            levels = c(0,1,2),
                            labels = c("healthy", "no hosp", "hosp"))

covid %>% 
  count(covtest_rekod)
covid$hosp01test_plus

covid$test_hosp[covid$covtest_rekod == "positive" & covid$hosp01test_plus == "yes"] <- 2
covid$test_hosp[covid$covtest_rekod == "positive" & covid$hosp01test_plus == "no"] <- 1
covid$test_hosp[covid$covtest_rekod == "no"] <- 0

covid$test_hosp <- factor(covid$test_hosp,
                         levels = c(0,1,2),
                         labels = c("healthy", "no hosp", "hosp"))


# Braki danych ---------------
summary(is.na(covid$wiek))
summary(is.na(covid$plec01))
summary(is.na(covid$edu3gr))
summary(is.na(covid$stan_cywilny))
summary(is.na(covid$syt_zaw))
summary(is.na(covid$BMI))
summary(is.na(covid$czy_pali))
summary(is.na(covid$Stan_zdrowia))
summary(is.na(covid$Zmiana_masy012))
summary(is.na(covid$afciagla))
summary(is.na(covid$dieta3))
summary(is.na(covid$covtest))
summary(is.na(covid$covd_chory))
summary(is.na(covid$Kwarantanna01))
summary(is.na(covid$cov_szczepienie))
summary(is.na(covid$cov_szczep2gr))


table(covid$Kwarantanna01)
table(covid$cov_szczepienie)

hist(covid$afciagla)


covid$nohosp_vshealthy[covid$covtest_rekod == "positive" & covid$hosp01test_plus == "no"] <- 0
covid$nohosp_vshealthy[covid$covtest_rekod == "no"] <- 1

covid$nohosp_vshealthy <- factor(covid$nohosp_vshealthy,
                                 levels = c(0,1),
                                 labels = c("no hosp", "healthy"))

covid$nohosp_vshealthy <- relevel(covid$nohosp_vshealthy, ref = "healthy")



# Reshape dataset --------------------
#### all data reshape ####
table(covid$Zmiana_masy012)
covid$zmiana <- relevel(covid$Zmiana_masy012, ref = "more")

covid$edu3gr <- relevel(covid$edu3gr, ref = "Low")
covid$dieta3 <- relevel(covid$dieta3, ref = "no change")

multi_diet  <- mlogit.data(covid, 
                           choice = "zmiana",
                           shape = "wide")

#### covid test done data reshape ####
test.data <- covid %>% 
  filter(covtest=="done")

multi_diet2 <- mlogit.data(test.data, 
                           choice = "zmiana",
                           shape = "wide")


#### data reshape - men ####
covid.men <- covid %>% 
  filter(plec01=="M")


table(covid.men$Zmiana_masy012)

covid.men$edu3gr <- relevel(covid.men$edu3gr, ref = "Low")
covid.men$dieta3 <- relevel(covid.men$dieta3, ref = "no change")

multi_diet.men  <- mlogit.data(covid.men, 
                               choice = "zmiana",
                               shape = "wide")

#### covid test done - men ####
test.data2 <- covid.men %>% 
  filter(covtest=="done")

multi_diet.men2 <- mlogit.data(test.data2, 
                               choice = "zmiana",
                               shape = "wide")


#### data reshape - women ####
covid.women <- covid %>% 
  filter(plec01=="K")

table(covid.men$Zmiana_masy012)

covid.women$edu3gr <- relevel(covid.women$edu3gr, ref = "Low")
covid.women$dieta3 <- relevel(covid.women$dieta3, ref = "no change")

multi_diet.women  <- mlogit.data(covid.women, 
                                 choice = "zmiana",
                                 shape = "wide")

#### covid test done - women ####
test.data3 <- covid.women %>% 
  filter(covtest=="done")

multi_diet.women2 <- mlogit.data(test.data3, 
                                 choice = "zmiana",
                                 shape = "wide")




# Tabela 1 ----------------
#### wiek ####
describe(covid$wiek)
describeBy(covid$wiek, group = covid$plec01)

#### edu ####
table(covid$edu3gr)
prop.table(table(covid$edu3gr))

covid %>% 
  count(plec01, edu3gr) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### marital status ####
table(covid$stancyw01)
prop.table(table(covid$stancyw01))

covid %>% 
  count(plec01, stancyw01) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### employment ####
table(covid$sytzaw01)
prop.table(table(covid$sytzaw01))

covid %>% 
  count(plec01, sytzaw01) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### BMI ####
describe(covid$BMI)
describeBy(covid$BMI, group = covid$plec01)

#### BMI cat ####
table(covid$BMI3gr)
prop.table(table(covid$BMI3gr))

covid %>% 
  count(plec01, BMI3gr) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### body mass difference ####
table(covid$Zmiana_masy012)
prop.table(table(covid$Zmiana_masy012))

covid %>% 
  count(plec01, Zmiana_masy012) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### tabacco ####
table(covid$pali01)
prop.table(table(covid$pali01))

covid %>% 
  count(plec01, pali01) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### self-rated health ####
table(covid$samooc3gr)
prop.table(table(covid$samooc3gr))

covid %>% 
  count(plec01, samooc3gr) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### physical activity ####
hist(covid$afciagla)
kruskal.test(covid$plec01, covid$afciagla)


describe(covid.men$afciagla)

describeBy(covid$afciagla, group = covid$plec01)


summary(covid.men$afciagla)
summary(covid.women$afciagla)



#### dietary change ####
table(covid$dieta3)
prop.table(table(covid$dieta3))

covid %>% 
  count(plec01, dieta3) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


# Tabela 2 ----------------

#### kwarantanna ####
table(covid$Kwarantanna01)
prop.table(table(covid$Kwarantanna01))

covid %>% 
  count(plec01, Kwarantanna01) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### covid test ####
table(covid$covtest)
prop.table(table(covid$covtest))

covid %>% 
  count(plec01, covtest) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))
  

#### covid test dodatni ####
table(covid$covtest_rekod)
prop.table(table(covid$covtest_rekod))

covid %>% 
  count(plec01, covtest, covtest_rekod) %>% 
  group_by(plec01, covtest) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


#### covid w wywiadzie ####
table(covid$covidchor_taknie)
prop.table(table(covid$covidchor_taknie))

covid %>% 
  count(plec01, covidchor_taknie) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$covidchor_taknie, covid$plec01, correct = FALSE)

#### hospital in 289 covid+ ####
table(covid$hosp01z289covtak)
prop.table(table(covid$hosp01z289covtak))

covid %>% 
  count(plec01, hosp01z289covtak) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

  
chisq.test(covid$hosp01z289covtak, covid$plec01, correct = FALSE)

#### covid+ test or diagnosis ####  
table(covid$covlekitest)
prop.table(table(covid$covlekitest))

  
covid %>% 
  count(plec01, covlekitest) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


#### hospital in 217 covid+ ####
table(covid$hosp01z217covtak)
prop.table(table(covid$hosp01z217covtak))


covid %>% 
  count(plec01, hosp01z217covtak) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


chisq.test(covid$hosp01z217covtak, covid$plec01, correct = FALSE)


#### covid+ test only ####
table(covid$covtest_rekod)
prop.table(table(covid$covtest_rekod))


covid %>% 
  count(plec01, covtest_rekod) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### hospital in 191 covid+ ####
table(covid$hosp01test_plus)
prop.table(table(covid$hosp01test_plus))

covid %>% 
  count(plec01, hosp01test_plus) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chi1 <- chisq.test(covid$hosp01test_plus, covid$plec01, correct = FALSE)

#### covid vaccin. ####
table(covid$cov_szczep2gr)
prop.table(table(covid$cov_szczep2gr))

covid %>% 
  count(plec01, cov_szczep2gr) %>% 
  group_by(plec01) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


# Tabela 3 ---------------

# wiek
describeBy(covid$wiek, group = covid$Zmiana_masy012)
summary(aov(wiek~Zmiana_masy012, data = covid))

#gender
covid %>% 
  count(Zmiana_masy012, plec01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$Zmiana_masy012, covid$plec01, correct = FALSE)

#education 
covid %>% 
  count(Zmiana_masy012, edu3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$Zmiana_masy012, covid$edu3gr, correct = FALSE)

#zwiazek
covid %>% 
  count(Zmiana_masy012, stancyw01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$Zmiana_masy012, covid$stancyw01, correct = FALSE)

#employment
covid %>% 
  count(Zmiana_masy012, sytzaw01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#BMI
describeBy(covid$BMI, group = covid$Zmiana_masy012)
summary(aov(BMI~Zmiana_masy012, data = covid))

#BMI cat
covid %>% 
  count(Zmiana_masy012, BMI3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$Zmiana_masy012, covid$BMI3gr, correct = FALSE)

#smoking
covid %>% 
  count(Zmiana_masy012, pali01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$Zmiana_masy012, covid$pali01, correct = FALSE)

#stan zdrowia
covid %>% 
  count(Zmiana_masy012, samooc3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$Zmiana_masy012, covid$samooc3gr, correct = FALSE)

#leisure activity
describeBy(covid$afciagla, group = covid$Zmiana_masy012)
kruskal.test(covid$Zmiana_masy012, covid$afciagla)

#dietary change
covid %>% 
  count(Zmiana_masy012, dieta3) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$Zmiana_masy012, covid$dieta3, correct = FALSE)

# Tabela 3a ----------------------
#### men ####

# wiek
describeBy(covid.men$wiek, group = covid.men$Zmiana_masy012)
summary(aov(wiek~Zmiana_masy012, data = covid.men))

#education 
covid.men %>% 
  count(Zmiana_masy012, edu3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.men$Zmiana_masy012, covid.men$edu3gr, correct = FALSE)

#zwiazek
covid.men %>% 
  count(Zmiana_masy012, stancyw01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.men$Zmiana_masy012, covid.men$stancyw01, correct = FALSE)

#employment
covid.men %>% 
  count(Zmiana_masy012, sytzaw01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#BMI
describeBy(covid.men$BMI, group = covid.men$Zmiana_masy012)
summary(aov(BMI~Zmiana_masy012, data = covid.men))

#BMI cat
covid.men %>% 
  count(Zmiana_masy012, BMI3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.men$Zmiana_masy012, covid.men$BMI3gr, correct = FALSE)

#smoking
covid.men %>% 
  count(Zmiana_masy012, pali01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.men$Zmiana_masy012, covid.men$pali01, correct = FALSE)

#stan zdrowia
covid.men %>% 
  count(Zmiana_masy012, samooc3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.men$Zmiana_masy012, covid.men$samooc3gr, correct = FALSE)

#leisure activity
describeBy(covid.men$afciagla, group = covid.men$Zmiana_masy012)
kruskal.test(covid.men$Zmiana_masy012, covid.men$afciagla)


covid.men %>% 
  group_by(Zmiana_masy012) %>% 
  summarise(quants=quantile(afciagla, probs = c(0.25, 0.5, 0.75), na.rm=TRUE))



#dietary change
covid.men %>% 
  count(Zmiana_masy012, dieta3) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.men$Zmiana_masy012, covid.men$dieta3, correct = FALSE)

#### women ####
# wiek
describeBy(covid.women$wiek, group = covid.women$Zmiana_masy012)
summary(aov(wiek~Zmiana_masy012, data = covid.women))

#education 
covid.women %>% 
  count(Zmiana_masy012, edu3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.women$Zmiana_masy012, covid.women$edu3gr, correct = FALSE)

#zwiazek
covid.women %>% 
  count(Zmiana_masy012, stancyw01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.women$Zmiana_masy012, covid.women$stancyw01, correct = FALSE)

#employment
covid.women %>% 
  count(Zmiana_masy012, sytzaw01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#BMI
describeBy(covid.women$BMI, group = covid.women$Zmiana_masy012)
summary(aov(BMI~Zmiana_masy012, data = covid.women))

#BMI cat
covid.women %>% 
  count(Zmiana_masy012, BMI3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.women$Zmiana_masy012, covid.women$BMI3gr, correct = FALSE)

#smoking
covid.women %>% 
  count(Zmiana_masy012, pali01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.women$Zmiana_masy012, covid.women$pali01, correct = FALSE)

#stan zdrowia
covid.women %>% 
  count(Zmiana_masy012, samooc3gr) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.women$Zmiana_masy012, covid.women$samooc3gr, correct = FALSE)

#leisure activity
describeBy(covid.women$afciagla, group = covid.women$Zmiana_masy012)
kruskal.test(covid.women$Zmiana_masy012, covid.women$afciagla)

covid.women %>% 
  group_by(Zmiana_masy012) %>% 
  summarise(quants=quantile(afciagla, probs = c(0.25, 0.5, 0.75), na.rm=TRUE))

ggplot(data = covid.women, aes(afciagla))+
  geom_histogram(color="black",fill="white")+
  facet_grid(.~Zmiana_masy012)+
  theme_minimal()

ggplot(data = covid.women, aes(afciagla))+
  geom_histogram(color="black",fill="white")+
  theme_minimal()


#dietary change
covid.women %>% 
  count(Zmiana_masy012, dieta3) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.women$Zmiana_masy012, covid.women$dieta3, correct = FALSE)



# Tabela 4 --------------
#kwarantanna
covid %>% 
  count(Zmiana_masy012, Kwarantanna01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#covid w wywiadzie
covid %>% 
  count(Zmiana_masy012, covidchor_taknie) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#covid lekarz + test
covid %>% 
  count(covlekitest, Zmiana_masy012) %>% 
  group_by(covlekitest) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


#covid+ tested
covid %>% 
  count(covtest_rekod, Zmiana_masy012) %>% 
  group_by(covtest_rekod) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


#hospital in 191
covid %>% 
  count(hosp01test_plus, Zmiana_masy012) %>% 
  group_by(hosp01test_plus) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid$Zmiana_masy012, covid$hosp01test_plus, correct = FALSE)

#covid test in 597
covid %>% 
  filter(covtest == "done") %>% 
  count(covtest_rekod, Zmiana_masy012) %>% 
  group_by(covtest_rekod) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


cov1 <- covid %>% 
  filter(covtest == "done")
chisq.test(cov1$Zmiana_masy012, cov1$covtest_rekod, correct = FALSE)
  
#covid vaccin.
covid %>% 
  count(cov_szczep2gr, Zmiana_masy012) %>% 
  group_by(cov_szczep2gr) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

# Tabela 4a -------------
#### men ####
#kwarantanna
covid.men %>% 
  count(Zmiana_masy012, Kwarantanna01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#covid w wywiadzie
covid.men %>% 
  count(Zmiana_masy012, covidchor_taknie) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#covid lekarz + test
covid.men %>% 
  count(covlekitest, Zmiana_masy012) %>% 
  group_by(covlekitest) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


#covid+ tested
covid.men %>% 
  count(covtest_rekod, Zmiana_masy012) %>% 
  group_by(covtest_rekod) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


#hospital in 191
covid.men %>% 
  count(hosp01test_plus, Zmiana_masy012) %>% 
  group_by(hosp01test_plus) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.men$Zmiana_masy012, covid.men$hosp01test_plus, correct = FALSE)
#covid test in 597
covid.men %>% 
  filter(covtest == "done") %>% 
  count(covtest_rekod, Zmiana_masy012) %>% 
  group_by(covtest_rekod) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


cov1 <- covid.men %>% 
  filter(covtest == "done")
chisq.test(cov1$Zmiana_masy012, cov1$covtest_rekod, correct = FALSE)

#covid vaccin.
covid.men %>% 
  count(cov_szczep2gr, Zmiana_masy012) %>% 
  group_by(cov_szczep2gr) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#### women ####
#kwarantanna
covid.women %>% 
  count(Zmiana_masy012, Kwarantanna01) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#covid w wywiadzie
covid.women %>% 
  count(Zmiana_masy012, covidchor_taknie) %>% 
  group_by(Zmiana_masy012) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

#covid lekarz + test
covid.women %>% 
  count(covlekitest, Zmiana_masy012) %>% 
  group_by(covlekitest) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


#covid+ tested
covid.women %>% 
  count(covtest_rekod, Zmiana_masy012) %>% 
  group_by(covtest_rekod) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


#hospital in 191
covid.women %>% 
  count(hosp01test_plus, Zmiana_masy012) %>% 
  group_by(hosp01test_plus) %>% 
  na.omit() %>% 
  mutate(prop.table(n))

chisq.test(covid.women$Zmiana_masy012, covid.women$hosp01test_plus, correct = FALSE)$expected

#covid test in 597
covid.women %>% 
  filter(covtest == "done") %>% 
  count(covtest_rekod, Zmiana_masy012) %>% 
  group_by(covtest_rekod) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


cov1 <- covid.women %>% 
  filter(covtest == "done")
chisq.test(cov1$Zmiana_masy012, cov1$covtest_rekod, correct = FALSE)

#covid vaccin.
covid.women %>% 
  count(cov_szczep2gr, Zmiana_masy012) %>% 
  group_by(cov_szczep2gr) %>% 
  na.omit() %>% 
  mutate(prop.table(n))


# Tabela 5  ---------------

#### multinomial logit single-predictor ####
#wiek
wiek1 <- mlogit(data = multi_diet,
       zmiana ~ 1|wiek,
       reflevel = "no change")
summary(wiek1)

exp(cbind("Odds ratio" = coef(wiek1), 
          confint.default(wiek1, 
                          level = 0.95)))


#plec
plec1 <- mlogit(data = multi_diet,
                zmiana ~ 1|plec01,
                reflevel = "no change")
summary(plec1)

exp(cbind("Odds ratio" = coef(plec1), 
          confint.default(plec1, 
                          level = 0.95)))

#education
edu1 <- mlogit(data = multi_diet,
               zmiana ~ 1|edu3gr,
               reflevel = "no change")
summary(edu1)

exp(cbind("Odds ratio" = coef(edu1), 
          confint.default(edu1, 
                          level = 0.95)))

#marital status
mar1 <- mlogit(data = multi_diet,
               zmiana ~ 1|stancyw01,
               reflevel = "no change")
summary(mar1)

exp(cbind("Odds ratio" = coef(mar1), 
          confint.default(mar1, 
                          level = 0.95)))
#employment
employ1 <- mlogit(data = multi_diet,
               zmiana ~ 1|sytzaw01,
               reflevel = "no change")
summary(employ1)

exp(cbind("Odds ratio" = coef(employ1), 
          confint.default(employ1, 
                          level = 0.95)))

#health
zdrow1 <- mlogit(data = multi_diet,
                  zmiana ~ 1|samooc3gr,
                  reflevel = "no change")
summary(zdrow1)

exp(cbind("Odds ratio" = coef(zdrow1), 
          confint.default(zdrow1, 
                          level = 0.95)))

#BMI
bmi1 <- mlogit(data = multi_diet,
                 zmiana ~ 1|BMI,
                 reflevel = "no change")
summary(bmi1)

exp(cbind("Odds ratio" = coef(bmi1), 
          confint.default(bmi1, 
                          level = 0.95)))

#BMI cat
bmi2 <- mlogit(data = multi_diet,
               zmiana ~ 1|BMI3gr,
               reflevel = "no change")
summary(bmi2)

exp(cbind("Odds ratio" = coef(bmi2), 
          confint.default(bmi2, 
                          level = 0.95)))

#smoke
smoke1 <- mlogit(data = multi_diet,
               zmiana ~ 1|pali01,
               reflevel = "no change")
summary(smoke1)

exp(cbind("Odds ratio" = coef(smoke1), 
          confint.default(smoke1, 
                          level = 0.95)))

#leisure activity
af1 <- mlogit(data = multi_diet,
                 zmiana ~ 1|afciagla,
                 reflevel = "no change")
summary(af1)

exp(cbind("Odds ratio" = coef(af1), 
          confint.default(af1, 
                          level = 0.95)))


#dietary changes
diet_ch1 <- mlogit(data = multi_diet,
                   zmiana ~ 1|dieta3,
                   reflevel = "no change")
summary(diet_ch1)

exp(cbind("Odds ratio" = coef(diet_ch1), 
          confint.default(diet_ch1, 
                          level = 0.95)))


#quarantine
quarant1 <- mlogit(data = multi_diet,
                   zmiana ~ 1|Kwarantanna01,
                   reflevel = "no change")
summary(quarant1)

exp(cbind("Odds ratio" = coef(quarant1), 
          confint.default(quarant1, 
                          level = 0.95)))


#covid w wywiadzie
cov.int1 <- mlogit(data = multi_diet,
                   zmiana ~ 1|covidchor_taknie,
                   reflevel = "no change")
summary(cov.int1)

exp(cbind("Odds ratio" = coef(cov.int1), 
          confint.default(cov.int1, 
                          level = 0.95)))

#covid lekarz + test
cov.lek1 <- mlogit(data = multi_diet,
                   zmiana ~ 1|covlekitest,
                   reflevel = "no change")
summary(cov.lek1)

exp(cbind("Odds ratio" = coef(cov.lek1), 
          confint.default(cov.lek1, 
                          level = 0.95)))

#covid+ tested
cov.test1 <- mlogit(data = multi_diet,
                   zmiana ~ 1|covtest_rekod,
                   reflevel = "no change")
summary(cov.test1)

exp(cbind("Odds ratio" = coef(cov.test1), 
          confint.default(cov.test1, 
                          level = 0.95)))

#hospital in 191 tested
hosp1 <- mlogit(data = multi_diet,
                    zmiana ~ 1|hosp01test_plus,
                    reflevel = "no change")
summary(hosp1)

exp(cbind("Odds ratio" = coef(hosp1), 
          confint.default(hosp1, 
                          level = 0.95)))


#hospital vs. healthy
hospvshealthy <- mlogit(data = multi_diet,
                zmiana ~ 1|nohosp_vshealthy,
                reflevel = "no change")
summary(hospvshealthy)

exp(cbind("Odds ratio" = coef(hospvshealthy), 
          confint.default(hospvshealthy, 
                          level = 0.95)))


#test + vs. -

cov.test2 <- mlogit(data = multi_diet2,
                zmiana ~ 1|covtest_rekod,
                reflevel = "no change")
summary(cov.test2)

exp(cbind("Odds ratio" = coef(cov.test2), 
          confint.default(cov.test2, 
                          level = 0.95)))



# Tabela 6 -------------



#### multinomial logit single-predictor ####
#wiek
wiek2 <- mlogit(data = multi_diet.men,
                zmiana ~ 1|wiek,
                reflevel = "no change")
summary(wiek2)

exp(cbind("Odds ratio" = coef(wiek2), 
          confint.default(wiek2, 
                          level = 0.95)))

#education
edu2 <- mlogit(data = multi_diet.men,
               zmiana ~ 1|edu3gr,
               reflevel = "no change")
summary(edu2)

exp(cbind("Odds ratio" = coef(edu2), 
          confint.default(edu2, 
                          level = 0.95)))

#marital status
mar2 <- mlogit(data = multi_diet.men,
               zmiana ~ 1|stancyw01,
               reflevel = "no change")
summary(mar2)

exp(cbind("Odds ratio" = coef(mar2), 
          confint.default(mar2, 
                          level = 0.95)))
#employment
employ2 <- mlogit(data = multi_diet.men,
                  zmiana ~ 1|sytzaw01,
                  reflevel = "no change")
summary(employ2)

exp(cbind("Odds ratio" = coef(employ2), 
          confint.default(employ2, 
                          level = 0.95)))

#health
zdrow2 <- mlogit(data = multi_diet.men,
                 zmiana ~ 1|samooc3gr,
                 reflevel = "no change")
summary(zdrow2)

exp(cbind("Odds ratio" = coef(zdrow2), 
          confint.default(zdrow2, 
                          level = 0.95)))

#BMI
bmi3 <- mlogit(data = multi_diet.men,
               zmiana ~ 1|BMI,
               reflevel = "no change")
summary(bmi3)

exp(cbind("Odds ratio" = coef(bmi3), 
          confint.default(bmi3, 
                          level = 0.95)))

#BMI cat
bmi4 <- mlogit(data = multi_diet.men,
               zmiana ~ 1|BMI3gr,
               reflevel = "no change")
summary(bmi4)

exp(cbind("Odds ratio" = coef(bmi4), 
          confint.default(bmi4, 
                          level = 0.95)))

#smoke
smoke2 <- mlogit(data = multi_diet.men,
                 zmiana ~ 1|pali01,
                 reflevel = "no change")
summary(smoke2)

exp(cbind("Odds ratio" = coef(smoke2), 
          confint.default(smoke2, 
                          level = 0.95)))

#leisure activity
af2 <- mlogit(data = multi_diet.men,
              zmiana ~ 1|afciagla,
              reflevel = "no change")
summary(af2)

exp(cbind("Odds ratio" = coef(af2), 
          confint.default(af2, 
                          level = 0.95)))


#dietary changes
diet_ch2 <- mlogit(data = multi_diet.men,
                   zmiana ~ 1|dieta3,
                   reflevel = "no change")
summary(diet_ch2)

exp(cbind("Odds ratio" = coef(diet_ch2), 
          confint.default(diet_ch2, 
                          level = 0.95)))


#quarantine
quarant2 <- mlogit(data = multi_diet.men,
                   zmiana ~ 1|Kwarantanna01,
                   reflevel = "no change")
summary(quarant2)

exp(cbind("Odds ratio" = coef(quarant2), 
          confint.default(quarant2, 
                          level = 0.95)))


#covid w wywiadzie
cov.int2 <- mlogit(data = multi_diet.men,
                   zmiana ~ 1|covidchor_taknie,
                   reflevel = "no change")
summary(cov.int2)

exp(cbind("Odds ratio" = coef(cov.int2), 
          confint.default(cov.int2, 
                          level = 0.95)))

#covid lekarz + test
cov.lek2 <- mlogit(data = multi_diet.men,
                   zmiana ~ 1|covlekitest,
                   reflevel = "no change")
summary(cov.lek2)

exp(cbind("Odds ratio" = coef(cov.lek2), 
          confint.default(cov.lek2, 
                          level = 0.95)))

#covid+ tested
cov.test2 <- mlogit(data = multi_diet.men,
                    zmiana ~ 1|covtest_rekod,
                    reflevel = "no change")
summary(cov.test2)

exp(cbind("Odds ratio" = coef(cov.test2), 
          confint.default(cov.test2, 
                          level = 0.95)))

#hospital in 191 tested
hosp2 <- mlogit(data = multi_diet.men,
                zmiana ~ 1|hosp01test_plus,
                reflevel = "no change")
summary(hosp2)

exp(cbind("Odds ratio" = coef(hosp2), 
          confint.default(hosp2, 
                          level = 0.95)))

#test + vs. -



cov.test2 <- mlogit(data = multi_diet.men2,
                    zmiana ~ 1|covtest_rekod,
                    reflevel = "no change")
summary(cov.test2)

exp(cbind("Odds ratio" = coef(cov.test2), 
          confint.default(cov.test2, 
                          level = 0.95)))




#### multinomial logit single-predictor ####
#wiek
wiek2 <- mlogit(data = multi_diet.women,
                zmiana ~ 1|wiek,
                reflevel = "no change")
summary(wiek2)

exp(cbind("Odds ratio" = coef(wiek2), 
          confint.default(wiek2, 
                          level = 0.95)))

#education
edu2 <- mlogit(data = multi_diet.women,
               zmiana ~ 1|edu3gr,
               reflevel = "no change")
summary(edu2)

exp(cbind("Odds ratio" = coef(edu2), 
          confint.default(edu2, 
                          level = 0.95)))

#marital status
mar2 <- mlogit(data = multi_diet.women,
               zmiana ~ 1|stancyw01,
               reflevel = "no change")
summary(mar2)

exp(cbind("Odds ratio" = coef(mar2), 
          confint.default(mar2, 
                          level = 0.95)))
#employment
employ2 <- mlogit(data = multi_diet.women,
                  zmiana ~ 1|sytzaw01,
                  reflevel = "no change")
summary(employ2)

exp(cbind("Odds ratio" = coef(employ2), 
          confint.default(employ2, 
                          level = 0.95)))

#health
zdrow2 <- mlogit(data = multi_diet.women,
                 zmiana ~ 1|samooc3gr,
                 reflevel = "no change")
summary(zdrow2)

exp(cbind("Odds ratio" = coef(zdrow2), 
          confint.default(zdrow2, 
                          level = 0.95)))

#BMI
bmi3 <- mlogit(data = multi_diet.women,
               zmiana ~ 1|BMI,
               reflevel = "no change")
summary(bmi3)

exp(cbind("Odds ratio" = coef(bmi3), 
          confint.default(bmi3, 
                          level = 0.95)))

#BMI cat
bmi4 <- mlogit(data = multi_diet.women,
               zmiana ~ 1|BMI3gr,
               reflevel = "no change")
summary(bmi4)

exp(cbind("Odds ratio" = coef(bmi4), 
          confint.default(bmi4, 
                          level = 0.95)))

#smoke
smoke2 <- mlogit(data = multi_diet.women,
                 zmiana ~ 1|pali01,
                 reflevel = "no change")
summary(smoke2)

exp(cbind("Odds ratio" = coef(smoke2), 
          confint.default(smoke2, 
                          level = 0.95)))

#leisure activity
af2 <- mlogit(data = multi_diet.women,
              zmiana ~ 1|afciagla,
              reflevel = "no change")
summary(af2)

exp(cbind("Odds ratio" = coef(af2), 
          confint.default(af2, 
                          level = 0.95)))


#dietary changes
diet_ch2 <- mlogit(data = multi_diet.women,
                   zmiana ~ 1|dieta3,
                   reflevel = "no change")
summary(diet_ch2)

exp(cbind("Odds ratio" = coef(diet_ch2), 
          confint.default(diet_ch2, 
                          level = 0.95)))


#quarantine
quarant2 <- mlogit(data = multi_diet.women,
                   zmiana ~ 1|Kwarantanna01,
                   reflevel = "no change")
summary(quarant2)

exp(cbind("Odds ratio" = coef(quarant2), 
          confint.default(quarant2, 
                          level = 0.95)))


#covid w wywiadzie
cov.int2 <- mlogit(data = multi_diet.women,
                   zmiana ~ 1|covidchor_taknie,
                   reflevel = "no change")
summary(cov.int2)

exp(cbind("Odds ratio" = coef(cov.int2), 
          confint.default(cov.int2, 
                          level = 0.95)))

#covid lekarz + test
cov.lek2 <- mlogit(data = multi_diet.women,
                   zmiana ~ 1|covlekitest,
                   reflevel = "no change")
summary(cov.lek2)

exp(cbind("Odds ratio" = coef(cov.lek2), 
          confint.default(cov.lek2, 
                          level = 0.95)))

#covid+ tested
cov.test2 <- mlogit(data = multi_diet.women,
                    zmiana ~ 1|covtest_rekod,
                    reflevel = "no change")
summary(cov.test2)

exp(cbind("Odds ratio" = coef(cov.test2), 
          confint.default(cov.test2, 
                          level = 0.95)))

#hospital in 191 tested
hosp2 <- mlogit(data = multi_diet.women,
                zmiana ~ 1|hosp01test_plus,
                reflevel = "no change")
summary(hosp2)

exp(cbind("Odds ratio" = coef(hosp2), 
          confint.default(hosp2, 
                          level = 0.95)))

#test + vs. -



cov.test2 <- mlogit(data = multi_diet.women2,
                    zmiana ~ 1|covtest_rekod,
                    reflevel = "no change")
summary(cov.test2)

exp(cbind("Odds ratio" = coef(cov.test2), 
          confint.default(cov.test2, 
                          level = 0.95)))

# Tabela 7 -------------
#### overall model 1, adj: age, sex, BMI ####

#kwarantanna
quarantine1 <- mlogit(data = multi_diet,
                zmiana ~ 1|Kwarantanna01+wiek+plec01+BMI,
                reflevel = "no change")
summary(quarantine1)

exp(cbind("Odds ratio" = coef(quarantine1), 
          confint.default(quarantine1, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw <- mlogit(data = multi_diet,
                      zmiana ~ 1|covidchor_taknie+wiek+plec01+BMI,
                      reflevel = "no change")
summary(covid_wyw)

exp(cbind("Odds ratio" = coef(covid_wyw), 
          confint.default(covid_wyw, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest <- mlogit(data = multi_diet,
                    zmiana ~ 1|covlekitest+wiek+plec01+BMI,
                    reflevel = "no change")
summary(covid_lektest)

exp(cbind("Odds ratio" = coef(covid_lektest), 
          confint.default(covid_lektest, 
                          level = 0.95)))



#hospitalizacja vs. zdrowie
hosp_vs_zdrowi <- mlogit(data = multi_diet,
                         zmiana ~ 1|nohosp_vshealthy+wiek+plec01+BMI,
                         reflevel = "no change")
summary(hosp_vs_zdrowi)

exp(cbind("Odds ratio" = coef(hosp_vs_zdrowi), 
          confint.default(hosp_vs_zdrowi, 
                          level = 0.95)))



#covid+ test
covid_test <- mlogit(data = multi_diet,
                        zmiana ~ 1|covtest_rekod+wiek+plec01+BMI,
                        reflevel = "no change")
summary(covid_test)

exp(cbind("Odds ratio" = coef(covid_test), 
          confint.default(covid_test, 
                          level = 0.95)))

table(covid$covtest_rekod)

#hospotalizacja u 191
covid_hosp191 <- mlogit(data = multi_diet,
                     zmiana ~ 1|hosp01test_plus+wiek+plec01+BMI,
                     reflevel = "no change")
summary(covid_hosp191)

exp(cbind("Odds ratio" = coef(covid_hosp191), 
          confint.default(covid_hosp191, 
                          level = 0.95)))
covid %>% 
  count(hosp01test_plus, co)



#covid + vs. covid-
#data= multi_diet2
pos_vs_neg <- mlogit(data = multi_diet2,
                        zmiana ~ 1|covtest_posneg+wiek+plec01+BMI,
                        reflevel = "no change")
summary(pos_vs_neg)

exp(cbind("Odds ratio" = coef(pos_vs_neg), 
          confint.default(pos_vs_neg, 
                          level = 0.95)))


#covid test- vs. nie chorowal
neg_vs_zdrowy <- mlogit(data = multi_diet,
                     zmiana ~ 1|negative_healthy+wiek+plec01+BMI,
                     reflevel = "no change")
summary(neg_vs_zdrowy)

exp(cbind("Odds ratio" = coef(neg_vs_zdrowy), 
          confint.default(neg_vs_zdrowy, 
                          level = 0.95)))

table(covid$negative_healthy)

#### gendered model 1: age, BMI ####
##### men #####
#kwarantanna
quarantine1.men <- mlogit(data = multi_diet.men,
                      zmiana ~ 1|Kwarantanna01+wiek+BMI,
                      reflevel = "no change")
summary(quarantine1.men)

exp(cbind("Odds ratio" = coef(quarantine1.men), 
          confint.default(quarantine1.men, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw.men <- mlogit(data = multi_diet.men,
                    zmiana ~ 1|covidchor_taknie+wiek+BMI,
                    reflevel = "no change")
summary(covid_wyw.men)

exp(cbind("Odds ratio" = coef(covid_wyw.men), 
          confint.default(covid_wyw.men, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest.men <- mlogit(data = multi_diet.men,
                        zmiana ~ 1|covlekitest+wiek+BMI,
                        reflevel = "no change")
summary(covid_lektest.men)

exp(cbind("Odds ratio" = coef(covid_lektest.men), 
          confint.default(covid_lektest.men, 
                          level = 0.95)))

#covid+ test
covid_test.men <- mlogit(data = multi_diet.men,
                     zmiana ~ 1|covtest_rekod+wiek+BMI,
                     reflevel = "no change")
summary(covid_test.men)

exp(cbind("Odds ratio" = coef(covid_test.men), 
          confint.default(covid_test.men, 
                          level = 0.95)))

table(covid$covtest_rekod)

#hospotalizacja u 191
covid_hosp191.men <- mlogit(data = multi_diet.men,
                        zmiana ~ 1|hosp01test_plus+wiek+BMI,
                        reflevel = "no change")
summary(covid_hosp191.men)

exp(cbind("Odds ratio" = coef(covid_hosp191.men), 
          confint.default(covid_hosp191.men, 
                          level = 0.95)))

table(covid$covtest_rekod)


#covid + vs. covid-
pos_vs_neg.men <- mlogit(data = multi_diet.men2,
                     zmiana ~ 1|covtest_posneg+wiek+BMI,
                     reflevel = "no change")
summary(pos_vs_neg.men)

exp(cbind("Odds ratio" = coef(pos_vs_neg.men), 
          confint.default(pos_vs_neg.men, 
                          level = 0.95)))


neg_vs_zdrowy.men <- mlogit(data = multi_diet.men,
                        zmiana ~ 1|negative_healthy+wiek+plec01+BMI,
                        reflevel = "no change")
summary(neg_vs_zdrowy)

exp(cbind("Odds ratio" = coef(neg_vs_zdrowy), 
          confint.default(neg_vs_zdrowy, 
                          level = 0.95)))

table(covid$negative_healthy)


##### women #####
#kwarantanna
quarantine1.women <- mlogit(data = multi_diet.women,
                          zmiana ~ 1|Kwarantanna01+wiek+BMI,
                          reflevel = "no change")
summary(quarantine1.women)

exp(cbind("Odds ratio" = coef(quarantine1.women), 
          confint.default(quarantine1.women, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw.women <- mlogit(data = multi_diet.women,
                        zmiana ~ 1|covidchor_taknie+wiek+BMI,
                        reflevel = "no change")
summary(covid_wyw.women)

exp(cbind("Odds ratio" = coef(covid_wyw.women), 
          confint.default(covid_wyw.women, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest.woman <- mlogit(data = multi_diet.women,
                            zmiana ~ 1|covlekitest+wiek+BMI,
                            reflevel = "no change")
summary(covid_lektest.woman)

exp(cbind("Odds ratio" = coef(covid_lektest.woman), 
          confint.default(covid_lektest.woman, 
                          level = 0.95)))

#covid+ test
covid_test.women <- mlogit(data = multi_diet.women,
                         zmiana ~ 1|covtest_rekod+wiek+BMI,
                         reflevel = "no change")
summary(covid_test.women)

exp(cbind("Odds ratio" = coef(covid_test.women), 
          confint.default(covid_test.women, 
                          level = 0.95)))


#hospotalizacja u 191
covid_hosp191.women <- mlogit(data = multi_diet.women,
                            zmiana ~ 1|hosp01test_plus+wiek+BMI,
                            reflevel = "no change")
summary(covid_hosp191.women)

exp(cbind("Odds ratio" = coef(covid_hosp191.women), 
          confint.default(covid_hosp191.women, 
                          level = 0.95)))

table(covid$covtest_rekod)


#covid + vs. covid-
pos_vs_neg.women <- mlogit(data = multi_diet.women2,
                         zmiana ~ 1|covtest_posneg+wiek+BMI,
                         reflevel = "no change")
summary(pos_vs_neg.women)

exp(cbind("Odds ratio" = coef(pos_vs_neg.women), 
          confint.default(pos_vs_neg.women, 
                          level = 0.95)))


covid.women %>% 
  filter(covidchor_taknie=="yes") %>% 
  count(covtest, covtest_posneg) %>% 
  print(n=3000)

#covid- vs. nie chorowal
neg_vs_zdrowy.women <- mlogit(data = multi_diet.women,
                            zmiana ~ 1|negative_healthy+wiek+BMI,
                            reflevel = "no change")
summary(neg_vs_zdrowy.women)

exp(cbind("Odds ratio" = coef(neg_vs_zdrowy.women), 
          confint.default(neg_vs_zdrowy.women, 
                          level = 0.95)))



#### overall model 2, adj: age, sex, BMI, health status ####
#kwarantanna
quarantine2 <- mlogit(data = multi_diet,
                      zmiana ~ 1|Kwarantanna01+wiek+plec01+BMI+samooc3gr,
                      reflevel = "no change")
summary(quarantine2)

exp(cbind("Odds ratio" = coef(quarantine2), 
          confint.default(quarantine2, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw2 <- mlogit(data = multi_diet,
                    zmiana ~ 1|covidchor_taknie+wiek+plec01+BMI+samooc3gr,
                    reflevel = "no change")
summary(covid_wyw2)

exp(cbind("Odds ratio" = coef(covid_wyw2), 
          confint.default(covid_wyw2, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest2 <- mlogit(data = multi_diet,
                        zmiana ~ 1|covlekitest+wiek+plec01+BMI+samooc3gr,
                        reflevel = "no change")
summary(covid_lektest2)

exp(cbind("Odds ratio" = coef(covid_lektest2), 
          confint.default(covid_lektest2, 
                          level = 0.95)))

#covid+ test
covid_test2 <- mlogit(data = multi_diet,
                     zmiana ~ 1|covtest_rekod+wiek+plec01+BMI+samooc3gr,
                     reflevel = "no change")
summary(covid_test2)

exp(cbind("Odds ratio" = coef(covid_test2), 
          confint.default(covid_test2, 
                          level = 0.95)))

table(covid$covtest_rekod)


#hospitalizacja vs. zdrowie
hosp_vs_zdrowi2 <- mlogit(data = multi_diet,
                         zmiana ~ 1|nohosp_vshealthy+wiek+plec01+BMI+samooc3gr,
                         reflevel = "no change")
summary(hosp_vs_zdrowi2)

exp(cbind("Odds ratio" = coef(hosp_vs_zdrowi2), 
          confint.default(hosp_vs_zdrowi2, 
                          level = 0.95)))

#hospotalizacja u 191
covid_hosp191_2 <- mlogit(data = multi_diet,
                        zmiana ~ 1|hosp01test_plus+wiek+plec01+BMI+samooc3gr,
                        reflevel = "no change")
summary(covid_hosp191_2)

exp(cbind("Odds ratio" = coef(covid_hosp191_2), 
          confint.default(covid_hosp191_2, 
                          level = 0.95)))



#covid + vs. covid-
#data= multi_diet2
pos_vs_neg2 <- mlogit(data = multi_diet2,
                     zmiana ~ 1|covtest_posneg+wiek+plec01+BMI+samooc3gr,
                     reflevel = "no change")
summary(pos_vs_neg2)

exp(cbind("Odds ratio" = coef(pos_vs_neg2), 
          confint.default(pos_vs_neg2, 
                          level = 0.95)))


#covid test- vs. nie chorowal
neg_vs_zdrowy <- mlogit(data = multi_diet,
                        zmiana ~ 1|negative_healthy+wiek+plec01+BMI+samooc3gr,
                        reflevel = "no change")
summary(neg_vs_zdrowy)

exp(cbind("Odds ratio" = coef(neg_vs_zdrowy), 
          confint.default(neg_vs_zdrowy, 
                          level = 0.95)))



#### gendered model 2: age, BMI, health status ####
##### men #####
quarantine2 <- mlogit(data = multi_diet.men,
                      zmiana ~ 1|Kwarantanna01+wiek+BMI+samooc3gr,
                      reflevel = "no change")
summary(quarantine2)

exp(cbind("Odds ratio" = coef(quarantine2), 
          confint.default(quarantine2, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw2 <- mlogit(data = multi_diet.men,
                     zmiana ~ 1|covidchor_taknie+wiek+BMI+samooc3gr,
                     reflevel = "no change")
summary(covid_wyw2)

exp(cbind("Odds ratio" = coef(covid_wyw2), 
          confint.default(covid_wyw2, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest2 <- mlogit(data = multi_diet.men,
                         zmiana ~ 1|covlekitest+wiek+BMI+samooc3gr,
                         reflevel = "no change")
summary(covid_lektest2)

exp(cbind("Odds ratio" = coef(covid_lektest2), 
          confint.default(covid_lektest2, 
                          level = 0.95)))

#covid+ test
covid_test2 <- mlogit(data = multi_diet.men,
                      zmiana ~ 1|covtest_rekod+wiek+BMI+samooc3gr,
                      reflevel = "no change")
summary(covid_test2)

exp(cbind("Odds ratio" = coef(covid_test2), 
          confint.default(covid_test2, 
                          level = 0.95)))

table(covid$covtest_rekod)

#hospotalizacja u 191
covid_hosp191_2 <- mlogit(data = multi_diet.men,
                          zmiana ~ 1|hosp01test_plus+wiek+BMI+samooc3gr,
                          reflevel = "no change")
summary(covid_hosp191_2)

exp(cbind("Odds ratio" = coef(covid_hosp191_2), 
          confint.default(covid_hosp191_2, 
                          level = 0.95)))



#covid + vs. covid-
#data= multi_diet2
pos_vs_neg2 <- mlogit(data = multi_diet.men2,
                      zmiana ~ 1|covtest_posneg+wiek+BMI+samooc3gr,
                      reflevel = "no change")
summary(pos_vs_neg2)

exp(cbind("Odds ratio" = coef(pos_vs_neg2), 
          confint.default(pos_vs_neg2, 
                          level = 0.95)))



#covid test- vs. nie chorowal
neg_vs_zdrowy <- mlogit(data = multi_diet.men,
                        zmiana ~ 1|negative_healthy+wiek+plec01+BMI+samooc3gr,
                        reflevel = "no change")
summary(neg_vs_zdrowy)

exp(cbind("Odds ratio" = coef(neg_vs_zdrowy), 
          confint.default(neg_vs_zdrowy, 
                          level = 0.95)))


##### women #####
quarantine2 <- mlogit(data = multi_diet.women,
                      zmiana ~ 1|Kwarantanna01+wiek+BMI+samooc3gr,
                      reflevel = "no change")
summary(quarantine2)

exp(cbind("Odds ratio" = coef(quarantine2), 
          confint.default(quarantine2, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw2 <- mlogit(data = multi_diet.women,
                     zmiana ~ 1|covidchor_taknie+wiek+BMI+samooc3gr,
                     reflevel = "no change")
summary(covid_wyw2)

exp(cbind("Odds ratio" = coef(covid_wyw2), 
          confint.default(covid_wyw2, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest2 <- mlogit(data = multi_diet.women,
                         zmiana ~ 1|covlekitest+wiek+BMI+samooc3gr,
                         reflevel = "no change")
summary(covid_lektest2)

exp(cbind("Odds ratio" = coef(covid_lektest2), 
          confint.default(covid_lektest2, 
                          level = 0.95)))

#covid+ test
covid_test2 <- mlogit(data = multi_diet.women,
                      zmiana ~ 1|covtest_rekod+wiek+BMI+samooc3gr,
                      reflevel = "no change")
summary(covid_test2)

exp(cbind("Odds ratio" = coef(covid_test2), 
          confint.default(covid_test2, 
                          level = 0.95)))

table(covid$covtest_rekod)

#hospotalizacja u 191
covid_hosp191_2 <- mlogit(data = multi_diet.women,
                          zmiana ~ 1|hosp01test_plus+wiek+BMI+samooc3gr,
                          reflevel = "no change")
summary(covid_hosp191_2)

exp(cbind("Odds ratio" = coef(covid_hosp191_2), 
          confint.default(covid_hosp191_2, 
                          level = 0.95)))



#covid + vs. covid-
#data= multi_diet2
pos_vs_neg2 <- mlogit(data = multi_diet.women2,
                      zmiana ~ 1|covtest_posneg+wiek+BMI+samooc3gr,
                      reflevel = "no change")
summary(pos_vs_neg2)

exp(cbind("Odds ratio" = coef(pos_vs_neg2), 
          confint.default(pos_vs_neg2, 
                          level = 0.95)))





#### overall model 3, adj: age, sex, BMI, health status, education #### 
#### cohabitation, smoking, physical activity, dietary changes ####
#kwarantanna
quarantine2 <- mlogit(data = multi_diet,
                      zmiana ~ 1|Kwarantanna01+wiek+plec01+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")
summary(quarantine2)


exp(cbind("Odds ratio" = coef(quarantine2), 
          confint.default(quarantine2, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw2 <- mlogit(data = multi_diet,
                     zmiana ~ 1|covidchor_taknie+wiek+plec01+BMI+samooc3gr+
                       edu3gr+stancyw01+pali01+afciagla+dieta3,
                     reflevel = "no change")
summary(covid_wyw2)

exp(cbind("Odds ratio" = coef(covid_wyw2), 
          confint.default(covid_wyw2, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest2 <- mlogit(data = multi_diet,
                         zmiana ~ 1|covlekitest+wiek+plec01+BMI+samooc3gr+
                           edu3gr+stancyw01+pali01+afciagla+dieta3,
                         reflevel = "no change")
summary(covid_lektest2)

exp(cbind("Odds ratio" = coef(covid_lektest2), 
          confint.default(covid_lektest2, 
                          level = 0.95)))

#covid+ test
covid_test2 <- mlogit(data = multi_diet,
                      zmiana ~ 1|covtest_rekod+wiek+plec01+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")

covid_test2 <- mlogit(data = multi_diet,
                      zmiana ~ 1|covtest_rekod+wiek+plec01+BMI+samooc3gr,
                      reflevel = "no change")

covid_test2 <- mlogit(data = multi_diet,
                      zmiana ~ 1|covtest_rekod+wiek+plec01+BMI,
                      reflevel = "no change")


summary(covid_test2)

exp(cbind("Odds ratio" = coef(covid_test2), 
          confint.default(covid_test2, 
                          level = 0.95)))

table(covid$covtest_rekod)

#hospitalizacja vs. zdrowie
hosp_vs_zdrowi3 <- mlogit(data = multi_diet,
                          zmiana ~ 1|nohosp_vshealthy+wiek+plec01+BMI+samooc3gr+
                            edu3gr+stancyw01+pali01+afciagla+dieta3,
                          reflevel = "no change")
summary(hosp_vs_zdrowi3)

exp(cbind("Odds ratio" = coef(hosp_vs_zdrowi3), 
          confint.default(hosp_vs_zdrowi3, 
                          level = 0.95)))

#hospotalizacja u 191
covid_hosp191_2 <- mlogit(data = multi_diet,
                          zmiana ~ 1|hosp01test_plus+wiek+plec01+BMI+samooc3gr+
                            edu3gr+stancyw01+pali01+afciagla+dieta3,
                          reflevel = "no change")
summary(covid_hosp191_2)

exp(cbind("Odds ratio" = coef(covid_hosp191_2), 
          confint.default(covid_hosp191_2, 
                          level = 0.95)))



#covid + vs. covid-
#data= multi_diet2
pos_vs_neg2 <- mlogit(data = multi_diet2,
                      zmiana ~ 1|covtest_posneg+wiek+plec01+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")

pos_vs_neg2 <- mlogit(data = multi_diet2,
                      zmiana ~ 1|covtest_posneg+wiek+plec01+BMI+samooc3gr,
                      reflevel = "no change")

pos_vs_neg2 <- mlogit(data = multi_diet2,
                      zmiana ~ 1|covtest_posneg+wiek+plec01+BMI,
                      reflevel = "no change")

summary(pos_vs_neg2)

exp(cbind("Odds ratio" = coef(pos_vs_neg2), 
          confint.default(pos_vs_neg2, 
                          level = 0.95)))



#covid test- vs. nie chorowal
neg_vs_zdrowy <- mlogit(data = multi_diet,
                        zmiana ~ 1|ujemny01a+wiek+plec01+BMI+samooc3gr+
                          edu3gr+stancyw01+pali01+afciagla+dieta3,
                        reflevel = "no change")

neg_vs_zdrowy <- mlogit(data = multi_diet,
                        zmiana ~ 1|ujemny01a+wiek+plec01+BMI+samooc3gr,
                        reflevel = "no change")

neg_vs_zdrowy <- mlogit(data = multi_diet,
                        zmiana ~ 1|ujemny01a+wiek+plec01+BMI,
                        reflevel = "no change")

summary(neg_vs_zdrowy)

exp(cbind("Odds ratio" = coef(neg_vs_zdrowy), 
          confint.default(neg_vs_zdrowy, 
                          level = 0.95)))

#niepotw
niepotw <- mlogit(data = multi_diet,
                  zmiana ~ 1|chory_niepotw+wiek+plec01+BMI+samooc3gr+
                    edu3gr+stancyw01+pali01+afciagla+dieta3,
                  reflevel = "no change")

niepotw <- mlogit(data = multi_diet,
                  zmiana ~ 1|chory_niepotw+wiek+plec01+BMI+samooc3gr,
                  reflevel = "no change")

niepotw <- mlogit(data = multi_diet,
                  zmiana ~ 1|chory_niepotw+wiek+plec01+BMI,
                  reflevel = "no change")

summary(niepotw)

exp(cbind("Odds ratio" = coef(niepotw), 
          confint.default(niepotw, 
                          level = 0.95)))



#### gendered model 3, adj: age, BMI, health status, education #### 
#### cohabitation, smoking, physical activity, dietary changes ####

#### men #####
#kwarantanna
quarantine2 <- mlogit(data = multi_diet.men,
                      zmiana ~ 1|Kwarantanna01+wiek+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")
summary(quarantine2)


exp(cbind("Odds ratio" = coef(quarantine2), 
          confint.default(quarantine2, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw2 <- mlogit(data = multi_diet.men,
                     zmiana ~ 1|covidchor_taknie+wiek+BMI+samooc3gr+
                       edu3gr+stancyw01+pali01+afciagla+dieta3,
                     reflevel = "no change")
summary(covid_wyw2)

exp(cbind("Odds ratio" = coef(covid_wyw2), 
          confint.default(covid_wyw2, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest2 <- mlogit(data = multi_diet.men,
                         zmiana ~ 1|covlekitest+wiek+BMI+samooc3gr+
                           edu3gr+stancyw01+pali01+afciagla+dieta3,
                         reflevel = "no change")
summary(covid_lektest2)

exp(cbind("Odds ratio" = coef(covid_lektest2), 
          confint.default(covid_lektest2, 
                          level = 0.95)))

#covid+ test
covid_test2 <- mlogit(data = multi_diet.men,
                      zmiana ~ 1|covtest_rekod+wiek+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")
summary(covid_test2)

exp(cbind("Odds ratio" = coef(covid_test2), 
          confint.default(covid_test2, 
                          level = 0.95)))

table(covid$covtest_rekod)

#hospotalizacja u 191
covid_hosp191_2 <- mlogit(data = multi_diet.men,
                          zmiana ~ 1|hosp01test_plus+wiek+BMI+samooc3gr+
                            edu3gr+stancyw01+pali01+afciagla+dieta3,
                          reflevel = "no change")
summary(covid_hosp191_2)

exp(cbind("Odds ratio" = coef(covid_hosp191_2), 
          confint.default(covid_hosp191_2, 
                          level = 0.95)))



#covid + vs. covid-
#data= multi_diet2
pos_vs_neg2 <- mlogit(data = multi_diet.men2,
                      zmiana ~ 1|covtest_posneg+wiek+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")

pos_vs_neg2 <- mlogit(data = multi_diet.men2,
                      zmiana ~ 1|covtest_posneg+wiek+BMI+samooc3gr,
                      reflevel = "no change")

pos_vs_neg2 <- mlogit(data = multi_diet.men2,
                      zmiana ~ 1|covtest_posneg+wiek+BMI,
                      reflevel = "no change")



summary(pos_vs_neg2)

exp(cbind("Odds ratio" = coef(pos_vs_neg2), 
          confint.default(pos_vs_neg2, 
                          level = 0.95)))


#covid test- vs. nie chorowal
neg_vs_zdrowy <- mlogit(data = multi_diet.men,
                        zmiana ~ 1|ujemny01a+wiek+BMI+samooc3gr+
                          edu3gr+stancyw01+pali01+afciagla+dieta3,
                        reflevel = "no change")

neg_vs_zdrowy <- mlogit(data = multi_diet.men,
                        zmiana ~ 1|ujemny01a+wiek+BMI+samooc3gr,
                        reflevel = "no change")

neg_vs_zdrowy <- mlogit(data = multi_diet.men,
                        zmiana ~ 1|ujemny01a+wiek+BMI,
                        reflevel = "no change")

summary(neg_vs_zdrowy)



exp(cbind("Odds ratio" = coef(neg_vs_zdrowy), 
          confint.default(neg_vs_zdrowy, 
                          level = 0.95)))

#niepotw
niepotw <- mlogit(data = multi_diet.men,
                        zmiana ~ 1|chory_niepotw+wiek+BMI+samooc3gr+
                          edu3gr+stancyw01+pali01+afciagla+dieta3,
                        reflevel = "no change")

niepotw <- mlogit(data = multi_diet.men,
                  zmiana ~ 1|chory_niepotw+wiek+BMI+samooc3gr,
                  reflevel = "no change")

niepotw <- mlogit(data = multi_diet.men,
                  zmiana ~ 1|chory_niepotw+wiek+BMI,
                  reflevel = "no change")

summary(niepotw)

exp(cbind("Odds ratio" = coef(niepotw), 
          confint.default(niepotw, 
                          level = 0.95)))

##### women #####
#kwarantanna
quarantine2 <- mlogit(data = multi_diet.women,
                      zmiana ~ 1|Kwarantanna01+wiek+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")
summary(quarantine2)


exp(cbind("Odds ratio" = coef(quarantine2), 
          confint.default(quarantine2, 
                          level = 0.95)))

#covid w wywiadzie
covid_wyw2 <- mlogit(data = multi_diet.women,
                     zmiana ~ 1|covidchor_taknie+wiek+BMI+samooc3gr+
                       edu3gr+stancyw01+pali01+afciagla+dieta3,
                     reflevel = "no change")
summary(covid_wyw2)

exp(cbind("Odds ratio" = coef(covid_wyw2), 
          confint.default(covid_wyw2, 
                          level = 0.95)))


#covid lekarz | test
covid_lektest2 <- mlogit(data = multi_diet.women,
                         zmiana ~ 1|covlekitest+wiek+BMI+samooc3gr+
                           edu3gr+stancyw01+pali01+afciagla+dieta3,
                         reflevel = "no change")
summary(covid_lektest2)

exp(cbind("Odds ratio" = coef(covid_lektest2), 
          confint.default(covid_lektest2, 
                          level = 0.95)))

#covid+ test
covid_test2 <- mlogit(data = multi_diet.women,
                      zmiana ~ 1|covtest_rekod+wiek+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")
summary(covid_test2)

exp(cbind("Odds ratio" = coef(covid_test2), 
          confint.default(covid_test2, 
                          level = 0.95)))



#hospotalizacja u 191
covid_hosp191_2 <- mlogit(data = multi_diet.women,
                          zmiana ~ 1|hosp01test_plus+wiek+BMI+samooc3gr+
                            edu3gr+stancyw01+pali01+afciagla+dieta3,
                          reflevel = "no change")
summary(covid_hosp191_2)

exp(cbind("Odds ratio" = coef(covid_hosp191_2), 
          confint.default(covid_hosp191_2, 
                          level = 0.95)))



#covid + vs. covid-
#data= multi_diet2
pos_vs_neg2 <- mlogit(data = multi_diet.women2,
                      zmiana ~ 1|covtest_posneg+wiek+BMI+samooc3gr+
                        edu3gr+stancyw01+pali01+afciagla+dieta3,
                      reflevel = "no change")

pos_vs_neg2 <- mlogit(data = multi_diet.women2,
                      zmiana ~ 1|covtest_posneg+wiek+BMI+samooc3gr,
                      reflevel = "no change")

pos_vs_neg2 <- mlogit(data = multi_diet.women2,
                      zmiana ~ 1|covtest_posneg+wiek+BMI,
                      reflevel = "no change")

summary(pos_vs_neg2)

exp(cbind("Odds ratio" = coef(pos_vs_neg2), 
          confint.default(pos_vs_neg2, 
                          level = 0.95)))


#covid test- vs. nie chorowal
neg_vs_zdrowy <- mlogit(data = multi_diet.women,
                        zmiana ~ 1|ujemny01a+wiek+BMI+samooc3gr+
                          edu3gr+stancyw01+pali01+afciagla+dieta3,
                        reflevel = "no change")

neg_vs_zdrowy <- mlogit(data = multi_diet.women,
                        zmiana ~ 1|ujemny01a+wiek+BMI+samooc3gr,
                        reflevel = "no change")

neg_vs_zdrowy <- mlogit(data = multi_diet.women,
                        zmiana ~ 1|ujemny01a+wiek+BMI,
                        reflevel = "no change")

summary(neg_vs_zdrowy)

exp(cbind("Odds ratio" = coef(neg_vs_zdrowy), 
          confint.default(neg_vs_zdrowy, 
                          level = 0.95)))


# niepotwierdzony 
niepotw <- mlogit(data = multi_diet.women,
                        zmiana ~ 1|chory_niepotw+wiek+BMI+samooc3gr+
                          edu3gr+stancyw01+pali01+afciagla+dieta3,
                        reflevel = "no change")

niepotw <- mlogit(data = multi_diet.women,
                  zmiana ~ 1|chory_niepotw+wiek+BMI+samooc3gr,
                  reflevel = "no change")


niepotw <- mlogit(data = multi_diet.women,
                  zmiana ~ 1|chory_niepotw+wiek+BMI,
                  reflevel = "no change")

summary(niepotw)

exp(cbind("Odds ratio" = coef(niepotw), 
          confint.default(niepotw, 
                          level = 0.95)))
  
# Hospitalizacja 4.12.2022 -----------------

#### covid w wywiadzie #####
covid %>% 
  count(wywiad_hosp, zmiana) %>% 
  group_by(zmiana)

wywiad_szpital <- mlogit(data = multi_diet,
                         zmiana ~ 1|wywiad_hosp+wiek+plec01+BMI,
                         reflevel = "no change")
summary(wywiad_szpital)

exp(cbind("Odds ratio" = coef(wywiad_szpital), 
          confint.default(wywiad_szpital, 
                          level = 0.95)))


wywiad_szpital2 <- mlogit(data = multi_diet,
                         zmiana ~ 1|wywiad_hosp+wiek+plec01+BMI+samooc3gr,
                         reflevel = "no change")
summary(wywiad_szpital2)

exp(cbind("Odds ratio" = coef(wywiad_szpital2), 
          confint.default(wywiad_szpital2, 
                          level = 0.95)))


wywiad_szpital3 <- mlogit(data = multi_diet,
                          zmiana ~ 1|wywiad_hosp+wiek+plec01+BMI+samooc3gr+
                            edu3gr+stancyw01+pali01+afciagla+dieta3,
                          reflevel = "no change")
summary(wywiad_szpital3)

exp(cbind("Odds ratio" = coef(wywiad_szpital3), 
          confint.default(wywiad_szpital3, 
                          level = 0.95)))


#### covid lekarz lub test (lek_hosp) #####
covid %>% 
  count(lek_hosp, zmiana) %>% 
  group_by(zmiana)

lekarz_szpital <- mlogit(data = multi_diet,
                         zmiana ~ 1|lek_hosp+wiek+plec01+BMI,
                         reflevel = "no change")
summary(lekarz_szpital)

exp(cbind("Odds ratio" = coef(lekarz_szpital), 
          confint.default(lekarz_szpital, 
                          level = 0.95)))


lekarz_szpital2 <- mlogit(data = multi_diet,
                          zmiana ~ 1|lek_hosp+wiek+plec01+BMI+samooc3gr,
                          reflevel = "no change")
summary(lekarz_szpital2)

exp(cbind("Odds ratio" = coef(lekarz_szpital2), 
          confint.default(lekarz_szpital2, 
                          level = 0.95)))


lekarz_szpital3 <- mlogit(data = multi_diet,
                          zmiana ~ 1|lek_hosp+wiek+plec01+BMI+samooc3gr+
                            edu3gr+stancyw01+pali01+afciagla+dieta3,
                          reflevel = "no change")
summary(lekarz_szpital3)

exp(cbind("Odds ratio" = coef(lekarz_szpital3), 
          confint.default(lekarz_szpital3, 
                          level = 0.95)))


#### covid lekarz lub test (test_hosp) #####
covid %>% 
  count(test_hosp, zmiana) %>% 
  group_by(zmiana)

test_szpital <- mlogit(data = multi_diet,
                         zmiana ~ 1|test_hosp+wiek+plec01+BMI,
                         reflevel = "no change")
summary(test_szpital)

exp(cbind("Odds ratio" = coef(test_szpital), 
          confint.default(test_szpital, 
                          level = 0.95)))


test_szpital2 <- mlogit(data = multi_diet,
                          zmiana ~ 1|test_hosp+wiek+plec01+BMI+samooc3gr,
                          reflevel = "no change")
summary(test_szpital2)

exp(cbind("Odds ratio" = coef(test_szpital2), 
          confint.default(test_szpital2, 
                          level = 0.95)))


test_szpital3 <- mlogit(data = multi_diet,
                          zmiana ~ 1|test_hosp+wiek+plec01+BMI+samooc3gr+
                            edu3gr+stancyw01+pali01+afciagla+dieta3,
                          reflevel = "no change")
summary(test_szpital3)

exp(cbind("Odds ratio" = coef(test_szpital3), 
          confint.default(test_szpital3, 
                          level = 0.95)))
















