library(tidyverse)
library(DataCombine)
library(fastDummies)

weeddata <- read_csv('weeddata.csv') %>%
  rename('USPS' = 'USPS Code')

accident2006 <- read_csv('ACCIDENT2006.csv') %>%
  mutate(index = 1) %>%
  group_by(STATE) %>%
  summarise(crashes = sum(index) , fatals = sum(FATALS) , drunks = sum(DRUNK_DR)) %>%
  mutate(year = 2006) %>%
  inner_join(weeddata , by = c('STATE' , 'year'))

cleanup <-function(x , y) {
  accident_y <- read_csv(x) %>%
    mutate(index = 1) %>%
    group_by(STATE) %>%
    summarise(crashes = sum(index) , fatals = sum(FATALS) , drunks = sum(DRUNK_DR)) %>%
    mutate(year = y) %>%
    return(accident_y)
}


frames2 <- tribble(
  ~name , ~year , ~df ,
  'ACCIDENT2006.CSV' , 2006 , 'accident2006' , 
  'ACCIDENT2007.CSV' , 2007 , 'accident2007' , 
  'ACCIDENT2008.CSV' , 2008 , 'accident2008' , 
  'ACCIDENT2009.CSV' , 2009 , 'accident2009' , 
  'ACCIDENT2010.CSV' , 2010 , 'accident2010' , 
  'ACCIDENT2011.CSV' , 2011 , 'accident2011' , 
  'ACCIDENT2012.CSV' , 2012 , 'accident2012' , 
  'ACCIDENT2013.CSV' , 2013 , 'accident2013' , 
  'ACCIDENT2014.CSV' , 2014 , 'accident2014' , 
  'ACCIDENT2015.CSV' , 2015 , 'accident2015' , 
  'accident2016.CSV' , 2016 , 'accident2016' , 
  'accident2017.CSV' , 2017 , 'accident2017' , 
  'accident2018.CSV' , 2018 , 'accident2018' , 
  'accident2019.CSV' , 2019 , 'accident2019'  
)


accident2007 <- cleanup('ACCIDENT2007.CSV' , 2007)
datalist <- map2(frames2$name , frames2$year , cleanup)


names(datalist) <- frames2$df
list2env(datalist , envir = .GlobalEnv)

accidents <- rbind(accident2006 , accident2007 , accident2008 , accident2009 , accident2010 ,accident2011 , accident2012 , 
                   accident2013 , accident2014, accident2015 , accident2016 , accident2017 , accident2018 , accident2019)

weed_accidents <- inner_join(accidents , weeddata , by = c('STATE' , 'year')) %>%
  mutate(years2legal = legal_year - year)

view <- weed_accidents %>%
  filter(LEGAL == 1) 

pop_data <- read_csv('pop_data.csv') 


weed_accidents <- inner_join(weed_accidents , pop_data , by = c('STATE' , 'year')) %>%
  mutate(fatals_pc = (fatals/pop.x)*100,000 , crashes_pc = crashes/pop , drunks_pc = drunks/pop) %>%
  dummy_cols(select_columns = c('STATE' , 'year')) %>%
  mutate(treated_legal = ifelse(is.na(years2legal) == F , 1 , 0))  

weed_accidents <- weed_accidents %>%  
  mutate(date = ifelse(year == 2017, 1 , 0)) %>%
  mutate(treated_dcrim = ifelse(STATE %in% c(6,8,9,10,15,17,24,27,28,29,31,33,35,36,37,38,39,44,50) , 1 , 0)) %>%
  mutate(washcodummy = ifelse(STATE %in% c(8 , 53) , 1 , 0)) %>%
  mutate(dummy2016 = ifelse(STATE %in% c(8 , 53 , 6 , 23 , 25 , 32) , 1 , 0)) %>%
  mutate(delaytreat = ifelse(years2legal > -2 | is.na(years2legal) , 0 , 1)) %>%
  mutate(did_legal = date*treated_legal) %>%
  mutate(did_dcrim = date*treated_dcrim) %>%
  mutate(did_washco = date*washcodummy) %>%
  mutate(did_2016 = date*dummy2016) %>%
  mutate(yearsafter = case_when(year-legal_year > 0 ~ year-legal_year , 
                                year-legal_year <= 0 ~ 0 ,
                                is.na(legal_year) ~ 0))
  mutate(did_delay = date*delaytreat)
  

weed_change <- weed_accidents %>%
  group_by(LEGAL) %>%
  summarise(fatals = sum(fatals), crashes = sum(crashes) , drunks=sum(drunks) , pop=sum(pop)) %>%
  mutate(fatals_pc = fatals/pop , crashes_pc = crashes/pop , drunks_pc = drunks/pop) %>%
  mutate(fatals_pct_chg = (fatals_pc/lead(fatals_pc , order_by = year) - 1) * 100 , crashes_pct_chg = (crashes_pc/lag(crashes_pc) - 1) * 100 , drunks_pct_chg = (drunks_pc/lag(drunks_pc) - 1) * 100 )

weed_change <- weed_accidents %>%
  group_by(treated_legal , year) %>%
  summarise(fatals = sum(fatals), crashes = sum(crashes) , drunks=sum(drunks) , pop=sum(pop)) %>%
  mutate(fatals_pc = fatals/pop , crashes_pc = crashes/pop , drunks_pc = drunks/pop) %>%
  change('fatals_pc' , GroupVar = 'treated_legal' ,  TimeVar = 'year' , NewVar = 'fatals_pct_chg', type = 'percent') %>%
  change('crashes_pc' , GroupVar = 'treated_legal' ,  TimeVar = 'year' , NewVar = 'crashes_pct_chg', type = 'percent') %>%
  mutate(legal_ch = ifelse(treated_legal == 1 , 'y' , 'n'))


reg <- lm(fatals_pc ~ yearsafter +  STATE_1    +   STATE_2       + STATE_4    +   STATE_5    +  
               STATE_6  +  STATE_8 +  STATE_9 + STATE_10 +STATE_11+   +   STATE_12 +     STATE_13   +   STATE_15   +   STATE_16   +   STATE_17   +   STATE_18 +
             STATE_19  +    STATE_20  +  STATE_21  +    STATE_22  +    STATE_23  +    STATE_24  +    STATE_25  +  STATE_26  +
              STATE_27  +    STATE_28  +    STATE_29  +    STATE_30  +    STATE_31  +    STATE_32  +    STATE_33  +    STATE_34  +
              STATE_35  +    STATE_36  +    STATE_37  +    STATE_38  +    STATE_39  +  STATE_40  +    STATE_41    +  STATE_42  +
              STATE_44    +  STATE_45  +    STATE_46  +    STATE_47+    STATE_48  +    STATE_49  +    STATE_50    +  STATE_51  +
              STATE_53+    STATE_54  +    STATE_55+    STATE_56  +    year_2010   +  year_2011   +  year_2012 +    year_2013 +   
               year_2014 + year_2015 +year_2016 + year_2017 + year_2018 + year_2019 , data = weed_accidents)
summary(reg)

ggplot(weed_change) +
  geom_line(aes(x = year , y = crashes_pct_chg , color = legal_ch)) 


didreg <- lm(drunks_pc ~ treated_legal + date + did_legal, data = washcoweed)
summary(didreg)

colNames <- colnames(weed_accidents)

chart <- weed_accidents %>%
  group_by(year) %>% 
  filter(treated_legal == 1) %>%
  summarize(fatals_pc = sum(fatals_pc))


ggplot(chart) +
  geom_line(aes(x = year , y = fatals_pc))


estimates <- pop_data %>%
  filter(year == 2019) %>%
  mutate(newdeaths = pop/100000*0.25496) %>%
  filter(STATE %in% c(8 , 53 , 6 , 23 , 25 , 32, 26 , 50 ,17))

totpop <- tribble (
  ~year , ~pop , 
  2006 , 295593000 , 
  2007 , 301580000 , 
  2008 , 304375000 , 
  2009 , 307007000 ,
  2010 , 309330000 ,
  2011 , 311583000 , 
  2012 , 313129000 , 
  2013 , 316113000 , 
  2014 , 319113000 ,
  2015 , 321442000 , 
  2016 , 323100000 , 
  2017 , 352719000 , 
  2018 , 326687000 , 
  2019 , 331449281
)


data <- accidents %>%
  group_by(year) %>%
  summarize(fatals = sum(fatals)) %>%
  inner_join(totpop , by = 'year') %>%
  mutate(fatals_pc = (fatals/pop)*100000)

ggplot() +
  geom_smooth(data = weed_accidents , mapping = aes(x = yearsafter , y = fatals_pc) , formula = fatals_pc ~ yearsafter + STATE_1    +   STATE_2       + STATE_4    +   STATE_5    +  
                STATE_6  +  STATE_8 +  STATE_9 + STATE_10 +STATE_11+   +   STATE_12 +     STATE_13   +   STATE_15   +   STATE_16   +   STATE_17   +   STATE_18 +
                STATE_19  +    STATE_20  +  STATE_21  +    STATE_22  +    STATE_23  +    STATE_24  +    STATE_25  +  STATE_26  +
                STATE_27  +    STATE_28  +    STATE_29  +    STATE_30  +    STATE_31  +    STATE_32  +    STATE_33  +    STATE_34  +
                STATE_35  +    STATE_36  +    STATE_37  +    STATE_38  +    STATE_39  +  STATE_40  +    STATE_41    +  STATE_42  +
                STATE_44    +  STATE_45  +    STATE_46  +    STATE_47+    STATE_48  +    STATE_49  +    STATE_50    +  STATE_51  +
                STATE_53+    STATE_54  +    STATE_55+    STATE_56  +    year_2010   +  year_2011   +  year_2012 +    year_2013 +   
                year_2014 + year_2015 +year_2016 + year_2017 + year_2018 + year_2019)