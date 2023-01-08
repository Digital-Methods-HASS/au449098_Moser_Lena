library(tidyverse)

# download tables from Old Bailey with tables x = Verdict Category, y = Decades for defendants 1-14 and 15-21
# Change column names "Not Guilty" to "Not_Guilty", "Special Verdict" to "Special_Verdict"
# name column 1 "Decade"

# manually added age groups column
old_bailey1_14 <- read_csv2("data/oldbailey1-14.csv")

names(old_bailey1_14)[1] = "Decade"
names(old_bailey1_14)[4] = "Not_Guilty"
names(old_bailey1_14)[5] = "Special_Verdict" 

old_bailey15_21 <- read_csv2("data/oldbailey15-21.csv")

names(old_bailey15_21)[1] = "Decade"
names(old_bailey15_21)[4] = "Not_Guilty"
names(old_bailey15_21)[5] = "Special_Verdict" 

old_bailey22_99<- read_csv2("data/old_bailey22_99.csv")

names(old_bailey22_99)[1] = "Decade"
names(old_bailey22_99)[4] = "Not_Guilty"
names(old_bailey22_99)[5] = "Special_Verdict" 

# merge dataframes, see 5.19 here: https://rc2e.com/datastructures
#identify name of first table - remove total
ob_table1 <- old_bailey1_14[2:13,]
#identify name of second table
ob_table2 <- old_bailey15_21[2:13,]
#merge only the first 6 columns of first table that match columns in second
ob_table4 <- rbind(ob_table1[,1:7], ob_table2)
#merge last table
ob_table3 <- old_bailey22_99[2:13,]
total <- rbind(ob_table4, ob_table3[,1:7])

# Arrange chronologically per decade
total %>% 
  arrange(Decade)

# trials per Age Group in absolute numbers
total %>% 
  ggplot(aes(x = Decade, y = Total, fill = Age_Group))+
  geom_col()+
  labs(title = "Trials per Age Group 1800-1913", 
       y = "Total number of Trials",
       fill = "Age Group")+
  scale_fill_discrete(name = "Age Group",
                      breaks = c("1_14'", "15_21'", "22_99'"),
                      labels = c("1-14 year olds", "15-21 year olds", "22-99 year olds"))

#create object with only totals of all trials
totaltrials <- old_bailey1_14[2:13,2]+old_bailey15_21[2:13,2]+old_bailey22_99[2:13,2]

# add column decade to prepare for merging
decade <- old_bailey1_14[2:13,1]
totaltrials["Decade"] <- decade

#rename column
names(totaltrials)[1] = "Total_Trials"

# merge dataframes, see 5.19 here: https://rc2e.com/datastructures
#identify name of first table - remove total
trials1 <- old_bailey1_14[2:13,]
trials1a <- left_join(trials1, totaltrials, by = "Decade")

#identify name of second table
trials2 <- old_bailey15_21 [2:13,]
trials2a <- left_join(trials2, totaltrials, by = "Decade")

#merge only the first 6 columns of first table that match columns in second
trials4 <- rbind(trials1a[,1:8], trials2a)

#merge last table
trials3 <- old_bailey22_99[2:13,]
trials3a <- left_join(trials3, totaltrials, by = "Decade")
totaltrialslong <- rbind(trials4, trials3a[,1:8])

totaltrialslong %>% 
  mutate(Prop = Total/Total_Trials*100) %>% 
  ggplot(aes(x = Decade, y = Prop, color = Age_Group, group = 1))+
  geom_point()+
  geom_line()+
  facet_wrap(~Age_Group, ncol = 2,3)+
  labs(title = "Proportion of Trials per Age Group",
       y = "Percentage",
       color = "Age Group")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_discrete(name = "Age Group",
                       breaks = c("1_14'", "15_21'", "22_99'"),
                       labels = c("1-14 year olds", "15-21 year olds", "22-99 year olds"))

# Join data frames by common column for wide format, see 5.27 here: https://rc2e.com/datastructures
# Rename categories
names(old_bailey1_14)[2] = "Total_1_14"
names(old_bailey1_14)[4] = "Not_Guilty_1_14"
names(old_bailey1_14)[5] = "Special_Verdict_1_14"

names(old_bailey15_21)[2] = "Total_15_21"
names(old_bailey15_21)[4] = "Not_Guilty_15_21"
names(old_bailey15_21)[5] = "Special_Verdict_15_21"

names(old_bailey22_99)[2] = "Total_22_99"
names(old_bailey22_99)[4] = "Not_Guilty_22_99"
names(old_bailey22_99)[5] = "Special_Verdict_22_99"

ob_young<-left_join(old_bailey1_14, old_bailey15_21, by = "Decade")
ob_age_groups<-left_join(ob_young, old_bailey22_99, by = "Decade")

# Remove row "Total" and create new table
ob_age_groups1 <- ob_age_groups[2:13,]

# Add census data to the visualisations
# Census from https://data.london.gov.uk/dataset/historic-census-population
census <- read_csv("data/census-historic-population-borough_in_number.csv")

# Isolate Greater London
London_census <- census[36,]

# Change row to column, see http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
London_wide <- London_census %>% 
  gather(key = "1")

# Rename columns
names(London_wide)[1] = "Year"
names(London_wide)[2] = "Population_Greater_London"

# Remove area code and isolate census information per year, starting with 1811, presumably showing the population of
# the 1800s. 
London_no_code <- London_wide[4:15,]

# change column value to numeric, see https://statisticsglobe.com/convert-data-frame-column-to-numeric-in-r
London_no_code$Population_Greater_London <- as.numeric(as.character(London_no_code$Population_Greater_London))

# check class
sapply(London_no_code, class)

# add column Decade to census data
London_no_code["Decade"] <- decade

# remove column year
Greater_London <- London_no_code[2:3]

# merge data frames
ob_population<-left_join(ob_age_groups1, Greater_London, by = "Decade")

# remove scientific notation
options(scipen = 999)

# visualise the population change
ob_population %>% 
  ggplot(aes(x= Decade, y = Population_Greater_London, group = 1))+
  geom_point()+
  geom_line()+
  labs(title = "Population Change in Greater London",
       y = "Population of Greater London")

# Visualise trials per population
ob_population %>% 
  mutate(total_trials = Total_1_14+Total_15_21+Total_22_99) %>% 
  mutate(trials_per_10k = total_trials/Population_Greater_London * 10000) %>% 
  ggplot(aes(x= Decade, y = trials_per_10k, group = 1))+
  geom_point()+
  geom_line()+
  labs(title = "Trials in Proportion to the Population of Greater London",
       y = "Trials per 10 000 Inhabitants")

# Visualise proportion of guilty verdicts per age group
# see: https://datavizpyr.com/rotate-x-axis-text-labels-in-ggplot2/ 
# see: https://r-graphics.org/recipe-axes-range
total %>% 
  mutate(PropGuilt = Guilty/Total*100) %>% 
  mutate(PropNot = Not_Guilty/Total*100) %>% 
  ggplot(aes(x = Decade, y = PropGuilt, color = Age_Group, group = 1))+
  geom_point()+
  geom_line()+
  facet_wrap(~Age_Group, ncol = 2,3)+
  labs(title = "Proportion of Guilty Verdict per Age Group",
       y = "Percentage",
       color = "Age Group")+
  theme(axis.text.x = element_text(angle = 90))+
  coord_cartesian(ylim = c(60,100))+
  scale_color_discrete(name = "Age Group",
                       breaks = c("1_14'", "15_21'", "22_99'"),
                       labels = c("1-14 year olds", "15-21 year olds", "22-99 year olds"))
