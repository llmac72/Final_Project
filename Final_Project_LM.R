library(readxl)
library(tidyverse)
library(dplyr)
highway <- read_excel('C:/Users/lisa.mcdaniel/DS202/Final_Project/Final_Project/2021_2025_5YrProg_Highway_Program.xlsx', 
                      sheet="Highway", skip = 2)
view(highway)
str(highway)
names(highway)

??fill
??rename
??unique

highway <- highway %>% fill(Location, .direction = "down") %>%
  rename(District = `Iowa DOT District`, Route = `Project Route`, 
         Work = `Type of Work`) %>%
  mutate(`2021` = replace(`2021`,is.na(`2021`),0), 
         `2022` = replace(`2022`,is.na(`2022`),0),
         `2023` = replace(`2023`,is.na(`2023`),0),
         `2024` = replace(`2024`,is.na(`2024`),0), 
         `2025` = replace(`2025`,is.na(`2025`),0))

view(highway)

highway1 <- highway %>%
  select(-1, -3, -4, -5, -6, -7, -8, -12, -13)

bridge1 <- highway %>%
  select(-1, -3, -4, -5, -6, -7, -8, -12, -13) %>%
  filter(grepl("BRIDGE", Work)) %>%
  count(Work)

BW <- c("BRIDGE REMOVAL", "BRIDGE WIDENING", "BRIDGE PAINTING", "BRIDGE NEW",
        "BRIDGE CLEANING", "BRIDGE REHABILITATION", "BRIDGE REPLACEMENT", 
        "BRIDGE DECK OVERLAY")

ggplot(bridge1, aes(x=Work, y=n, fill=Work)) + 
  geom_bar(stat= 'identity') +  ggtitle("Number of Bridge Projects by Work Type") + 
  theme(plot.title = element_text(hjust = 0.5)) +  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) + ylab("Number of Projects") + 
  xlab("Bridge Work Type") + scale_x_discrete(limits = BW)

bridge2 <- highway %>%
  filter(grepl("BRIDGE", Work)) %>% 
  select(Location)

unique(bridge2$Location)

??aggregate

bridge2 <- highway1 %>% 
  semi_join(bridge2) %>%
  mutate(Location = as.factor(Location)) %>%
  summarise(sum( , 7:11))
##  group_by(Location) %>%
##  mutate(Pro_Cost = `2021`+`2022`+`2023`+`2024`+`2025`)
##  aggregate(by=list(`2021`, `2022`, `2023`, `2024`, `2025`), FUN = sum)
 
YearBridge <- bridge2 %>%
  summarise(Total_2021 = sum(`2021`))

bridge1
unique(bridge1$Location)
  
