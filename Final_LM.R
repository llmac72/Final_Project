library(readxl)
library(tidyverse)
library(dplyr)
##library(`scales`)

FiveYear <- read_excel('C:/Users/lisa.mcdaniel/DS202/Final_Project/Final_Project/2021_2025_5YrProg_Highway_Program.xlsx')

str(FiveYear) 
names(FiveYear)

FiveYear <- FiveYear %>% 
  fill(Location, .direction = "down") %>% 
  rename(District = `Iowa DOT District`, Route = `Project Route`, 
         Work = `Type of Work`) %>%
  group_by(County, District, Route, Location) %>%
  fill(Miles, .direction = "down") %>%
  mutate(District = as.numeric(District))
  
   
str(FiveYear)

unique(FiveYear$Location)
##There are 632 projects in the 5-year program.

##US Map with Iowa Highlighted in Blue
##install.packages("maps")
states <- map_data('state')

iowa <- c("iowa")

states %>% ggplot(aes(x = long, y = lat)) + 
  geom_path(aes(group = group)) + 
  geom_polygon(fill="blue", data = filter(states, region %in% iowa)) 

##BRIDGES/CULVERTS

##How many bridges/culverts are included in the federal 
##work types:  construction, replacement, reconstruction, maintainance or preservation? 

Bridge <- FiveYear %>%
  select(County, District, Route, Location, Miles, Work, `2021`,
         `2022`, `2023`, `2024`, `2025`) %>%
  filter(grepl(c("BRIDGE|CULVERT"), Work)) %>%
##  filter(Work != "BRIDGE REMOVAL") %>%
  mutate(FedWork = str_replace_all(Work, c("BRIDGE PAINTING|BRIDGE CLEANING" = "Preservation",
                                           "CULVERT EXTENSION|CULVERT REPAIR" = "Maintenance",
                                           "BRIDGE DECK OVERLAY|BRIDGE REHABILITATION" = "Rehabilitation",
                                           "BRIDGE REPLACEMENT|CULVERT REPLACEMENT|BRIDGE REMOVAL" = "Replacement",
                                           "BRIDGE NEW|CULVERT NEW|PIPE CULVERTS|BRIDGE WIDENING" = "Construction")))
  

unique(Bridge$Location)
##There are 439 bridge/culvert projects.

Bri1 <- Bridge %>%
  pivot_longer('2021':'2025', names_to='Year', values_to='Cost')%>%
  filter(Cost !="NA") %>%
  group_by(FedWork) %>%
  summarise(WorkCost = (sum(Cost)/1000)) %>%
##            Miles = sum(Miles))%>%
  mutate(WorkCost = round(WorkCost, 2))

Bri1a <- Bridge %>%
  pivot_longer('2021':'2025', names_to='Year', values_to='Cost')%>%
  filter(Cost !="NA", FedWork != "NA") %>%
  group_by(FedWork) %>%
  count(unique(FedWork))%>%
  rename(Projects = n) %>%
  inner_join(Bri1)

Bridge1 <- ggplot(Bri1a, aes(x=reorder(FedWork, WorkCost), WorkCost, fill = WorkCost)) + 
  geom_bar(stat='identity', show.legend = FALSE) + 
  ggtitle("Bridge Assets:\n Federal Work Type") + 
  labs(x="") +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.ticks.y=element_blank()) +
  theme(axis.text.y = element_blank()) +
  geom_text(aes(x=FedWork,y=WorkCost,label=paste("$",WorkCost),vjust=-1.5))

Bridge1 + geom_text(aes(x=FedWork,y=WorkCost,
                        label=paste("(",Projects,")"),vjust=-0.5), size=3) +
  ylab(expression(atop("Cost: $Millions", paste=("Projects"))))
                        
                      
##What is the average cost by federal work type?  
##Is there a difference in cost between Districts? 
  
Bri2 <- Bridge %>%
  pivot_longer('2021':'2025', names_to='Year', values_to='Cost')%>%
  filter(Cost !="NA", Miles != "NA") %>%
  group_by(District, FedWork) %>%
  summarise(WorkCost = (mean(Cost))/1000) %>%
##            Miles = sum(Miles)) %>%
  mutate(WorkCost = round(WorkCost, 2))
  
ggplot(Bri2, aes(District, WorkCost, fill = District)) + 
  geom_bar(stat='identity', show.legend = FALSE) + 
  ggtitle("Average Bridge Costs") + 
  labs(y="Cost, $Millions", x="District") +
  theme(axis.text.x = element_text(size = 8, vjust = 0.2, hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.ticks.y=element_blank()) +
  theme(axis.text.y = element_blank()) + 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels =c(1, 2, 3, 4, 5, 6)) +
##  scale_fill_brewer(palette="Set1") +
  geom_text(aes(x=District,y=WorkCost,label=paste("$",WorkCost), vjust=-0.5, hjust=0.6)) + 
  facet_wrap(~FedWork) 



##Annual Cost by Federal Work Type 
##How do funding levels in the 5 Year Program compare to those in the Asset Management Plan?
#install.packages("scales")
#library(`scales`)

Bri3 <- Bridge %>%
  group_by(FedWork) %>%
  summarise(`20215YrPlan` = sum(`2021`/1000, na.rm = TRUE),
            `20225YrPlan` = sum(`2022`/1000, na.rm = TRUE), 
            `20235YrPlan` = sum(`2023`/1000, na.rm = TRUE), 
            `20245YrPlan` = sum(`2024`/1000, na.rm = TRUE),
            `20255YrPlan` = sum(`2025`/1000, na.rm = TRUE)) %>%
  add_column(`2021Asset` = c(43, 1, 5, 28, 133),
             `2022Asset` = c(72, 1, 6, 32, 152),
             `2023Asset` = c(17, 1, 5, 25, 119),
             `2024Asset` = c(8, 1, 6, 32, 153),
             `2025Asset` = c(25, 1, 6, 35, 163)) %>%
  pivot_longer(`20215YrPlan`:`2025Asset`, names_to="Year", 
               values_to = "Cost") %>%
  mutate(Plan = str_replace_all(Year, c("20215YrPlan|20225YrPlan|20235YrPlan|20245YrPlan|20255YrPlan" = "5 Year",
                                        "2021Asset|2022Asset|2023Asset|2024Asset|2025Asset" = "Asset Management"))) %>%
  mutate(Year2 = str_replace_all(Year, c("20215YrPlan|2021Asset" = "2021", "20225YrPlan|2022Asset" ="2022", 
                                        "20235YrPlan|2023Asset" = "2023", "20245YrPlan|2024Asset" = "2024", 
                                        "20255YrPlan|2025Asset" = "2025"))) %>%
  select(-Year) %>%
  rename(Year = Year2) %>%
  mutate(Year = as.numeric(Year))


##colors <- c(`20215YrPlan`="red", `20255YrPlan`="red",
##            `20225YrPlan`="red", `20235YrPlan`="red",
##            `20245YrPlan`="red", `2022Asset`="blue",
##            `2021Asset`="blue", `2025Asset` = "blue",
##            `2023Asset`="blue", `2024Asset` = "blue")
  
ggplot(Bri3, aes(Year, Cost, fill=Plan)) + 
  geom_bar(stat='identity', position = "dodge") + 
  ggtitle("Bridge Assets\n 5 Year Program vs Asset Management Plan") + 
  labs(y="Cost, $Millions", x="") +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.2, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.ticks.y=element_blank()) +
  theme(axis.text.y = element_blank()) +
  facet_wrap(~FedWork, scales = "free") +
  geom_text(aes(x=Year,y=Cost,label=paste("$",Cost)), 
            position = position_dodge(0.9), vjust=-0.5, hjust=0.6, size=2)

##  scale_fill_manual(values = colors, labels = c("5 Year Plan", 
                                                "Asset Management Plan"), guide = "legend")

??pivot_wider

Bri3a <- Bri3 %>%
##  mutate(Plan = str_replace_all(Year, c("20215YrPlan|20225YrPlan|20235YrPlan|20245YrPlan|20255YrPlan" = "5 Year",
##                                          "2021Asset|2022Asset|2023Asset|2024Asset|2025Asset" = "Asset Management"))) %>%
##  select(-Year) %>%
  group_by(FedWork, Plan) %>%
  summarise(Cost = sum(Cost)) %>%
  mutate(Cost = round(Cost, 2))

Bri3b <- Bri3a %>%
  group_by(Plan) %>%
  summarise(FiveYear = sum(Cost))


ggplot(Bri3a, aes(FedWork, Cost, fill = Plan)) + 
  geom_bar(stat='identity', position="dodge") + 
  ggtitle("Bridge Assets\n Funding Levels Over the 5 Year Period") + 
  labs(x="", y="Cost, $Millions") +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.ticks.y=element_blank()) +
  theme(axis.text.y = element_blank()) +
  geom_text(aes(x=FedWork,y=Cost,label=paste("$",Cost)), 
            position = position_dodge(0.9), vjust=-0.5, hjust=0.6)


##PAVEMENTS

##How many pavement projects are included in the federal 
##work types:  construction, replacement, reconstruction, maintenance or preservation? 

Pavement <- FiveYear %>%
  select(County, District, Route, Location, Miles, Work, `2021`,
         `2022`, `2023`, `2024`, `2025`) %>%
  filter(grepl(c("GRADE|PAVE|PAVING|PAVEMENT|GRADING|RIGHT|SLOPE|TRAFFIC|EROSION|BARRIER|MISCELLANEOUS|NOISE"), Work))

unique(Pavement$Location)
##There are 172 pavement projects.

Pave1 <- Pavement %>%
  pivot_longer('2021':'2025', names_to='Year', values_to='Cost') %>%
  filter(Cost != "NA") %>%
  group_by(Year) %>%
  summarise(Cost = sum(Cost)/1000) %>%
  mutate(Cost = round(Cost, 2)) %>%
  add_column(Plan = c( "5 Year", "5 Year", "5 Year", "5 Year", "5 Year")) %>%
  relocate(Plan) %>%
  add_row(Cost = c(511, 388, 535, 478, 459), 
          Plan = c("Asset Mgmt", "Asset Mgmt", "Asset Mgmt", "Asset Mgmt", "Asset Mgmt"), 
          Year = c("2021", "2022", "2023", "2024", "2025"))

Pave2 <- Pave1 %>%
  group_by(Plan)%>%
  summarise(Cost = sum(Cost))

ggplot(Pave1, aes(Year, Cost, fill = Plan)) + 
  geom_bar(stat='identity', position="dodge") + 
  ggtitle("Pavement Assets\n Funding Levels Over the 5 Year Period") + 
  labs(x="", y="Cost, $Millions") +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.2, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.ticks.y=element_blank()) +
  theme(axis.text.y = element_blank()) +
  geom_text(aes(x=Year,y=Cost,label=paste("$",Cost)), 
            position = position_dodge(0.9), vjust=-0.5, hjust=0.6)
