
#Resit Exam

library(tidyverse)
library(readxl)

#Problem C
#Q1


MD_0201 <- read_excel("messy-data.xlsx", skip = 1)

#Q2

MD_0201
MD_0201 <- separate(MD_0201,"Month, period", c("Month","Period"), sep = ",")

#Q3

MD_0201$`Lake Victoria` <- as.numeric(gsub("mm","",MD_0201$`Lake Victoria`))
MD_0201$Simiyu <- as.numeric(gsub("mm","", MD_0201$Simiyu))

glimpse(MD_0201)

#Q4

MD_0201 <- MD_0201 %>%
  mutate_if(is.numeric, ~round(., 2))

#Q5

summary(MD_0201)

boxplot(MD_0201$`Lake Victoria`)
boxplot(MD_0201$Simiyu)

 #Yes, in the variable Lake Victoria the month of May is an outlier, since the reported
#values in that period are of significant distance from the other values in the same variable,
#as we can observe in the boxplot graph. For the variable Simiyu, there are no observable outliers,
#since there are no values with a clear distance from the center of observations.


#Problem A
#Q1

FA_0201 <- read_tsv("ForeignAid.tsv ")


#Q2

unique(FA_0201$Country)

badnorth <- c('Korea, N','Korea, No','Korea::North','Korea north','Korea xNorth',
              'Korea-North','Korea ;North','Korea North','Korea, North',
              'Korea-North','Korea. No.','Korea, Nor','N Korea')

FA_0201 <- FA_0201 %>%
  mutate(Country=ifelse(Country %in% badnorth, 'North Korea', Country))

unique(FA_0201$Country)

badsouth <- c('Korea, So;', 'SouthKorea', 'Korea South', 'Korea-South', 'south Korea',
              'Korea: South', 'Korea::South', 'So Korea', 'Korea `South', 'Korea, South',
              'south korea')

FA_0201 <- FA_0201 %>%
  mutate(Country=ifelse(Country %in% badsouth, 'South Korea', Country))

unique(FA_0201$Country)


#Q3

unique(FA_0201$FundingAgency)

FA_0201 %>%
  filter(grepl("Agr", FundingAgency,ignore.case = TRUE)) %>%
  select(FundingAgency) %>%
  unique() %>%
  View()

badagri <- c('Dept of Agriculture','Department of agriculture',
             'Department - Agriculture','Department of Agricult','Department of Argic')

FA_0201 <- FA_0201 %>%
  mutate(FundingAgency=ifelse(FundingAgency %in% badagri, 'Department of Agriculture', FundingAgency))

unique(FA_0201$FundingAgency)

FA_0201 %>%
  filter(grepl("Sta", FundingAgency,ignore.case = TRUE)) %>%
  select(FundingAgency) %>%
  unique() %>%
  View()

badstate <- c('department of State','Department of State','Department Of State',
              'Department - State')

FA_0201 <- FA_0201 %>%
  mutate(FundingAgency=ifelse(FundingAgency %in% badstate, 'State Department', FundingAgency))

FA_0201 %>%
  filter(grepl("Int", FundingAgency,ignore.case = TRUE)) %>%
  select(FundingAgency) %>%
  unique() %>%
  View()

badinter <- c('U.S. Agency for International Development',
              'U.S. Agency f/ International Development',
              'U.S. Agency: International Development',
              "U.S. Agency for INt'l Development", 
              "USA Agency for International Development")


FA_0201 <- FA_0201 %>%
  mutate(FundingAgency=ifelse(FundingAgency %in% badinter, 'US Agency for International Development', FundingAgency))

              
unique(FA_0201$FundingAgency)

badhealth <- c("Department of Health and Human Svc","Department- Health and Human Services")


FA_0201 <- FA_0201 %>%
  mutate(FundingAgency=ifelse(FundingAgency %in% badhealth, 'Department of Health and Human Services', FundingAgency))

unique(FA_0201$FundingAgency)

#There are in total 12 agencies

agencyday <- 12*02

#Q4

FA_0201$FundingAmount <-lapply(FA_0201$FundingAmount,gsub,pattern="$",fixed=TRUE,replacement="")
FA_0201$FundingAmount <-lapply(FA_0201$FundingAmount,gsub,pattern=",",fixed=TRUE,replacement="")
FA_0201$FundingAmount <-lapply(FA_0201$FundingAmount,gsub,pattern="a",fixed=TRUE,replacement="")
FA_0201$FundingAmount <-lapply(FA_0201$FundingAmount,gsub,pattern=" ",fixed=TRUE,replacement="")


FA_0201 <- FA_0201 %>%
  mutate(FundingAmount=as.numeric(FundingAmount))

#Q5

agencydist <- table(FA_0201$FundingAgency)
agencyfreq <- prop.table(agencydist)
agencyfreq
