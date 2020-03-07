# install.package("usmap")
library(tidyverse)
library(usmap)
sba <- read_csv("https://uofi.box.com/shared/static/vi37omgitiaa2yyplrom779qvwk1g14x.csv", 
                col_types = cols(ApprovalDate = col_date(format = "%d-%b-%y"), 
                                 BalanceGross = col_number(), ChgOffDate = col_date(format = "%d-%b-%y"), 
                                 ChgOffPrinGr = col_number(), DisbursementDate = col_date(format = "%d-%b-%y"), 
                                 DisbursementGross = col_number(), 
                                 ApprovalFY = col_integer(),
                                 GrAppv = col_number(), SBA_Appv = col_number()))



library(ggplot2)
# Idea: Create a map comparing percentage of defaults throughout the country 
g <- mutate(sba, defaultvalue = ifelse(MIS_Status=="CHGOFF",1,0), pifvalue=ifelse(MIS_Status=="P I F", 1,0))
g <- group_by(g, State)
defaultperstate <- summarise(g, defaultrate=sum(defaultvalue, na.rm=TRUE)/(sum(defaultvalue,na.rm=TRUE)+sum(pifvalue,na.rm=TRUE)), totaldefauts = sum(defaultvalue,na.rm = TRUE), totalpif = sum(pifvalue, na.rm=TRUE))


g2 <- statepop
for (i in g2$abbr) {
  g2$defaultrate[g2$abbr==i]=defaultperstate$defaultrate[defaultperstate$State==i]
}

plot_usmap(data = g2, values = "defaultrate", color = "pink") + 
  labs(title = "Default Rates in the US", 
       subtitle = "A map showing the loan default rates across all US states.") +
  scale_fill_continuous(name = "Default Rates", low="white",high="red", label = scales::percent) + 
  theme(legend.position = c(0.93,0.3), legend.title = element_text(size=12),legend.text = element_text(size=9),plot.title = element_text(hjust = 0.5,vjust = -5 ,size = 25), plot.subtitle = element_text(hjust=0.5,vjust = -13.5)) +
  ggsave(filename = "USdefaults.jpeg", dpi = 400)
  
