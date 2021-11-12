setwd("E:/Data analysis 360/Assignments/R/Projects and assignment/Case study 3 Visualisation/R - Visualization case study/R case study 3 (Visualization)")
SalesData<- read.csv("E:/Data analysis 360/Assignments/R/Projects and assignment/Case study 3 Visualisation/R - Visualization case study/R case study 3 (Visualization)/SalesData.csv")

###############################################################################################
#Import libraries
require(dplyr)
require(ggplot2)
require(plotly)

options(scipen = 999) 

#1. Compare sales by region for 2016 and 2015 using bar chart


#Data preperation
#Combining the columns of 2016 and 2015 data together and making a new column mentioning year

Data_2016<- SalesData[,c("Region", "Sales2016")]
Data_2016$Year<- 2016
Data_2016<- rename(Data_2016, Sales= Sales2016)

Data_2015<- SalesData[,c("Region", "Sales2015")]
Data_2015$Year<- 2015
Data_2015<- rename(Data_2015, Sales= Sales2015)


Region_Sales_Data<- rbind(Data_2016,Data_2015)
Region_Sales_Data$Year<- as.character(Region_Sales_Data$Year)
Region_Sales_Data<- Region_Sales_Data %>% group_by(Region, Year) %>% summarise(TotalSales=sum(Sales))

A1.Region_Sales_bar<- ggplot2::ggplot(data=Region_Sales_Data) + 
  aes(x= Region, y= TotalSales, fill=Year) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= TotalSales))

A1.Region_Sales_bar



###################################################################################################

#2. Pie chart for sales of ewach region in 2016

Sales_region_2016<- SalesData[,c("Region","Sales2016")]

A2.Sales_region_2016_pie<- plot_ly(Sales_region_2016, labels= ~Region, values= ~Sales2016, type="pie", title= "Sales by region in 2016")
A2.Sales_region_2016_pie


####################################################################################################

#3. Compare sales of 2015 and 2016 with region and tiers

#This will need grouping the data so splitting and merging the 2016 2015 data

sales_2016_region_tier<- SalesData[,c("Region", "Sales2016", "Tier")]  
sales_2016_region_tier<- sales_2016_region_tier %>% group_by(Region, Tier) %>% summarise(Sales2016= sum(Sales2016))
sales_2016_region_tier<- rename(sales_2016_region_tier, Sales= Sales2016)
sales_2016_region_tier$Year<- as.character(2016)


sales_2015_region_tier<- SalesData[,c("Region", "Sales2015", "Tier")]                                                                   
sales_2015_region_tier<- sales_2015_region_tier %>% group_by(Region, Tier) %>% summarise(Sales2015= sum(Sales2015))
sales_2015_region_tier<- rename(sales_2015_region_tier, Sales= Sales2015)
sales_2015_region_tier$Year<- as.character(2015)

final_sales_region_tier<- rbind(sales_2016_region_tier,sales_2015_region_tier)

#### Creating graph

A3.Compare_sales_tiers_bar<- ggplot2::ggplot(data=final_sales_region_tier) + 
  aes(x=Tier, y=Sales, fill= Year) + geom_bar(stat="identity", position="dodge") + facet_grid(.~Region)

A3.Compare_sales_tiers_bar

######################################################################################################

#4. In east region which state registered a decline in 2016 as compared to 2015

Data_2015<- SalesData [,c("Region","Sales2015", "State")]
Data_2015<- rename(Data_2015, Sales=Sales2015)
Data_2015<- Data_2015 %>% dplyr::filter(Region=="East")
Data_2015$Year<- as.character(2015)

Data_2016<- SalesData[,c("Region","Sales2016", "State")]
Data_2016<- rename(Data_2016, Sales=Sales2016)
Data_2016<- Data_2016 %>% dplyr::filter(Region=="East")
Data_2016$Year<- as.character(2016)

East_Sales_By_Year<- rbind(Data_2015,Data_2016)  

#### Creating graph

A4.East_Sales_by_Year<- ggplot2::ggplot(data= East_Sales_By_Year) +
  aes(x= State, y= Sales, fill= Year) + geom_bar(stat="identity", position="dodge") + labs(title= "Comparison of sales in eastern states for 2015 and 2016", caption = "OBVSERVATION= The Eastern states NY, GA and RI has shown decline in sales over the year" )
A4.East_Sales_by_Year

##########################################################################################################

#5. In all high tier, which division saw a decline in number of units sold in 2016 compared to 2015

Data_2015<- SalesData [,c("Tier","Division", "Units2015")]
Data_2015<- rename(Data_2015, Units=Units2015)
Data_2015<- Data_2015 %>% dplyr::filter(Tier=="High") %>% dplyr::group_by(Division) %>% dplyr::summarise(Units=sum(Units))
Data_2015$Year<- as.character(2015)

Data_2016<- SalesData [,c("Tier","Division", "Units2016")]
Data_2016<- rename(Data_2016, Units=Units2016)
Data_2016<- Data_2016 %>% dplyr::filter(Tier=="High") %>% dplyr::group_by(Division) %>% dplyr::summarise(Units=sum(Units))
Data_2016$Year<- as.character(2016)

High_Units_By_Year<- rbind(Data_2015,Data_2016)

## Creating Graph

A5.High_Units_By_Year<- ggplot2::ggplot(data=High_Units_By_Year) +
  aes(x= Division, y= Units, fill= Year) + geom_bar(stat="identity", position="dodge") + labs(title = "Comparison of devisions in High Tier by Year", caption = "OBSERVATION: All of the devisions in High Tier made more sales in comparison to the previous year")
A5.High_Units_By_Year

###############################################################################################################

#6. Create a new column Qtr 
SalesData_try<- SalesData
SalesData_try$Month<- match(SalesData_try$Month, month.abb)

SalesData_try$Qtr <- ifelse(SalesData_try$Month<4,
                            yes = "Q1",
                            no= ifelse(SalesData_try$Month<7,
                                   yes="Q2",
                                   no= ifelse(SalesData_try$Month<10,
                                              yes = "Q3",
                                              no= "Q4") ))
SalesData<- SalesData_try
################################################################################################################

#7.Compare Qtr wise sales in 2015 and 2016 in a bar plot

Data_2015<- SalesData[,c("Sales2015", "Qtr")]
Data_2015<- rename(Data_2015, Sales=Sales2015)
Data_2015$Year<- as.character(2015)


Data_2016<- SalesData[,c("Sales2016", "Qtr")]
Data_2016<- rename(Data_2016, Sales=Sales2016)
Data_2016$Year<- as.character(2016)

Qtr_Sales<- rbind(Data_2015, Data_2016)
Qtr_Sales<- Qtr_Sales %>% dplyr::group_by(Qtr, Year) %>% summarise(TotalSales=sum(Sales))

Qtr_Sales_bar<- ggplot2::ggplot(data= Qtr_Sales) + aes(x= Qtr, y=TotalSales, fill= Year) + geom_bar(stat="identity", position= "dodge")
Qtr_Sales_bar

##################################################################################################################

#8. Determine the composition of Qtr wise sales in 2015 with regards to all the Tiers in a pie chart

Data_2015<- SalesData[,c("Sales2015", "Qtr","Tier")]

Data_2015<- rename(Data_2015, Sales=Sales2015)

Qtr_Sales_Tier_2015<- Data_2015 %>% dplyr::group_by(Qtr, Tier) %>% summarise(TotalSales=sum(Sales))
Qtr1<- Qtr_Sales_Tier_2015 %>% dplyr::filter(Qtr=="Q1")

Qtr1_pie<- ggplot2::ggplot(data = Qtr1)+ aes(x=Qtr,y= TotalSales, fill= Tier)+ geom_bar(stat="identity") + geom_text(aes(label= Tier))

Qtr1_pie<- Qtr1_pie + coord_polar(theta = "y")
Qtr1_pie<- Qtr1_pie + theme( axis.line= element_blank(),
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title.y= element_blank(),
                             ) +
  labs(y="Q1")
Qtr1_pie

#######
Qtr2<- Qtr_Sales_Tier_2015 %>% dplyr::filter(Qtr=="Q2")

Qtr2_pie<- ggplot2::ggplot(data = Qtr2)+ aes(x=Qtr,y= TotalSales, fill= Tier)+ geom_bar(stat="identity") + geom_text(aes(label= Tier)) +labs(title = "Q2")

Qtr2_pie<- Qtr1_pie + coord_polar(theta = "y")
Qtr2_pie<- Qtr1_pie + theme( axis.line= element_blank(),
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title.y= element_blank(),
) +
  labs(y="Q2")
Qtr2_pie

#########

Qtr3<- Qtr_Sales_Tier_2015 %>% dplyr::filter(Qtr=="Q3")

Qtr3_pie<- ggplot2::ggplot(data = Qtr3)+ aes(x=Qtr,y= TotalSales, fill= Tier)+ geom_bar(stat="identity") + geom_text(aes(label= Tier))

Qtr3_pie<- Qtr1_pie + coord_polar(theta = "y")
Qtr3_pie<- Qtr1_pie + theme( axis.line= element_blank(),
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title.y= element_blank(),
) +
  labs(y="Q3")
Qtr3_pie
###########

Qtr4<- Qtr_Sales_Tier_2015 %>% dplyr::filter(Qtr=="Q4")

Qtr4_pie<- ggplot2::ggplot(data = Qtr4)+ aes(x=Qtr,y= TotalSales, fill= Tier)+ geom_bar(stat="identity") + geom_text(aes(label= Tier))

Qtr4_pie<- Qtr1_pie + coord_polar(theta = "y")
Qtr4_pie<- Qtr1_pie + theme( axis.line= element_blank(),
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title.y= element_blank(),
) +
  labs(y="Q4")

Qtr4_pie
