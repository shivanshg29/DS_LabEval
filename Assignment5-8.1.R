#Assignment 5

#q1
data<-read.csv("C:\\Users\\shiva\\Downloads\\daily_show_guests.csv")
head(data,10)

#q2
colnames(data)
data<-rename(data,year=YEAR,job=GoogleKnowlege_Occupation,date=Show,category=Group,guest_name=Raw_Guest_List)
head(data)

#q3
select(data,year,date,guest_name)

#q4
head(select(data,!year))

#q5
filter(data,job=="actor",guest_name=="ABC")

#q6
arrange(data,desc(date))

#q7
head(mutate(data,experience=2024-year))



#Assignment 6

data <- data.frame(
  Country = c("USA", "Canada", "Brazil", "UK", "Germany", "France", "India", "China", "Australia", "Japan",
              "South Africa", "Egypt", "Mexico", "Russia", "Italy", "Spain", "Argentina", "Norway", "Sweden", "Finland"),
  Continent = c("North America", "North America", "South America", "Europe", "Europe", "Europe", "Asia", "Asia", "Australia", "Asia",
                "Africa", "Africa", "North America", "Europe", "Europe", "Europe", "South America", "Europe", "Europe", "Europe"),
  Year = rep(seq(2000, 2019, 1), each = 1),
  LifeExp = c(78.9, 82.3, 72.6, 80.9, 81.3, 83.2, 68.8, 76.9, 82.5, 84.6, 64.8, 71.3, 75.4, 72.6, 82.0, 82.9, 76.8, 81.5, 82.2, 82.1),
  Pop = c(331002651, 37742154, 212559417, 67886011, 83783942, 65273511, 1380004385, 1439323776, 25499884, 126476461,
          59308690, 102334404, 128932753, 145934462, 60461826, 46754778, 45195774, 5421241, 10099265, 5540720),
  gdpPerc = c(65279, 46298, 8667, 42354, 47455, 41041, 2100, 10261, 56778, 40185, 6865, 12256, 10112, 11289, 34125, 29719, 9877, 75445, 52826, 48846)
)
head(data)
#Q1--
#q1
data %>%
  group_by(Continent) %>%
  summarise(n_distinct(Country))

#q2
data %>%
  filter(Continent=="Europe") %>%
  group_by(Year) %>%
  arrange(gdpPerc) %>%
  filter(gdpPerc==min(gdpPerc))

#q3
data %>%
  group_by(Continent) %>%
  summarise(mean(LifeExp))

#q4
data %>%
  arrange(desc(gdpPerc)) %>%
  slice_head(n=5)

#q5
data %>%
  filter(LifeExp>80) %>%
  select(Country,Year)

#q6
data %>%
  group_by(Country) %>%
  mutate(Correal=cor(LifeExp,gdpPerc)) %>%
  arrange(desc(Correal))

#q7
data %>%
  filter(Country!="Asia") %>%
  group_by(Continent,Year) %>%
  summarise(avg_pop=mean(Pop)) %>%
  arrange(desc(avg_pop))  

#q8
data %>%
  group_by(Continent) %>%
  summarise(standD=sd(Pop)) %>%
  arrange(standD) %>%
  head(3)

#q9
data %>%
  arrange(Country,Year) %>%
  mutate(pop_c=Pop-lag(Pop),life_c=LifeExp-lag(LifeExp)) %>%
  filter(pop_c<0,life_c>0)

#Q2--
#q1
med_data <- data.frame(
  MedID = 1:10,
  Med_Name = c("MedA", "MedB", "MedC", "MedD", "MedE", "MedF", "MedG", "MedH", "MedI", "MedJ"),
  Company = c("Comp1", "Comp2", "Comp3", "Comp1", "Comp2", "Comp3", "Comp1", "Comp2", "Comp3", "Comp1"),
  Manf_year = sample(2010:2020, 10, replace = TRUE),
  Exp_date = sample(2021:2030, 10, replace = TRUE),
  Quantity_in_stock = sample(100:500, 10),
  Sales = sample(1000:5000, 10)
)

#q2
head(data)

#q3
tail(data)

#q4
data$exp<-as.numeric(data$Exp_date)
cor(data$Quantity_in_stock,data$exp)

#q5
barplot(data$Sales,data$Manf_year,xlab = 'Sales',ylab = 'Manufacturing Year',main = 'Sales vs Year')

#q6
data %>%
  group_by(Company) %>%
  filter(n_distinct(Med_Name)>1)%>%
  select(Company)

#q7
unique(data$Med_Name)

#q8
ggplot(data,aes(x=as.factor(Exp_date),y=Quantity_in_stock))+
  geom_boxplot()

#q9
data %>%
  summarise(avg_stock=mean(Quantity_in_stock))

#q10
y<-lm(data$Manf_year~data$Sales)
plot(data$Manf_year,data$Sales)
abline(y)


#Assignment 8.1

#part1
project1 <- read.csv("https://raw.githubusercontent.com/biocorecrg/CRG_RIntroduction/master/ex12_normalized_intensities.csv", 
                     row.names = 1)
project1 <- as.data.frame(project1)
head(project1)

#q1
ggplot(project1, aes(x=sampleB, y=sampleH)) +
  geom_point() +
  labs(title = "Gene expression of sample B against sampple H", x="sample B", y="sample H")

#q2
project1 <- project1 %>%
  mutate(expr_limits = case_when(
    sampleB >13 & sampleH >13 ~ "high",
    sampleB <6 & sampleH <6 ~ "low",
    TRUE ~ "normal"
  ) )
#or
project1$expr_limits <- NA
for(i in 1:nrow(project1)){
  if(project1$sampleB[i] >13 & project1$sampleH[i] > 13){
    project1$expr_limits[i] <- "high"
  }else if (project1$sampleB[i] <6 & project1$sampleH[i] <6 ){
    project1$expr_limits[i] <- "low"
  }else {
    project1$expr_limits[i] <- "normal"
  }
}
head(project1)

#q3
p <- ggplot(project1, aes(x=sampleB, y=sampleH)) +
  geom_point(aes(color=expr_limits)) +
  labs(title="Gene expression of sample B against sample H", x="sample B", y="sample H")
p

project1_long <- reshape2::melt(project1, id.vars = "expr_limits", variable.name = "Sample", value.name = "Expression")
ggplot(project1_long, aes(x = Sample, y = Expression)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Expression Levels for All Samples", x = "Sample", y = "Expression")

#q5
ggplot(project1_long, aes(x = Sample, y = Expression, fill = expr_limits)) +
  geom_boxplot() +
  labs(title = "Sub-boxplots by Expression Limits", x = "Sample", y = "Expression") +
  scale_fill_manual(values = c("high" = "red", "normal" = "blue", "low" = "green"))

#q6
ggplot(project1, aes(x=expr_limits)) +
  geom_bar()

#part2
zomato <- read.csv("C:/Users/shiva/Downloads/zomato.csv")
colnames(zomato)

#q1
class(zomato$rate)
zomato$rate <- as.numeric(gsub("/5","",zomato$rate))
highest_rating <- zomato %>%
  group_by(listed_in.type.) %>%
  summarise(max_rating = max(rate, na.rm=TRUE))
highest_rating  
ggplot(highest_rating, aes(x=listed_in.type., y=max_rating)) +
  geom_bar(stat="identity") +
  labs(title = "highest rating by type of service", x="type rest", y="max rating")

#q2a
highest_votes <- zomato %>%
  group_by(listed_in.type.) %>%
  summarise(total_votes = sum(votes, na.rm=TRUE))
highest_votes
ggplot(highest_votes, aes(x=listed_in.type., y=total_votes)) +
  geom_point() +
  labs(title = "types of rest with highest votes", x="rest type", y="total votes")

#q2b
cost_by_location <- zomato %>%
  group_by(location) %>%
  arrange(desc(approx_cost.for.two.people.)) %>%
  head(3)
cost_by_location
ggplot(cost_by_location, aes(x=location, y=approx_cost.for.two.people.)) +
  geom_point() +
  labs(title="top 3 locations with highest approx cost for 2", x="location", y="cost")

#q3- doubt
class(cost_by_type$approx_cost.for.two.people.)
cost_by_type$approx_cost.for.two.people.<- as.numeric(gsub(",","",cost_by_type$approx_cost.for.two.people.))
cost_by_type <- zomato %>%
  group_by(listed_in.type.) 
ggplot(cost_by_type, aes(x=listed_in.type., y=approx_cost.for.two.people.)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y")


#q4a
votes_by_booking <- zomato %>%
  group_by(listed_in.type., book_table) %>%
  summarise(total_votes = sum(votes, na.rm=TRUE))
ggplot(votes_by_booking, aes(x=listed_in.type., y=total_votes, fill=book_table)) +
  geom_bar(stat="identity") +
  labs(title = " votes by rest type and table booking", x="rest type", y="total votes")

#q4b
votes_by_online <- zomato %>%
  group_by(listed_in.type., online_order) %>%
  summarise(total_votes = sum(votes, na.rm=TRUE))
ggplot(votes_by_online, aes(x=listed_in.type., y=total_votes, fill=online_order))+
  geom_bar(stat="identity") +
  labs(title = "votes by rest type and online order", x="rest type", y="total votes")

#q5 - doubt 

#q6
ggplot(zomato, aes(x=listed_in.city., y=rate)) +
  geom_boxplot() +
  labs(title = "rating distribution by city", x="city", y="rating")

