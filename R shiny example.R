#General Libraries ----
library(tidyverse)
library(reshape2)
library(shiny)
library(gridExtra)

#Date&Time Processing libraries ----
library(anytime)
library(lubridate)

#Sankey Library ---- 
library(networkD3)

#Data mining libraries ---- 
library(tm)
library(SnowballC) 
library(wordcloud)
library(arules)
library(arulesViz)
library(datasets)

#Set the work directory ----
setwd("~your directory")

#----------------------------------------SIMPLE DATA ANALYTICS----------------------------------------------------#
# read in the CSV file ----
MyData <- read.csv(file="~filename- needs to be a csv", header=T, sep=",", na.strings = c("", "NA")) 

# create a new data frame with a subset of the important information ----
#because the data frame is like a list, it's hard to do numerical calculations on it
importantData <- MyData[,c(15,21,18,34,42,29,124,14,27,19,77,127)] 

# get rid of unnecessary data from memory ----
remove(MyData)

# #The format of the data that goes into this application should take the following form:
# Building name| Complaint Create Date| Complaint Resolved Date| Complaint Category| Specific Complaint type| Floor #| Suite| Brief Description| Full Description| Technician Comments| Priority| Tenant Name|
# as character | As character         | As character           | As Factor         | As Factor              | Factor |Factor| As Character     | As Character    | As Character       | Factor  | Factor     |


# We omit all the complaints which weren't closed and separate the initial analysis from the text mining/ARM ----
NotMiningData <- na.omit(importantData[,c(1:7)])

# separate out the dates for response time calcs ----
dates <- NotMiningData[,c(2,3)]

# vectorize the dates so that response times can be calculated ----
vectDateCreate <- strptime(dates[,1],"%m/%d/%Y %I:%M:%S %p")
vectDateClosed <- strptime(dates[,2],"%m/%d/%Y %I:%M:%S %p")
vectRespTime <- difftime(vectDateClosed,vectDateCreate,units ="hours")

# convert the responsetimes to a list for analysis ----
RespTime <- as.list(vectRespTime)

#append this as a new column in NotMiningData
NotMiningData$resp.time <- RespTime

#split up the data into Overall, Lighting, Temperature, Janitorial and Maintenance for ALL RESPONSE TIMES
OverallResp <- NotMiningData
LightingResp <- OverallResp[ grep("Light", OverallResp$h_Category, invert = FALSE) , ]
TempResp <- OverallResp[ grep("Temp", OverallResp$h_Category, invert = FALSE) , ]
JanitorResp <- OverallResp[ grep("Jan", OverallResp$h_Category, invert = FALSE) , ]
MaintResp <- OverallResp[ grep("Main", OverallResp$h_Category, invert = FALSE) , ]

#--------Create a histogram of the time of day that the complaints are logged ----
h1 <- hist(hour(strptime(LightingResp$Create.Date, format = "%m/%d/%Y %I:%M:%S %p")), 
     xlim = c(0,23) , xlab = "Time of Day (hrs)", main = "Lighting", col = c("yellow"))
axis(side =1, at=seq(0,23,1))
h2 <- hist(hour(strptime(TempResp$Create.Date, format = "%m/%d/%Y %I:%M:%S %p")), breaks = 24, 
           xlim = c(0,23) , xlab = "Time of Day (hrs)", main = "Temperature", col = c("red"))
axis(side =1, at=seq(0,23,1))
h3 <- hist(hour(strptime(JanitorResp$Create.Date, format = "%m/%d/%Y %I:%M:%S %p")), 
           xlim = c(0,23) , xlab = "Time of Day (hrs)", main = "Janitorial", col = c("green"))
axis(side =1, at=seq(0,23,1))
h4 <- hist(hour(strptime(MaintResp$Create.Date, format = "%m/%d/%Y %I:%M:%S %p")), breaks = 24, 
           xlim = c(0,23) , xlab = "Time of Day (hrs)", main = "Maintenance", col = c("blue"))
axis(side =1, at=seq(0,23,1))

plot( h1, col=rgb(1,1,0,1/3), xlim=c(1,23))  # first histogram
plot( h2, col=rgb(1,0,0,1/3), xlim=c(1,23), add=T)  # first histogram
plot( h3, col=rgb(0,1,0,1/3), xlim=c(1,23), add=T)  # first histogram
plot( h4, col=rgb(0,0,1,1/3), xlim=c(1,23), add=T, main = NULL)  # second
axis(side =1, at=seq(0,23,1))

#--------Isloate the too hot/too cold complaints---- 
HotColdComps <- filter(TempResp, h_Type == "Too Hot" ) #| h_Type == "Too Hot"

h5 <- hist(hour(strptime(HotColdComps$Create.Date, format = "%m/%d/%Y %I:%M:%S %p")), 
           xlim = c(0,23) , xlab = "Time of Day (hrs)", main = "Temperature", col = c("red"))
axis(side =1, at=seq(0,23,1))

# try break it down by day

compdates <- date(strptime(HotColdComps$Create.Date, format = "%m/%d/%Y %I:%M:%S %p"))
hist(compdates, breaks = "year")

#--------Calculating 24 HOUR SURVIVAL CURVES - take out any rows where response time exceeds 24h or is 0h ---- 
OverallResp24H <- subset(OverallResp, resp.time <= 24 & resp.time > 0, 
                          select=c(Building.Name.., Create.Date, Closed.Date, h_Category, h_Type, Floor, Suite ,resp.time))
LightingResp24H <- OverallResp24H[ grep("Light", OverallResp24H$h_Category, invert = FALSE) , ]
TempResp24H <- OverallResp24H[ grep("Temp", OverallResp24H$h_Category, invert = FALSE) , ]
JanitorResp24H <- OverallResp24H[ grep("Jan", OverallResp24H$h_Category, invert = FALSE) , ]
MaintResp24H <- OverallResp24H[ grep("Main", OverallResp24H$h_Category, invert = FALSE) , ]

#Calling an empirical cumulative distribution function
P_overall = ecdf(unlist(OverallResp24H$resp.time))
P_lighting = ecdf(unlist(LightingResp24H$resp.time))
P_temperature = ecdf(unlist(TempResp24H$resp.time))
P_janitorial = ecdf(unlist(JanitorResp24H$resp.time))
P_maintenance = ecdf(unlist(MaintResp24H$resp.time))

X = 1:1000*0.1
Y_overall = P_overall(X)
Y_lighting = P_lighting(X)
Y_temperature = P_temperature(X) 
Y_janitorial = P_janitorial(X)
Y_maintenance = P_maintenance(X)

Ones= rep(1,length(X))
Survival_overall = Ones - Y_overall
Survival_lighting = Ones - Y_lighting
Survival_temperature = Ones - Y_temperature
Survival_janitorial = Ones - Y_janitorial
Survival_maintenance = Ones - Y_maintenance

#-------------------------------------------Create overall survival graphs ----
jpeg("Enter_your_JPEG_name.jpeg", width = 3, height = 3, units = 'in', res = 1080)
par(mgp=c(1.5,0.55,0),mar=c(3,3,3,2)+0.1)
plot(Survival_overall,main=bquote("Building x (X m"^"2"*" / X ft"^"2"*")"), cex.main=0.7, 
     xlab= "time(h)",ylab="fraction unaddressed complaints",xlim=c(0, 50), ylim=c(0, 1),yaxs="r",yaxp=c(0,1,10),
     col = "black", cex=0.2, cex.lab=0.7, cex.axis=0.6, las=1)
lines(Survival_overall, col = "black",lwd = 1)
points(Survival_lighting, col = "red", cex=0.4, pch = 0)
lines(Survival_lighting, col = "red", lwd = 1)
points(Survival_temperature, col = "blue", cex=0.3, pch = 2)
lines(Survival_temperature, col = "blue", lwd = 1)
points(Survival_janitorial, col = "green", cex=0.3, pch = 4)
lines(Survival_janitorial, col = "green", lwd = 1)
points(Survival_maintenance, col = "orange", cex=0.3, pch = 5)
lines(Survival_maintenance, col = "orange", lwd = 1)
legend("topright",legend =c("Overall","Lighting","Temperature","Janitorial","Maintenance"), 
       col=c("black","red","blue","green","orange"),lty=1:2, cex=0.45, box.lty=0, 
       title="Survival Curve Categories", bg="transparent",pch =c(19,0,24,4,5))
dev.off()

#-------------------------------------------Splitting up complaints per year ------------------------------------------
#Convert the time stamps to a format where they can be parsed via a year filter
closed_time_year <- mdy_hms(importantData$Create.Date, tz = "EST")
closed_year <- year(closed_time_year)

#2014 complaints for lighting, heating, janitorial and maintenance 
floor_2014_light <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2014  & importantData$h_Category == "Lighting")
floor_2014_heat <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2014  & importantData$h_Category == "Temperature")
floor_2014_jan <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2014  & importantData$h_Category == "Janitorial")
floor_2014_maint <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2014  & importantData$h_Category == "Maintenance")

#2015 complaints for lighting, heating, janitorial and maintenance
floor_2015_light <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2015  & importantData$h_Category == "Lighting")
floor_2015_heat <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2015  & importantData$h_Category == "Temperature")
floor_2015_jan <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2015  & importantData$h_Category == "Janitorial")
floor_2015_maint <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2015  & importantData$h_Category == "Maintenance")

#2016 complaints for lighting, heating, janitorial and maintenance
floor_2016_light <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2016  & importantData$h_Category == "Lighting")
floor_2016_heat <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2016  & importantData$h_Category == "Temperature")
floor_2016_jan <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2016  & importantData$h_Category == "Janitorial")
floor_2016_maint <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2016  & importantData$h_Category == "Maintenance")

#2017 complaints for lighting, heating, janitorial and maintenance
floor_2017_light <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2017  & importantData$h_Category == "Lighting")
floor_2017_heat <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2017  & importantData$h_Category == "Temperature")
floor_2017_jan <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2017  & importantData$h_Category == "Janitorial")
floor_2017_maint <- subset(importantData, year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2017  & importantData$h_Category == "Maintenance")

#-------------------------------------------Generate stacked line chart ---------------------------------------------------
#Complaints per floor area 
floor_area = 10000 #Enter your floor area here

complaints.per.m2 = c(nrow(floor_2014_light)/floor_area, nrow(floor_2014_heat)/floor_area, nrow(floor_2014_jan)/floor_area, nrow(floor_2014_maint)/floor_area,
                      nrow(floor_2015_light)/floor_area, nrow(floor_2015_heat)/floor_area, nrow(floor_2015_jan)/floor_area, nrow(floor_2015_maint)/floor_area, 
                      nrow(floor_2016_light)/floor_area, nrow(floor_2016_heat)/floor_area, nrow(floor_2016_jan)/floor_area, nrow(floor_2016_maint)/floor_area, 
                      nrow(floor_2017_light)/floor_area, nrow(floor_2017_heat)/floor_area, nrow(floor_2017_jan)/floor_area, nrow(floor_2017_maint)/floor_area)
complaints.per.ft2 = complaints.per.m2*0.0929

year= c(2014,2014,2014,2014,2015,2015,2015,2015,2016,2016,2016,2016,2017,2017,2017,2017)
line_plot <- data.frame(complaints.per.m2, year, Complaint.type = rep(c("Lighting","Thermal","Janitorial","Maintenance"), each = 1))

#------------------------------Spatial distribution of complaints (Sankey Diagrams) ----------------------------------
#Find the number of total complaints
SumComp <- length(importantData$Floor)
#view distribution of complaints per floor
comps_per_floor <- table(unlist(importantData$Floor))

#view per floor breakdown of complaint types i.e. lighting on the 3rd floor
#split the dataset up unto complaints per floor
floor_nums <- levels(unique(importantData$Floor))
#for loop from 1 to number of floors 
#for loop for lighting
floor_light <- list()
floor_heat <- list()
floor_jan <- list()
floor_maint <- list()
floor_other <- list()

for (i in 1:length(floor_nums)){
  floor_light[[i]] <- subset(importantData, importantData$Floor == floor_nums[i] & importantData$h_Category == "Lighting")
  floor_heat[[i]] <- subset(importantData, importantData$Floor == floor_nums[i] & importantData$h_Category == "Temperature")
  floor_jan[[i]] <- subset(importantData, importantData$Floor == floor_nums[i] & importantData$h_Category == "Janitorial")
  floor_maint[[i]] <- subset(importantData, importantData$Floor == floor_nums[i] & importantData$h_Category == "Maintenance")
  floor_other[[i]] <- subset(importantData, importantData$Floor == floor_nums[i] & (importantData$h_Category != "Lighting" & importantData$h_Category != "Temperature" & importantData$h_Category != "Janitorial" & importantData$h_Category != "Maintenance"))
}

#already have complaints per type on each floor (floor_light,floor_heat etc)
#now we have all the data needed for a sankey diagram
#create a list holding the names of all the nodes for the sankey
Node_Names <- list()
j=2

while (j <= length(floor_nums)+1) {
  Node_Names[j] <- floor_nums[j-1]
  j=j+1
}

#Append a header for the first node. We need this for the LHS of the sankey- showing us total complaints
Node_Names[1]="TOTAL"

#End the nodes list with the 4 categories (Lighting Temperature Janotorial Maintenance Other)
#We use floor_nums + 2 because for node names we already shifted down by 1 by adding the "total" section
Node_Names[length(floor_nums)+2]="Lighting"
Node_Names[length(floor_nums)+3]="Temperature"
Node_Names[length(floor_nums)+4]="Janitorial"
Node_Names[length(floor_nums)+5]="Maintenance"
Node_Names[length(floor_nums)+6]="Other"

#Turn it all into one data frame
df <- data.frame(matrix(unlist(Node_Names), nrow=length(Node_Names), byrow=T))
names(df) <- c("name")

#Building the matrix to hold the nodes
#The matrix consists of 2 parts- the LHS and the RHS
comps_per_floor = matrix(data=NA, nrow=length(floor_nums), ncol=1)

#the RHS is more complex. It has the complaint categorical breakdown of each floor
y = matrix(data=NA, nrow=length(floor_nums)*5, ncol=3)
j=1
i=1
#populate column 1
for(j in 1:3){
  if (j==1)
  {
    for(i in 1:(length(floor_nums)*5)){
      y[i,j] = trunc(((i-1)/5)+1)
    }
  }
  #populate column 2
  else if (j==2)
  {
    y[,2]=rep(c(length(floor_nums)+1):(length(floor_nums)+5), times=length(floor_nums))
  }
  #populate column 3
  else
  {
    for(i in 1:(length(floor_nums)*5)){
      if(i%%5 == 1)
      {
        y[i,j] = nrow(floor_light[[y[i,1]]])
      }
      else if(i%%5 == 2)
      {
        y[i,j] = nrow(floor_heat[[y[i,1]]])
      }
      else if(i%%5 == 3)
      {
        y[i,j] = nrow(floor_jan[[y[i,1]]])
      }
      else if(i%%5 == 4)
      {
        y[i,j] = nrow(floor_maint[[y[i,1]]])
      }
      else
      {
        y[i,j] = nrow(floor_other[[y[i,1]]])
      }
    }
  }
}
#the LHS shows the distribution of complaints from the total# of comps to each floor
x = matrix(data=NA, nrow=length(floor_nums), ncol=3)
j=1
i=1
for(j in 1:3){
  if (j==1)
  {
    for(i in 1:length(floor_nums)){
      x[i,j] = 0
    }
  }
  else if (j==2)
  {
    for(i in 1:length(floor_nums)){
      x[i,j] = i
    }
  }
  else
  {
    for(i in 1:length(floor_nums)){
      x[i,j] = nrow(floor_light[[y[i,1]]])+nrow(floor_heat[[i]])+nrow(floor_jan[[i]])+nrow(floor_maint[[i]])+nrow(floor_other[[i]])
    }
  }
}
#bind the two matrices together to create the nodes
vals = do.call(rbind,list(x,y))
print(vals)

#-------------------------------------------------DETAILED TEXT MINING --------------------------------------------
#Separate out the complaints by floor and type
#the number inside the double brackets indicates the floor#
ARM_Dataframe_Generic <- importantData[ importantData$h_Category == "Lighting" & importantData$Floor== "03" &
                                          year(mdy_hms(importantData$Create.Date, tz = "EST")) == 2015, ]
ARM_Dataframe_Specific <- subset(ARM_Dataframe_Generic$Comments, ARM_Dataframe_Generic$Comments != 'is.na')

## Preprocessing to create a document term matrix
mydata <- ARM_Dataframe_Specific
documents <- VectorSource(t(mydata))
jeopCorpus <- Corpus(documents)
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, content_transformer(tolower))
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, stripWhitespace)
jeopCorpus <- tm_map(jeopCorpus, removeNumbers)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords("english"))
jeopCorpus <- tm_map(jeopCorpus, stemDocument)
jeopCorpus <- tm_map(jeopCorpus, removeWords, c("enter", "your", "own", "words", "to", "filter"))
jeopCorpus <- tm_map(jeopCorpus, content_transformer(gsub), pattern = "\\b(stat|tstat|statsk|tstatsk|statssk)\\b", 
                                                                      replacement = "tstat")


df_experiment <- data.frame(text = get("content", jeopCorpus))
head(df_experiment)

dtm <- DocumentTermMatrix(jeopCorpus)

#-----------Create a bar plot of the most common terms ----------------------------
#create a term document matrix instead of document term matrix
tdm <- TermDocumentMatrix(jeopCorpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

# Write the raw document text matrix to a CSV ----
write.csv(as.matrix(dtm), file="dtm.csv")

#Word frequency matrix
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
m <- as.matrix(freq)
dim(m)

#creating term matrix with TF-IDF weighting
terms <-DocumentTermMatrix(jeopCorpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
write.csv(as.matrix(terms), file="tfidf.csv")

#Word frequency matrix after TF-IDF weighting
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
m <- as.matrix(freq)
dim(m)

dtmss <- removeSparseTerms(dtm, 0.994) # This makes a matrix that is only 15% empty space, maximum.  0.9999 usual 0.98
write.csv(as.matrix(dtmss), file="dtmss.csv")

## ---- Using non-sparse dtm conduct an association rule analysis

mydata <- read.csv('dtmss.csv', stringsAsFactors = FALSE)
mydata[1] <- NULL

# Set support and confidence values
Support_ARM = 0.01
Confidence_ARM = 0.85
Number_of_rules = 20

## Support 0.005 and confidence 0.80 support sort
mydata <- sapply(mydata,as.logical)
rules <- apriori(mydata,parameter = list(sup = Support_ARM, conf = Confidence_ARM,target="rules"))
summary(rules)
rules.sorted <- sort(rules, by="support")

subrules <- head(sort(rules, by="support"),Number_of_rules)
view(subrules) 
write(subrules, file = "dataNoClusteringSup00005Conf_support.csv", sep = ",")

set.seed(1)

plot(subrules, method="graph", control=list(type="items"))

## Support 0.005 and confidence 0.80 lift sort
mydata <- sapply(mydata,as.logical)
rules <- apriori(mydata,parameter = list(sup = Support_ARM, conf = Confidence_ARM,target="rules"))
summary(rules)
rules.sorted <- sort(rules, by="lift")
 
subrules <- head(sort(rules, by="lift"),Number_of_rules)
view(subrules)
write(subrules, file = "dataNoClusteringSup00005Conf_lift.csv", sep = ",")

set.seed(1)

plot(subrules, method="graph", control=list(type="items"))

## Support 0.0001 and confidence 0.75 confidence sort
mydata <- sapply(mydata,as.logical)
rules <- apriori(mydata,parameter = list(sup = Support_ARM, conf = Confidence_ARM,target="rules"))
summary(rules)
rules.sorted <- sort(rules, by="confidence")

subrules <- head(sort(rules, by="confidence"),Number_of_rules)
view(subrules)
write(subrules, file = "dataNoClusteringSup00005Conf_confidence.csv", sep = ",")

set.seed(1)

plot(subrules, method="graph", control=list(type="items"))




# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Title of your app"),
  
  # # Sidebar layout with input and output definitions ----
  # sidebarLayout(
  #   
    # Sidebar panel for inputs ----
    sidebarPanel(

      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
    ),
  #   
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Survival Curve", plotOutput("survivalcurve", width = "100%")),
                  tabPanel("Stacked Area", plotOutput("stackedarea", width = "100%")),
                  tabPanel("Sankey", sankeyNetworkOutput("sankey", width = "100%")),
                  tabPanel("Word Frequency", plotOutput("wordfreq", width = "80%")),
                  tabPanel("Node Network", plotOutput("NodeNetwork", width = "100%"))
      )
      
    )
  )
#)


# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  output$survivalcurve <- renderPlot({
    survival_all_curves <- cbind(c(1:1000),as.data.frame(Survival_overall), as.data.frame(Survival_lighting), 
                                 as.data.frame(Survival_temperature), as.data.frame(Survival_janitorial), 
                                 as.data.frame(Survival_maintenance))
    colnames(survival_all_curves) <- c("Hours","Survival_overall","Survival_lighting","Survival_temperature",
                                       "Survival_janitorial","Survival_maintenance")
    options( warn = -1 )
    ggplot(survival_all_curves, aes(Hours)) +
          theme(panel.background = element_blank(), legend.text = element_text(size = 10),
                axis.title.x = element_text(color="black", size=18, face="bold"), 
                axis.title.y = element_text(color="black", size=18, face="bold"),
                axis.text = element_text(color="black", size=18)) +
          xlab("Time (Hours)") + ylab("Fraction of complaints unaddressed") +
          geom_line(aes(x = Hours,y = Survival_overall, colour = 'overall', size = 2)) +
          geom_line(aes(x = Hours,y = Survival_lighting, colour = 'lighting', size = 2)) +
          geom_line(aes(x = Hours,y = Survival_temperature, colour = 'temperature', size = 2)) +
          geom_line(aes(x = Hours,y = Survival_janitorial, colour = 'janitorial', size = 2)) +
          geom_line(aes(x = Hours,y = Survival_maintenance, colour = 'maintenance', size = 2)) +
          xlim(0,72)
  })
  output$stackedarea <- renderPlot({
    ggplot(line_plot, aes(x = year, y = complaints.per.m2, fill = Complaint.type)) + geom_area(position = 'stack') + 
          theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size=28)) + 
          xlab("Year") + 
          ylab("Complaints/m\u00B2") +
          theme (axis.title.x = element_text(color="black", size=18, face="bold"), 
                 axis.title.y = element_text(color="black", size=18, face="bold"),
                 axis.text = element_text(color="black", size=18)
          )
  })
  output$sankey <- renderSankeyNetwork({
    nodes=df
    links = as.data.frame(vals, byrow= TRUE, ncol=3)
    names(links) = c("source", "target", "value")
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize= 18, nodeWidth = 30)
  })
  # Generate a summary of the data ----
  output$wordfreq <- renderPlot({
    ggplot(d, aes(x = reorder(word, -freq), y = freq))+ 
      geom_bar(stat = "identity")+ 
      coord_flip()+ 
      xlab("Terms")+
      theme (axis.title.x = element_text(color="black", size=18, face="bold"), 
             axis.title.y = element_text(color="black", size=18, face="bold"),
             axis.text = element_text(color="black", size=18),
             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank())
  })
  # Generate an HTML table view of the data ----
  output$NodeNetwork <- renderPlot({
    plot(subrules, method="graph", control=list(type="items"))
  })
}
shinyApp(ui,server)
