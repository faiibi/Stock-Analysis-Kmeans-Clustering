#This project aims to build a stock analysis clustering system by K means clustering

#install the required packages
install.packages("VIM")
library(VIM)
install.packages("cluster")
library(cluster)
install.packages("ggrepel")
library(ggrepel)
install.packages("factoextra")
library(factoextra)
install.packages("skimr")
library(skimr)
install.packages('caret')
library(caret)
install.packages("tidyverse")
library(tidyverse)
install.packages('GGally')
library(GGally)
install.packages('plotly')
library(plotly)
install.packages('shiny')
library(shiny)
install.packages('shinydashboard')
library(shinydashboard)

#import dataset in csv format 
stock_data <- read.csv("stock_prices_jan.csv", header = TRUE)

#inspect dataset
names(stock_data)
head(stock_data)
tail(stock_data)
summary(stock_data)
str(stock_data)
dim(stock_data)

#check dataset for missing values
skim(stock_data)

#check for outliers in the dataset
boxplot(stock_data[3:7])
boxplot(stock_data$open)
boxplot(stock_data$close)
boxplot(stock_data$low)
boxplot(stock_data$high)
boxplot(stock_data$volume)


#Data preparation and cleaning
#create new column: movement which is a subtraction of opening from closing price
stock_data$movement <- stock_data$close - stock_data$open

#create a grouped summary of dataset by averages
avg_stock_data <- aggregate(stock_data, list(stock_data$symbol), FUN = mean )
summary(avg_stock_data)


#inspect the dataset
head(avg_stock_data)

#rename column titles
avg_stock_data<- avg_stock_data %>% 
  rename("company" = "Group.1")

#inspect the dataset
names(avg_stock_data)



#plot the amount of missing/incomplete data
aggr(avg_stock_data)

#remove missing data
avg_stock_data$date <- NULL
avg_stock_data$symbol <- NULL

#inspect new dataset
head(avg_stock_data)

#visualize the relationships between the dataset
pairs(avg_stock_data [2:7])


#visualize relationship between opening and closing prices of stocks
plot(close ~ open,  data = avg_stock_data)

ggplot(data = avg_stock_data, aes(x= open, y=close,label = company)) + 
  geom_label_repel(aes(fill = "col2"), colour = "black", max.overlaps = 200)


#remove company name before normalizing
com_name <- avg_stock_data[,1]
row.names(avg_stock_data)<- com_name
avg_stock_data$company <- NULL


write.csv(avg_stock_data, "C:/Users/FAITH/OneDrive - University of Salford/Documents/Uni Salford/Msc Data Science/ASDM/Coursework/stockanalysis2.csv")


#normalize dataset variables
preproc_stock_data<- preProcess(avg_stock_data, method = "range")
preproc_stock_data
avg_stock_data<-predict(preproc_stock_data,avg_stock_data)


#inspect new dataset
summary(avg_stock_data)





#calculate the clustering tendency
tend_stock <- get_clust_tendency(avg_stock_data, n = 5, graph = TRUE)
tend_stock$hopkins_stat

#calculate the optimal number of clusters 
fviz_nbclust(avg_stock_data, kmeans, method = "wss")


#create kmeans cluster where k=3
stock_clust<-kmeans(avg_stock_data,3) 
stock_clust


stock_clust$cluster

stock_clust$size

#visualize the clusters
clusplot(avg_stock_data, stock_clust$cluster, color=TRUE, shade=TRUE, lines=0)

fviz_cluster(stock_clust,avg_stock_data)


#remove BAC and PCLN as they are outliers
avg_stock_data_2<-subset(avg_stock_data, !(rownames(avg_stock_data) %in%
                                             c("BAC","PCLN")))
#create kmeans cluster where k=3
stock_clust2<-kmeans(avg_stock_data_2,3) 
stock_clust2

stock_clust2$cluster

stock_clust2$size

#visualize the clusters
clusplot(avg_stock_data_2, stock_clust2$cluster, color=TRUE, shade=TRUE, lines=0)

fviz_cluster(stock_clust2,avg_stock_data_2)

#use silhoutte coefficinet to evaluate goodness of clustering
sil_stock_data <- silhouette(stock_clust2$cluster, dist(avg_stock_data_2))
sil_stock_data
fviz_silhouette(sil_stock_data)

#examine features of clustered groups
avg_stock_data_2$cluster<- stock_clust2$cluster

stock_data_p <- ggparcoord(data = avg_stock_data_2, groupColumn = "cluster",
                           scale = "std") + labs(x = "stock features", 
                                                 y = "value (in standard-deviation units)", 
                                                 title = "Clustering of Stock Prices")
ggplotly(stock_data_p, color = 'cluster')


#return company name to avg data set
avg_stock_data <- cbind(com_name, avg_stock_data)
avg_stock_data<- avg_stock_data %>% 
  rename("company" = "com_name")

#create shiny dashboard

clust_ui <- shinyUI(
  dashboardPage(
    
    dashboardHeader(title ="Stock Analysis Clustering"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Stock Data",tabName = "data",icon=icon("table")),
        menuItem("Cluster Plots",tabName = "clusterplots",icon=icon("bar-chart-o"))
      )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "clusterplots",h1("Stock Cluster Plot 1"), 
                fluidRow(
                  box(sliderInput(inputId = 'k', label = "No of Clusters",2,5,3))
                ),
                
                fluidRow(
                  box(plotOutput("scatterplot"), title = 'Relationship Btw Open and Close'),
                  box(plotOutput("clust"), title = 'Cluster Plot'),
                  box(plotOutput("intclust"), title = 'Cluster Plot 2'),
                  box(plotOutput("sil"), title = 'Silhoutte Plot')
                ),
                
                
        ),
        tabItem(tabName = "data",h1("Stock Data"),fluidRow(column(7,tableOutput("stockdata"))))
        
        
        
      ))))    

server <- shinyServer(function(input,output){
  
  k <- reactive({
    kmeans(avg_stock_data, centers = input$k)
  })
  output$scatterplot <- renderPlot({
    plot(close ~ open,  data = avg_stock_data)
    
    ggplot(data = avg_stock_data, aes(x= open, y=close,label = company)) + 
      geom_label_repel(aes(fill = "col2"), colour = "black", max.overlaps = 200)
  })
  output$clust <- renderPlot({
    fviz_cluster(stock_clust2,avg_stock_data_2)
  })
  output$intclust <- renderPlot({
    clusplot(avg_stock_data_2, stock_clust2$cluster, color=TRUE, shade=TRUE, lines=0)
  })
  output$sil <- renderPlot({
    fviz_silhouette(sil_stock_data)
  })
  
  output$stockdata <- renderTable(avg_stock_data)
  
  
})

shinyApp(clust_ui,server)
