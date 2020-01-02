####No longer necessary, I think####
#Install the devtools package then github packages
library(devtools)
install_github("twitter/AnomalyDetection")


#Loading the libraries

library(Rcpp)
library(wikipediatrend)
library(AnomalyDetection)


#Plotting data
library(ggplot2)
ggplot(fifa_data_wikipedia, aes(x=date, y=views, color=views)) + geom_line()

anomaly

#Keep only date & page views and discard all other variables
columns_to_keep=c("date","views")
fifa_data_wikipedia=fifa_data_wikipedia[,columns_to_keep]



#Apply anomaly detection and plot the results
anomalies = AnomalyDetectionTs(data2, direction="pos", plot=TRUE)
anomalies$plot




# Look at the anomaly dates
anomalies$anoms

######################################
#Installing anomalize
install.packages('anomalize')
#Update from github
library(devtools)
install_github("business-science/anomalize")
#Load the package
library(anomalize)
# We will also use tidyverse package for processing and coindeskr to get bitcoin data
library(tidyverse)




#Convert bitcoin data to a time series
data2_ts = data2 %>% rownames_to_column() %>% as_tibble() %>% mutate(Date = as.Date(rowname)) %>% select(-one_of('rowname'))

data2_ts = data2 %>% as_tibble()
data2_ts$Date <- as.Date(data2_ts$Date)

data2_ts <- data2_ts[order(Time),]







#Decompose
data2_ts %>% time_decompose(DOC, method = "stl", frequency = "auto", trend = "auto") %>%  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% plot_anomaly_decomposition()



bitcoin_data_ts %>% time_decompose(Price) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
Converting from tbl_df to tbl_time.
Auto-index message: index = date
frequency = 7 days
trend = 90.5 days



#Extract the anomalies
anomalies=bitcoin_data_ts %>% time_decompose(Price) %>%  anomalize(remainder) %>%  time_recompose() %>%  filter(anomaly == 'Yes')

