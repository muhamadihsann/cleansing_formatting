## Title : Cleansing & Formatting Online Retail Dataset
## Author : Muhamad Ihsan

# Load library
library(tidyverse)
library(lubridate)
library(DataExplorer)

# Load data into a variable
onlineretail <- readxl::read_xlsx('Online Retail.xlsx')

# Preview of the first and last data
head(onlineretail)
tail(onlineretail)

# Missing data checking
plot_missing(onlineretail)

# Exclude missing value observation
onlineretail_drop <- onlineretail[!is.na(onlineretail$CustomerID),]

# Re-checking missing data
plot_missing(onlineretail_drop)

# Now we're going to make a new table contained of :
# CustomerID | Recency | Frequency | Monetary, with the details below:
# Recency   : Days count transaction made (in days)
# Frequency : Transaction count in the last 6 months
# Monetary  : Money spent for transaction by distinct CustomerID (in dollars)

frequency <- onlineretail_drop %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo)) 

monetary <- onlineretail_drop %>% group_by(CustomerID) %>% summarise(monetary=sum(UnitPrice*Quantity))                                               

recency <- onlineretail_drop %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>%   filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate,ymd("2011-12-31"))))/86400) %>% select(CustomerID, recency)

# Make the new table by joining the 3 new variable
newonlineretail <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID")

# Make a csv file from the table
write.csv(newonlineretail, 'Results\\newonlineretail.csv')