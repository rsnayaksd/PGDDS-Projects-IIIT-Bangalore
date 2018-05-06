
# Loading required packages
library(dplyr)
library(tidyr)
library(stringr)

# Creating dataframes by loading data from txt and csv files

companies <- read.delim("companies.txt" , stringsAsFactors = FALSE)

rounds2 <- read.csv("rounds2.csv" , stringsAsFactors = FALSE)

mapping <- read.csv("mapping.csv" , stringsAsFactors = FALSE )
#mapping <- read.csv("mapping.csv" , stringsAsFactors = FALSE , check.names = FALSE)


# Checkpoint 1: Data Cleaning 1 ----

#1 How many unique companies are present in rounds2?
# Answer: 66368

# applying unique to get the distinct companies names from rounds2 dataframe

length(unique(tolower(rounds2$company_permalink)))


#2 How many unique companies are present in the companies file?
# Answer: 66368

# applying unique to get the distinct companies names from companies dataframe

length(unique(tolower(companies$permalink)))

#3 In the companies data frame, which column can be used as the unique key for each company?
## Write the name of the column.
# Answer: permalink

#4 Are there any companies in the rounds2 file which are not present in companies ? 
## Answer Y/N.
# Answer : N

# comparing the no. of companies in rounds2 and companies dataframes 
sum(!(tolower(rounds2$company_permalink) %in% tolower(companies$permalink)))

#5 Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
## Name the merged frame master_frame.
## How many observations are present in master_frame ?
# Answer : 114949

# converting the text to lowercase for permalink in rounds2 dataframe
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# converting the text to lowercase for permalink in companies dataframe
companies$permalink <- tolower(companies$permalink)

# merging rounds2 and companies into master_frame
master_frame <- merge(rounds2, companies, by.x= "company_permalink" , by.y="permalink")

# getting no. of rows from master_frame to determine no. of observations
nrow(master_frame)

#######################################################################
# Checkpoint 2: Funding Type Analysis ----

# groupping master_frame data by funding_round_type into new df group_fund_type
group_fund_type <- group_by(master_frame , funding_round_type)

# summarize group_fund_type by avg_raised_amount into new df avg_raised_amount
avg_raised_amount <- summarise(group_fund_type , avg_raised_amount =mean(raised_amount_usd , na.rm = TRUE ))

#1 Average funding amount of venture type
# Answer: 11748949

# Filtering avg_raised_amount by funding_round_type as "venture"
filter(avg_raised_amount, funding_round_type == 'venture')

#2 Average funding amount of angel type
# Answer: 958694.5

# Filtering avg_raised_amount by funding_round_type as "angel"

filter(avg_raised_amount, funding_round_type == 'angel')

#3 Average funding amount of seed type
# Answer: 719818

# Filtering avg_raised_amount by funding_round_type as "seed"

filter(avg_raised_amount, funding_round_type == 'seed')


#4 Average funding amount of private equity type
# Answer: 73308593

# Filtering avg_raised_amount by funding_round_type as "private_equity"

filter(avg_raised_amount, funding_round_type == 'private_equity')

#5 Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
## which investment type is the most suitable for them?
# Answer: Venture

# Filtering group_fund_type by (5000000 <= avg_raised_amount <= 15000000)

filter(summarise(group_fund_type , avg_raised_amount =mean(raised_amount_usd , na.rm = TRUE )), 
       avg_raised_amount >= 5000000 , avg_raised_amount <= 15000000)

#################################################################################
#Checkpoint 3: Country Analysis ----
# Finding Top English speaking countries for Funding type as Venture

# Filtering master_frame by funding_round_type as 'venture' to exclude observations with no country code
Venture_filter <- filter(master_frame, funding_round_type== 'venture' , country_code != "")

# Goupping venture company data by country_code
country_group <- group_by(Venture_filter, country_code)

# Summarizing country_group by raised_amount_usd
summarise(country_group , raised_amount =sum(raised_amount_usd, na.rm = TRUE))

# considering the top9 countries by arranging them in descending order
top9 <- arrange(summarise(country_group , raised_amount =sum(raised_amount_usd, na.rm = TRUE)), desc(raised_amount))[1:9,]

# Answer:
# USA is Top English speaking country
# GRB is Second English speaking country
# IND is Third English speaking country

# CHN is not English speaking country

# so top three english speaking country on the basis of total investment 
# having funding type venture are USA, GBR and IND 


#################################################################################
#Checkpoint 4: Sector Analysis 1 ----
# Extract the primary sector of each category list from the category_list column

# Converting mapping data into long format
mapping1 <- gather(mapping, main_category, values, Automotive...Sports:Social..Finance..Analytics..Advertising )

# filtering mapping for TRUE values only and deleting the False observations
mapping_category <- filter(mapping1,values!=0 )[,1:2]

# Converting the text to lowercase 
mapping_category$category_list <- tolower(mapping_category$category_list)

# creating copy of master_frame
master_frame1 <- master_frame

# Getting total no. of NA in primary_sector
sum(is.na(master_frame1$primary_sector)) 
# 0 NA values found in primary_sector

# Extracting primary_sector by spliting category_list value separated "|" 
master_frame1$primary_sector <-  sapply(str_split(master_frame1$category_list , "\\|" , n=2 ), `[`, 1)

# Converting the text to lowercase
master_frame1$primary_sector <- tolower(master_frame1$primary_sector)

# Merging master_frame1 and mapping_category 
master_frame1 <- merge(master_frame1, mapping_category , by.x = "primary_sector" , by.y = "category_list" , all.x = TRUE)

# Renaming the master_frame1 column main_category to main_sector
names(master_frame1)[17] <- "main_sector"


#after merger we found that there are 8142 records in master_frame where main_sector is NA.

sum(is.na(master_frame1$main_sector)) # 8142

# after further analysis , it was found that there are many places where is "0" instead of "na"
mapping_category12 <- mapping_category

# replacing 0 by na
mapping_category12$category_list <- str_replace_all(mapping_category12$category_list, "0" , "na")

# Creating a copy of master_frame
master_frame12 <- master_frame

# After replacing 0 by na, extracting primary_sector by spliting category_list value separated "|"
master_frame12$primary_sector <-  sapply(str_split(master_frame12$category_list , "\\|" , n=2 ), `[`, 1)

# Verifying for na in primary_sector
sum(is.na(master_frame1$primary_sector)) 
#0

# Converting primary_sector text to lowercase
master_frame12$primary_sector <- tolower(master_frame12$primary_sector)

# Merging master_frame12, mapping_category12 into master_frame123
master_frame123 <- merge(master_frame12, mapping_category12 , by.x = "primary_sector" , by.y = "category_list" , all.x = TRUE)

# Renaming column name from main_category to main_sector in master_frame123
names(master_frame123)[17] <- "main_sector"

# Rearranging master_frame123 variables
master_frame123 <- master_frame123[,c(2:16,1,17)]

# Verifying for na values in main_sector 
sum(is.na(master_frame123$main_sector)) 
#-- 98 out of 114949 ~ 0.08 %

# Listing unique primary_sector  
unique(master_frame123$primary_sector[is.na(master_frame123$main_sector)]) 
# 42 out of 689 ~ 6%

# We will ignore this 0.08% NA


###################################################################################
# Checkpoint 5: Sector Analysis 2 ----

# Creating three data frames D1_USA, D2_GBR and D3_IND
# for USA, England and India respectively
# with Funding type "venture"
# and raised_amount_usd between 5000000 and 15000000

D1_USA <- filter(master_frame123 , country_code=='USA' , funding_round_type== 'venture' , raised_amount_usd >=5000000 , raised_amount_usd <= 15000000)

D2_GBR <- filter(master_frame123 , country_code=='GBR' , funding_round_type== 'venture' , raised_amount_usd >=5000000 , raised_amount_usd <= 15000000)

D3_IND <- filter(master_frame123 , country_code=='IND' , funding_round_type== 'venture' , raised_amount_usd >=5000000 , raised_amount_usd <= 15000000)

# Creating a user-defined function new_dataframe to return country df with two additional column(investment_count and investment_sum)
new_dataframe <- function(investment_group){ 
  investment_group <- group_by(investment_group, main_sector)
  invest_count_df <- summarise(investment_group , investment_count = n())
  invest_sum_df <- aggregate(investment_group$raised_amount_usd, by=list(main_sector=investment_group$main_sector), FUN=sum)
  names(invest_sum_df)[2] <- "investment_sum"
  
  final_df <- merge(investment_group, invest_count_df, by.x = "main_sector" , by.y = "main_sector" , all.x = TRUE)
  final_df <- merge(final_df, invest_sum_df, by.x = "main_sector" , by.y = "main_sector" , all.x = TRUE)
  final_df <- final_df[,c(2:17,1,18:19)]
  
  return (final_df)
}

# Calling the new_dataframe function to get investment details for USA, England and India
D1 <- new_dataframe(D1_USA)
D2 <- new_dataframe(D2_GBR)
D3 <- new_dataframe(D3_IND)

#1) Total number of Investments (count)
# Counting no. of rows for each dataframe to determine no. of investments for each country

nrow(D1_USA)
# 12150 for USA

nrow(D2_GBR)
# 628 for England

nrow(D3_IND)
# 330 for India

#2) Total amount of investment (USD)
# Adding raised_amount_usd in each df to determine total investment made for each country

sum(D1_USA$raised_amount_usd)
# total raised_amount_usd for USA = 108531347515 

sum(D2_GBR$raised_amount_usd)
# total raised_amount_usd for England = 5436843539

sum(D3_IND$raised_amount_usd)
# total raised_amount_usd for India = 2976543602


# 3 Top Sector name (no. of investment-wise)
# 4 Second Sector name (no. of investment-wise)
# 5 Third Sector name (no. of investment-wise)
# 6 Number of investments in top sector (3)
# 7 Number of investments in second sector (4)
# 8 Number of investments in third sector (5)


# For USA, determining the main_sector and their count

counts_usa <- as.data.frame(summary(as.factor(D1_USA$main_sector)))

# naming the column as counts
names(counts_usa) <- "counts"

# creating a new df with main_sector and their counts for US
counts_usa_df <- data.frame(main_sector = row.names(counts_usa), counts =counts_usa$counts)

# Arrangng the df in descending order of the no. of counts
arrange(counts_usa_df , desc(counts))

# For England, determining the main_sector and their count

counts_gbr <- as.data.frame(summary(as.factor(D2_GBR$main_sector)))

# naming the column as counts
names(counts_gbr) <- "counts"

# creating a new df with main_sector and their counts for England
counts_gbr_df <- data.frame(main_sector = row.names(counts_gbr), counts =counts_gbr$counts)

# Arrangng the df in descending order of the no. of counts
arrange(counts_gbr_df , desc(counts))

# For India, determining the main_sector and their count

counts_ind <- as.data.frame(summary(as.factor(D3_IND$main_sector)))

# naming the column as counts
names(counts_ind) <- "counts"

# creating a new df with main_sector and their counts for India
counts_ind_df <- data.frame(main_sector = row.names(counts_ind), counts =counts_ind$counts)

# Arrangng the df in descending order of the no. of counts
arrange(counts_ind_df , desc(counts))



# 9) For point 3 (top sector count-wise), which company received the highest investment?

# Filtering data for main_sector as Others in US
D1_USA_others <- filter(D1_USA ,main_sector=='Others')

# Retrieving company name with max raised_amount_usd in US
D1_USA_others[which.max(D1_USA_others$raised_amount_usd),"name"]
# Edmodo

# Filtering data for main_sector as Others in England
D2_GBR_others <- filter(D2_GBR ,main_sector=='Others')

# Retrieving company name with max raised_amount_usd in England
D2_GBR_others[which.max(D2_GBR_others$raised_amount_usd),"name"]
# Notonthehighstreet

# Filtering data for main_sector as Others in India
D3_IND_others <- filter(D3_IND ,main_sector=='Others')

# Retrieving company name with max raised_amount_usd in India
D3_IND_others[which.max(D3_IND_others$raised_amount_usd),"name"]
# Pepperfry.com

# 10) For point 4 (second best sector count-wise)
# which company received the highest investment?

# Filtering data for main_sector as Social..Finance..Analytics..Advertising in US
D1_USA_Social <- filter(D1_USA ,main_sector=='Social..Finance..Analytics..Advertising')
# Retrieving company name with max raised_amount_usd in US
D1_USA_Social[which.max(D1_USA_Social$raised_amount_usd),"name"]
# Intacct

# Filtering data for main_sector as Social..Finance..Analytics..Advertising in England
D2_GBR_Social <- filter(D2_GBR ,main_sector=='Social..Finance..Analytics..Advertising')
# Retrieving company name with max raised_amount_usd in England
D2_GBR_Social[which.max(D2_GBR_Social$raised_amount_usd),"name"]
# myThings

# Filtering data for main_sector as Social..Finance..Analytics..Advertising in India
D3_IND_Social <- filter(D3_IND ,main_sector=='Social..Finance..Analytics..Advertising')
# Retrieving company name with max raised_amount_usd in India
D3_IND_Social[which.max(D3_IND_Social$raised_amount_usd),"name"]
# Komli Media
