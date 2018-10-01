
library(stringdist)
library(stringr)
library(rlang)
library(tidyverse)

# Set the working directory - NOTE the use of backslash
setwd("C:/Users/Rob/Documents/git/Refine/Refine")


# Read CSV into R
Refine_tb <- read.csv(file="refine_original.csv", header=TRUE, sep=",")

# Make company names all lower case
Refine_tb$company <- tolower(Refine_tb$company)


# Create known list of names
company_names <- c("phillips", "akzo", "van houten", "unilever")

#use amatch to compare fuzzy text stings to the list of known companies
comp_match <- amatch(Refine_tb$company,company_names,maxDist=4)


# Create a vector from the dataframe, as "Refine_tb$company" did not work in the loop that follows
# Do I have to pull out the vector or is there a way to do it in place?
Company_list <-Refine_tb$company

for(i in 1:length(company_names))
{Company_list <- replace(Company_list,comp_match==i,company_names[i])}

#Changed the dataframe to add columns, Any way to avoid this step?
tibble(Company_list)

#Add clean name onto the dataframe
Refine_tb <- add_column(Refine_tb, Company_list, .before = "company")

#Split the product code from the product number
Refine_tb <- separate(Refine_tb, Product.code...number, c("prodcode", "prodnumber"), sep = "-" )

#initialize productcodelong, Is there a better way of doing this?
prodcodelong <- character(nrow(Refine_tb))

for(i in 1:nrow(Refine_tb)) {
  if (Refine_tb$prodcode[i] == "p") {
  prodcodelong[i] <- "Smartphone" 
  } else if (Refine_tb$prodcode[i] == "v") {
  prodcodelong[i] <- "TV"
  } else if (Refine_tb$prodcode[i] == "x") {
  prodcodelong[i] <- "Laptop"
  } else if (Refine_tb$prodcode[i] == "q") {
  prodcodelong[i] <- "Tablet"
  } else {prodcodelong[i] <- "NA"}
  
}

#Add new product code definition to the dataframe
Refine_tb <- add_column(Refine_tb, prodcodelong, .before = "address")

########################################################################
# Add full address column
#
#
Refine_tb <- mutate(Refine_tb, full_address = paste(address, city, country, sep = ", "))

#Clean up Refine_tb by removing the old "company" column
Refine_tb <- mutate(Refine_tb, -"company")

########################################################################
# Add Company Binary Columns
# company_philips, company_akzo, company_van_houten and company_unilever
#
#Original Approach that did not work, need to discuss addressing df[i,j]
#for(i in 1:length(company_names)) {
#  for(j in 1:length(company_names)) {
#   binarycompany_tb <- replace(
#    binarycompany_tb,binarycompany_tb[i,j]==company_names[i], c(TRUE, FALSE)
#      )}
#  }
# 
#Second approach did not work either...
#use !! to say that you want to unquote an input, so that its evaluated
#  use := to dynamically assign paramete
#
# no idea why  the following 2 lines of code didn't work
# Refine_tb <- Refine_tb %>% as_tibble() %>% mutate(
#   !!varname := if_else(Company_list == company_names[4], 1, 0))
#


#This line dynamically assembles the company column used in the function
#varname <- paste("company", company_names[i], sep="_")


multinames <- function(df, patterntest, n) {
  varname <- paste("company", company_names[n], sep="_")
  df[[varname]] <- with(df, grepl(company_names[n], patterntest))
  df
}

for(i in 1:length(company_names)) {
  Refine_tb <- multinames(df=Refine_tb, patterntest = Refine_tb$Company_list, n=i)
}


########################################################################
# Add Product Binary Columns
# product_smartphone, product_tv, product_laptop and product_tablet

product_names <- c("smartphone", "tv","laptop", "tablet")

multiproduct <- function(df, patterntest, n) {
  varname <- paste("product", product_names[n], sep="_")
  df[[varname]] <- with(df, grepl(product_names[n], patterntest, ignore.case = TRUE))
  df
}

for(i in 1:length(product_names)) {
  Refine_tb <- multiproduct(df=Refine_tb, patterntest = Refine_tb$prodcodelong, n=i)
}

#########################################################################
# Write output to CSV

write.csv(Refine_tb, file="refine_clean.csv")


