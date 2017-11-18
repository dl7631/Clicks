library(tidyverse)
library(mice)
library(VIM)

#----------------------------------------------------------------------------------------
# Read in impressions:
#----------------------------------------------------------------------------------------


imp <- read_csv("sample_impressions58dd0bc0bae81f0013d47d411510679198069.csv")
glimpse(imp)  # All characters, except for:
# X1, ad, age, backendStatus, bidPrice, campaign, day, 
# deviceType, elbStatus, price, targetGroup, timestamp (dttm)
dim(imp) # 22,806 by 34

# What is this first column, X1?
sum(imp$X1 - (0:(nrow(imp) - 1)))  # Not zero - why? Is it some index from a data base?
imp$X1[7576:7578]  # Numeration restarts with zero in row 7577 (higher number is 7575)
# Maybe irrelevant

# write.csv(names(imp), 'varnames impressions.csv')
write.csv(unlist(sapply(imp, function(x) class(x))), "vartypes impressions.csv")

imp$timestamp[1:10]

###############################################
# Questions:
###############################################

# How can I match 3 files? What are the connecting keys (ids)
# What is the first column of imp?
# is 'auctionID' the ID for the add?
# What's 'path'?
# What is the meaning of a few numeric values in  'region' (like 00, 03, 05)

imp$auctionId[1:100]

###############################################
# Observations:
###############################################

# Region is total mess - needs recoding - it's mostly US states but also Canada
# Do we want to mess with it? 13% are missing values
# Rather use 'state' - it is cleaner and has no missing values (still some recoding needed)
View(count(imp, region))


#----------------------------------------------------------------------------------------
# Missing Values:
#----------------------------------------------------------------------------------------


sapply(imp, function(x) sum(is.na(x)))
write.csv(round(sapply(imp, function(x) sum(is.na(x)))/nrow(imp) * 100, 1), "missings impressions.csv")
# age - 73%
# gender - 57%
# udid - 26%
# zip - 15$
# region - 13%
# carrier - 9%
# deviceName = 5%
# location - 2%

md.pattern(imp)

imp_aggr = aggr(imp, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, combined = F,
                labels=names(imp), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
?aggr

View(count(imp, region))
View(count(imp, state))

#----------------------------------------------------------------------------------------
# Looking at variables:
#----------------------------------------------------------------------------------------
summary(imp)
types = unlist(sapply(imp, function(x) class(x)))
types = types[-31]
catvars = names(imp)[types == "character"]
sapply(imp[catvars], function(x) length(unique(x)))
length(unique(imp$zip))
sapply(imp[catvars], function(x) length(unique(x)))

count(imp, adSize)
count(imp, adType)
View(count(imp, carrier))
distinct(imp, bestVenueName)[1000:1020,]
count(imp, country)
distinct(imp, deviceName)[100:120,]
distinct(imp, dimensions)[100:120,]
count(imp, exchange)
distinct(imp, dimensions)[10:120,]
table(imp$gender)
count(imp, gender)
View(count(imp, iabCategories))
View(count(imp, landingPage))
count(imp, landingPage)[1,]
imp$landingPage[2]
View(count(imp, location)[1:100,])
count(imp, os)
count(imp, path)
View(count(imp, region))
View(count(imp, state))
count(imp, udid)[1:10,]
count(imp, venueType)
View(count(imp, zip)[1:100,])


count(imp, ad)
View(unique(imp$ad))
length(unique(imp$ad))
count(imp, campaign)

count(imp, backendStatus)
count(imp, day)/nrow(imp)
count(imp, deviceType)/nrow(imp)

count(imp, elbStatus)
summary(imp)
count(imp, month)
View(imp[1:100, c("bidPrice", "price")])
