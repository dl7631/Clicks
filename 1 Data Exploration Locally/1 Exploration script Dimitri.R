library(tidyverse)
library(mice)
library(VIM)

#----------------------------------------------------------------------------------------
# Read in impressions:
#----------------------------------------------------------------------------------------


imp <- read_csv("sample_impressions.csv")
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

write.csv(unlist(sapply(imp, function(x) sum(length(unique(x))))), "unique levels impressions.csv")

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
count(imp, path)[2]/nrow(imp)
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


View(count(imp, targetGroup) %>% mutate(percent = n/nrow(imp)*100) %>% 
       arrange(-percent))
count(imp, venueType) %>% mutate(percent = n/nrow(imp)*100) %>% 
  arrange(-percent)

#----------------------------------------------------------------------------------------
# Read in impressions:
#----------------------------------------------------------------------------------------

bids <- read_csv("sample_bid_requests.csv")
dim(bids)  # 10,406 by 77
glimpse(bids)

# First column - row number:
sum(bids$X1 - (0:(nrow(bids) - 1)))
write.csv(round(sapply(bids, function(x) sum(is.na(x)))/nrow(bids) * 100, 1), "missings bids.csv")
write.csv(unlist(sapply(bids, function(x) class(x))), "vartypes bids.csv")
write.csv(unlist(sapply(bids, function(x) sum(length(unique(x))))), "unique levels bids.csv")

View(count(bids, bidRequest_app_bundle) %>% mutate(percent = n/nrow(bids)*100))
View(count(bids, bidRequest_app_cat) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_domain) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_ext_nex_sdkv) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_id) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_keywords) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_name) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_publisher_ext_nex_else) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_publisher_id) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_publisher_name) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_storeurl) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_app_ver) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_badv) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_bcat) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_carrier) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_connectiontype) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_didmd5) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_didsha1) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_dpidmd5) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_dpidsha1) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_geo_city) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
table(is.na(bids$bidRequest_device_geo_lat), is.na(bids$bidRequest_device_geo_lon))
bids$bidRequest_device_geo_lat[1:12]
bids$bidRequest_device_geo_lon[1:12]
View(count(bids, bidRequest_device_geo_metro) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_geo_region) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_geo_zip) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_h) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_hwv) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_ifa) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_ip) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_language) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_make) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_device_make) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(bidRequest_device_make))
View(count(bids, bidRequest_device_model) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(bidRequest_device_model))
View(count(bids, bidRequest_device_osv) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(bidRequest_device_osv))
bids$bidRequest_id[1:20]
View(count(bids, bidRequest_imp) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
nrow(bids)

count(bids, bidRequest_device_geo_country, bidRequest_user_geo_country)

count(bids, bidRequest_app_publisher_ext_nex_else)/nrow(bids)*100
count(bids, bidRequest_at)
count(bids, bidRequest_at)/nrow(bids)*100
count(bids, bidRequest_device_connectiontype)
count(bids, bidRequest_device_connectiontype)/nrow(bids)*100
count(bids, bidRequest_device_devicetype)
count(bids, bidRequest_device_devicetype)/nrow(bids)*100
count(bids, bidRequest_device_dnt)
count(bids, bidRequest_device_dnt)/nrow(bids)*100
count(bids, bidRequest_device_geo_country)
count(bids, bidRequest_device_geo_country)[,2]/nrow(bids)*100
count(bids, bidRequest_device_geo_country) %>% 
  filter(bidRequest_device_geo_country %in% c("CAN", "USA")) %>% 
  mutate(percent = n/nrow(bids)) %>% summarize(sum(percent))
count(bids, bidRequest_device_geo_type) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_device_js) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_device_lmt) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_device_macmd5) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_device_macsha1) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_device_os) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_device_ppi) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_device_ua) %>% mutate(percent = n/nrow(bids)*100)
# View(count(bids, bidRequest_device_w) %>% mutate(percent = n/nrow(bids)*100))
count(bids, bidRequest_regs_coppa) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_cat) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_domain) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_id) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_keywords) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_mobile) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_name) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_page) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_publisher_ext_nex_else) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_publisher_id) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_publisher_name) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_site_ref) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_user_ext_nex_dma) %>% mutate(percent = n/nrow(bids)*100)
str(bids$bidRequest_user_ext_nex_eth)
count(bids, bidRequest_user_ext_nex_eth) %>% mutate(percent = n/nrow(bids)*100)
View(count(bids, bidRequest_user_ext_nex_hhi) %>% mutate(percent = n/nrow(bids)*100))
count(bids, bidRequest_user_ext_nex_marital) %>% mutate(percent = n/nrow(bids)*100)
count(bids, bidRequest_user_gender) %>% mutate(percent = n/nrow(bids)*100)
View(count(bids, bidRequest_user_geo_city) %>% mutate(percent = n/nrow(bids)*100))
View(count(bids, bidRequest_user_geo_country) %>% mutate(percent = n/nrow(bids)*100))
View(count(bids, bidRequest_user_geo_region) %>% mutate(percent = n/nrow(bids)*100))
count(bids, bidRequest_user_geo_type) %>% mutate(percent = n/nrow(bids)*100)
View(count(bids, bidRequest_user_geo_zip) %>% mutate(percent = n/nrow(bids)*100))
count(bids, bidRequest_user_geo_zip) %>% mutate(percent = n/nrow(bids)*100)
View(count(bids, bidRequest_user_id) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_user_keywords) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent))
View(count(bids, bidRequest_user_yob) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(bidRequest_user_yob))

count(bids, exchange) %>% mutate(percent = n/nrow(bids)*100)
count(bids, timestamp) %>% mutate(percent = n/nrow(bids)*100)
bids$timestamp


#----------------------------------------------------------------------------------------
# Read in clicks:
#----------------------------------------------------------------------------------------

cl <- read_csv("sample_clicks.csv")
dim(cl)  # 48,729 by 33
glimpse(cl)

# First column - row number:
sum(cl$X1 - (0:(nrow(cl) - 1)))
View(count(cl, X1) %>% mutate(percent = n/nrow(bids)*100) %>% 
       arrange(-percent) %>% top_n(100))


write.csv(round(sapply(cl, function(x) sum(is.na(x)))/nrow(cl) * 100, 1), "missings clicks.csv")
write.csv(unlist(sapply(cl, function(x) class(x))), "vartypes clicks.csv")
write.csv(unlist(sapply(cl, function(x) sum(length(unique(x))))), "unique levels clicks.csv")

View(count(cl, bidPrice) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(-percent))

View(count(cl, dimensions) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(-percent))
length(unique(cl$dimensions))

summary(cl[-1])
View(count(cl, ad) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent))
View(count(cl, bestVenueName) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(-percent))
View(count(cl, carrier) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(-percent))

View(count(cl, deviceName) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent))
View(count(cl, deviceName) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(deviceName))

count(cl, adType) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
count(cl, campaign) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
count(cl, country) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
count(cl, day) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
count(cl, deviceType) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
count(cl, exchange) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
count(cl, gender) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
count(cl, iabCategories) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
count(cl, impId) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
length(unique(cl$impId))
View(cl %>% select(dimensions, impId) %>% top_n(n = 20))
View(count(cl, landingPage) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(-percent))
View(count(cl, location) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(-percent))
View(count(cl, path) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(-percent))
count(cl, venueType) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)

count(cl, udid) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)
length(unique(cl$udid))
cl$timestamp

View(count(cl, state) %>% mutate(percent = n/nrow(cl)*100) %>% 
       arrange(-percent))


count(cl, bidPrice) %>% mutate(percent = n/nrow(cl)*100) %>% 
  arrange(-percent)


dim(cl)

#------------------------------------------------------------------------------------
# Can we find a combination of variables that creates a unique identifier
# for impressions (imp) and clicks (cl)
#------------------------------------------------------------------------------------

# Columns present in both impressions and clicks files and have no NAs:
# ad, adSize, adType, bestVenueName, bidPrice, campaign, country, day,
# deviceType, dimensions
# 

nrow(imp)  # 22,806
imp <-  imp %>% unite(forID, ad, dimensions, remove = F)
names(imp)
View(select(imp, ad, dimensions, forID)[1:20,])
n_distinct(imp$ad)         # 1,004
n_distinct(imp$dimensions) # 15,180
n_distinct(imp$bidPrice)   # 65
n_distinct(imp$campaign)   # 150
n_distinct(imp$day)   # 2
n_distinct(imp$bestVenueName)   # 1,836

n_distinct(imp$forID)      # 15,180 (if based only on ad & dimensions)

imp <-  imp %>% unite(forID, bidPrice, dimensions, remove = F)
n_distinct(imp$forID)      # 15,180 (if based only on bidPrice & dimensions)

imp <-  imp %>% unite(forID, ad, bidPrice, remove = F)
n_distinct(imp$forID)      # 15,180 (if based only on ad & bidPrice)

# Combining 2 variables only:

mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"), 
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(imp[[mygrid$Var1[i]]], imp[[mygrid$Var2[i]]],sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 15,180

# Combining 3 variables:

mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid <- filter(mygrid, !(Var1 == Var3))
mygrid <- filter(mygrid, !(Var2 == Var3))

mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(imp[[mygrid$Var1[i]]], imp[[mygrid$Var2[i]]], imp[[mygrid$Var3[i]]], sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 15,180

# Combining 4 variables:
mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      stringsAsFactors = F)
dim(mygrid)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid <- filter(mygrid, !(Var1 == Var3))
mygrid <- filter(mygrid, !(Var1 == Var4))
mygrid <- filter(mygrid, !(Var2 == Var3))
mygrid <- filter(mygrid, !(Var2 == Var4))
mygrid <- filter(mygrid, !(Var3 == Var4))
dim(mygrid)

mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(imp[[mygrid$Var1[i]]], imp[[mygrid$Var2[i]]], 
                imp[[mygrid$Var3[i]]], imp[[mygrid$Var4[i]]], sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 15,180

# # Most frequent value of dimensions:
dim_frequent <- count(imp, dimensions) %>% arrange(-n) 
dim_frequent <- dim_frequent$dimensions[1]
sum(imp$dimensions %in% dim_frequent)

temp <- imp %>% filter(dimensions == dim_frequent) %>% 
  select(ad, adSize, adType, bidPrice, campaign, country, day, deviceType)
View(temp)

write.csv(imp[imp$dimensions == dim_frequent,], "x temp.csv", row.names = F)

##-------------------------------------------------------------------------------------------------------
## No, there are no unique combinations - some rows are identical!

######################################################################################
## Removing duplicate rows
 
dim(imp)  # 22,806
imp <- imp[-1]
imp <- unique(imp)
dim(imp)  # 15,208 by 33
names(imp)

# Combining 2 variables only:

mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"), 
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(imp[[mygrid$Var1[i]]], imp[[mygrid$Var2[i]]],sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 15,180 - still < 15,208


# Combining 3 variables:

mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid <- filter(mygrid, !(Var1 == Var3))
mygrid <- filter(mygrid, !(Var2 == Var3))

mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(imp[[mygrid$Var1[i]]], imp[[mygrid$Var2[i]]], imp[[mygrid$Var3[i]]], sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 15,180 - still < 15,208

# # Most frequent value of dimensions:
dim_frequent <- count(imp, dimensions) %>% arrange(-n) 
View(dim_frequent)
dim_frequent <- dim_frequent$dimensions[1]
sum(imp$dimensions %in% dim_frequent)

write.csv(imp[imp$dimensions == dim_frequent,], "x temp.csv", row.names = F)

# Looking at rows with double entries:
dim_frequent <- count(imp, dimensions) %>% arrange(-n) %>% 
  filter(n > 1) %>% select(dimensions) %>% unlist()
View(dim_frequent)

write.csv(imp[imp$dimensions %in% dim_frequent,], "x temp.csv", row.names = F)

######################################################################################
# Further deduping - this time ignoring the timestamp
# (just to see if we get this way at the unique identifyer)
names(imp)
dim(imp) # 33 columns
imp <- imp %>% select(-timestamp)
dim(imp) # 15,208 by 32 columns
imp <- unique(imp)
dim(imp)  # 15,187 by 32
names(imp)


# Combining 2 variables only:

mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"), 
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(imp[[mygrid$Var1[i]]], imp[[mygrid$Var2[i]]],sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 15,180 - still < 15,187


# Combining 3 variables:

mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"),
                      stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid <- filter(mygrid, !(Var1 == Var3))
mygrid <- filter(mygrid, !(Var2 == Var3))

mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(imp[[mygrid$Var1[i]]], imp[[mygrid$Var2[i]]], imp[[mygrid$Var3[i]]], sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 15,180 - still < 15,187

# # Most frequent value of dimensions:
dim_frequent <- count(imp, dimensions) %>% arrange(-n)
View(dim_frequent)
dim_frequent <- dim_frequent$dimensions[1]
sum(imp$dimensions %in% dim_frequent)

# Looking at rows with double entries:
dim_frequent <- count(imp, dimensions) %>% arrange(-n) %>% 
  filter(n > 1) %>% select(dimensions) %>% unlist()
write.csv(imp[imp$dimensions %in% dim_frequent,], "x temp.csv", row.names = F)

# Now, only auctionId results in duplicates - let's take it out and dedupe:
# names(imp)
dim(imp) # 32 columns
imp <- imp %>% select(-auctionId)
dim(imp) # 15,187 by 31 columns
imp <- unique(imp)
dim(imp)  # 15,181 by 31
names(imp)

# To achive uniqueness, I removed:
# Unnamed column, timestamp, auctionId
# we can't find unique IDs based on columns in-common with click data file because the final prices
# are sometimes different (for those 2 ads that are shown at the same time (or almost the same) and sold on the same or
# different auctions and show exactly the same ads, but are sold separately - as a result, they either:
# have the same timestamp, but are purchased on different aucionts,
# or they have different timestaps (within seconds from each other), but are bought on the same auction

# When we use price (instead of bid price - we get unique combinations)

mygrid <- expand.grid(c("ad", "bidPrice", "price", "campaign", "day", "dimensions", "bestVenueName"), 
                      c("ad", "bidPrice", "price", "campaign", "day", "dimensions", "bestVenueName"),
                      stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(imp[[mygrid$Var1[i]]], imp[[mygrid$Var2[i]]],sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 15,181 = 15,181 - it's price + dimensions that gives us the final unique identifier

# Looking at rows with double entries:
dim_frequent <- count(imp, bestVenueName, dimensions) %>% arrange(-n) %>% 
  filter(n > 1) %>% select(bestVenueName, dimensions) # %>% unlist()
View(dim_frequent)
write.csv(imp[imp$dimensions %in% dim_frequent$dimensions,], "x temp.csv", row.names = F)


####################################################################################
# For clicks:
 
dim(cl) # 48,729 by 33
mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"), 
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"), stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(cl[[mygrid$Var1[i]]], cl[[mygrid$Var2[i]]],sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 16,195 < 47,729

# Let's remove the first column and dedupe:
names(cl)
cl <- cl[-1]
dim(cl) # 48,729 x 32
cl <- unique(cl)
dim(cl)  # 16,241 by 31 - WOW, we shortened our file by 66%
# Let's remove timestamp and dedupe:
cl <- cl %>% select(-timestamp)
cl <- unique(cl)
dim(cl) # 16,200
# Let's remove impId and dedupe
cl <- cl %>% select(-impId)
cl <- unique(cl)
dim(cl) # 16,195

mygrid <- expand.grid(c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"), 
                      c("ad", "bidPrice", "campaign", "day", "dimensions", "bestVenueName"), stringsAsFactors = F)
mygrid <- filter(mygrid, !(Var1 == Var2))
mygrid$LevelNumber <- NA
for (i in 1:nrow(mygrid)) {
  temp <- paste(cl[[mygrid$Var1[i]]], cl[[mygrid$Var2[i]]],sep = '_')
  mygrid$LevelNumber[i] <- n_distinct(temp)
}
max(mygrid$LevelNumber)  # 16,195 = 16,195 - uniquely identified by ad & dimesnsions or bidPrice & dimensions or even day & dimensions

# Looking at rows with double entries:
dim_frequent <- count(cl, dimensions) %>% arrange(-n) %>% 
  filter(n > 1) %>% select(dimensions) # %>% unlist()
# View(dim_frequent)
write.csv(cl[cl$dimensions %in% dim_frequent$dimensions,], "x temp.csv", row.names = F)

# Summary:

# To achive uniqueness in "impressions", one needs to remove:
# Unnamed column, timestamp, auctionId and then use dimensions + price (actual price paid)
# dimensions + bidPrice don't produce uniqueness nor do dimensions + ad (althought they should) nor
# dimensions + campaign
# Unfortunately, we can't find unique IDs based on columns in-common with click data file because the final prices paid
# are sometimes different (for those 2 ads that are shown at the same time (or almost the same) and sold on the same or
# different auctions and show exactly the same ads, but are sold separately - as a result, they either:
# have the same timestamp, but are purchased on different aucionts, or have different timestaps (within seconds from each other), 
# but are bought on the same auction, or have the same time stamp and auction, but different final prices

# To achieve uniqueness in "clicks" one needs to remove:
# Unnamed column, timestamp, impId (impressionId) and then use dimensions + ad or bidPrice & dimensions)














