rm(list=ls())
house = read.csv('house.csv')

# clean data
house = house %>%
  rename(
    HOA = new_HOA
  )






# remove HOA column: 1386 missing data
library(dplyr)
# house = select(house, -c(new_HOA))

# remove with 2NA in (taxes,year_build,bedrooms,bathrooms,sqrt_ft)
del_rows = list()
for(i in 1:5000){
  count = 0
  if(is.na(house$taxes[i])){count=count+1}
  if(is.na(house$sqrt_ft[i])){count=count+1}
  if(is.na(house$bedrooms[i])){count=count+1}
  if(is.na(house$bathrooms[i])){count=count+1}
  if(is.na(house$year_built[i])){count=count+1}
  if(count>=2){del_rows=append(del_rows,i)}
}
del_rows = unlist(del_rows)
house_2 = house[-c(del_rows),]
##############################################
# remove taxes>4e4
house_2 = house_2[house_2$taxes<=4e4,]

# remove sold_price>4e6
house_2 = house_2[house_2$sold_price<=4e6,]

# PLOT CHECK
par(mfrow=c(1,2))
plot(house$taxes,house$sold_price,pch=19)
plot(house_2$taxes,house_2$sold_price,pch=19)
##############################################
# remove year_built<1500
house_2 = house_2[house_2$year_built>1500,]
# PLOT CHECK
par(mfrow=c(1,2))
plot(house$year_built,house$sold_price,pch=19)
plot(house_2$year_built,house_2$sold_price,pch=19)
##############################################
# remove bedrooms>15 or bathrooms>15
house_2 = house_2[house_2$bedrooms<15,]
house_2 = house_2[house_2$bathrooms<15,]
# PLOT CHECK
par(mfrow=c(1,2))
plot(house$bedrooms,house$bathrooms,pch=19)
plot(house_2$bedrooms,house_2$bathrooms,pch=19)
##############################################
# remove sqrt_ft>12000&sold_price<1e6
index = which(house_2$sqrt_ft>12000&house_2$sold_price<1e6)
house_2 = house_2[-c(index),]
# PLOT CHECK
par(mfrow=c(1,2))
plot(house$sqrt_ft,house$sold_price,pch=19)
plot(house_2$sqrt_ft,house_2$sold_price,pch=19)
##############################################
# remove garages>=15 or fireplaces>=8
house_2 = house_2[house_2$garage<15,]
house_2 = house_2[house_2$fireplaces<8,]
# PLOT CHECK
par(mfrow=c(1,2))
plot(house$garage,house$fireplaces,pch=19)
plot(house_2$garage,house_2$fireplaces,pch=19)
##############################################
# remove sqrt_ft is NA
index = which(is.na(house_2$sqrt_ft))
house_2 = house_2[-c(index),]
# PLOT CHECK
par(mfrow=c(1,2))
plot(house$sqrt_ft,house$sold_price,pch=19)
plot(house_2$sqrt_ft,house_2$sold_price,pch=19)
##############################################
length(house_2$sqrt_ft[is.na(house_2$sqrt_ft)])
length(house_2$bedrooms[is.na(house_2$bedrooms)])
length(house_2$bathrooms[is.na(house_2$bathrooms)])
length(house_2$garage[is.na(house_2$garage)])
length(house_2$fireplaces[is.na(house_2$fireplaces)])
length(house_2$lot_acres[is.na(house_2$lot_acres)])
length(house_2$taxes[is.na(house_2$taxes)])
############ SAVE CSV #############
write.csv(house_2, 'house_cleaned_final.csv', row.names=FALSE)

############ READ CSV #############
rm(list=ls())
house = read.csv('house_cleaned.csv')








