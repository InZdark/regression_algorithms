# load raw house csv file
house = read.csv('raw_house_data.csv')
# set all None to NA
house$MLS[house$MLS == 'None'] = NA
house$sold_price[house$sold_price == 'None'] = NA
house$zipcode[house$zipcode == 'None'] = NA
house$longitude[house$longitude == 'None'] = NA
house$latitude[house$latitude == 'None'] = NA
house$lot_acres[house$lot_acres == 'None'] = NA
house$taxes[house$taxes == 'None'] = NA
house$year_built[house$year_built == 'None'] = NA
house$bedrooms[house$bedrooms == 'None'] = NA
house$bathrooms[house$bathrooms == 'None'] = NA
house$sqrt_ft[house$sqrt_ft == 'None'] = NA
house$garage[house$garage == 'None'] = NA
house$kitchen_features[house$kitchen_features == 'None'] = NA
house$fireplaces[house$fireplaces == 'None'] = NA
house$floor_covering[house$floor_covering == 'None'] = NA
house$HOA[house$HOA == 'None'] = NA
# save csv
write.csv(house, 'house.csv', row.names=FALSE)



#######################################
############ load csv #################
house = read.csv('house.csv')
# DATA ATTRIBUTES:
# MLS/sold_price/zipcode/longitude/latitude/lot_acres/taxes/year_built/bedrooms/bathrooms/
# sqrt_ft/garage/kitchen_features/fireplaces/floor_covering/HOA

#######################################
############ visualize data ###########
# MLS vs Price
par(mfrow=c(1,2))
#plot(house$MLS,house$sold_price,
#     col='red',pch=19,main='MLS vs Price',
#     xlab='MLS',ylab='price')

plot(house$MLS[house$MLS>2e+7],house$sold_price[house$MLS>2e+7],col='blue',pch=19,xlim=c(3e+6,2.3e+7),
     xlab='MLS',ylab='Price',main='MLS vs Price',cex.lab=1.2)
points(house$MLS[house$MLS<4e+6],house$sold_price[house$MLS<4e+6],col='red',pch=19)
points(house$MLS[house$MLS>4e+6 & house$MLS<5e+6],house$sold_price[house$MLS>4e+6 & house$MLS<5e+6],col='green',pch=19)
text(1e+7,5e+6,labels='red pts: 56')
text(1e+7,4.5e+6,labels='green pts: 32')
text(1e+7,4e+6,labels='blue pts: 4912')
length(house$MLS[house$MLS>2e+7])
length(house$MLS[house$MLS<4e+6])
length(house$MLS[house$MLS>4e+6 & house$MLS<5e+6])

# MLS vs Zipcode
plot(house$MLS[house$MLS>2e+7],house$zipcode[house$MLS>2e+7],col='blue',pch=19,xlim=c(3e+6,2.3e+7),
     main='MLS vs Zipcode',xlab='MLS',ylab='zipcode',cex.lab=1.2)
points(house$MLS[house$MLS<4e+6],house$zipcode[house$MLS<4e+6],col='red',pch=19)
points(house$MLS[house$MLS>4e+6 & house$MLS<5e+6],house$zipcode[house$MLS>4e+6 & house$MLS<5e+6],col='green',pch=19)
text(1e+7,86200,labels='red pts: 56')
text(1e+7,86100,labels='green pts: 32')
text(1e+7,86000,labels='blue pts: 4912')
length(house$MLS[house$MLS>2e+7])
length(house$MLS[house$MLS<4e+6])
length(house$MLS[house$MLS>4e+6 & house$MLS<5e+6])

# Zipcode vs Price
plot(house$zipcode,house$sold_price,
     col='red',pch=19,main='Zipcode vs Price',
     xlab='zipcode',ylab='price',cex.lab=1.2)
house[house$zipcode<85400,]
house[house$zipcode>85800,]

# Longitude vs Latitude
par(mfrow=c(1,2))
plot(house$longitude,house$latitude,
     col='blue',pch=19,main='Longitude vs Latitude',
     xlab='Longitude',ylab='Latitude',cex.lab=1.2)
points(house$longitude[house$MLS>4e+6 & house$MLS<5e+6],house$latitude[house$MLS>4e+6 & house$MLS<5e+6],col='green',pch='o')
points(house$longitude[house$MLS<4e+6],house$latitude[house$MLS<4e+6],col='red',pch='*')

plot(house$longitude,house$latitude,
     col='blue',pch=19,main='Longitude vs Latitude',
     xlab='Longitude',ylab='Latitude',cex.lab=1.2,
     xlim=c(-111.4,-110.5),
     ylim=c(31.2,32.6))
points(house$longitude[house$MLS>4e+6 & house$MLS<5e+6],house$latitude[house$MLS>4e+6 & house$MLS<5e+6],col='green',pch='o')
points(house$longitude[house$MLS<4e+6],house$latitude[house$MLS<4e+6],col='red',pch='*')
par(mfrow=c(1,1))

# Longitude vs Latitude: sold_price
par(mfrow=c(1,2))
hist(house$sold_price,breaks=20,freq=FALSE,xlab='Price',ylab='Desnsity',main='Price Histogram',cex.lab=1.2)
price_l = 6e5
price_h = 1e6
plot(house$longitude,house$latitude,
     col='gray',pch=19,main='Longitude vs Latitude (Price)',
     xlab='Longitude',ylab='Latitude',cex.lab=1.2)
points(house$longitude[house$sold_price<price_l],house$latitude[house$sold_price<price_l],col='green',pch=15)
points(house$longitude[house$sold_price>price_h],house$latitude[house$sold_price>price_h],col='blue',pch=9)
legend(-112.6,32.1,legend=c(paste0(as.character(price_l),'<p<',as.character(price_h)),
                            paste0('<',as.character(price_l)),
                            paste0('>',as.character(price_h))),
       col=c('gray','green','blue'),pch = c(19,15,9),bty = "n")
# zoom in above
par(mfrow=c(1,1))
grid = matrix(c(1,1,1,2,3,4),nrow=2,ncol=3,byrow=TRUE)
layout(grid)
plot(house$longitude,house$latitude,
     col='gray',pch=19,main='Longitude vs Latitude',
     xlab='Longitude',ylab='Latitude',cex.lab=1.2,
     xlim=c(-111.35,-110.5),
     ylim=c(31.2,32.6))
points(house$longitude[house$sold_price<price_l],house$latitude[house$sold_price<price_l],col='green',pch=15)
points(house$longitude[house$sold_price>price_h],house$latitude[house$sold_price>price_h],col='blue',pch=9)
legend(-111.38,32.65,legend=c(paste0(as.character(price_l),'<p<',as.character(price_h)),
                              paste0('<',as.character(price_l)),
                              paste0('>',as.character(price_h))),
       col=c('gray','green','blue'),pch = c(19,15,9),bty = "n")
plot(house$longitude[house$sold_price<price_h&house$sold_price>price_l],
     house$latitude[house$sold_price<price_h&house$sold_price>price_l],
     col='gray',pch=19,main=paste0(as.character(price_l),'<p<',as.character(price_h)),
     xlab='Longitude',ylab='Latitude',cex.lab=1.2,
     xlim=c(-111.35,-110.5),
     ylim=c(31.2,32.6))

plot(house$longitude[house$sold_price<price_l],house$latitude[house$sold_price<price_l],
     col='green',pch=15,main=paste0('<',as.character(price_l)),
     xlab='Longitude',ylab='Latitude',cex.lab=1.2,
     xlim=c(-111.35,-110.5),
     ylim=c(31.2,32.6))

plot(house$longitude[house$sold_price>price_h],house$latitude[house$sold_price>price_h],
     col='blue',pch=9,main=paste0('>',as.character(price_h)),
     xlab='Longitude',ylab='Latitude',cex.lab=1.2,
     xlim=c(-111.35,-110.5),
     ylim=c(31.2,32.6))

# lot_acres vs sold_price
par(mfrow=c(1,1))
plot(house$lot_acres,house$sold_price,
     col='gray',pch=19,main='Lot_acres vs Price',
     xlab='Lot_acres',ylab='Price',cex.lab=1.2)
points(house$lot_acres[house$lot_acres>400],house$sold_price[house$lot_acres>400],col='red',pch=19)
points(house$lot_acres[house$lot_acres==0],house$sold_price[house$lot_acres==0],col='red',pch=19)
# cc before removing outliers
cor(house$lot_acres[!is.na(house$lot_acres)],house$sold_price[!is.na(house$lot_acres)])
# cc after removing outliers
cor(house$lot_acres[!is.na(house$lot_acres)&house$lot_acres<=400],house$sold_price[!is.na(house$lot_acres)&house$lot_acres<=400])

# taxes vs sold_price
par(mfrow=c(1,1))
grid = matrix(c(1,1,2,3),nrow=2,ncol=2,byrow=TRUE)
layout(grid)
plot(house$taxes,house$sold_price,
     col='gray',pch=19,main='Figure 1. Taxes vs Price',
     xlab='Taxes',ylab='Price',cex.lab=1.2)
points(house$taxes[house$taxes>2e5],house$sold_price[house$taxes>2e5],col='red',pch=19)
#
plot(house$taxes,house$sold_price,
     col='gray',pch=19,main='Figure 2. Taxes vs Price',
     xlab='Taxes',ylab='Price',cex.lab=1.2,
     xlim=c(0,4e4))
points(house$taxes[house$sold_price>4e6],house$sold_price[house$sold_price>4e6],col='red',pch=19)
#
plot(house$taxes,house$sold_price,
     col='gray',pch=19,main='Figure 3. Taxes vs Price',
     xlab='Taxes',ylab='Price',cex.lab=1.2,
     xlim=c(0,4e4),ylim=c(0,4e6))
# cc before removing outliers
cor(house$taxes[!is.na(house$taxes)],house$sold_price[!is.na(house$taxes)])
# cc after removing outliers based on taxes
cor(house$taxes[!is.na(house$taxes)&house$taxes<5e4],house$sold_price[!is.na(house$taxes)&house$taxes<5e4])
# cc after removing outliers based on taxes and prices
cor(house$taxes[!is.na(house$taxes)&house$taxes<5e4&house$sold_price<4e6],
    house$sold_price[!is.na(house$taxes)&house$taxes<5e4&house$sold_price<4e6])


# year_built vs sold_price
par(mfrow=c(1,1))
grid = matrix(c(1,1,2,3),nrow=2,ncol=2,byrow=TRUE)
layout(grid)
plot(house$year_built,house$sold_price,
     col='gray',pch=19,
     main='Figure 1. Year_built vs Price',
     xlab='Year_built',ylab='Price',cex.lab=1.2)
points(house$year_built[house$year_built<1000],house$sold_price[house$year_built<1000],col='red',pch=19)
#
plot(house$year_built,house$sold_price,
     col='gray',pch=19,
     main='Figure 1. Year_built vs Price',
     xlab='Year_built',ylab='Price',cex.lab=1.2,
     xlim=c(1875,2020))
points(house$year_built[house$sold_price>4.4e+6],house$sold_price[house$sold_price>4.4e+6],col='red',pch=19)
#
plot(house$year_built[house$sold_price<4.4e+6],house$sold_price[house$sold_price<4.4e+6],
     col='gray',pch=19,
     main='Figure 1. Year_built vs Price',
     xlab='Year_built',ylab='Price',cex.lab=1.2,
     xlim=c(1875,2020))
# cc before removing outliers
length(house$year_built)
cor(house$year_built,house$sold_price)
# cc after removing outliers based on years
length(house$year_built[house$year_built>1000])
cor(house$year_built[house$year_built>1000],house$sold_price[house$year_built>1000])
# cc after removing outliers based on years and prices
length(house$year_built[house$year_built>1000&house$sold_price<4.4e+6])
cor(house$year_built[house$year_built>1000&house$sold_price<4.4e+6],house$sold_price[house$year_built>1000&house$sold_price<4.4e+6])



# bedrooms vs sold_price
par(mfrow=c(1,1))
plot(house$bedrooms,house$sold_price,
     col='gray',pch=19,
     main='Bedrooms vs Price',
     xlab='Bedrooms',ylab='Price',cex.lab=1.2)
points(house$bedrooms[house$bedrooms>15],house$sold_price[house$bedrooms>15],col='red',pch=19)
# cc before removing outliers
length(house$bedrooms)
cor(house$bedrooms,house$sold_price)
# cc after removing outliers based on bedrooms
length(house$bedrooms[house$bedrooms<15])
cor(house$bedrooms[house$bedrooms<15],house$sold_price[house$bedrooms<15])

# bathrooms vs sold_price
plot(house$bathrooms,house$sold_price,
     col='gray',pch=19,
     main='Bathrooms vs Price',
     xlab='Bathrooms',ylab='Price',cex.lab=1.2)
bathrooms = 15
points(house$bathrooms[house$bathrooms>bathrooms|house$sold_price>5e6],
       house$sold_price[house$bathrooms>bathrooms|house$sold_price>5e6],col='red',pch=19)
# cc before removing outliers
length(house$bathrooms[!is.na(house$bathrooms)])
cor(house$bathrooms[!is.na(house$bathrooms)],house$sold_price[!is.na(house$bathrooms)])
# cc after removing outliers
length(house$bathrooms[!is.na(house$bathrooms)&house$bathrooms<=bathrooms&house$sold_price<5e6])
cor(house$bathrooms[!is.na(house$bathrooms)&house$bathrooms<=bathrooms&house$sold_price<5e6],
    house$sold_price[!is.na(house$bathrooms)&house$bathrooms<=bathrooms&house$sold_price<5e6])


# sqrt_ft vs sold_price
plot(house$sqrt_ft,house$sold_price,
     col='gray',pch=19,
     main='Sqrt_ft vs Price',
     xlab='Sqrt_ft',ylab='Price',cex.lab=1.2)
points(house$sqrt_ft[house$sqrt_ft>12000&house$sold_price<1e6],
       house$sold_price[house$sqrt_ft>12000&house$sold_price<1e6],col='red',pch=19)
# cc before removing outliers
a = house[!is.na(house$sqrt_ft),]
length(a$sqrt_ft)
cor(a$sqrt_ft,a$sold_price)
# cc after removing outliers
length(a$sqrt_ft[!(a$sqrt_ft>12000&a$sold_price<1e6)])
cor(a$sqrt_ft[!(a$sqrt_ft>12000&a$sold_price<1e6)],a$sold_price[!(a$sqrt_ft>12000&a$sold_price<1e6)])



# garage vs sold_price
plot(house$garage,house$sold_price,
     col='gray',pch=19,
     main='Garage vs Price',
     xlab='Garage',ylab='Price',cex.lab=1.2)
garages = 10
points(house$garage[house$garage>=garages],
       house$sold_price[house$garage>=garages],col='red',pch=19)
# cc before removing outliers
length(house$garage[!is.na(house$garage)])
cor(house$garage[!is.na(house$garage)],house$sold_price[!is.na(house$garage)])
# cc
length(house$garage[!is.na(house$garage)&house$garage<garages])
cor(house$garage[!is.na(house$garage)&house$garage<garages],house$sold_price[!is.na(house$garage)&house$garage<garages])




# fireplaces vs sol_price
plot(house$fireplaces,house$sold_price,
     col='gray',pch=19,
     main='Fireplaces vs Price',
     xlab='Fireplaces',ylab='Price',cex.lab=1.2)
fireplaces = 8
points(house$fireplaces[house$fireplaces>=fireplaces],house$sold_price[house$fireplaces>=fireplaces],col='red',pch=19)
# cc before removing outliers
length(house$fireplaces[!is.na(house$fireplaces)])
cor(house$fireplaces[!is.na(house$fireplaces)],house$sold_price[!is.na(house$fireplaces)])
# cc
length(house$fireplaces[!is.na(house$fireplaces)&house$fireplaces<fireplaces])
cor(house$fireplaces[!is.na(house$fireplaces)&house$fireplaces<fireplaces],
    house$sold_price[!is.na(house$fireplaces)&house$fireplaces<fireplaces])


# HOA vs sold_price
# a_str = as.character(house$HOA)
# HOA = array(numeric(),c(5000)) 
# for(i in 1:length(a_str)){
#         a = unlist(strsplit(a_str[i],','))
#         if(length(a)>1){
#                 s=paste0(a[1],a[2])
#                 HOA[i] = as.numeric(s)}
#         else{HOA[i] = as.numeric(a)}
# }
# house$new_HOA = HOA
# drop old HOA
# library(dplyr)
# house = select(house, -c(HOA))

plot(house$new_HOA[house$new_HOA!=0],house$sold_price[house$new_HOA!=0],
     col='gray',pch=19,
     main='HOA vs Price',
     xlab='HOA',ylab='Price',cex.lab=1.2)













