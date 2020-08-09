# CLEAN BEFORE RUN
rm(list=ls())
house_ori = read.csv('house.csv')
house_new = read.csv('house_cleaned.csv')
# MLS
par(mfrow=c(1,2))
y_max = max(house_ori$sold_price)
plot(house_ori$MLS,house_ori$sold_price,pch=19,xlim=c(0,2.5e7),ylim=c(0,y_max),xlab='MLS',ylab='Price',cex.lab=1.2)
plot(house_new$MLS,house_new$sold_price,pch=19,xlim=c(0,2.5e7),ylim=c(0,y_max),xlab='MLS',ylab='Price',cex.lab=1.2)
length(house_ori$MLS[house_ori$MLS>3.5e6&house_ori$MLS<4.5e6]) # points removed in MLS range (3.5e6, 4.5e6)
# MLS & zipcode
par(mfrow=c(1,2))
x_max = max(house_ori$MLS)
x_min = 0
y_max = max(house_ori$zipcode)
y_min = min(house_ori$zipcode)
plot(house_ori$MLS,house_ori$zipcode,pch=19,col='red',xlim=c(x_min,x_max),ylim=c(y_min,y_max),xlab='MLS',ylab='Zipcode',cex.lab=1.2)
points(house_ori$MLS[house_ori$MLS>4e+6 & house_ori$MLS<5e+6],house_ori$zipcode[house_ori$MLS>4e+6 & house_ori$MLS<5e+6],pch=19,col='green')
points(house_ori$MLS[house_ori$MLS>2e+7],house_ori$zipcode[house_ori$MLS>2e+7],pch=19,col='blue')


plot(house_new$MLS,house_new$zipcode,pch=19,col='red',xlim=c(x_min,x_max),ylim=c(y_min,y_max),xlab='MLS',ylab='Zipcode',cex.lab=1.2)
points(house_new$MLS[house_new$MLS>4e+6 & house_new$MLS<5e+6],house_new$zipcode[house_new$MLS>4e+6 & house_new$MLS<5e+6],pch=19,col='green')
points(house_new$MLS[house_new$MLS>2e+7],house_new$zipcode[house_new$MLS>2e+7],pch=19,col='blue')

length(house_new$MLS[house_new$MLS>2e+7])
length(house_new$MLS[house_new$MLS<4e+6])
length(house_new$MLS[house_new$MLS>4e+6 & house_new$MLS<5e+6])

length(house_ori$MLS[house_ori$MLS>2e+7])
length(house_ori$MLS[house_ori$MLS<4e+6])
length(house_ori$MLS[house_ori$MLS>4e+6 & house_ori$MLS<5e+6])
# Longitude & Latitude
par(mfrow=c(1,2))
x_min = min(house_ori$longitude)
x_max = max(house_ori$longitude)
y_min = min(house_ori$latitude)
y_max = max(house_ori$latitude)
plot(house_ori$longitude,house_ori$latitude,col='gray',
     pch=19,xlim=c(x_min,x_max),ylim=c(y_min,y_max))
plot(house_new$longitude,house_new$latitude,col='gray',
     pch=19,xlim=c(x_min,x_max),ylim=c(y_min,y_max))
# sqrt_ft
par(mfrow=c(1,2))
plot(house_ori$sqrt_ft,house_ori$sold_price,pch=19)
plot(house_new$sqrt_ft,house_new$sold_price,pch=19)
# bedrooms vs bathrooms
par(mfrow=c(1,2))
plot(house_ori$bedrooms,house_ori$bathrooms,pch=19,xlab='bedrooms',ylab='bathrooms',cex.lab=1.2,col='gray')
plot(house_new$bedrooms,house_new$bathrooms,pch=19,xlab='bedrooms',ylab='bathrooms',cex.lab=1.2,col='gray')













house = read.csv('house.csv')


par(mfrow=c(1,1))
plot(house$longitude,house$latitude,pch=19,col='red')
points(house$longitude[house$sold_price>2e6],house$latitude[house$sold_price>2e6],pch=19,col='blue')
points(house$longitude[house$sold_price<4e5],house$latitude[house$sold_price<4e5],pch=19,col='gray')







