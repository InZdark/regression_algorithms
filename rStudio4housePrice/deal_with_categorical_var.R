rm(list=ls())
house = read.csv('house_cleaned_with_MLS_HOA_LongiLati.csv')
# kitchen features
kitchen_list = list()
for(j in 1:length(house$kitchen_features)){
  a = as.character(house$kitchen_features[j])
  b = unlist(strsplit(a,','))
  for(i in 1:length(b)){
    c = unlist(strsplit(b[i],' '))
    c = paste0(c,collapse='')
    d = unlist(strsplit(c,''))
    d = d[1:4]
    c = paste0(d,collapse='')
    c = tolower(c)
    if(c %in% kitchen_list){
    }else{
      kitchen_list = append(kitchen_list,c)
    }
  }
}
# floor covering
floor = list()
for(j in 1:length(house$floor_covering)){
  a = as.character(house$floor_covering[j])
  b = unlist(strsplit(a,','))
  for(i in 1:length(b)){
    c = unlist(strsplit(b[i],' '))
    c = paste0(c,collapse='')
    d = unlist(strsplit(c,''))
    d = d[1:4]
    c = paste0(d,collapse='')
    c = tolower(c)
    if(c %in% floor){
    }else{
      floor = append(floor,c)
    }
  }
}



#######################
temp = 'indo'
#######################
x = array(numeric(),c(4916,1))
for(j in 1:length(house$kitchen_features)){
  a = as.character(house$kitchen_features[j])
  b = unlist(strsplit(a,','))
  for(i in 1:length(b)){
    c = unlist(strsplit(b[i],' '))
    c = paste0(c,collapse='')
    d = unlist(strsplit(c,''))
    d = d[1:4]
    c = paste0(d,collapse='')
    c = tolower(c)
    if(c == temp){
      x[j] = 1}}
  if(is.na(x[j])){x[j]=0}
}
print(sum(x))
#######################
house$lami = x










#######################
temp = 'indo'
#######################
x = array(numeric(),c(4916,1))
for(j in 1:length(house$floor_covering)){
  a = as.character(house$floor_covering[j])
  b = unlist(strsplit(a,','))
  for(i in 1:length(b)){
    c = unlist(strsplit(b[i],' '))
    c = paste0(c,collapse='')
    d = unlist(strsplit(c,''))
    d = d[1:4]
    c = paste0(d,collapse='')
    c = tolower(c)
    if(c == temp){
      x[j] = 1}}
  if(is.na(x[j])){x[j]=0}
}
print(sum(x))
#######################
house$lami = x











