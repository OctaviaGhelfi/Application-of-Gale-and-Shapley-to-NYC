library(dplyr)
library(tidyr)
library(tigris) #This is to retrieve census tract data
library(acs) #This downloads data from the US Census although I think I downloaded everything manually
library(stringr)
library(maptools) 
library(ggplot2) #This is to map coordinate points or regions in the form of (lat,long)
library(geosphere) #This is to calculate distance between 2 coordinate points in the form (lat,long)
library(matrixcalc) #Contains the Hadamard product
library(foreach) #Parallel
library(doParallel)
#(1) Data 
#(1.A) Retrieving Census tracts data 

lookup_code("New York", "New York")
lookup_code("New York", "Kings")
lookup_code("New York", "Queens")
lookup_code("New York", "Bronx")
lookup_code("New York", "Richmond?")

nyc_tracts <- tracts(state = '36', county = c('061','047','081','005','085'))
# nyc_tracts@data$GEOID<-as.character(nyc_tracts@data$GEOID)

#(1.A) Cleaning up the dataset and manipulating data to obtain geo coordinates of schools 

colnames(X2017_2018_School_Locations)<-X2017_2018_School_Locations[1,]
X2017_2018_School_Locations<-X2017_2018_School_Locations[-1,]

#(1.B) Merging the school database with the building capacity database 

Schools_with_Capacities <- left_join(X2017_2018_School_Locations,School_Capacities, by=c("CODE"))

x<-cbind(Schools_with_Capacities$CODE,Schools_with_Capacities$`Bldg ID`,Schools_with_Capacities$LOCATION_NAME, Schools_with_Capacities$`Location 1`,Schools_with_Capacities$LOCATION_CATEGORY_DESCRIPTION,Schools_with_Capacities$Enroll,Schools_with_Capacities$`Target Capacity`,Schools_with_Capacities$`Target Utilization`,Schools_with_Capacities$`Data As Of`)
y<-data.frame(x)
y<- y %>% separate(X4, c("Address","ZipCode","lat","long","Other"), sep = "([\\(\\,\\)])")
colnames(y)[1]<-"CODE"
colnames(y)[2]<-"Bldg_Id"
colnames(y)[3]<-"School_Names"
colnames(y)[9]<-"Category"
colnames(y)[10]<-"Enroll"
colnames(y)[11]<-"Target Capacity"
colnames(y)[12]<-"Target Utilization"
colnames(y)[13]<-"Date"
# (1.A) Correcting lines 533-540 that have an extra part to the address 

y[c(533:540),5]=y[(533:540),6]
y[(533:540),6]=y[(533:540),7]
y[(533:540),7]=y[(533:540),8]
y$Other<-NULL

Schools <- data.frame(
  CODE = y$CODE,
  Bldg_Id = y$Bldg_Id,
  long = as.numeric(y$long),
  lat = as.numeric(y$lat),
  category = y$Category,
  names = y$School_Names,
  enroll = as.numeric(as.character(y$Enroll)),
  capacity = as.numeric(as.character(y$`Target Capacity`)),
  utilization = as.numeric(as.character(y$`Target Utilization`)),
  date = y$Date,
  stringsAsFactors = FALSE
)  

Schools<-Schools[-8896,] #coordinate (0,0)
which(is.na(Schools$Latitude))
#Extract data for year 2017

Schools_2017<- Schools[grep("2017", Schools$date),]
rownames(Schools_2017) <- 1:nrow(Schools_2017)

PS_2017 <- Schools_2017[which(duplicated(Schools_2017$CODE)==FALSE),]
rownames(PS_2017) <- 1:nrow(PS_2017)

X<-Schools_2017[which(duplicated(Schools_2017$CODE)==TRUE),]
rownames(X)<-1:nrow(X)

for (i in 1:nrow(PS_2017)){
  PS_2017$enroll[i] <- PS_2017$enroll[i] + sum(X$enroll[which(X$CODE == PS_2017$CODE[i])])
  PS_2017$capacity[i] <- PS_2017$capacity[i] + sum(X$capacity[which(X$CODE == PS_2017$CODE[i])])
}

#Utilization
PS_2017[,9]<-PS_2017[,7]/PS_2017[,8] 

#Split the schools in Kindergarten, Elementary, Middle and High school 
mylist <- split(PS_2017, PS_2017$category)
Kindergarten <- rbind.data.frame(mylist$`Early Childhood`,mylist$`K-8`,mylist$`K-12 all grades`)
Elementary <- rbind.data.frame(mylist$Elementary,mylist$`K-8`,mylist$`K-12 all grades`)
Middle <- rbind.data.frame(mylist$`Junior High-Intermediate-Middle`,mylist$`K-8`,mylist$`K-12 all grades`,mylist$`Secondary School`)
High <- rbind.data.frame(mylist$`High school`,mylist$`K-12 all grades`,mylist$`Secondary School`)

rownames(Kindergarten)<-1:nrow(Kindergarten)
rownames(Elementary)<-1:nrow(Elementary)
rownames(Middle)<-1:nrow(Middle)
rownames(High)<-1:nrow(High)
311? 
sea <- c(30,83,91,92,130,138,230,250,289,307,308,312,314,316,320,334,341)
ggplot() + 
  geom_polygon(data = nyc_tracts[,], aes(x=long, y = lat, group = group), color="grey",fill='white', cex = 0.1)+
  geom_polygon(data = nyc_tracts[300:350,], aes(x=long, y = lat, group = group, fill = "Tract"), color="grey50", fill = 'grey', cex = 0.1)+
  geom_polygon(data = nyc_tracts[sea,], aes(x=long, y = lat, group = group, fill = "Tract"), color="grey50", fill = 'lightblue', cex = 0.1)

#Plot all schools together, color-coded
ggplot() + geom_polygon(data = nyc_tracts, aes(x=long, y = lat, group = group), color="grey",fill='white', cex = 0.1) + 
  coord_fixed(1.3) + geom_point(data = Kindergarten, aes(x = long, y = lat), color = "yellow", size = 0.5, pch=20) +  
  geom_point(data = Elementary, aes(x = long, y = lat), color = "green", size = .5, pch=20) +
  geom_point(data = Middle, aes(x = long, y = lat), color = "blue", size = .5, pch=20) + 
  geom_point(data = High, aes(x = long, y = lat), color = "mediumorchid1", size = .5, pch=20)

#Plot Pre-K and K 
ggplot() + geom_polygon(data = nyc_tracts, aes(x=long, y = lat, group = group), color="white", cex = 0.1) + 
  coord_fixed(1.3) + geom_point(data = Kindergarten, aes(x = long, y = lat), color = "yellow", size = 0.5, pch=20) + labs(title = "Pre-K and Kindergarten")

#Plot Elementary 
ggplot() + geom_polygon(data = nyc_tracts, aes(x=long, y = lat, group = group), color="white", cex = 0.1) + 
  coord_fixed(1.3) + geom_point(data = Elementary, aes(x = long, y = lat), color = "red", size = .5, pch=20) + labs(title = "Elementary")

#Plot Middle School
ggplot() + geom_polygon(data = nyc_tracts, aes(x=long, y = lat, group = group), color="white", cex = 0.1) + 
  coord_fixed(1.3) + geom_point(data = Middle, aes(x = long, y = lat), color = "cadetblue1", size = .5, pch=20) + labs(title = "Junior High-Middle Schools")

#Plot High School 
ggplot() + geom_polygon(data = nyc_tracts, aes(x=long, y = lat, group = group), color="white", cex = 0.1) + 
  coord_fixed(1.3) + geom_point(data = High, aes(x = long, y = lat), color = "mediumorchid1", size = .5, pch=20) + labs(title = "High Schools")
#Census Data for Population numbers 

Census <- read_excel("/Users/Octavia/Desktop/Galichon/School Project/Census.xlsx")
colnames(Census)<-Census[1,]
colnames(Census)[3]<- "Geography" 
Census <- Census[c(-1,-2),]

#Extract only the variables that are needed

data <- dplyr::select(Census, GEO.id2, Geography, HC01_VC09,HC01_VC10,HC01_VC11,HC01_VC12)

#Extract only the observations that are needed

New_York_County<-data[grep("New York County", data$Geography),]
Queens_County<-data[grep("Queens County", data$Geography),]
Kings_County<-data[grep("Kings County", data$Geography),]
Bronx_County<-data[grep("Bronx County", data$Geography),]
Richmond_County <- data[grep("Richmond County", data$Geography),]
data<-rbind(New_York_County,Queens_County,Kings_County,Bronx_County,Richmond_County)
colnames(data)<-c("id","Geography","Under_5","from_5_to_9","from_10_to_14","from_15_to_19")
data<-data[-1,]
data <- mutate(data, id=as.character(id),
               Geography=as.character(Geography),
               Under_5 = as.numeric(Under_5),
               from_5_to_9 = as.numeric(from_5_to_9),
               from_10_to_14 = as.numeric(from_10_to_14),
               from_15_to_19 = as.numeric(from_15_to_19))
Under_19<-as.numeric(rowSums(data[,3:6]))
data<-cbind(data,Under_19)

#Combine the geographical data with the tabular data using fortify

ggtract<-fortify(nyc_tracts, region = "GEOID")

# join tabular data

ggtract<-left_join(ggtract, data, by=c("id")) 

#Map the different densities 
#Under 5

ggplot() +
  geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=Under_5), color="grey50", cex=0.1) +
  scale_fill_gradientn(colours = c("gold", "yellow", "white"),
                       values = c(1,0.5, .3, .2, .1, 0))+ 
  coord_fixed(1.3)+ labs(title = "Pre-K and Kindergarten") + geom_point(data = Kindergarten, aes(x = long, y = lat), color = "grey40", size = 0.5, pch=3) + labs(title = "Kindergarten")

#From 5 to 9

ggplot() +
  geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=from_5_to_9), color="grey50", cex=0.1) +
  scale_fill_gradientn(colours = c("red3","red","white"),
                       values = c(1,0.5, .3, .2, .1, 0)) + geom_point(data = rbind.data.frame(mylist$Elementary,mylist$`K-8`,mylist$`K-12 all grades`), aes(x = long, y = lat), color = "grey40", size = 0.5, pch=3) + labs(title = "Elementary") + coord_fixed(1.3)
#From 10 to 14

ggplot() +
  geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=from_10_to_14), color="grey50", cex=0.1) +
  scale_fill_gradientn(colours = c("royalblue4","royalblue1","white"),
                       values = c(1,0.5, .3, .2, .1, 0)) + geom_point(data = rbind.data.frame(mylist$`Junior High-Intermediate-Middle`,mylist$`K-8`,mylist$`K-12 all grades`,mylist$`Secondary School`), aes(x = long, y = lat), color = "grey40", size = .5, pch=3) + labs(title = "Junior High-Middle Schools") + coord_fixed(1.3)

#From 15 to 19 

ggplot() +
  geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=from_15_to_19), color="grey50", cex=0.1) +
  scale_fill_gradientn(colours = c("maroon3","maroon1","white"),
                       values = c(1,0.5, .3, .2, .1, 0)) +  geom_point(data = rbind.data.frame(mylist$`High school`,mylist$`K-12 all grades`,mylist$`Secondary School`), aes(x = long, y = lat), color = "grey40", size = .5, pch=3) + labs(title = "High Schools") + coord_fixed(1.3)
#Under 19 

ggplot() +
  geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=Under_19), color="grey50", cex=0.1) +
  scale_fill_gradientn(colours = c("red4","red","white"),
                       values = c(1,0.5, .3, .2, .1, 0)) + labs(title = "Population Density") + coord_fixed(1.3)
#Centroids of each Tract

centroids <- data.frame(centroid(nyc_tracts),nyc_tracts$GEOID)
colnames(centroids)<-c("long","lat","id")
centroids$id <- as.character(centroids$id)
centroids <- left_join(centroids, data, by=c("id"))
centroids <- na.omit(centroids)

#Calculate the distance from the centroid of each tract to each Kindergarten

Kindergarten <- Kindergarten[-223,]
d<-matrix(0,nrow=nrow(centroids),ncol=nrow(Kindergarten))

for (i in 1:nrow(centroids)) {
  for (j in 1:nrow(Kindergarten)) {
    d[i,j]=distm(c(centroids$long[i],centroids$lat[i]),c(Kindergarten$long[j],Kindergarten$lat[j]), fun=distHaversine)
  }
}
d <- d[,-223]
#Ranking of Schools for each Centroid 

x <- matrix(0,nrow(centroids),ncol = ncol(d))

for (i in 1:nrow(centroids)){
  x[i,] <- sort(d[i,])
}
colnames(d)<-Kindergarten$names 

R <- matrix(0,nrow(centroids),nrow(Kindergarten))

for (i in 1:nrow(centroids)) {
  k<-1
  j<-1
  while (j < nrow(Kindergarten)+1) {
    l <- length(which(d[i,] == x[i,j]))
    for (k in 1:l) {
      R[i,j+k-1] <- which(d[i,] == x[i,j])[[k]]
    }
    k<-1
    j<-j+l
  } 
}

#Ranking of Centroids for every School

y <- matrix(0,nrow(centroids),nrow(Kindergarten))
for (j in 1:nrow(Kindergarten)){
  y[,j] <- sort(d[,j])
}

S <- matrix(0,nrow(centroids),nrow(Kindergarten))

for (j in 1:nrow(Kindergarten)) {
  k<-1
  i<-1
  while (i < nrow(centroids)+1) {
    l <- length(which(d[,j] == y[i,j]))
    for (k in 1:l) {
      S[i+k-1,j] <- which(d[,j] == y[i,j])[[k]]
    }
    k<-1
    i<-i+l
  } 
}

for (j in 1:nrow(Kindergarten)){
  for (i in 1:nrow(centroids)) {
    if (length(which(d[,j] == y[i,j])) == 1){
      S[i,j] <- which(d[,j] == y[i,j])
    }
    else{
      for (t in 1:length(which(d[,j] == y[i,j]))) {
        S[i,j+t-1] <- which(d[,j] == y[i,j])[t]
      }
    }
  }
}

# (2) Allocation Algorithm

# (2A) Proposal

mu_A <- matrix(1,nrow(centroids),nrow(Kindergarten))
mu_Pi <- matrix(0, nrow = nrow(centroids), ncol = nrow(Kindergarten))
mu_P <- matrix(0,nrow(centroids),nrow(Kindergarten))
mu_Ei <- matrix(0, nrow = nrow(centroids), ncol = nrow(Kindergarten))
mu_E <- matrix(0, nrow = nrow(centroids), ncol = nrow(Kindergarten))
E <- rep(0,nrow(Kindergarten)) #Vector indicating how many students have been tentatively accepted by each school
t <- rep(1,nrow(centroids)) #Rank of the school centroid i is applying to
r <- rep(1,nrow(Kindergarten)) #Number of different centroids accepted by kindergarten j 
psi <- matrix(0,nrow(centroids)+1,nrow(Kindergarten)) #Number of Kindergarten assigned to centroid i
CONT <- TRUE
ITER <- 0 
SumE<-rep(0,nrow(centroids)) #total fraction of students engaged per centroid
C <- Kindergarten$capacity
for (i in which(is.na(C))) {
  C[i]<-Kindergarten$enroll
}

&& (ITER<255)

while ((CONT == TRUE)) {
  for (i in which(SumE<1)){
    if (t[i]<=nrow(Kindergarten)){
      if ((all(mu_E[i,]==0)) && (mu_A[i,R[i,t[i]]] == 1)) {
      psi[i,1] <- R[i,t[i]]
      mu_P[i,psi[i,1]] <- 1 
      }
      if ((all(mu_E[i,]==0)==FALSE) && (mu_A[i,R[i,t[i]]] == 1) || (mu_A[i,R[i,t[i]]] > 0) && (mu_A[i,R[i,t[i]]] < 1 )){
        if (t[i] == nrow(Kindergarten)){
          l<-length(which(psi[i,]!=0))
          psi[i,l+1] <- R[i,t[i]]
          mu_P[i,psi[i,l+1]] <- 1-SumE[i]
        }
        if (t[i] < nrow(Kindergarten)){
          while (((R[i,t[i]] %in% which(mu_A[i,]==1)) == FALSE) && (t[i] <= nrow(Kindergarten))){
            t[i] <- t[i]+1
          }
          if (t[i] > nrow(Kindergarten)) {
            l<-length(which(psi[i,]!=0))
            psi[i,l+1] <- -Inf
          }
          if (t[i] < nrow(Kindergarten)){
            l<-length(which(psi[i,]!=0))
            psi[i,l+1] <- R[i,t[i]]
            mu_P[i,psi[i,l+1]] <- 1-SumE[i]
          }
          }
      }
    }
    if (t[i] > nrow(Kindergarten)) {
      l<-length(which(psi[i,]!=0))
      psi[i,l+1] <- -Inf
    }
      if (mu_A[i,R[i,t[i]]] == 0){
        while (((R[i,t[i]] %in% which(mu_A[i,]==1)) == FALSE) & ((t[i]<nrow(Kindergarten)))){
          t[i] <- t[i]+1
          if (t[i] <= nrow(Kindergarten)){
            psi[i,1] <- R[i,t[i]]
            mu_P[i,psi[i,1]] <- 1-SumE[i]
          }
          else{
            l<-length(which(psi[i,])!=0)
            psi[i,l+1] <- -Inf
          }
        }}
    }
  #Makes schools tentatively accept closest students up to capacity 
  D <- t(centroids$Under_5)  %*% mu_P 
  ED <- D-C 
  E <- rep(0,nrow(Kindergarten))
  r <- rep(1,nrow(Kindergarten))
  cont <- rep(TRUE,nrow(Kindergarten))
  for (i in which(ED<=0)) {
    mu_E[,i] <- mu_P[,i]
    E[i]<-D[i]
  }
  for (i in which(ED>0)){
    while ( (cont[i] == TRUE) && (E[i] < C[i]) ) {
      s <- which(S[,i]==sort(S[which(mu_P[,i]>0),i])[r[i]])
      if (E[i]+(centroids$Under_5[s]*mu_P[s,i]) < C[i]) {
        mu_E[s,i] <- mu_P[s,i]
        E[i] <- E[i] + (centroids$Under_5[s]*mu_P[s,i])
        r[i] <- r[i]+1
      }
      else {
        mu_E[s,i] <- (C[i]-E[i])/centroids$Under_5[s]
        E[i] <- C[i]
        cont[i] <- FALSE
      }
    }
  }
  if ((all(mu_E == mu_Ei) == TRUE) && (all(mu_P == mu_Pi) == TRUE)) {
    CONT<-FALSE
  }
  else{
    mu_Pi <- mu_P
    mu_Ei <- mu_E
    for (j in 1:nrow(Kindergarten)){
      for (i in 1:nrow(centroids)){
        if (mu_P[i,j] != mu_E[i,j]){
          mu_A[i,j] <- 1-(mu_P[i,j]-mu_E[i,j])
          mu_P[i,j] <- mu_E[i,j]
        }
      }
    }
    for (i in 1:nrow(centroids)) {
      SumE[i]<-sum(mu_E[i,])
    }
    ITER <- ITER + 1 
    print(ITER)
}
}
  
which(SumE>1)


# Map

Zone <- 12
K<-psi[Zone,which(psi[Zone,]>0)]

ggplot() + 
  geom_polygon(data = nyc_tracts, aes(x=long, y = lat, group = group), color="grey50", cex = 0.1) + 
  geom_polygon(data = nyc_tracts[Zone,], aes(x=long, y = lat, group = group, fill = "Tract 12"), color="grey50", cex = 0.1) +
  coord_fixed(1.3) + 
  geom_point(data = Kindergarten, aes(x = long, y = lat), color = "white", size = 0.5, pch=20) + 
  geom_point(data = Kindergarten[K,], aes(x = long, y = lat), color = "red", size = 0.5, pch=20) +
  labs(title = "Pre-K and Kindergarten")


geom_point(data = centroids[Zone,], aes(x = long, y = lat), color = "blue", size = 0.5, pch=20)  


# Elementary school

#Calculate the distance from the centroid of each tract to each Elementary School

d<-matrix(0,nrow=nrow(centroids),ncol=nrow(Elementary))

for (i in 1:nrow(centroids)) {
  for (j in 1:nrow(Elementary)) {
    d[i,j]=distm(c(centroids$long[i],centroids$lat[i]),c(Elementary$long[j],Elementary$lat[j]), fun=distHaversine)
  }
}


#Ranking of Schools for each Centroid 

x <- matrix(0,nrow(centroids),nrow(Elementary))
for (i in 1:nrow(centroids)){
  x[i,] <- sort(d[i,])
}

colnames(d)<-Elementary$names 
R <- matrix(0,nrow(centroids),nrow(Elementary))

for (i in 1:nrow(centroids)) {
  k<-1
  j<-1
  while (j < nrow(Elementary)+1) {
    l <- length(which(d[i,] == x[i,j]))
    for (k in 1:l) {
      R[i,j+k-1] <- which(d[i,] == x[i,j])[[k]]
    }
    k<-1
    j<-j+l
  } 
}

#Ranking of Centroids for every School

y <- matrix(0,nrow(centroids),nrow(Elementary))
for (j in 1:nrow(Elementary)){
  y[,j] <- sort(d[,j])
}

S <- matrix(0,nrow(centroids),nrow(Elementary))

for (j in 1:nrow(Elementary)) {
  k<-1
  i<-1
  while (i < nrow(centroids)+1) {
    l <- length(which(d[,j] == y[i,j]))
    for (k in 1:l) {
      S[i+k-1,j] <- which(d[,j] == y[i,j])[[k]]
    }
    k<-1
    i<-i+l
  } 
}

for (j in 1:nrow(Elementary)){
  for (i in 1:nrow(centroids)) {
    if (length(which(d[,j] == y[i,j])) == 1){
      S[i,j] <- which(d[,j] == y[i,j])
    }
    else{
      for (t in 1:length(which(d[,j] == y[i,j]))) {
        S[i,j+t-1] <- which(d[,j] == y[i,j])[t]
      }
    }
  }
}

# (2) Allocation Algorithm

# (2A) Proposal

mu_A <- matrix(1,nrow(centroids),nrow(Elementary))
mu_Pi <- matrix(0, nrow = nrow(centroids), ncol = nrow(Elementary))
mu_P <- matrix(0,nrow(centroids),nrow(Elementary))
mu_Ei <- matrix(0, nrow = nrow(centroids), ncol = nrow(Elementary))
mu_E <- matrix(0, nrow = nrow(centroids), ncol = nrow(Elementary))
E <- rep(0,nrow(Elementary)) #Vector indicating how many students have been tentatively accepted by each school
t <- rep(1,nrow(centroids)) #Rank of the school centroid i is applying to
r <- rep(1,nrow(Elementary)) #Number of different centroids accepted by kindergarten j 
psi <- matrix(0,nrow(centroids)+1,nrow(Elementary)) #Number of Kindergarten assigned to centroid i
CONT <- TRUE
ITER <- 0 
SumE<-rep(0,nrow(centroids)) #total fraction of students engaged per centroid
C <-Elementary$capacity
for (i in which(is.na(C))) {
  C[i]<-Elementary$enroll
}

&& (ITER<255)

while ((CONT == TRUE)) {
  for (i in which(SumE<1)){
    if (t[i]<=nrow(Elementary)){
      if ((all(mu_E[i,]==0)) && (mu_A[i,R[i,t[i]]] == 1)) {
        psi[i,1] <- R[i,t[i]]
        mu_P[i,psi[i,1]] <- 1 
      }
      if ((all(mu_E[i,]==0)==FALSE) && (mu_A[i,R[i,t[i]]] == 1) || (mu_A[i,R[i,t[i]]] > 0) && (mu_A[i,R[i,t[i]]] < 1 )){
        if (t[i] == nrow(Elementary)){
          l<-length(which(psi[i,]!=0))
          psi[i,l+1] <- R[i,t[i]]
          mu_P[i,psi[i,l+1]] <- 1-SumE[i]
        }
        if (t[i] < nrow(Elementary)){
          while (((R[i,t[i]] %in% which(mu_A[i,]==1)) == FALSE) && (t[i] <= nrow(Elementary))){
            t[i] <- t[i]+1
          }
          if (t[i] > nrow(Elementary)) {
            l<-length(which(psi[i,]!=0))
            psi[i,l+1] <- -Inf
          }
          if (t[i] < nrow(Elementary)){
            l<-length(which(psi[i,]!=0))
            psi[i,l+1] <- R[i,t[i]]
            mu_P[i,psi[i,l+1]] <- 1-SumE[i]
          }
        }
      }
    }
    if (t[i] > nrow(Elementary)) {
      l<-length(which(psi[i,]!=0))
      psi[i,l+1] <- -Inf
    }
    if (mu_A[i,R[i,t[i]]] == 0){
      while (((R[i,t[i]] %in% which(mu_A[i,]==1)) == FALSE) & ((t[i]<nrow(Elementary)))){
        t[i] <- t[i]+1
        if (t[i] <= nrow(Elementary)){
          psi[i,1] <- R[i,t[i]]
          mu_P[i,psi[i,1]] <- 1-SumE[i]
        }
        else{
          l<-length(which(psi[i,])!=0)
          psi[i,l+1] <- -Inf
        }
      }}
  }
  #Makes schools tentatively accept closest students up to capacity 
  D <- t(centroids$Under_5)  %*% mu_P 
  ED <- D-C 
  E <- rep(0,nrow(Elementary))
  r <- rep(1,nrow(Elementary))
  cont <- rep(TRUE,nrow(Elementary))
  for (i in which(ED<=0)) {
    mu_E[,i] <- mu_P[,i]
    E[i]<-D[i]
  }
  for (i in which(ED>0)){
    while ( (cont[i] == TRUE) && (E[i] < C[i]) ) {
      s <- which(S[,i]==sort(S[which(mu_P[,i]>0),i])[r[i]])
      if (E[i]+(centroids$Under_5[s]*mu_P[s,i]) < C[i]) {
        mu_E[s,i] <- mu_P[s,i]
        E[i] <- E[i] + (centroids$Under_5[s]*mu_P[s,i])
        r[i] <- r[i]+1
      }
      else {
        mu_E[s,i] <- (C[i]-E[i])/centroids$Under_5[s]
        E[i] <- C[i]
        cont[i] <- FALSE
      }
    }
  }
  if ((all(mu_E == mu_Ei) == TRUE) && (all(mu_P == mu_Pi) == TRUE)) {
    CONT<-FALSE
  }
  else{
    mu_Pi <- mu_P
    mu_Ei <- mu_E
    for (j in 1:nrow(Kindergarten)){
      for (i in 1:nrow(centroids)){
        if (mu_P[i,j] != mu_E[i,j]){
          mu_A[i,j] <- 1-(mu_P[i,j]-mu_E[i,j])
          mu_P[i,j] <- mu_E[i,j]
        }
      }
    }
    for (i in 1:nrow(centroids)) {
      SumE[i]<-sum(mu_E[i,])
    }
    ITER <- ITER + 1 
    print(ITER)
  }
}

which(SumE>1)

  