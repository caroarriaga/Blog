install.packages("dplyr")

library(dplyr)
library(rafalib)

#Using iris database
#Set A is the petal length data from setosa species
setosaData<-filter(DataFrame, Species=="setosa") %>% select(Petal.Length) %>% unlist

#Set B is the petal length data from versicolor species
versicolorData<-filter(DataFrame, Species=="versicolor") %>% select(Petal.Length) %>% unlist

#Show two graphs next to each other
mypar(1,2)

#Graph an histogram of each specie
hist(setosaData,
     main = "Histogram of Setosa",
     xlab = "Petal length",
     xlim = c(0,6),
     col = "light blue")
hist(versicolorData,
     main = "Histogram of Versicolor",
     xlab = "Petal length",
     xlim = c(0,5.5),
     col = "light blue")

#Get some samples (<30) and graph again
#set seed to repeat results when taking random samples
set.seed(123)

#Take 15 & 18 random samples from each subset respectively
setosaSetA<-sample(setosaData, 15)
versicolorSetB<-sample(versicolorData,18)

#Histogram of subsets
hist(setosaSetA,
     prob = TRUE,
     main = "Histogram of Setosa, n=15",
     xlab = "Petal length",
     xlim = c(0,6),
     col = "light blue")
hist(versicolorSetB,
     main = "Histogram of Versicolor, n=18",
     xlab = "Petal length",
     xlim = c(0,6),
     col = "light blue")

#Create a subset with random samples of size n=5, calculate the mean and repeat 100 times
set.seed(123)
SMSet_A <-replicate(100, {
  x<-sample(setosaSetA,5,replace = TRUE)
  mean(x)
})

#histogram of 100 sample means of Set A - Setosa
par(mar = c(3, 3, 2, 2))
mypar(1,1)
histSetSM<- hist(SMSet_A,
     prob = TRUE,
     main = "Sample distribution of sample means,
     rep = 100, n=5",
     xlab = "Petal length",
     #xlim = c(0,6),
     col = "light blue")
#probability distribution
lines(density(SMSet_A, adjust = 2), 
      col = "blue", 
      lwd = 2)
#Histogram of Set A - Setosa
histSetSA<- hist(setosaSetA,
                 prob = TRUE,
                 main = "Histogram of Setosa, n=15",
                 xlab = "Petal length",
                 col = "light blue")
lines(density(setosaSetA, adjust = 2), 
      col = "blue", 
      lwd = 2)

#calculate range of histogram
xlim<- range(histSetSM$breaks,histSetSA$breaks)
ylim<-range(0,histSetSM$density, histSetSA$density)

#Plot the first graph: Set A
plot(histSetSA, xlim = xlim, ylim = ylim,
     col = "light blue", xlab = "Petal Length",
     freq = FALSE,
     main = "Distribution of Setosa Set and Sample means")

#Plot the second graph on top
opar<- par(new = FALSE)
plot(histSetSM, xlim = xlim, ylim = ylim,
     xaxt = 'n', yaxt = 'n', #don't add axis
     col = rgb(.4,0,0,1/4), add = TRUE,
     freq = FALSE)
legend("topleft", c("Setosa Set", "Set Sample Means"),
       fill = c("blue","brown"), bty = "n",
                border = NA)
par(opar)

#Create 500 samples and calculate the mean of each
SMSet_B <-replicate(300, {
  x<-sample(versicolorSetB,5,replace = TRUE)
  mean(x)
})

#histogram of 100 sample means of Set A - Setosa
par(mar = c(3, 3, 5, 2))
mypar(1,2)
histSetSMB<- hist(SMSet_B,
                  prob = TRUE,
                  main = "Sample distribution of sample means,
                  rep = 300, n=5",
                  xlab = "Petal length",
                  #xlim = c(0,6),
                  col = "light blue")

#Histogram of Set B - Versicolor
histSetSB<- hist(versicolorSetB,
                 prob = TRUE,
                 main = "Histogram of Versicolor, n=18",
                 xlab = "Petal length",
                 col = "light blue")
lines(density(versicolorSetB, adjust = 2), 
      col = "blue", 
      lwd = 2)

#calculate range of histogram
xlim<- range(histSetSB$breaks,histSetSB$breaks)
ylim<-range(0,histSetSMB$density, histSetSMB$density)

#Plot the first graph: Set B - Versicolor
mypar(1,1)
plot(histSetSB, xlim = xlim, ylim = ylim,
     col = "light blue", xlab = "Petal Length",
     freq = FALSE,
     main = "Distribution of Versicolor Set and Sample means")

#Plot the second graph on top
opar<- par(new = FALSE)
plot(histSetSMB, xlim = xlim, ylim = ylim,
     xaxt = 'n', yaxt = 'n', #don't add axis
     col = rgb(.4,0,0,1/4), add = TRUE,
     freq = FALSE)
legend("topleft", c("Versicolor Set", "Set Sample Means"),
       fill = c("blue","brown"), bty = "n",
       border = NA)
par(opar)

#Calculate means of sets
mean_setosa<-mean(setosaData)
mean_setosa_set<- mean(setosaSetA)
mean_setosa_SM<- mean(SMSet_A)

mean_versicolor<-mean(versicolorData)
mean_versicolor_set<- mean(versicolorSetB)
mean_versicolor_SM<- mean(SMSet_B)

#Calculate Std Dev of sets
SD_Setosa <-sd(setosaData)
SD_Setosa_set <-sd(setosaSetA)
SD_SM_Setosa <- sd(SMSet_A)

SD_Versicolor <- sd(versicolorData)
SD_Versicolor_Set<- sd(versicolorSetB)
SD_SM_Versicolor <- sd(SMSet_B)

#t-test
ttest<-t.test(SMSet_A,SMSet_B)
ttest
