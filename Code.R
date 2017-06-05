#####EDA PROJECT#####
#####Deleting Environment Variables#####
rm(list = ls())

#####Getting Working Directory#####
getwd()

#####Setting Working Directory#####
setwd("C:/Users/Tej/Documents")

#####Reading Data#####
data = read.csv(file= "Movies.csv", header = T)
attach(data)

#####Names of the columns in Data#####
names(data)

#####Considering the Facebook Likes column #####
#####length of Facebook Likes data#####
data_length = length(Facebook_Likes)
data_length

#####Excluding Stemplot as observations are more than 50#####
#####Boxplot#####
boxplot(Facebook_Likes)
boxplot(Facebook_Likes, main = "Facebook Like Boxplot")
boxplot(Facebook_Likes, main = "Facebook Like Boxplot", ylab = "No.of Likes")

#####Finding Summary values#####
summary(Facebook_Likes)

#####Finding letter values#####
#####Finding Fivenum summary values#####
f = fivenum(Facebook_Likes)
fourth_spread = f[4]-f[2]
fourth_spread
STEP<- 1.5*fourth_spread
STEP
Lower_inner_fence<-f[2]-STEP
Lower_inner_fence
Upper_inner_fence<-f[4]+STEP
Upper_inner_fence
Lower_outer_fence<-f[2]-2*STEP
Lower_outer_fence
Upper_outer_fence<-f[4]+2*STEP
Upper_outer_fence

#####Identifying the mild outliers#####
outlier_mild<-Facebook_Likes[Facebook_Likes>Upper_inner_fence | Facebook_Likes<Lower_inner_fence]
outlier_mild

#####Identifying extreme outliers#####
outlier_extreme<-Facebook_Likes[Facebook_Likes>Upper_outer_fence | Facebook_Likes<Lower_outer_fence]
outlier_extreme

#####Finding the fivenum summary and letter values by Genre groups#####
A<-aggregate(Facebook_Likes,list(group=Genres), fivenum)
A$group
gf = A$x
gSTEP<- 1.5*(gf[,4]-gf[,2])
gLower_inner_fence<-gf[,2]-gSTEP
gUpper_inner_fence<-gf[,4]+gSTEP
gLower_outer_fence<-gf[,2]-2*gSTEP
gUpper_outer_fence<-gf[,4]+2*gSTEP

#####Grouping data based on the Genre Group#####
library(base64enc)
Facebook_Likes.Action = subset(Facebook_Likes, Genres=="Action")
Facebook_Likes.Action
Facebook_Likes.Adventure = subset(Facebook_Likes, Genres=="Adventure")
Facebook_Likes.Adventure
Facebook_Likes.Animation = subset(Facebook_Likes, Genres=="Animation")
Facebook_Likes.Animation
Facebook_Likes.Biography = subset(Facebook_Likes, Genres=="Biography")
Facebook_Likes.Biography
Facebook_Likes.Comedy = subset(Facebook_Likes, Genres=="Comedy")
Facebook_Likes.Comedy
Facebook_Likes.Crime = subset(Facebook_Likes, Genres=="Crime")
Facebook_Likes.Crime
Facebook_Likes.Documentary = subset(Facebook_Likes, Genres=="Documentary")
Facebook_Likes.Documentary
Facebook_Likes.Drama = subset(Facebook_Likes, Genres=="Drama")
Facebook_Likes.Drama
Facebook_Likes.Family = subset(Facebook_Likes, Genres=="Family")
Facebook_Likes.Family
Facebook_Likes.Horror = subset(Facebook_Likes, Genres=="Horror")
Facebook_Likes.Horror
Facebook_Likes.Mystery = subset(Facebook_Likes, Genres=="Mystery")
Facebook_Likes.Mystery
Facebook_Likes.Thriller = subset(Facebook_Likes, Genres=="Thriller")
Facebook_Likes.Thriller

#####Finding outliers by Genre group#####
Facebook_Likes.Action[(Facebook_Likes.Action<gLower_inner_fence[1]|Facebook_Likes.Action>gUpper_inner_fence[1])]
Facebook_Likes.Adventure[(Facebook_Likes.Adventure<gLower_inner_fence[2]|Facebook_Likes.Adventure>gUpper_inner_fence[2])]
Facebook_Likes.Animation[(Facebook_Likes.Animation<gLower_inner_fence[3]|Facebook_Likes.Animation>gUpper_inner_fence[3])]
Facebook_Likes.Biography[(Facebook_Likes.Biography<gLower_inner_fence[4]|Facebook_Likes.Biography>gUpper_inner_fence[4])]
Facebook_Likes.Comedy[(Facebook_Likes.Comedy<gLower_inner_fence[5]|Facebook_Likes.Comedy>gUpper_inner_fence[5])]
Facebook_Likes.Crime[(Facebook_Likes.Crime<gLower_inner_fence[6]|Facebook_Likes.Crime>gUpper_inner_fence[6])]
Facebook_Likes.Documentary[(Facebook_Likes.Documentary<gLower_inner_fence[7]|Facebook_Likes.Documentary>gUpper_inner_fence[7])]
Facebook_Likes.Drama[(Facebook_Likes.Drama<gLower_inner_fence[8]|Facebook_Likes.Drama>gUpper_inner_fence[8])]
Facebook_Likes.Family[(Facebook_Likes.Family<gLower_inner_fence[9]|Facebook_Likes.Family>gUpper_inner_fence[9])]
Facebook_Likes.Horror[(Facebook_Likes.Horror<gLower_inner_fence[10]|Facebook_Likes.Horror>gUpper_inner_fence[10])]
Facebook_Likes.Mystery[(Facebook_Likes.Mystery<gLower_inner_fence[11]|Facebook_Likes.Mystery>gUpper_inner_fence[11])]
Facebook_Likes.Thriller[(Facebook_Likes.Thriller<gLower_inner_fence[12]|Facebook_Likes.Thriller>gUpper_inner_fence[12])]
boxplot(Facebook_Likes~Genres, xlab="Genres", ylab="Facebook Likes", main="Boxplot of Facebook Likes vs Genres", las=3, col = "skyblue")