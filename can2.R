library("caTools")
library("ggplot2")
library("corrplot")
cancer <- read.csv(choose.files(), header = TRUE)
cancer
head(cancer)
str(cancer)
dim(cancer)
glimpse(cancer)
class(cancer)
cancer <- cancer[-33]
summary(cancer)

#no. of women affected in benign and malignant stage
cancer %>% count(diagnosis)

#percentage of women affected in benign and malignant stage
cancer %>% count(diagnosis)%>%group_by(diagnosis)%>%
  summarise(perc_dx = round(n / 569*100, 2))
#data visualization
#frequency of cancer diagnosis
diagnosis.table <- table(cancer$diagnosis)
colors <- terrain.colors(2)


#pie chart
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielables <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")
pie(diagnosis.prop.table,lables = pielables, clockwise = TRUE,col = colors,
    border = "gainsboro",radius = 0.8,cex = 0.8, main = "frequency of cancer diagnosis")
legend(1, .4, legend = diagnosis.prop.df[,1],cex = 0.7, fill = colors)
#legend command that describes each and every part eg plot chr

#correlation plot
#calculate collinearity
c <- cor(cancer[,3:21])
corrplot(c, order = "hclust", tl.cex = 0.7) #hclust for clustal

#comparing the radius, area concavity of benign and malignant stage
ggplot(cancer, aes(x=diagnosis, y=area_mean,fill="pink"))+geom_boxplot(fill="yellow")+ggtitle("radius of benign")
ggplot(cancer, aes(x=diagnosis, y=area_mean,fill="pink"))+geom_boxplot()+ggtitle("area of beign vs malignant")
#same for concavity, 
#barplot
ggplot(cancer, aes(x=diagnosis, fill = texture_mean))+geom_bar()+ggtitle("women affected with benign and malignant")

ggplot(cancer, aes(x=diagnosis, fill = texture_mean))+geom_bar(fill="green")+ggtitle("women affected with benign and malignant")

#women affected at higher level based on mean from the analysis of boxplot
sel_data <- cancer[cancer$radius_mean]>10&cancer$radius_mean<15&cancer$compactness_mean>0.1

#density plot based on texture and mean
ggplot(cancer, aes(x=texture_mean,fill=as.factor(diagnosis)))+geom_density()+ggtitle("texture mean")

#barplot for area_se 15
ggplot(cancer, aes(x=area_se>15, fill = diagnosis))+ geom_bar(position = "fill")+ggtitle("area of beign vs maligant")

#distribution of data via histogram
ggplot(cancer, aes(x=concavity_mean,fill=diagnosis))+geom_histogram(binwidth = 15)+ggtitle(("concavity_mean"))
ggplot(cancer, aes(x= texture_se)) + facet_wrap(~ diagnosis)+ggtitle("texture se for beign vs malignant")
ggplot(cancer, aes(x = perimeter_mean))+geom_histogram(binwidth = 15) + facet_wrap(~ diagnosis)+ggtitle("perimeter mean for benign and malignant")

