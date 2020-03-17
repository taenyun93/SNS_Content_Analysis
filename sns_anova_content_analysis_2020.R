library(data.table)
library(dplyr)
library(extrafont)
loadfonts(device = "win")
library(ggplot2)

df_sns <- fread('data/df_sns_cleaned.csv')
df_sns <- data.frame(df_sns)

#AOV
df_sns2 <- df_sns[(!is.na(df_sns["Like"])),]
dv_sns <- "X1SNS"
#Normalise
library(robustHD)
df_sns2$Like = as.numeric(df_sns2$Like)
df_sns2[df_sns2[dv_sns] == 1,'Like_std'] <- standardize(df_sns2[df_sns2[dv_sns] == 1,'Like'])
df_sns2[df_sns2[dv_sns] == 2,'Like_std'] <- standardize(df_sns2[df_sns2[dv_sns] == 2,'Like'])
#3. Model
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X3model"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

output <- lm(Like_std ~ X1SNS*X3model
               ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X1SNS")

#3-1. Model Number
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X3model"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

dv2 <- "X31number"
dv_cat <- "X31number_cat"

df_sns_aov[!is.na(df_sns_aov[dv2]),dv2] <- as.integer(df_sns_aov[!is.na(df_sns_aov[dv2]),dv2])
df_sns_aov[is.na(df_sns_aov[dv2]),dv_cat] <- '0'
df_sns_aov[(df_sns_aov[dv2] == 1)&(!is.na(df_sns_aov[dv2])),dv_cat] <- '1'
df_sns_aov[(df_sns_aov[dv2] == 2)&(!is.na(df_sns_aov[dv2])),dv_cat] <- '2'
df_sns_aov[(df_sns_aov[dv2] == 3)&(!is.na(df_sns_aov[dv2])),dv_cat] <- '3'
df_sns_aov[(df_sns_aov[dv2] > 3)&(!is.na(df_sns_aov[dv2])),dv_cat] <- 'group'

output <- lm(Like_std ~ X1SNS*X31number_cat
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X31number_cat")

#graph
y_lab <- "Like"
x_lab <- "SNS"
legend_name <- "Number"


df_graph <- df_sns_aov %>% group_by(X31number_cat) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p31 <- ggplot(df_graph,aes(x=X31number_cat,y=mean,fill = X31number_cat))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  scale_x_discrete(name = legend_name)+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p31



#3-2. Model Face
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X3model"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

dv2 <- "X32face"
dv_cat <- "X32face_cat"
df_sns_aov[is.na(df_sns_aov[dv2]),dv2] <- 0
df_sns_aov[!is.na(df_sns_aov[dv2]),dv2] <- as.integer(df_sns_aov[!is.na(df_sns_aov[dv2]),dv2])
#df_sns_aov[is.na(df_sns_aov[dv2]),dv_cat] <- '0'
df_sns_aov[(df_sns_aov[dv2] == 1)&(!is.na(df_sns_aov[dv2])),dv_cat] <- 'Y'
df_sns_aov[(df_sns_aov[dv2] == 2)&(!is.na(df_sns_aov[dv2])),dv_cat] <- 'N'
df_sns_aov[(df_sns_aov[dv2] == 0)&(!is.na(df_sns_aov[dv2])),dv_cat] <- 'X'


output <- lm(Like_std ~ X1SNS*X32face_cat
             ,data = df_sns_aov)
result <- aov(output)
summary(result)

#3-3. Model body
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X3model"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

dv2 <- "X33body"
dv_cat <- "X33body_cat"
df_sns_aov[is.na(df_sns_aov[dv2]),dv2] <- 0
df_sns_aov[!is.na(df_sns_aov[dv2]),dv2] <- as.integer(df_sns_aov[!is.na(df_sns_aov[dv2]),dv2])
#df_sns_aov[is.na(df_sns_aov[dv2]),dv_cat] <- '0'
df_sns_aov[(df_sns_aov[dv2] == 1)&(!is.na(df_sns_aov[dv2])),dv_cat] <- 'Y'
df_sns_aov[(df_sns_aov[dv2] == 2)&(!is.na(df_sns_aov[dv2])),dv_cat] <- 'N'
df_sns_aov[(df_sns_aov[dv2] == 0)&(!is.na(df_sns_aov[dv2])),dv_cat] <- 'X'


output <- lm(Like_std ~ X1SNS*X33body_cat
             ,data = df_sns_aov)
result <- aov(output)
summary(result)


#4. Background
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X4background"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]

df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Wall'
df_sns_aov[df_sns_aov[dv] == 2,dv] <- 'Indoor'
df_sns_aov[df_sns_aov[dv] == 3,dv] <- 'Outdoor'
df_sns_aov[df_sns_aov[dv] == 4,dv] <- 'Other'
df_sns_aov2 <- df_sns_aov[df_sns_aov[dv] != 'Other',]
table(df_sns_aov2$X4background)
output <- lm(Like_std ~ X1SNS*X4background
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X1SNS")
TukeyHSD(result,"X4background")
TukeyHSD(result,"X1SNS:X4background")



#graph
y_lab <- "Like"
x_lab <- "SNS"
legend_name <- "Background"
df_graph <- df_sns_aov %>% group_by(X4background) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p4_col <- ggplot(df_graph,aes(x=X4background,y=mean,fill = X4background))+
  geom_col(size = 1)+
  geom_hline(aes(yintercept = 0),linetype="dashed")+
  ylab(y_lab)+
  xlab(x_lab)+
  scale_x_discrete(limit = c("Indoor","Outdoor","Wall","Other"))+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p4_col

df_graph <- df_sns_aov %>% group_by(X1SNS,X4background) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p4 <- ggplot(df_graph,aes(x=X1SNS,y=mean,group = X4background,color = X4background))+
  geom_line(size = 1)+
  geom_hline(aes(yintercept = 0),linetype="dashed")+
  ylab(y_lab)+
  xlab(x_lab)+
  scale_x_discrete()+
  scale_color_discrete(limit = c("Indoor","Outdoor","Wall","Other"))+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p4


#5. Text
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X5text"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

output <- lm(Like_std ~ X1SNS*X5text
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X5text")

#graph
y_lab <- "Like"
x_lab <- "SNS"
legend_name <- "Text"


df_graph <- df_sns_aov %>% group_by(X5text) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p5 <- ggplot(df_graph,aes(x=X5text,y=mean,fill = X5text))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  scale_x_discrete(name = legend_name)+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p5


#6. Brand Name
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X6brandname"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

output <- lm(Like_std ~ X1SNS*X6brandname
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X5text")

#Brand logo
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X7logo"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

output <- lm(Like_std ~ X1SNS*X7logo
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X7logo")
TukeyHSD(result,"X1SNS:X7logo")

#graph

legend_name = 'Logo'
x_lab = "SNS"
y_lab = "Like"
df_graph <- df_sns_aov %>% group_by(X7logo) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p7_col <- ggplot(df_graph,aes(x=X7logo,y=mean,fill = X7logo))+
  geom_col(size = 1)+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p7_col


legend_name = 'Logo'
x_lab = "SNS"
y_lab = "Like"
df_graph <- df_sns_aov %>% group_by(X1SNS,X7logo) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p7 <- ggplot(df_graph,aes(x=X1SNS,y=mean,group = X7logo,color = X7logo))+
  geom_line(size = 1)+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p7


#Brand Name
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X6brandname"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

output <- lm(Like_std ~ X1SNS*X6brandname
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X5text")

#7. Brand logo
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X7logo"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]
df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'

df_sns_aov[df_sns_aov[dv] == 1,dv] <- 'Y'
df_sns_aov[df_sns_aov[dv] == 0,dv] <- 'N'

output <- lm(Like_std ~ X1SNS*X7logo
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X1SNS:X7logo")

#graph
legend_name = 'Logo' 
x_lab = "SNS"
y_lab = "Like"
df_graph <- df_sns_aov %>% group_by(X1SNS,X7logo) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p7 <- ggplot(df_graph,aes(x=X1SNS,y=mean,group = X7logo,color = X7logo))+
  geom_line(size = 1)+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p7

#7-1. Brand logo exposure
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X71logoexposure"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]

df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'
#dv2 <- "X71logoexposure"
df_sns_aov[df_sns_aov[dv] == 1, dv] <- 'whole'
df_sns_aov[df_sns_aov[dv] == 0, dv]<- 'part'

output <- lm(Like_std ~ X1SNS*X71logoexposure
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X71logoexposure")

#7-4. Brand logo exposure
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X74logotype"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]

df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'
#dv2 <- "X71logoexposure"
df_sns_aov[df_sns_aov[dv] == 1, dv] <- 'Text'
df_sns_aov[df_sns_aov[dv] == 2, dv] <- 'Image'
df_sns_aov[df_sns_aov[dv] == 3, dv] <- 'Both'
output <- lm(Like_std ~ X1SNS*X74logotype
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X1SNS:X74logotype")

#graph

legend_name = 'Logo' 
x_lab = "SNS"
y_lab = "Like"
df_graph <- df_sns_aov %>% group_by(X1SNS,X74logotype) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p74 <- ggplot(df_graph,aes(x=X1SNS,y=mean,group = X74logotype,color = X74logotype))+
  geom_line(size = 1)+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p74

#7-5. Brand logo exposure
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X7logo"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]

df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'
dv2 <- "X75logovisib"
df_sns_aov[is.na(df_sns_aov[dv2]), dv2] <- 'no'
df_sns_aov[df_sns_aov[dv2] == 0, dv2] <- "low"
df_sns_aov[df_sns_aov[dv2] == 1, dv2] <- "high"

output <- lm(Like_std ~ X1SNS*X75logovisib
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X75logovisib")
TukeyHSD(result,"X1SNS:X75logovisib")

#graph
legend_name = 'Angle' 
x_lab = "LogoVis"
y_lab = "Like"
df_graph <- df_sns_aov %>% group_by(X75logovisib) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p75_col <- ggplot(df_graph,aes(x=X75logovisib,y=mean,fill = X75logovisib))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p75_col

legend_name = 'LogoVis' 
x_lab = "SNS"
y_lab = "Like"

df_graph <- df_sns_aov %>% group_by(X1SNS,X75logovisib) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p75_inter <- ggplot(df_graph,aes(x=X1SNS,y=mean,group = X75logovisib,color = X75logovisib))+
  geom_line(size = 1)+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p75_inter

#8. Product exposure
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X8product"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]

df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'
#dv2 <- "X75logovisib"
df_sns_aov[df_sns_aov[dv] == 0, dv] <- "No"
df_sns_aov[df_sns_aov[dv] == 1, dv] <- "Yes"

output <- lm(Like_std ~ X1SNS*X8product
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X1SNS:X8product")

#8-1. Product exposure degree
iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X8product"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]



df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'
dv2 <- "X81productexpose"
df_sns_aov[is.na(df_sns_aov[dv2]), dv2] <- "No"
df_sns_aov[df_sns_aov[dv2] == 0, dv2] <- "Part"
df_sns_aov[df_sns_aov[dv2] == 1, dv2] <- "Whole"

output <- lm(Like_std ~ X1SNS*X81productexpose
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X1SNS:X81productexpose")

#10. Camera angle

iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "X10angle"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]

df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'
#dv2 <- "X75logovisib"
df_sns_aov[df_sns_aov[dv] == 1, dv] <- "high"
df_sns_aov[df_sns_aov[dv] == 2, dv] <- "high"
df_sns_aov[df_sns_aov[dv] == 3, dv] <- "eye"
df_sns_aov[df_sns_aov[dv] == 4, dv] <- "low"
df_sns_aov[df_sns_aov[dv] == 5, dv] <- "other"
df_sns_aov[df_sns_aov[dv] == 6, dv] <- "other"
df_sns_aov2 <- df_sns_aov[df_sns_aov[dv] != "other",]
output <- lm(Like_std ~ X1SNS*X10angle
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"X10angle")
TukeyHSD(result,"X1SNS:X10angle")

legend_name = 'Angle' 
x_lab = "Angle"
y_lab = "Like"

df_graph <- df_sns_aov2 %>% group_by(X10angle) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p10_col <- ggplot(df_graph,aes(x=X10angle,y=mean,fill = X10angle))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p10_col

legend_name = 'Angle' 
x_lab = "SNS"
y_lab = "Like"

df_graph <- df_sns_aov %>% group_by(X1SNS,X10angle) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p10_inter <- ggplot(df_graph,aes(x=X1SNS,y=mean,group = X10angle,color = X10angle))+
  geom_line(size = 1)+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p10_inter

#Logo X Cat

iv <- "Like_std"
dv_sns <- "X1SNS"
dv <- "BrandCategory"
df_sns_aov <- df_sns2[(!is.na(df_sns2[dv_sns]))&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[iv])),]

df_sns_aov[df_sns_aov[dv_sns] == 1,dv_sns] <- 'FB'
df_sns_aov[df_sns_aov[dv_sns] == 2,dv_sns] <- 'Insta'
output <- lm(Like_std ~ X1SNS*BrandCategory
             ,data = df_sns_aov)
result <- aov(output)
summary(result)
TukeyHSD(result,"BrandCategory")
TukeyHSD(result,"X1SNS:BrandCategory")

#graph
legend_name = 'Angle' 
x_lab = "Brand"
y_lab = "Like"
df_graph <- df_sns_aov %>% group_by(BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_col <- ggplot(df_graph,aes(x=BrandCategory,y=mean,fill = BrandCategory))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_col

legend_name = 'Brand' 
x_lab = "SNS"
y_lab = "Like"

df_graph <- df_sns_aov %>% group_by(X1SNS,BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand <- ggplot(df_graph,aes(x=X1SNS,y=mean,group = BrandCategory,color = BrandCategory))+
  geom_line(size = 1)+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand


#
#Logo X Cat
#Insta
iv <- "Like"
dv_sns <- "X1SNS"
dv <- "BrandCategory"
dv2 <- "X7logo"

df_sns_aov_insta <- df_sns2[(df_sns2[dv_sns]==2)&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[dv2]))&(!is.na(df_sns2[iv])),]

dv2 <- "X75logovisib"
df_sns_aov_insta[is.na(df_sns_aov_insta[dv2]), dv2] <- "No"
df_sns_aov_insta[df_sns_aov_insta[dv2] == 0, dv2] <- "Low"
df_sns_aov_insta[df_sns_aov_insta[dv2] == 1, dv2] <- "High"

output <- lm(Like ~ BrandCategory*X75logovisib
             ,data = df_sns_aov_insta)
result <- aov(output)
summary(result)

TukeyHSD(result,"BrandCategory")
TukeyHSD(result,"BrandCategory:X75logovisib")
test <- data.frame(TukeyHSD(result,"BrandCategory:X75logovisib")[1])


legend_name = 'Angle' 
x_lab = "Brand"
y_lab = "Like"
df_graph <- df_sns_aov_insta %>% group_by(BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_insta_col <- ggplot(df_graph,aes(x=BrandCategory,y=mean,fill = BrandCategory))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_insta_col

legend_name = 'Logo' 
x_lab = "Brand"
y_lab = "Like"

df_graph <- df_sns_aov_insta %>% group_by(X75logovisib,BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_insta <- ggplot(df_graph,aes(x=BrandCategory,y=mean, fill = X75logovisib))+
  geom_col(width = 0.8,position = position_dodge(width=0.9))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=0.1,position = position_dodge(width=0.9))+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_insta

#Facebook
iv <- "Like"
dv_sns <- "X1SNS"
dv <- "BrandCategory"
dv2 <- "X7logo"

df_sns_aov_fb <- df_sns2[(df_sns2[dv_sns]==1)&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[dv2]))&(!is.na(df_sns2[iv])),]

dv2 <- "X75logovisib"
df_sns_aov_fb[is.na(df_sns_aov_fb[dv2]), dv2] <- "No"
df_sns_aov_fb[df_sns_aov_fb[dv2] == 0, dv2] <- "Low"
df_sns_aov_fb[df_sns_aov_fb[dv2] == 1, dv2] <- "High"

output <- lm(Like ~ BrandCategory*X75logovisib
             ,data = df_sns_aov_fb)
result <- aov(output)
summary(result)

TukeyHSD(result,"BrandCategory")
TukeyHSD(result,"BrandCategory:X75logovisib")


legend_name = 'Angle' 
x_lab = "Brand"
y_lab = "Like"
df_graph <- df_sns_aov_fb %>% group_by(BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_fb_col <- ggplot(df_graph,aes(x=BrandCategory,y=mean,fill = BrandCategory))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_insta_col

legend_name = 'Logo' 
x_lab = "Brand"
y_lab = "Like"

df_graph <- df_sns_aov_fb %>% group_by(X75logovisib,BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_fb <- ggplot(df_graph,aes(x=BrandCategory,y=mean, fill = X75logovisib))+
  geom_col(width = 0.8,position = position_dodge(width=0.9))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=0.1,position = position_dodge(width=0.9))+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_fb

p_brand_insta



#Logo X Cat
#Insta
iv <- "Like"
dv_sns <- "X1SNS"
dv <- "BrandCategory"
dv2 <- "X7logo"

df_sns_aov_insta <- df_sns2[(df_sns2[dv_sns]==2)&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[dv2]))&(!is.na(df_sns2[iv])),]

dv2 <- "X75logovisib"
df_sns_aov_insta[is.na(df_sns_aov_insta[dv2]), dv2] <- "No"
df_sns_aov_insta[df_sns_aov_insta[dv2] == 0, dv2] <- "Low"
df_sns_aov_insta[df_sns_aov_insta[dv2] == 1, dv2] <- "High"

output <- lm(Like ~ BrandCategory*X75logovisib
             ,data = df_sns_aov_insta)
result <- aov(output)
summary(result)

TukeyHSD(result,"BrandCategory")
TukeyHSD(result,"BrandCategory:X75logovisib")
test <- data.frame(TukeyHSD(result,"BrandCategory:X75logovisib")[1])


legend_name = 'Angle' 
x_lab = "Brand"
y_lab = "Like"
df_graph <- df_sns_aov_insta %>% group_by(BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_insta_col <- ggplot(df_graph,aes(x=BrandCategory,y=mean,fill = BrandCategory))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_insta_col

legend_name = 'Logo' 
x_lab = "Brand"
y_lab = "Like"

df_graph <- df_sns_aov_insta %>% group_by(X75logovisib,BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_insta <- ggplot(df_graph,aes(x=BrandCategory,y=mean, fill = X75logovisib))+
  geom_col(width = 0.8,position = position_dodge(width=0.9))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=0.1,position = position_dodge(width=0.9))+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_insta

#Facebook
iv <- "Like"
dv_sns <- "X1SNS"
dv <- "BrandCategory"
dv2 <- "X7logo"

df_sns_aov_fb <- df_sns2[(df_sns2[dv_sns]==1)&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[dv2]))&(!is.na(df_sns2[iv])),]

dv2 <- "X75logovisib"
df_sns_aov_fb[is.na(df_sns_aov_fb[dv2]), dv2] <- "No"
df_sns_aov_fb[df_sns_aov_fb[dv2] == 0, dv2] <- "Low"
df_sns_aov_fb[df_sns_aov_fb[dv2] == 1, dv2] <- "High"

output <- lm(Like ~ BrandCategory*X75logovisib
             ,data = df_sns_aov_fb)
result <- aov(output)
summary(result)

TukeyHSD(result,"BrandCategory")
TukeyHSD(result,"BrandCategory:X75logovisib")


legend_name = 'Angle' 
x_lab = "Brand"
y_lab = "Like"
df_graph <- df_sns_aov_fb %>% group_by(BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_fb_col <- ggplot(df_graph,aes(x=BrandCategory,y=mean,fill = BrandCategory))+
  geom_col(size = 1,)+
  geom_hline(aes(yintercept=0))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=.05)+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_fb_col

legend_name = 'Logo' 
x_lab = "Brand"
y_lab = "Like"

df_graph <- df_sns_aov_fb %>% group_by(X75logovisib,BrandCategory) %>% summarise(mean = mean(Like_std),n = n(), sd = sd(Like_std),se = sd/sqrt(n))
df_graph
p_brand_fb <- ggplot(df_graph,aes(x=BrandCategory,y=mean, fill = X75logovisib))+
  geom_col(width = 0.8,position = position_dodge(width=0.9))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=0.1,position = position_dodge(width=0.9))+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_fb

p_brand_insta
p_brand_insta_col


#Insta
iv <- "Like"
dv_sns <- "X1SNS"
dv <- "BrandCategory"
dv2 <- "type_video"

df_sns_aov_insta <- df_sns2[(df_sns2[dv_sns]==2)&(!is.na(df_sns2[dv]))&(!is.na(df_sns2[dv2]))&(!is.na(df_sns2[iv])),]

df_sns_aov_fb[df_sns_aov_fb[dv2] == 0, dv2] <- "N"
df_sns_aov_fb[df_sns_aov_fb[dv2] == 1, dv2] <- "Y"
output <- lm(Like ~ type_video*BrandCategory
             ,data = df_sns_aov_fb)
result <- aov(output)
summary(result)
TukeyHSD(result,"type_video")
TukeyHSD(result,"type_video:BrandCategory")

legend_name = 'Video' 
x_lab = "BrandCat"
y_lab = "Like"

df_graph <- df_sns_aov_insta %>% group_by(type_video,BrandCategory) %>% summarise(mean = mean(Like),n = n(), sd = sd(Like),se = sd/sqrt(n))
df_graph
p_brand_insta <- ggplot(df_graph,aes(x=BrandCategory,y=mean, fill = type_video))+
  geom_col(width = 0.8,position = position_dodge(width=0.9))+
  ylab(y_lab)+
  xlab(x_lab)+
  geom_hline(aes(yintercept=0),linetype ="dashed")+
  scale_color_discrete(name = legend_name)+
  geom_errorbar(aes(ymin = mean-se,ymax=mean+se),width=0.1,position = position_dodge(width=0.9))+theme_bw()+theme(legend.position = "bottom")+theme(text=element_text(family="Verdana", size=18))
p_brand_insta

