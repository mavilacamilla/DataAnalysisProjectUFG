setwd("~/Estudos")
library(readxl)
library(ggpubr)
library(ggplot2)
library(tidyr)
library(dplyr)

data<-read_excel("dados.xlsx")

ano<-unlist(data[,1])
ano<-as.Date(as.character(ano),format = "%Y")
fertilityRate<-as.numeric(unlist(data[,2]))
laborRate<-as.numeric(unlist(data[,3]))
gini<-as.numeric(unlist(data[,4]))
desemprego<--as.numeric(unlist(data[,5]))

newData<-data.frame(ano,fertilityRate,laborRate,gini,desemprego)


##############################################
####### Visualiza????o de Dados ################

maximo.FR<-max(newData_semNa$fertilityRate)

newData_semNa$ano[which(newData_semNa$fertilityRate==maximo.FR)]

min.FR<-min(newData_semNa$fertilityRate)
newData_semNa$ano[which(newData_semNa$fertilityRate==min.FR)]



plot(fertilityRate,gini)
df <- newData %>%
  select(ano, fertilityRate, gini) %>%
  gather(key = "variable", value = "value", -ano)
head(df, 3)


graf<-ggplot(df, aes(x = ano, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#5a7d6a", "#fa756e"),name="Vari??veis",
                     breaks=c("fertilityRate", "gini"),
                     labels=c("Fecundidade", "??ndice de Gini")) +
  theme_minimal()+ 
  xlab("Anos")+
  ylab("Valor") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")+
  theme(axis.title = element_text(size = 15),legend.background = element_rect(fill="white", size=.3, linetype="solid"))

graf+theme(
  panel.background = element_rect(fill = "#f3ebdb",
                                  colour = "#f3ebdb",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)

graf+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)

graf+scale_x_date(date_breaks = "5 year", date_labels = "%Y")

########################################
######An??lises Estat??sticas#############
########################################

p<-cor(fertilityRate,gini, method = "pearson", use = "complete.obs")


fertility_labor<-ggscatter(newData, x = "fertilityRate", y = "laborRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Fertility Rate", ylab = "Labor Rate")

fertility_gini<-ggscatter(newData, x = "fertilityRate", y = "gini", 
                          add = "reg.line",
                          conf.int = TRUE,
                          add.params = list(color = "black",fill = "lightgrey"),
                          cor.coef = TRUE, 
                          cor.method = "pearson",
                          xlab = "Fecundidade",
                          ylab = "??ndice de Gini",font.label = c(12, "plain") )

fertility_gini+theme(panel.background = element_rect(fill = "#f3ebdb",colour = "#f3ebdb",
                     size = 0.5, linetype = "solid"))

#fertility_desemprego<-ggscatter(newData, x = "fertilityRate", y = "desemprego", 
#                                add = "reg.line",
#                                conf.int = TRUE,
#                                add.params = list(color = "darkBlue",fill = "lightgray"),
#                                cor.coef = TRUE, 
#                                cor.method = "pearson",
#                                xlab = "Fecundidade",
#                                ylab = "Desemprego",font.label = c(12, "plain") )

#Fun????o criada manualmente 
pearson<- function(x,y) { 
  
  med.x<-mean(x)
  med.y<-mean(y)
  
  dif.med.x<-x-med.x
  dif.med.y<-y-med.y
  
  covariancia<-sum(dif.med.y*dif.med.x)
  
  
  dm.quadX<-dif.med.x^2
  dm.medY<-dif.med.y^2
  
  sum.dmqX<-sum(dm.quadX)
  sum.dmqY<-sum(dm.medY)
  
  sqrtX<-sqrt(sum.dmqX)
  sqrtY<-sqrt(sum.dmqY)
  
  dem<-sqrtX*sqrtY
  
  r<-covariancia/dem
  
  return(r)
}

# Retirando linhas contendo valores ausentes 
newData_semNa<-na.omit(newData)

#Calculando o coeficiente de pearson por meio da fun????o criada anteriormente 
p.pearson<-pearson(newData_semNa$fertilityRate, newData_semNa$gini)


#Calculando p-valor manualmente 
dist<-numeric(1000000)
for(i in 1:1000000){ 
  dist[i]<-pearson(sample(newData_semNa$fertilityRate),sample(newData_semNa$gini))}

as.numeric(dist>p.pearson)
sum(as.numeric(dist>p.pearson))/length(dist)

#Calculando coeficiente de pearson e p-valor a partir das
# fun????es presentes no R 
cor.test(newData_semNa$fertilityRate, newData_semNa$gini)


# Calculando coeficiente de regress??o 
lm(newData_semNa$fertilityRate~ newData_semNa$gini)
plot(newData_semNa$fertilityRate~ newData_semNa$gini)
abline(lm(newData_semNa$fertilityRate~ newData_semNa$gini))

summary(lm(newData_semNa$gini~newData_semNa$fertilityRate))
