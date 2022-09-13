# lasfname
#Acá van los análisis de la prueba LASFNAME.

#Voy desde CURVA ROC, VALIDEZ CONCURRENTE y CONFIABILIDAD




#CURVA ROC

#Acá voy a instalar los paquetes que necesito

install.packages("rmarkdown")

install.packages("pROC")

library(pROC)

install.packages("car")

library(car)


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}



packages <- c("effectsize","haven", "apaTables", "DescTools","report","dplyr","rstatix","FSA","rcompanion","tidyBF")

ipak(packages)


#Necesitamos antes una base de datos

lasfname <- BASE_LASFNAME



with(lasfname,tapply(edad,dx_lasfname,mean))

with(lasfname,tapply(sexo,dx_lasfname,mean))

with(lasfname,tapply(escolaridad,dx_lasfname,mean))



t.test(lasfname$edad~lasfname$dx_lasfname)

t.test(lasfname$escolaridad~lasfname$dx_lasfname)

t.test(craft$sexo~craft$dx_clinico)



#Acá voy a empezar simplemente a codear para construir la curva
curva_lasfname_inm <- roc(lasfname$dx_lasfname, lasfname$inm_total, col = "#32374c")

auc(curva_lasfname_inm)

plot(curva_lasfname_inm, col = "#fba31b")


#Ahora voy a agregar la curva del MoCA

curva_moca <- roc(lasfname$dx_lasfname, lasfname$moca)
auc(curva_moca)

curva_moca <- lines.roc(lasfname$dx_lasfname, lasfname$moca, col = "#32374c")


curva_lasfname_dif <- lines.roc(lasfname$dx_lasfname, lasfname$dif_total, col = "#a90123")


curva_lasfname_total <- lines.roc(lasfname$dx_lasfname, lasfname$fname_total, col = "#578001")




legend("bottomright", legend=c("lasfname inm", "MoCA", "lasfname dif", "lasfname total"), cex = 1, bty = "n", col =c("#fba31b","#32374c", "#a90123", "#578001"), lwd=3)



palette_fleni<-c("#fba31b", "#32374c", "#a90123", "#578001")


svg(filename = "Rplot02.svg")
dev.off()
    


#Punto de corte y sensibilidad y especificidad (INM) 

corte <- plot.roc(lasfname$dx_lasfname, lasfname$inm_total, percent = TRUE, ci = TRUE, of = "thresholds", thresholds = "best", print.thres = "best")


#Punto de corte y sensibilidad y especificidad (DIF) 

corte <- plot.roc(lasfname$dx_lasfname, lasfname$dif_total, percent = TRUE, ci = TRUE, of = "thresholds", thresholds = "best", print.thres = "best")


#Punto de corte y sensibilidad y especificidad (TOTAL) 

corte <- plot.roc(lasfname$dx_lasfname, lasfname$fname_total, percent = TRUE, ci = TRUE, of = "thresholds", thresholds = "best", print.thres = "best")



#Vamos con otro tipo de validez (VALIDEZ CONCURRENTE)
library(tidyverse)
library(corrplot)


cor(lasfname$inm_total, lasfname$craft_story_in_p)

cor(lasfname$dif_total, lasfname$craft_story_dif_p)

cor.test(~inm_total+craft_story_in_p, data = lasfname, method = "pearson")

cor.test(~dif_total+craft_story_dif_p, data = lasfname, method = "pearson")


install.packages("corrplot")

library(corrplot)

library(ggplot2)

?corrplot


require(corrplot)

lasfname_corr <- lasfname_correlac

cor(lasfname_corr)

cor_lasfname <- cor(lasfname_corr)

corrplot(cor_lasfname, method = "circle")


svg(filename = "Rplot03.svg")
dev.off()


#Ahora vamos a evaluar su confiabilidad a través del alfa de cronbach

lasfname_alfa <- BASE_LASFNAME_alfa

install.packages("psych")

library(psych)

alfa <- alpha(lasfname_alfa, check.keys = TRUE)

alfa <- na.omit(alfa)

alfa
