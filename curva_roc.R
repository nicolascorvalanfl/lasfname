# 0 paquetes ===============================


library(pROC)
library(readxl)
lasfname <- read_excel("BASE LASFNAME.xlsx")

# 1. Curva ROC  ===============================

roc_lasfname_inm <- roc(lasfname$dx_lasfname, lasfname$inm_total, col = "#32374c")
roc_lasfname_total<-roc(lasfname$dx_lasfname, lasfname$fname_total)
roc_moca<-roc(lasfname$dx_lasfname, lasfname$moca)
roc_lasfname_dif<-roc(lasfname$dx_lasfname, lasfname$dif_total)

plot(roc_lasfname_inm, col = "#fba31b",lwd=2, lty = 1)
plot(roc_lasfname_total, col = "#578001",lwd=2, lty = 1, add=TRUE)
plot(roc_moca, col = "#32374c",lwd=1.5, lty = 2, add=TRUE)
plot(roc_lasfname_dif, col = "#a90123",lwd=2, lty = 1, add=TRUE)
legend("right",
       legend=c("lasfname inmediato", "lasfname total", "MoCA", "lasfname inmediato"),
       col=c("#fba31b", "#578001","#32374c", "#a90123"),
       lwd=4, cex =.5, xpd = F, horiz = F)



# 1. Tabla AUC  ===============================

library(dplyr)

names<-c("lasfname_inm","lasfname_total",  "lasfname_delay" , "MoCA")


auc_lasfname_inm<-ci.auc(roc_lasfname_inm)
auc_lasfname_total<-ci.auc(roc_lasfname_total)
auc_lasfname_dif<-ci.auc(roc_lasfname_dif)
auc_moca<-ci.auc(roc_moca)


tabla<-as.data.frame(rbind(auc_lasfname_inm,
                           auc_lasfname_total,
                           auc_lasfname_dif,
                           auc_moca))

tabla <- tabla %>% rename("CI-inf"=V1,
                          "AUC"=V2,
                          "CI-sup"=V3)

tabla<-round((tabla*100),2)

tabla<-cbind(names,tabla)

library (gt)

gt(tabla)
