library(reshape2)
palette_fleni<-c("#fba31b", "#32374c", "#a90123", "#578001")

inmediato<-lasfname %>% select(hc,cara_inm_total, 
                                 letra_inm_total, 
                                 nombre_inm_total, 
                                 inm_total, craft_story_in, dx_lasfname)%>%
  rename(cara=cara_inm_total,
         letra=letra_inm_total,
         nombre=nombre_inm_total,
         total=inm_total,
         craft=craft_story_in)

df_long <- melt(inmediato, id=c("hc", "dx_lasfname"))



ggplot(df_long, aes(x=variable, y=value, fill=dx_lasfname))+
  geom_boxplot()+
  scale_fill_manual(values=c("#5b59ac", "#fba31b"))+
  theme_minimal()+
  geom_vline(xintercept = 4.5)+
  ggtitle("Rendimiento en tarea de memoria inmediata")
  


dif<-lasfname %>% select(hc,cara_dif_total, 
                               letra_dif_total, 
                               nombre_dif_total, 
                               dif_total, craft_story_dif, dx_lasfname)%>%
  rename(cara=cara_dif_total,
         letra=letra_dif_total,
         nombre=nombre_dif_total,
         total=dif_total,
         craft=craft_story_dif)

df_long <- melt(dif, id=c("hc", "dx_lasfname"))



ggplot(df_long, aes(x=variable, y=value, fill=dx_lasfname))+
  geom_boxplot()+
  scale_fill_manual(values=c("#5b59ac", "#fba31b"))+
  theme_minimal()+
  geom_vline(xintercept = 4.5)+
  ggtitle("Rendimiento en tarea de memoria diferida")
