# Gestión del conocimiento - Proyecto Univalle: Descriptivos, Chi-Cuadrado, ACM, CLuster.
# Analisis Multivariante
# Cesar A. Saavedra Vanegas
# 2020

# R opciones
options(warn = -1); options(scipen = 999)

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(viridis))
suppressMessages(library(lsr))
suppressMessages(library(DescTools))
suppressMessages(library(plotly))
suppressMessages(library(psych))
suppressMessages(library(GGally))
suppressMessages(library(RColorBrewer))
suppressMessages(library(NbClust))
suppressMessages(library(tidyselect))
suppressMessages(library(backports))
suppressMessages(library(broom))
suppressMessages(library(ggrepel))
suppressMessages(library(xtable))
suppressMessages(library(scatterplot3d))
# ------------------------------------------------------- #
# Cargar datos
# ------------------------------------------------------- #
km_data <- read.spss(file = "/Users/cesar.saavedra/Documents/GitHub/_knowledge_management_project_univalle/_data/Base\ GConocimiento\ PymeS\ \ Valle_2017.sav", to.data.frame = T, use.value.labels = T) # F
names(km_data)

pilares <- km_data %>% select(P3_1:P3_8, P5_1:P9_6, P11_1:P13_16, P22_1:P25_5)
for(j in 1:ncol(pilares)){
  pilares[,j] <- factor(pilares[,j], levels = c("Totalmente en desacuerdo",
                                                "En desacuerdo",
                                                "Ni de acuerdo ni en desacuerdo",
                                                "De acuerdo",
                                                "Totalmente de acuerdo"), ordered = T)
}; rm(j)
pilares %>% glimpse
pilares %>% str

# ------------------------------------------------------- #
# Pilar aprendizaje 
# ------------------------------------------------------- #
aprendizaje <- pilares %>% dplyr::select(P5_4, P5_5, P6_1, P6_2, P6_5, P7_2, P8_1, P13_13:P13_16)
aprendizaje %>% glimpse


# Analisis descriptivo
fqTable <- aprendizaje %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(aprendizaje))
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Totalmente en desacuerdo",
                                                          "En desacuerdo",
                                                          "Ni de acuerdo ni en desacuerdo",
                                                          "De acuerdo",
                                                          "Totalmente de acuerdo"), ordered = T)
fqTable$Variable <- factor(fqTable$Variable, levels = c("P5_4", "P5_5", "P6_1",
                                                        "P6_2", "P6_5", "P7_2",
                                                        "P8_1", paste0("P13_", 13:16)),
                           ordered = T)

# Plot it (SAVE IT!!!!)
# x11()
gg <- fqTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat = "identity", show.legend = F) +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) + # , scales = "free"
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("../_results/_descriptive_analysis/frecuencias_aprendizaje.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

# Chi-square test
options(warn=-1)
p.chisq = matrix(0, nrow=ncol(aprendizaje), ncol=ncol(aprendizaje), byrow=T)
for(i in 1:ncol(aprendizaje)){
  for(j in 1:ncol(aprendizaje)){
    p.chisq[i,j] = round(chisq.test(aprendizaje[,i],aprendizaje[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq) = NA
colnames(p.chisq) = colnames(aprendizaje)
rownames(p.chisq) = colnames(aprendizaje)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png('../_results/_descriptive_analysis/chi_square_test_aprendizaje.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq,
          main="Aprendizaje",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))
dev.off()#; rm(catVar, p.chisq, color_scale)

# Cramer's V test
options(warn=-1)
p.cramer = matrix(0, nrow=ncol(aprendizaje), ncol=ncol(aprendizaje), byrow=T)
for(i in 1:ncol(aprendizaje)){
  for(j in 1:ncol(aprendizaje)){
    p.cramer[i,j] = round(lsr::cramersV(aprendizaje[,i],aprendizaje[,j]),3)
  }
}; rm(i); rm(j)

# diag(p.cramer) = NA
colnames(p.cramer) = colnames(aprendizaje)
rownames(p.cramer) = colnames(aprendizaje)

x11()
png(file = '../_results/_descriptive_analysis/cramer_v_test_aprendizaje.png', height = 7, width = 7, units = "in", res = 300)
corrplot::corrplot(corr = p.cramer, method = "square")
dev.off()

# ------------------------------------------------------- #
# Pilar Liderazgo 
# ------------------------------------------------------- #
liderazgo <- pilares %>% dplyr::select(P3_1:P3_3, P3_5:P3_8, P8_3, P8_4, P8_5, P13_1, P13_2, P13_3, P13_4)
liderazgo  %>% glimpse

# Analisis descriptivo
fqTable3 <- liderazgo  %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable3) <- c("Variable", "Categoria", "Frecuencia")
fqTable3 <- fqTable3 %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(liderazgo))
fqTable3$Categoria <- factor(fqTable3$Categoria, levels = c("Totalmente en desacuerdo",
                                                            "En desacuerdo",
                                                            "Ni de acuerdo ni en desacuerdo",
                                                            "De acuerdo",
                                                            "Totalmente de acuerdo"), ordered = T)
fqTable3$Variable <- factor(fqTable3$Variable, levels = c(paste0("P3_", 1:3), paste0("P3_", 5:8),
                                                          "P8_3", "P8_4", "P8_5", "P13_1", "P13_2",
                                                          "P13_3", "P13_4"),
                            ordered = T)

# Plot it (SAVE IT!!!!)
# x11()
gg <- fqTable3 %>% ggplot(aes(x = Categoria, y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat = "identity", show.legend = F) +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("../_results/_descriptive_analysis/frecuencias_liderazgo.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable3, gg)


options(warn=-1)
p.chisq3 = matrix(0, nrow=ncol(liderazgo), ncol=ncol(liderazgo), byrow=T)
for(i in 1:ncol(liderazgo)){
  for(j in 1:ncol(liderazgo)){
    p.chisq3[i,j] = round(chisq.test(liderazgo[,i],liderazgo[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq3) = NA
colnames(p.chisq3) = colnames(liderazgo)
rownames(p.chisq3) = colnames(liderazgo)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png('../_results/_descriptive_analysis/chi_test_liderazgo.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq3,
          main="Liderazgo",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))
dev.off()

# Cramer's V test
options(warn=-1)
p.cramer3 = matrix(0, nrow=ncol(liderazgo), ncol=ncol(liderazgo), byrow=T)
for(i in 1:ncol(liderazgo)){
  for(j in 1:ncol(liderazgo)){
    p.cramer3[i,j] = round(lsr::cramersV(liderazgo[,i],liderazgo[,j]),3)
  }
}; rm(i); rm(j)

# diag(p.cramer) = NA
colnames(p.cramer3) = colnames(liderazgo)
rownames(p.cramer3) = colnames(liderazgo)

x11()
png(file = '../_results/_descriptive_analysis/cramer_v_test_liderazgo.png', height = 7, width = 7, units = "in", res = 300)
corrplot::corrplot(corr = p.cramer3, method = "square")
dev.off()

# ------------------------------------------------------- #
# Pilar Organizacion 
# ------------------------------------------------------- #
organizacion <- pilares %>% dplyr::select(P5_1:P5_3, P6_3, P6_4, P7_1, P7_3, P8_2, P13_9, P13_11)
organizacion  %>% glimpse

# Analisis descriptivo
fqTable4 <- organizacion  %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable4) <- c("Variable", "Categoria", "Frecuencia")
fqTable4 <- fqTable4 %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(organizacion))
fqTable4$Categoria <- factor(fqTable4$Categoria, levels = c("Totalmente en desacuerdo",
                                                            "En desacuerdo",
                                                            "Ni de acuerdo ni en desacuerdo",
                                                            "De acuerdo",
                                                            "Totalmente de acuerdo"), ordered = T)
fqTable4$Variable <- factor(fqTable4$Variable, levels = c(paste0("P5_", 1:3), "P6_3", "P6_4",
                                                          "P7_1", "P7_3", "P8_2", "P13_9", "P13_11"),
                            ordered = T)

# Plot it (SAVE IT!!!!)
# x11()
gg <- fqTable4 %>% ggplot(aes(x = Categoria, y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat = "identity", show.legend = F) +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("../_results/_descriptive_analysis/frecuencias_organizacion.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable4, gg)


options(warn=-1)
p.chisq4 = matrix(0, nrow=ncol(organizacion), ncol=ncol(organizacion), byrow=T)
for(i in 1:ncol(organizacion)){
  for(j in 1:ncol(organizacion)){
    p.chisq4[i,j] = round(chisq.test(organizacion[,i],organizacion[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq4) = NA
colnames(p.chisq4) = colnames(organizacion)
rownames(p.chisq4) = colnames(organizacion)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png('../_results/_descriptive_analysis/chi_test_organizacion.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq4,
          main="Organizacion",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))
dev.off()

# Cramer's V test
options(warn=-1)
p.cramer4 = matrix(0, nrow=ncol(organizacion), ncol=ncol(organizacion), byrow=T)
for(i in 1:ncol(organizacion)){
  for(j in 1:ncol(organizacion)){
    p.cramer4[i,j] = round(lsr::cramersV(organizacion[,i],organizacion[,j]),3)
  }
}; rm(i); rm(j)

# diag(p.cramer) = NA
colnames(p.cramer4) = colnames(organizacion)
rownames(p.cramer4) = colnames(organizacion)

png(file = '../_results/_descriptive_analysis/cramer_v_test_organizacion.png', height = 7, width = 7, units = "in", res = 300)
corrplot::corrplot(corr = p.cramer4, method = "number")
dev.off()

# ------------------------------------------------------- #
# Pilar Gestion Del Conocimiento - Juan
# ------------------------------------------------------- #
gconocimiento <- pilares %>% dplyr::select(P9_1:P9_6)
gconocimiento  %>% glimpse

# Analisis descriptivo
fqTable5 <- gconocimiento  %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable5) <- c("Variable", "Categoria", "Frecuencia")
fqTable5 <- fqTable5 %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(gconocimiento))
fqTable5$Categoria <- factor(fqTable5$Categoria, levels = c("Totalmente en desacuerdo",
                                                            "En desacuerdo",
                                                            "Ni de acuerdo ni en desacuerdo",
                                                            "De acuerdo",
                                                            "Totalmente de acuerdo"), ordered = T)
fqTable5$Variable <- factor(fqTable5$Variable, levels = c(paste0("P9_", 1:6)),
                            ordered = T)

# Plot it (SAVE IT!!!!)
gg <- fqTable5 %>% ggplot(aes(x = Categoria, y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat = "identity", show.legend = F) +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("../_results/_descriptive_analysis/frecuencias_gestion_conocimiento.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable5, gg)


options(warn=-1)
p.chisq5 = matrix(0, nrow=ncol(gconocimiento), ncol=ncol(gconocimiento), byrow=T)
for(i in 1:ncol(gconocimiento)){
  for(j in 1:ncol(gconocimiento)){
    p.chisq5[i,j] = round(chisq.test(gconocimiento[,i],gconocimiento[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq5) = NA
colnames(p.chisq5) = colnames(gconocimiento)
rownames(p.chisq5) = colnames(gconocimiento)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png('../_results/_descriptive_analysis/chi_test_gestion_conocimiento.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq5,
          main="Gestion Conocimiento",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))
dev.off()

# Cramer's V test
options(warn=-1)
p.cramer5 = matrix(0, nrow=ncol(gconocimiento), ncol=ncol(gconocimiento), byrow=T)
for(i in 1:ncol(gconocimiento)){
  for(j in 1:ncol(gconocimiento)){
    p.cramer5[i,j] = round(lsr::cramersV(gconocimiento[,i],gconocimiento[,j]),3)
  }
}; rm(i); rm(j)

# diag(p.cramer) = NA
colnames(p.cramer5) = colnames(gconocimiento)
rownames(p.cramer5) = colnames(gconocimiento)

png(file = '../_results/_descriptive_analysis/cramer_v_test_gestion_conocimiento.png', height = 7, width = 7, units = "in", res = 300)
corrplot::corrplot(corr = p.cramer5, method = "square")
dev.off()


#----------------------------------------------------------#
# Analisis de Correspondencias Multiples - Aprendizaje
#----------------------------------------------------------#
mca1<-MCA(aprendizaje, graph = T) 

get_eigenvalue(mca1)
fviz_screeplot(mca1, addlabels = TRUE) + ggtitle("") + ylab("% varianza explicada") + xlab("Dimensiones") 
fviz_mca_var(mca1, choice = "mca.cor", repel = TRUE) + ggtitle("")

#fviz_mca_var(mca1, col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

ind <- get_mca_ind(mca1)
ind$contrib
ind$cos2
var <- get_mca_var(mca1)
var$contrib
var$cos2
#----------------------------------------------------------#
# Analisis de Correspondencias Multiples - Liderazgo
#----------------------------------------------------------#
mca2<-MCA(liderazgo, graph = F) 

get_eigenvalue(mca2)
fviz_screeplot(mca2, addlabels = TRUE) + ggtitle("") + ylab("% varianza explicada") + xlab("Dimensiones") 
fviz_mca_var(mca2, choice = "mca.cor", repel = TRUE) + ggtitle("")

ind <- get_mca_ind(mca2)
ind$contrib
res.desc <- dimdesc(mca2, axes = c(1,2))

ind2 <- get_mca_ind(mca2)
ind$contrib
ind$cos2
var2 <- get_mca_var(mca2)
var2$contrib
var$cos2
#----------------------------------------------------------#
# Analisis de Correspondencias Multiples - Organizacion
#----------------------------------------------------------#
mca3<-MCA(organizacion, graph = F) 

get_eigenvalue(mca3)
fviz_screeplot(mca3, addlabels = TRUE) + ggtitle("") + ylab("% varianza explicada") + xlab("Dimensiones") 
fviz_mca_var(mca3, choice = "mca.cor", repel = TRUE) + ggtitle("")

ind <- get_mca_ind(mca3)
ind$contrib
var <- get_mca_var(mca3)
var$contrib
res.desc <- dimdesc(mca3, axes = c(1,2))

ind3 <- get_mca_ind(mca3)
ind$contrib
ind$cos2
var3 <- get_mca_var(mca3)
var$contrib
var$cos2
#----------------------------------------------------------#
# Analisis de Correspondencias Multiples - G. del conocimiento
#----------------------------------------------------------#
mca4<-MCA(gconocimiento, graph = F) 

get_eigenvalue(mca4)
fviz_screeplot(mca4, addlabels = TRUE) + ggtitle("") + ylab("% varianza explicada") + xlab("Dimensiones") 
fviz_mca_var(mca4, choice = "mca.cor", repel = TRUE) + ggtitle("")

ind <- get_mca_ind(mca4)
ind$contrib
ind$cos2
var <- get_mca_var(mca4)
var$contrib
var$cos2
res.desc <- dimdesc(mca4, axes = c(1,2))

#----------------------------------------------------------#
# Cluster 1-aprendizaje
#----------------------------------------------------------#
clus1 <- HCPC(mca1, graph = F)
clus1$desc.var
clus1$data.clust

#Dendograma
ggdend1 <- fviz_dend(clus1, show_labels = T, palette = "Dark2", main="")
ggplotly(ggdend1) #Guardar el grafico en una variable para luego aplicar la funcion plotly para hacerlo iteractivo
#Representacion de individuos ACM + Cluster
ggclus1 <- fviz_cluster(clus1,
             repel = TRUE,            
             show.clust.cent = TRUE, 
             palette = "Dark2",        
             ggtheme = theme_minimal(),
             main = ""
)
ggplotly(ggclus1)
# Descripcion por categorias
clus1$desc.var$category

#----------------------------------------------------------#
# Cluster 2-Liderazgo
#----------------------------------------------------------#
clus2 <- HCPC(mca2, graph = F)
clus2$desc.var
clus2$data.clust
#Dendograma
ggdend2 <- fviz_dend(clus2, show_labels = T, palette = "Dark2", main="")
ggplotly(ggdend2)
#Representacion de individuos ACM + Cluster
ggclus2 <- fviz_cluster(clus2,
                        repel = TRUE,            
                        show.clust.cent = F, 
                        palette = "Dark2",        
                        ggtheme = theme_minimal(),
                        main = ""
) 
ggplotly(ggclus2)
# Descripcion por categorias
clus2$desc.var
#----------------------------------------------------------#
# Cluster 3-organizacion
#----------------------------------------------------------#
clus3 <- HCPC(mca3, graph = F)
clus3$desc.var
clus3$data.clust
#Dendograma
ggdend3 <- fviz_dend(clus3, show_labels = T, palette = "Dark2", main="")
ggplotly(ggdend3)
#Representacion de individuos ACM + Cluster
ggclus3 <- fviz_cluster(clus3,
                        repel = TRUE,            
                        show.clust.cent = TRUE, 
                        palette = "Dark2",        
                        ggtheme = theme_minimal(),
                        main = ""
)
ggplotly(ggclus3)
# Descripcion por categorias
clus3$desc.var
#----------------------------------------------------------#
# Cluster 4-gestion conocimiento
#----------------------------------------------------------#
clus4 <- HCPC(mca4, graph = F)
clus4$desc.var
clus4$data.clust
#Dendograma
ggdend4 <- fviz_dend(clus4, show_labels = T, palette = "Dark2", main="")
ggplotly(ggdend4)
#Representacion de individuos ACM + Cluster
ggclus4 <- fviz_cluster(clus4,
                        repel = TRUE,            
                        show.clust.cent = TRUE, 
                        palette = "Dark2",        
                        ggtheme = theme_minimal(),
                        main = ""
)
ggplotly(ggclus4)
# Descripcion por categorias
clus4$desc.var





