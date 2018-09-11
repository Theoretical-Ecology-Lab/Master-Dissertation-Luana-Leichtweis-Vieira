# Análises da comunidade de aranhas em fragmentos de mata numa paisagem dominada 
# por atividade agrária na transição do Cerrado com Floresta Atlântica

## Análise por fragmentos

hab.frag <- aggregate(hab.folhagem[, 5:6], list(hab.folhagem$fragmentos), mean)
colnames(hab.frag)[1] <- "fragmentos"
hab.frag #cobertura média da folhagem no sub-bosque e no dossel por fragmento

dados.frag <- merge(hab.frag, dados[, -c(22, 23)], all = T)
colnames(dados.frag)
View(dados.frag) #dados por fragmento replicados para cada aranha adulta coletada

spp.frag.bat <- matrix(table(dados.frag$fragmentos[which(dados.frag$method=="batedor")], 
                             dados.frag$morph_sp[which(dados.frag$method=="batedor")]), 8, 156)
colnames(spp.frag.bat) <- levels(dados.frag$morph_sp)
View(spp.frag.bat) #abundância de aranhas adultas em guarda-chuva entomológico
rowSums(spp.frag.bat) #aranhas adultas por fragmento
summary(rowSums(spp.frag.bat))

spp.frag.not <- matrix(table(dados.frag$fragmentos[which(dados.frag$method=="noturna")], 
               dados.frag$morph_sp[which(dados.frag$method=="noturna")]), 8, 156)
colnames(spp.frag.not) <- levels(dados.frag$morph_sp)
View(spp.frag.not) #abundância de aranhas adultas em busca ativa noturna
rowSums(spp.frag.not) #aranhas adultas por fragmento

spp.frag <- rbind(spp.frag.bat, spp.frag.not)
metodo <- rep(c("batedor", "noturna"), each = 8)
View(spp.frag) #abundância de aranhas adultas em oito fragmentos amostrados com
                #guarda-chuva entomológico e busca ativa noturna
rowSums(spp.frag) #aranhas adultas em oito fragmentos por método xe coleta

nspp.frag <- rarefy(spp.frag, 15) #número de espécies rarefeito em 
                                  #sub amostras de 15 indivíduos
nspp.frag.se <- rarefy(spp.frag, 15, se = T)[2, ] #erro padrão do número de espécies
nspp.frag.min <- nspp.frag - nspp.frag.se
nspp.frag.max <- nspp.frag + nspp.frag.se

frag.amb <- aggregate(dados.site[, c("sub_bosque", "dossel", 
                                      "perimetro", "area", "floresta_35",
                                      "cultivo_35", "solo_35", "agua_35",
                                      "floresta_5",  "cultivo_5", "solo_5",
                                      "agua_5","manchas")], 
                       list(dados.site$fragmentos, dados.site$method), mean)
colnames(frag.amb)[1:2] <- c("fragmentos", "method")
frag.amb[9:16, c("sub_bosque", "dossel")] <- frag.amb[1:8, c("sub_bosque", "dossel")]
View(frag.amb) #variáveis ambientais nos fragmentos replicadas por método de coleta


## Relação entre o número se espécies e a área dos fragmentos (barra de erros padrão)
set.seed(1)
jit <- rep(replicate(8, jitter(.3, 10)), 2) #fator de correção para usar no gráfico:
plot(jit+(frag.amb$area), nspp.frag, col = gray(rep(c(.8, 0), each = 8)), 
     pch = 19, cex = 2, bty="l", xlab = expression(paste("Área ")(Km^2)),
     ylab = "Número rarefeito de espécies")
segments(jit+(frag.amb$area), nspp.frag.min, y1 = nspp.frag.max, col = gray(rep(c(.8, 0), each = 8)))

## Abundância das espécies por fragmento:
spp.frag.total <- matrix(table(dados.frag$fragmentos, 
                             dados.frag$morph_sp), 8, 156)
colnames(spp.frag.total) <- levels(dados.frag$morph_sp)
View(spp.frag.total)
rowSums(spp.frag.total)

nspp.frag.total <- rarefy(spp.frag.total, 50) #rarefação do número de espécies em
                                              #sub amostras de 50 indivíduos

frag.amb.total <- aggregate(dados.frag[, c("sub_bosque", "dossel", 
                                     "perimetro", "area", "floresta_35",
                                     "cultivo_35", "solo_35", "agua_35",
                                     "floresta_5",  "cultivo_5", "solo_5",
                                     "agua_5","manchas")], 
                      list(dados.frag$fragmentos), mean)
View(frag.amb.total) #médias das variáveis ambientais em oito fragmentos

## Caracterização da paisagem ao redor dos fragmentos em buffers de 5 km e 35 km
pca.frag <- rda(frag.amb.total[, 6:13], scale = T) #PCA de correlações entre os 
                                                   #componentes da paisagem (NDVI)
plot(pca.frag)
summary(pca.frag) #o primeiro eixo da PCA recuperou 47% da variância total
pca.total_1 <- scores(pca.frag)$sites[,1]

### Vamos colocar os gráficos das variáveis em ordem da média de PCA 1
### ponderada pela média dos componentes da paisagem:

sort(colSums(
  frag.amb.total[, 6:13]*pca.total_1)/colSums(frag.amb.total[, 6:13])) #médias em ordem crescente
fat_5 <- decostand(frag.amb.total[, 10:13], "total") #médias relativizadas (5 km)
fat_35 <- decostand(frag.amb.total[, 6:9], "total") #médias relativizadas (35 km)

### Vamos usar essa informação para fazer um painel com todos os gráficos em 
### ordem:
jpeg("multi_pca1.jpg", width = 20, height = 20, units = "cm", res = 300)
layout(matrix(1:8, 4, 2), widths = rep(3,8), heights = rep(c(1.5, 2, 1.5, 2), c(3, 1, 3, 1)), respect = T)
par(mar = c(.1, 4.1, .5, 1.1))
plot(sort(pca.total_1), fat_5$solo_5[order(pca.total_1)], type = "b",  
     xlab = NA, ylab = "Solo (5 km)", xaxt = "n", bty = "l", 
     cex=sort(nspp.frag.total*.15), ylim = c(0, .5))
plot(sort(pca.total_1), fat_5$cultivo_5[order(pca.total_1)], type = "b",  
     xlab = NA, ylab = "Cultivo (5 km)", xaxt = "n", bty = "l", 
     cex=sort(nspp.frag.total*.15), ylim = c(0, .5))
plot(sort(pca.total_1), fat_5$floresta_5[order(pca.total_1)], type = "b",  
     xlab = NA, ylab = "Floresta (5 km)", xaxt = "n", bty = "l", 
     cex=sort(nspp.frag.total*.15), ylim = c(0, .5))
par(mar = c(4.1, 4.1, .5, 1.1))
plot(sort(pca.total_1), fat_5$agua_5[order(pca.total_1)], type = "b",  
     xlab = "Eixo PCA 1", ylab = "Água (5 km)", bty = "l", 
     cex=sort(nspp.frag.total*.15), ylim = c(0, .5))
par(mar = c(.1, 4.1, .5, 1.1))
plot(sort(pca.total_1), fat_35$floresta_35[order(pca.total_1)], type = "b",   
     xlab = NA, ylab = "Floresta (35 km)", xaxt = "n", bty = "l", 
     cex=sort(nspp.frag.total*.15), ylim = c(0, .5))
plot(sort(pca.total_1), fat_35$cultivo_35[order(pca.total_1)], type = "b",  
     xlab = NA, ylab = "Cultivo (35 km)", xaxt = "n", bty = "l", 
     cex=sort(nspp.frag.total*.15), ylim = c(0, .5))
plot(sort(pca.total_1), fat_35$agua_35[order(pca.total_1)], type = "b",  
     xlab = NA, ylab = "Água (35 km)", xaxt = "n", bty = "l", 
     cex=sort(nspp.frag.total*.15), ylim = c(0, .5))
par(mar = c(4.1, 4.1, .5, 1.1))
plot(sort(pca.total_1), fat_35$solo_35[order(pca.total_1)], type = "b",  
     xlab = "Eixo PCA 1", ylab = "Solo (35 km)", bty = "l", 
     cex=sort(nspp.frag.total*.15), ylim = c(0, .5))
dev.off()
##Figura. Proporções das áreas de cobertura pelos componentes da paisagem em 
##relação ao primeiro eixo da PCA de correlações entre esses componentes. Gráficos
##à esquerda para buffers de 5 km ao redor dos fragmentos e à direita buffers de 
##35 km. O tamanho dos pontos é diretamente proporcional ao número rarefeito de 
##espécies de aranhas em sub amostras de 50 indivíduos.

## Análise de variância mostrando o efeito marginalmente significante da paisagem
## sobre a riqueza de espécies (rarefação para 50 indivíduos):
summary(aov(nspp.frag.total ~ frag.amb.total$area + 
              frag.amb.total$sub_bosque + frag.amb.total$dossel + 
              pca.total_1))

## Gráficos dos resíduos parciais desse modelo de regressão múltipla:
jpeg("riq_frag_parciais.jpg", width = 17, height = 17, units = "cm", res = 300)
layout(matrix(1:4, 2, 2), respect = T)
par(mar = c(5, 4, 1, 1.5))
avPlot(aov(nspp.frag.total ~ frag.amb.total$area + 
             frag.amb.total$sub_bosque + frag.amb.total$dossel + 
             pca.total_1), variable = "pca.total_1", id = F, grid = F,
       col.lines = "gray50", bty="l", main = NA, cex = 1.5,
       xlab = "Eixo PCA 1 (resíduos parciais)",
       ylab = "Riqueza de espécies (resíduos parciais)")
avPlot(aov(nspp.frag.total ~ frag.amb.total$area + 
             frag.amb.total$sub_bosque + frag.amb.total$dossel + 
             pca.total_1), variable = "frag.amb.total$area", id = F, grid = F,
       lwd = 0, bty="l", main = NA, cex = 1.5, 
       xlab = "Área (resíduos parciais)",
       ylab = "Riqueza de espécies (resíduos parciais)")
avPlot(aov(nspp.frag.total ~ frag.amb.total$area + 
             frag.amb.total$sub_bosque + frag.amb.total$dossel + 
             pca.total_1), variable = "frag.amb.total$sub_bosque", id = F, grid = F,
       lwd = 0, bty="l", main = NA, cex = 1.5, 
       xlab = "Cobertura do sub-bosque (resíduos parciais)",
       ylab = "Riqueza de espécies (resíduos parciais)")
avPlot(aov(nspp.frag.total ~ frag.amb.total$area + 
             frag.amb.total$sub_bosque + frag.amb.total$dossel + 
             pca.total_1), variable = "frag.amb.total$dossel", id = F, grid = F,
       lwd = 0, bty="l", main = NA, cex = 1.5, 
       xlab = "Cobertura do dossel (resíduos parciais)",
       ylab = "Riqueza de espécies (resíduos parciais)")
dev.off()
##Figura. Efeitos parciais de variáveis ambientais relacionadas a estrutura do 
##habitat (área, cobertura da folhagem no sub-bosque e no dossel) e da paisagem 
##(primeiro eixo de uma PCA de correlações entre as áreas cobertas por floresta, 
##cultivo, solo exposto ou água em buffers de 5 km e 35 km). O modelo de regressão 
##múltipla revela efeito marginalmente significante da paisagem sobre a riqueza de 
##espécies (F = 9,24; gl = 1 e 3; p = 0,056).

View(spp.frag)
spp.frag.rel <- decostand(spp.frag, "total") #abundâncias relativas
View(frag.amb)
scores.pca <- rbind(scores(pca.frag)$sites, scores(pca.frag)$sites)

## Análise de redundância parcial baseada em distância (dbRDA)
### Para avaliar os efeitos das variáveis ambientais, usamos uma 
### análise de redundância baseada em distâncias (dbRDA), neste caso distâncias 
### Bray-Curtis. 
#### Usamos a função capscale do pacote vegan
?capscale
frag.cap <- capscale(spp.frag.rel ~ 
                       area + sub_bosque + dossel + 
                       scores.pca[, "PC1"] +  
                       Condition(method), frag.amb,
                     dist="bray", add = T)
frag.cap
summary(frag.cap)
### A variância total (inércia) na matriz de distâncias foi 3,315,
### sendo 10% condicionada ao método de coleta das aranhas (guarda-chuva 
### entomológico ou busca ativa noturna), 37% ligada as variáveis ambientais e
### 53% as diferenças em composição de espécies. 
### Os dois primeiros eixos da dbRDA recuperaram 71% da variância ligada as 
### variáveis ambientais (Accumulated constrained eingenvalues), sendo 42% no 
### primeiro eixo e 29% no segundo.
summary(frag.cap)$biplot #escores das variáveis ambientais
scores(frag.cap)$sites #escores dos locais de amostragem
scores(frag.cap)$species #escores das espécies
summary(abs(scores(frag.cap)$sp))
frag.cap.spp <- scores(frag.cap)$species[which(
  scores(frag.cap)$species[,1]>.09 | scores(frag.cap)$species[,2]>.09),]

jpeg("capscale.jpg", width = 15, height = 15, units = "cm", res = 300)
par(mar=c(4,4,0,0))
plot(scores(frag.cap)$sites, bty="n", xlim = c(-1.7, 1.7), ylim = c(-1.7, 1.7),
     xlab = "Eixo dbRDA 1 (42%)", ylab = "Eixo dbRDA 2 (29%)", type = "n")
points(scores(frag.cap)$sites, pch=19, cex=1.5,
       col = rep(c("gray", "black"), each = 8))
text(frag.cap.spp*3.2,  
     rownames(frag.cap.spp), cex = .5, adj = 1, font = 3)
text(summary(frag.cap)$biplot[,1:2]*1.5, 
     c("Área", "Folhagem (sub-bosque)", "Folhagem (dossel)", "Paisagem (eixo PCA 1)"),
     adj = .7, col = "gray", cex = .7)
arrows(0, 0, summary(frag.cap)$biplot[,1]*1.3, summary(frag.cap)$biplot[,2]*1.3,
       length = 0, col = "gray")
dev.off()
## Figura. Ordenação das amostras de aranhas em guarda-chuva entomológico (pontos 
## cinza) e busca ativa noturna (pontos pretos) por análise de redundância baseada 
## em distâncias (dbRDA) Bray-Curtis, considerando-se a abundância relativa de 
## indivíduos adultos. 

## Essa estrutura da comunidade de aranhas reduzida por dbRDA em função das 
## variáveis ambientais (área, cobertura da folhagem no sub-bosque e no dossel e 
## paisagem reduzida ao primeiro eixo de uma PCA) foi condicionada pelo método de 
## coleta. Tal modelo mostrou-se significativamente diferente do acaso em uma
## análise de variância por permutação (n = 1000; F = 1,75; gl = 4 e 10; p = 0,003):
anova.cca(frag.cap)
## Os efeitos parciais das variáveis ambientais:
anova.cca(frag.cap, by="term")
