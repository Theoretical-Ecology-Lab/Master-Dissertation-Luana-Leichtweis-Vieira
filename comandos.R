# Análises da comunidade de aranhas em fragmentos de mata numa paisagem dominada 
# por atividade agrária na transição do Cerrado com Floresta Atlântica

## Preparação
source("preparation.R")  #Habilita as funções e pacotes necessários

## Importação dos dados em spiders.xlsx usando o pacote readxl e salvando como spiders
spiders <- read_excel("spiders.xlsx", col_types = c("numeric", 
                                                    "text", "text", "text", "text", "numeric", 
                                                    "numeric", "numeric", "text", "text", 
                                                    "text", "text", "date", "text", "text", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric"))
View(spiders) #Os dados estão apresentados em uma data.frame em que cada linha equivale a uma (morfo)espécie de aranha em uma determinada amostra (sample).

str(spiders)  #Essa é a estrutura básica dos dados
colnames(spiders) #Nomes das colunas (variáveis)
spiders$method <- ifelse(spiders$method=="GUARDA CHUVA ENTOMOLOGICO", "batedor", "noturna") #Para facilitar substitui o nome dos métodos de coleta para algo "mais íntimo"

###Correção das proporções dos componentes da paisagem
rowSums(spiders[, 21:28])
View(spiders[143:156, 21:28]) #438109.2047 é 43.81092047
View(spiders[(394+997):(418+997), 21:28]) #438109.2047 é 43.81092047
spiders$CULTIVO_NDVI_5 <- ifelse(spiders$CULTIVO_NDVI_5==437953,437953/10000, spiders$CULTIVO_NDVI_5)
rowSums(spiders[c(248:340, 355:416), 21:28]) #soma dessas linhas tem que ser 200
rowSums(spiders[c(248:340, 355:416), 25:28]) #o problema está na escala de 5 Km
####Essas são as porcentagens corretas: 
spiders[c(248:340, 355:416), 25:28] <- data.frame(rep(26.19828, 155), rep(30.34322, 155), rep(25.21407, 155), rep(18.24444, 155))

## Número de amostras por método de coleta
length(table(spiders$sample[which(spiders$method=="batedor")]))
# Obtivemos 82 amostras com auxílio de guarda-chuva entomológico. Cada amostra era o conjunto de aranhas capturadas em 20 arbustos ou galhos de árvores batidos vigorosamente em cada ponto amostral (parcela de 10 m x 10 m) nos fragmentos florestais.

# Amostras com guarda-chuva por localidade (fragmento):
rowSums(table(spiders$locality[which(spiders$method == "batedor")], 
              spiders$sample[which(spiders$method == "batedor")])>0)

length(table(spiders$sample[which(spiders$method=="noturna")]))
# Obtivemos 78 amostras por busca ativa noturna. Cada amostra era o conjunto de aranhas capturadas por duas pessoas treinadas, em parcelas de 30 m x 10 m, com auxílio de lanterna de cabeça, potes e pinças, durante uma hora de procura em todos os substratos do chão até cerca de 2 m de altura.

# Amostras com busca ativa noturna por localidade (fragmento):
rowSums(table(spiders$locality[which(spiders$method == "noturna")], 
              spiders$sample[which(spiders$method == "noturna")])>0)

## Exploração inicial dos dados da comunidade

### Frequência de ocorrência das espécies de aranhas
FO_bat <- table(spiders$morph_sp[which(spiders$method=="batedor")])
FO_bat #Frequência de ocorrência (FO) das espécies em guarda-chuva entomológico.
sort(FO_bat)  #Ordem crescente da FO. A espécie mais frequente foi M. rubripes com adultos capturados em 66 amostras de guarda-chuva entomológico.
FO_not <- table(spiders$morph_sp[which(spiders$method=="noturna")]) 
FO_not  #frequência de ocorrência das espécies em busca ativa noturna.
sort(FO_not)  #Ordem crescente da FO. A espécie mais frequente foi M. rubripes com adultos capturados em 75 amostras de guarda-chuva entomológico.

#### Figura pronta para publicação em formato jpeg. 
#### Distribuição da frequência de ocorrência das espécies de aranhas em amostras obtidas com busca ativa noturna (n = 78) ou guarda-chuva entomológico (n = 82).
jpeg("freq_ocorr.jpg", width = 20, height = 15, units = "cm", res = 300)
par(mar = c(7.1, .1, .1, 0))
layout(matrix(c(3,1,3,2), 2, 2, byrow = TRUE), widths = lcm(c(1, 18)))
plot(sort(FO_bat, decreasing = T),
     xlab = NA, type="h", mgp = c(1.5, .5, 0), cex.lab = .7,
     ylab = NA, xaxt = "n", yaxt = "n")
axis(1, 1:length(FO_bat), labels(sort(FO_bat, decreasing = T))[[1]], font = 3, 
     cex.axis = .6, las=2)
axis(2, c(0, 20, 40, 60), c(0, 20, 40, 60), cex.axis = .6)
text(length(FO_bat)-1, 45, "Guarda-chuva\nentomológico", 
     srt = 90)

plot(sort(FO_not, decreasing = T),
     xlab = NA, type="h", mgp = c(1.5, .5, 0), cex.lab = .7,
     ylab = NA, xaxt = "n", yaxt = "n")
axis(1, 1:length(FO_not), labels(sort(FO_not, decreasing = T))[[1]], font = 3, 
     cex.axis = .6, las=2)
axis(2, c(0, 20, 40, 60), c(0, 20, 40, 60), cex.axis = .6)
text(length(FO_not), 40, "Busca ativa noturna", 
     srt = 90)

plot(1, type="n", xaxt="n", yaxt="n", ylab = NA, xlab = NA, bty="n")
mtext("Frequência de ocorrência das espécies de aranhas", 2, cex = .7)
dev.off()

### Abundância das espécies de aranhas
abund_bat <- tapply(
  spiders$adults[which(spiders$method=="batedor")], 
  spiders$morph_sp[which(spiders$method=="batedor")], sum)
abund_bat  #Abundância das espécies em guarda-chuva entomológico.
sort(abund_bat)  #Ordem crescente da abundância. A espécie mais abundante foi U. penicillatus com 230 adultos capturados em 51 amostras de guarda-chuva entomológico.

abund_not <- tapply(
  spiders$adults[which(spiders$method=="noturna")], 
  spiders$morph_sp[which(spiders$method=="noturna")], sum)
abund_not  #Abundância das espécies em busca ativa noturna.
sort(abund_not)  #Ordem crescente da abundância. A espécie mais abundante foi U. penicillatus com 812 adultos capturados em 72 amostras com busca ativa noturna.

#### Figura pronta para publicação em formato jpeg. 
#### Distribuição de abundância das espécies de aranhas em amostras obtidas com busca ativa noturna (n = 78) ou guarda-chuva entomológico (n = 82).
jpeg("abund.jpg", width = 20, height = 15, units = "cm", res = 300)
par(mar = c(7.1, .1, .1, 0))
layout(matrix(c(3,1,3,2), 2, 2, byrow = TRUE), widths = lcm(c(1, 18)))
plot(sort(abund_bat, decreasing = T),  bty="n", type = "h", 
     xlab = NA, mgp = c(1.5, .5, 0), 
     ylab = NA, xaxt = "n", yaxt = "n")
axis(1, 1:length(abund_bat), labels(sort(abund_bat, decreasing = T))[[1]], font = 3, 
     cex.axis = .6, las=2)
axis(2, c(0, 50, 100, 150, 200), c(0, 50, 100, 150, 200), cex.axis = .6)
text(length(abund_bat)-1, 100, "Guarda-chuva\nentomológico", 
     srt = 90)

plot(sort(abund_not, decreasing = T), bty="n",
     xlab = NA, type="h", mgp = c(1.5, .5, 0), 
     ylab = NA, xaxt = "n", yaxt = "n")
axis(1, 1:length(abund_not), labels(sort(abund_not, decreasing = T))[[1]], font = 3, 
     cex.axis = .6, las=2)
axis(2, c(0, 200, 400, 600, 800), c(0, 200, 400, 600, 800), cex.axis = .6)
text(length(abund_not), 400, "Busca ativa noturna", 
     srt = 90)

plot(1, type="n", xaxt="n", yaxt="n", ylab = NA, xlab = NA, bty="n")
mtext("Abundância das espécies de aranhas", 2, cex = .7)
dev.off()

#### TUDO JUNTO
jpeg("freq_abund.jpg", width = 25, height = 20, units = "cm", res = 300)
par(mar = c(8.1, .1, .1, 0))
layout(matrix(c(3,1,3,2,6,4,6,5), 4, 2, byrow = TRUE), widths = lcm(c(1, 18)))
plot(sort(abund_bat, decreasing = T),  bty="n", type = "h", 
     xlab = NA, mgp = c(1.5, .5, 0), 
     ylab = NA, xaxt = "n", yaxt = "n")
axis(1, 1:length(abund_bat), labels(sort(abund_bat, decreasing = T))[[1]], font = 3, 
     cex.axis = .7, las=2)
axis(2, c(0, 50, 100, 150, 200), c(0, 50, 100, 150, 200), cex.axis = .7)
text(length(abund_bat)-1, 100, "Guarda-chuva\nentomológico", cex = .8,
     srt = 90)

plot(sort(abund_not, decreasing = T), bty="n",
     xlab = NA, type="h", mgp = c(1.5, .5, 0), 
     ylab = NA, xaxt = "n", yaxt = "n")
axis(1, 1:length(abund_not), labels(sort(abund_not, decreasing = T))[[1]], font = 3, 
     cex.axis = .7, las=2)
axis(2, c(0, 200, 400, 600, 800), c(0, 200, 400, 600, 800), cex.axis = .7)
text(length(abund_not), 400, "Busca ativa noturna", cex = .8,
     srt = 90)

plot(1, type="n", xaxt="n", yaxt="n", ylab = NA, xlab = NA, bty="n")
mtext("Abundância das espécies de aranhas", 2, cex = .8)

plot(sort(FO_bat, decreasing = T),
     xlab = NA, type="h", mgp = c(1.5, .5, 0), cex.lab = .7,
     ylab = NA, xaxt = "n", yaxt = "n")
axis(1, 1:length(FO_bat), labels(sort(FO_bat, decreasing = T))[[1]], font = 3, 
     cex.axis = .7, las=2)
axis(2, c(0, 20, 40, 60), c(0, 20, 40, 60), cex.axis = .7)
text(length(FO_bat)-1, 45, "Guarda-chuva\nentomológico", cex = .8,
     srt = 90)

plot(sort(FO_not, decreasing = T),
     xlab = NA, type="h", mgp = c(1.5, .5, 0), cex.lab = .7,
     ylab = NA, xaxt = "n", yaxt = "n")
axis(1, 1:length(FO_not), labels(sort(FO_not, decreasing = T))[[1]], font = 3, 
     cex.axis = .7, las=2)
axis(2, c(0, 20, 40, 60), c(0, 20, 40, 60), cex.axis = .7)
text(length(FO_not), 40, "Busca ativa noturna", cex = .8,
     srt = 90)

plot(1, type="n", xaxt="n", yaxt="n", ylab = NA, xlab = NA, bty="n")
mtext("Frequência de ocorrência das espécies de aranhas", 2, cex = .8)
dev.off()


## Matriz de espécies por observação
names(spiders) #variáveis no banco de dados (detalhes no arquivo de metadados: metadata.txt)
abund <- aggregate(spiders$adults, 
                   list(spiders$long, spiders$lat, spiders$method, spiders$met2,
                        spiders$site, spiders$date, spiders$sample), sum)
colnames(abund) <- c("long", "lat", "method", "met2", "site", "date", "sample", "nindiv")
View(abund) #número de aranhas por observação (sample) | n = 160 (82 batedor e 78 noturna)
dim(abund)
names(abund) 
sum(abund$nindiv)  #Número total de aranhas adultas

### Esforço por data e local com dois métodos de amostragem
colSums(table(abund$date, abund$site)>0) #Número de dias por local
aggregate(abund$nindiv, list(abund$method, abund$site), length) #Número de amostras por local e método
aggregate(abund$nindiv, list(abund$method, abund$date), length) #Número de amostras por data e método
aggregate(abund$nindiv, list(abund$method, abund$date, abund$site), length) #Número de amostras por local, data e método
rowSums(table(abund$date[which(abund$method=="batedor")], 
              abund$sample[which(abund$method=="batedor")])) #número de amostras com batedor por data
rowSums(table(abund$date[which(abund$method=="noturna")], 
              abund$sample[which(abund$method=="noturna")])) #número de amostras com busca ativa noturna por data
rowSums(table(abund$site[which(abund$method=="batedor")], 
              abund$sample[which(abund$method=="batedor")])) #número de amostras com batedor por local
rowSums(table(abund$site[which(abund$method=="noturna")], 
              abund$sample[which(abund$method=="noturna")])) #número de amostras com busca ativa noturna por local

### Agora sim a matriz de espécies por observação (spp)
#### Primeiro replicamos os dados para cada uma das 3775 aranha adultas,
#### obtendo um banco de dados por aranha adulta:

sample <- rep(spiders$sample, spiders$adults)
morph_sp <- rep(spiders$morph_sp, spiders$adults) #espécie de cada indivíduo
lat <-  rep(spiders$lat, spiders$adults)
long <-  rep(spiders$long, spiders$adults)
altitude <- rep(spiders$altitude, spiders$adults)
perimetro <- rep(spiders$perimetro, spiders$adults)
area <- rep(spiders$area, spiders$adults)
fragmentos <- rep(spiders$locality, spiders$adults)
site <- rep(spiders$site, spiders$adults) 
date <- rep(spiders$date, spiders$adults) 
method <- rep(spiders$method, spiders$adults) 
met2 <- rep(spiders$met2, spiders$adults)
floresta_35 <- rep(spiders$FLORESTADA_NDVI_35, spiders$adults)
cultivo_35 <- rep(spiders$CULTIVO_NDVI_35, spiders$adults)
solo_35 <- rep(spiders$CULTIVO_SOLO_NDVI_35, spiders$adults)
agua_35 <- rep(spiders$AGUA_SOLO_NDVI_35, spiders$adults)
floresta_5 <- rep(spiders$FLORESTADA_NDVI_5, spiders$adults)
cultivo_5 <- rep(spiders$CULTIVO_NDVI_5, spiders$adults)
solo_5 <- rep(spiders$CULTIVO_SOLO_NDVI_5, spiders$adults)
agua_5 <- rep(spiders$AGUA_SOLO_NDVI_5, spiders$adults)
manchas <- rep(spiders$N_MANCHAS_CONTINUAS, spiders$adults)
sub_bosque <- rep(rowMeans(cbind(spiders$razão_sub_bosque1, spiders$razão_sub_bosque2), 
                           na.rm = T), spiders$adults)
dossel <- rep(rowMeans(cbind(spiders$razão_dossel1, spiders$razão_dossel2, 
                             spiders$razão_dossel3, spiders$razão_dossel4, 
                             spiders$razão_dossel5), na.rm = T), spiders$adults)

dados <- data.frame(sample, morph_sp, long, lat, altitude, 
                    perimetro, area, fragmentos, site, date, 
                    method, met2, floresta_35, cultivo_35, 
                    solo_35, agua_35, floresta_5, cultivo_5, 
                    solo_5, agua_5, manchas, sub_bosque, dossel)
View(dados)   #dados por aranha
dim(dados)   #3775 aranhas e 23 atributos
names(dados)

#### Agora usamos esse banco de dados para gerar a matriz 
#### observação X espécies (spp)

spp <- matrix(table(dados$sample, dados$morph_sp), 160, 156)
colnames(spp) <- levels(dados$morph_sp)
View(spp)
sum(spp)
dim(spp) #156 espécies em 160 amostras

#### Espécies em guarda-chuva entomológico
spp_bat <- matrix(table(dados$sample[which(dados$method=="batedor")], 
                        dados$morph_sp[which(dados$method=="batedor")]), 82, 156)
dim(spp_bat)
sort(rowSums(spp_not))
colnames(spp_bat) <- levels(dados$morph_sp)
View(spp_bat)
colSums(spp_bat)  #algumas espécies (61) não ocorreram em guarda-chuva entomológico
which(colSums(spp_bat)==0)
length(which(colSums(spp_bat)==0))
1-length(which(colSums(spp_bat)==0))/156 #ca. 61% das espécies ocorreram em guarda-chuva entomológico

spp_not <- matrix(table(dados$sample[which(dados$method=="noturna")], 
                        dados$morph_sp[which(dados$method=="noturna")]), 78, 156)
colnames(spp_not) <- levels(dados$morph_sp)
View(spp_not)
colSums(spp_not)  #algumas espécies (37) não ocorreram em busca atica noturna
which(colSums(spp_not)==0)
length(which(colSums(spp_not)==0))
1-length(which(colSums(spp_not)==0))/156 #ca. 76% das espécies ocorreram em busca ativa noturna

## Riqueza total em espécies (curva de acumulação por rarefação)
raref.tot <- specaccum(spp, method = "rarefaction") 
raref.tot #3775 aranhas de 156 espécies
raref.bat <- specaccum(spp_bat, method = "rarefaction") 
raref.bat #994 aranhas de 95 espécies em batedor
raref.not <- specaccum(spp_not, method = "rarefaction") 
raref.not #2781 aranhas de 119 espécies

### Plotar e salvar as curvas de acumulação de espécies:
jpeg("acumula.jpg", width=20, height=15, units="cm", res=300)
plot(raref.tot, ci.type="polygon", ci.col="gray80", ci.lty=0, 
     xlab="Número de aranhas adultas", 
     ylab="Número de espécies", bty="n", xvar="individuals")
plot(raref.bat, ci.type="polygon", ci.col="gray60", ci.lty=0, 
     xvar="individuals", add = T)
plot(raref.not, ci.type="polygon", ci.col="gray40", ci.lty=0, 
     xvar="individuals", add = T)
text(c(3500, 2500, 400), c(140, 100, 100), 
     c("Total", "Busca ativa\nnoturna", 
       "Guarda-chuva\nentomológico"), cex=.9)
dev.off()
#### Figura. Acumulação de 156 espécies em amostra de 3775 aranhas (total). Em amostras com guarda-chuva entomológico foram 1208 aranhas de 111 espécies e em amostras por busca ativa noturna 2567 aranhas de 118 espécies. As área em tons de cinza indicam intervalos de confiança de 95% para o número médio de espécies estimado por rarefação.

## Espécies por local e data (abundância média)

spp.local.data <- aggregate(spp, list(abund$met2, abund$long, abund$lat, abund$site, abund$date),
                            mean)
colnames(spp.local.data)[1:5] <- c("met2", "long", "lat", "site", "date")
View(spp.local.data)
dim(spp.local.data) # 39 observações (local e data) das 156 espécies (5 + 156 = 161)

### Esforço por data de coleta

range(spp.local.data$date)
range(unclass(spp.local.data$date)) #tempo desde 01/01/1970
hist(spp.local.data$date, breaks=10, freq = T) #esforço amostral durante o período de estudo

#### se quiser apresentar como figura jpg
jpeg("esforcoMensal.jpg", width = 22, height = 17, units = "cm", res = 300)
par(mar = c(5, 4, 1, .1))
hist(spp.local.data$date, 
     breaks=seq(min(spp.local.data$date), 
                max(spp.local.data$date), length.out = 11), 
     freq = T, 
     xlab = NA,
     ylab = "Número de pontos amostrados", main = NULL, xaxt= "n")
axis(1, seq(min(spp.local.data$date), max(spp.local.data$date), length.out = 11), c("nov/16", "dez/16", "jan/17", "fev/17", 
                "mar/17", "abr/17", "mai/17", "jun/17", 
                "jul/17", "ago/17", "set/17"))
dev.off()
### Figura. Esforço mensal de coleta de aranhas na transição do Cerrado com Floresta Atlântica.

## Esforço por local

table(spp.local.data$site) #Datas por local

## Só as espécies por local e data

spp.l.d <- spp.local.data[, -(1:5)]
View(spp.l.d)
dim(spp.l.d)  #156 espécies em 39 observações (um local em uma data)
which(rowSums(spp.l.d)==0) #nenhuma observação vazia

#### Número de espécies por observação
nspp.l.d <- rowSums(spp.l.d > 0)
summary(nspp.l.d)
hist(nspp.l.d, breaks = 20)
abline(v = 20, lwd = 3, lty = 2) #mediana do número de espécies por observação

### Espécies por local (15 locais amostrados)
spp.l <- aggregate(spp.l.d, list(spp.local.data$site), mean) [, -1]
spp.l #espécies em ordem alfabética dos locais
dim(spp.l)

#### Número de espécies por local

nspp.l <- rowSums(spp.l > 0)
summary(nspp.l)
hist(nspp.l, breaks = 10)
abline(v = 31, lwd = 3, lty = 2) #mediana do número de espécies por local

# Área de estudo no Google Maps.

coord.l <- aggregate(dados[, c("long", "lat")], 
                     list(dados$site), mean)
coord.l #coordenadas em ordem alfabética dos locais

## Obtendo a imagem com a função get_maps() do pacote ggmap

imagem <- get_map(location = colMeans(coord.l[, -1]), 
                  zoom = 9, maptype = "hybrid")

### Salvando essa imagem com a função ggmap() do pacote ggmap

sat.image <- ggmap(imagem) #

### Adicionando os locais de amostragem com tamanho proporcional ao número de espécies de aranhas

image.local <- sat.image +
  geom_point(aes(x = long, y = lat), data = coord.l, 
             size = nspp.l[order(coord.l[, 1])]*.08, colour = "orange", shape = 21) + 
  ylab("Latitude") + 
  xlab("Longitude")
image.local
ggsave("riqueza.jpg", width=15, height=15, unit="cm", dpi=300)
#### Figura. Distribuição do número de espécies (entre seis e 46) em 15 locais de oito fragmenstos florestais na transição do Cerrado com Floresta Atlântica. O tamanho dos pontos é diretamente proporcional ao número médio de espécies de aranhas capturadas em guarda-chuva entomológico e busca ativa noturna.

#####Junto com as curvas de acumulação de espécies.
jpeg("acumula.jpg", width=20, height=15, units="cm", res=300)
plot(raref.tot, ci.type="polygon", ci.col="gray80", ci.lty=0, 
     xlab="Número de aranhas adultas", 
     ylab="Número de espécies", bty="n", xvar="individuals")
plot(raref.bat, ci.type="polygon", ci.col="gray60", ci.lty=0, 
     xvar="individuals", add = T)
plot(raref.not, ci.type="polygon", ci.col="gray40", ci.lty=0, 
     xvar="individuals", add = T)
text(c(3500, 2500, 400), c(140, 100, 100), 
     c("Total", "Busca ativa\nnoturna", 
       "Guarda-chuva\nentomológico"), cex=.9)
text(3500, 50, "B")
dev.off()

image.local +
  annotate("text", x=-54.2, y=-22.8, label = "A", color = "white") 
ggsave("riqueza.jpg", width=15, height=15, unit="cm", dpi=300)
######

###### Imagens dos fragmentos

balneario <- ggmap(get_map(location = coord.l[1, -1], 
                           zoom = 15, maptype = "hybrid"))
balneario +
annotate("text", x=-54.825, y=-22.654, 
         label = "Balneário\n3 guarda-chuva entomológico\n3 busca ativa noturna", 
         color = "white", adj = 0, size = 3) +
annotate("point", x=coord.l[1, "long"], y=coord.l[1, "lat"], 
         color = "white", adj = 0, size = 3, shape = 22)
ggsave("balneario.jpg", width=10, height=10, unit="cm", dpi=300)

clube <- ggmap(get_map(location = coord.l[2, -1], 
                           zoom = 15, maptype = "hybrid"))
clube +
  annotate("text", x=-54.841, y=-22.64, 
           label = "Clube\n3 guarda-chuva entomológico\n3 busca ativa noturna", 
           color = "white", adj = 0, size = 3)+
  annotate("point", x=coord.l[2, "long"], y=coord.l[2, "lat"], 
           color = "white", adj = 0, size = 3, shape = 22)
ggsave("clube.jpg", width=10, height=10, unit="cm", dpi=300)


ecosystem <- ggmap(get_map(location = coord.l[3, -1], 
                       zoom = 15, maptype = "hybrid"))
ecosystem +
  annotate("text", x=-54.913, y=-22.25, 
           label = 
           "Ecosystem\n12 guarda-chuva entomológico\n13 busca ativa noturna", 
           color = "white", adj = 0, size = 3)+
  annotate("point", x=coord.l[3:5, "long"], y=coord.l[3:5, "lat"], 
           color = "white", adj = 0, size = 3, shape = 22)
ggsave("ecosystem.jpg", width=10, height=10, unit="cm", dpi=300)


Dania <- ggmap(get_map(location = coord.l[6, -1], 
                           zoom = 15, maptype = "hybrid"))
Dania +
  annotate("text", x=-54.488, y=-22.11, 
           label = 
             "Dânia\n3 guarda-chuva entomológico\n3 busca ativa noturna", 
           color = "white", adj = 0, size = 3)+
  annotate("point", x=coord.l[6, "long"], y=coord.l[6, "lat"], 
           color = "white", adj = 0, size = 3, shape = 22)
ggsave("dania.jpg", width=10, height=10, unit="cm", dpi=300)


Joel <- ggmap(get_map(location = coord.l[8, -1], 
                       zoom = 15, maptype = "hybrid"))
Joel +
  annotate("text", x=-54.917, y=-22.2825, 
           label = 
             "Joel\n24 guarda-chuva entomológico\n21 busca ativa noturna", 
           color = "white", adj = 0, size = 3)+
  annotate("point", x=coord.l[7:9, "long"], y=coord.l[7:9, "lat"], 
           color = "white", adj = 0, size = 3, shape = 22)
ggsave("Joel.jpg", width=10, height=10, unit="cm", dpi=300)


azulao <- ggmap(get_map(location = coord.l[11, -1], 
                      zoom = 15, maptype = "hybrid"))
azulao +
  annotate("text", x=-54.929, y=-22.204, 
           label = 
             "Azulão\n16 guarda-chuva entomológico\n19 busca ativa noturna", 
           color = "white", adj = 0, size = 3)+
  annotate("point", x=coord.l[10:12, "long"], y=coord.l[10:12, "lat"], 
           color = "white", adj = 0, size = 3, shape = 22)
ggsave("azulao.jpg", width=10, height=10, unit="cm", dpi=300)


Samu <- ggmap(get_map(location = coord.l[13, -1], 
                        zoom = 15, maptype = "hybrid"))
Samu +
  annotate("text", x=-54.915, y=-22.238, 
           label = 
             "Samu\n15 guarda-chuva entomológico\n10 busca ativa noturna", 
           color = "white", adj = 0, size = 3)+
  annotate("point", x=coord.l[13:14, "long"], y=coord.l[13:14, "lat"], 
           color = "white", adj = 0, size = 3, shape = 22)
ggsave("samu.jpg", width=10, height=10, unit="cm", dpi=300)


reserva <- ggmap(get_map(location = c(-54.6452, -22.98612), 
                      zoom = 15, maptype = "hybrid"))
reserva +
  annotate("text", x=-54.655, y=-22.98, 
           label = 
             "Reserva\n6 guarda-chuva entomológico\n6 busca ativa noturna", 
           color = "white", adj = 0, size = 3)+
  annotate("point", x=coord.l[15, "long"], y=coord.l[15, "lat"], 
           color = "white", adj = 0, size = 3, shape = 22)
ggsave("reserva.jpg", width=10, height=10, unit="cm", dpi=300)


### Espécies por local e data em guarda-chuva entomológico

abund.bat <- abund[which(abund$method=="batedor"), ]
spp.local.data.bat <- aggregate(spp_bat, 
                                list(abund.bat$long, 
                                     abund.bat$lat,
                                     abund.bat$site, 
                                     abund.bat$date),mean)
colnames(spp.local.data.bat)[1:4] <- c("long", "lat", "site", 
                                       "date")
View(spp.local.data.bat)
dim(spp.local.data.bat) # 20 observações (local e data)

#### Só as espécies por local e data em guarda-chuva entomológico

spp.l.d.bat <- spp.local.data.bat[, -(1:4)]
View(spp.l.d.bat)
dim(spp.l.d.bat)  #156 espécies em 20 observações (um local em uma data)
which(rowSums(spp.l.d.bat)==0) #nenhuma observação vazia
which(colSums(spp.l.d.bat)==0) #espécies ausentes
length(which(colSums(spp.l.d.bat)==0)) #61 espécies ausentes

##### Número de espécies

nspp.l.d.bat <- rowSums(spp.l.d.bat > 0)
summary(nspp.l.d.bat)
hist(nspp.l.d.bat, breaks = 10)
abline(v = 14.5, lwd = 3, lty = 2) #mediana do número de espécies por observação

#### Espécies nos 11 locais amostrados com guarda-chuva entomológico

spp.l.bat <- aggregate(spp.l.d.bat, 
                       list(spp.local.data.bat$site), mean)[, -1]
spp.l.bat #espécies em ordem alfabética dos locais
dim(spp.l.bat)

##### Número de espécies por local

nspp.l.bat <- rowSums(spp.l.bat > 0)
summary(nspp.l.bat)
hist(nspp.l.bat, breaks = 10)
abline(v = 18, lwd = 3, lty = 2) #mediana do número de espécies por local

### Espécies por local e data em busca ativa noturna

abund.not <- abund[which(abund$method=="noturna"), ]
spp.local.data.not <- aggregate(spp_not, 
                                list(abund.not$long, 
                                     abund.not$lat,
                                     abund.not$site, 
                                     abund.not$date), mean)
colnames(spp.local.data.not)[1:4] <- c("long", "lat", "site", "date")
View(spp.local.data.not)
dim(spp.local.data.not) # 22 observações (local e data)

#### Só as espécies por local e data em busca ativa noturna

spp.l.d.not <- spp.local.data.not[, -(1:4)]
View(spp.l.d.not)
dim(spp.l.d.not)  #156 espécies em 22 observações (um local em uma data)
which(rowSums(spp.l.d.not)==0) #nenhuma observação vazia
which(colSums(spp.l.d.not)==0) #espécies ausentes
length(which(colSums(spp.l.d.not)==0)) #37 espécies ausentes

##### Número de espécies

nspp.l.d.not <- rowSums(spp.l.d.not > 0)
summary(nspp.l.d.not)
hist(nspp.l.d.not, breaks = 10)
abline(v = 24, lwd = 3, lty = 2) #mediana do número de espécies por observação

#### Espécies nos 15 locais amostrados com busca ativa noturna

spp.l.not <- aggregate(spp.l.d.not, 
                       list(spp.local.data.not$site), mean)[, -1]
spp.l.not #espécies em ordem alfabética dos locais
dim(spp.l.not)

##### Número de espécies por local

nspp.l.not <- rowSums(spp.l.not > 0)
summary(nspp.l.not)
hist(nspp.l.not, breaks = 10)
abline(v = 26.5, lwd = 3, lty = 2) #mediana do número de espécies por local



# Variação em composição de espécies (diversidade Beta) por observação (local e data)
## Primeiro transformamos para abundância relativa

spp.rel <- decostand(spp.l.d, "total")
rowSums(spp.rel) #só pra conferir: todos 1
colMeans(spp.rel) #abundância relativa média (por observação) de cada espécie
colSums(spp.rel>0) #frequência de ocorrência por observação

## Ordenar os pontos amostrados em cada data usando NMDS pelas diferenças Bray-Curtis em composição de espécies: 

#nmds.1d <- metaMDS(spp.rel, k=1) #Escalonamento multidimensional não-métrico (NMDS) em uma dimensão
nmds.1d

#nmds.2d <- metaMDS(spp.rel) #Escalonamento multidimensional não-métrico (NMDS) em duas dimensões
nmds.2d

## Shepard plot

stressplot(nmds.1d)

### Diagrama de Shepard para as distâncias obtidas no NMDS acima com R2 = 0,91 para o ajuste não-métrico.
### Ou seja, as distâncias entre pares de observações na ordenação recuperaram 91% da variância na matriz de distâncias Bray-Curtis.

stressplot(nmds.2d)

### Para NMDS em duas dimensões recuperou 96% da variância.
### Decidimos avaliar o padrão recuperado pela ordenação em duas dimensões.
#### Para isso vamos salvar os escores dessa ordenação (nesse caso, os vetores que definem os eixos do gráfico de ordenação) 

nmds.escores <- scores(nmds.2d)
nmds.escores # NMDS scores ("posição dos pontos no plano da ordenação")

#### Definimos os loadings das espécies com a função wascores do pacote vegan. 
#### Os loadings correspondem a correlação das espécies com a ordenação.

spp.loads <- wascores(nmds.escores, spp.rel)
spp.loads #Loadings das espécies ("posição das espécies no plano da ordenação"). Indica quanto cada espécie contribuiu para a ordenação das amostras.

#### Vamos avaliar o gráfico da ordenação com todas as espécies:

plot(nmds.escores, bty = "n",  col = "gray",
     xlim = c(-1.5, 2),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
text(spp.loads,rownames(spp.loads),
     col = "gray20", cex = .6, font = 3)

##### Muitas espécies sobrepostas e próximas ao zero de correlação.
##### Vamos selecionar as espécies com maiores correlações
spp.loads1 <- spp.loads[, 1]
spp.loads2 <- spp.loads[, 2]
spp.loads_util <- spp.loads[which(abs(spp.loads1) > .4), ]/max(spp.loads)
View(spp.loads_util)
plot(nmds.escores, bty = "n",  col = "gray",
     xlim = c(-1.5, 2),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
tremida <- replicate(nrow(spp.loads_util), jitter(1.5, 10))
tremida <- tremida2
tremida2 <- replicate(nrow(spp.loads_util), jitter(1.5, 25))
tremida2
text(spp.loads_util*tremida,rownames(spp.loads_util),
     col = "gray20", cex = .6, font = 3)

###### Correção da posição dos pontos no gráfico para evitar sobreposição#######
####### Eu fui olhando no gráfico a seguir já em jpg e fui ajustando a altura de cada espécie para tirar as sobreposições usando os seguintes comandos
sort(rownames(spp.loads_util)) #facilta achar o nome da espécie para colar no comando abaixo
spp.loads_util[which(rownames(spp.loads_util)=="Tmarus sp.1"), 2]
spp.loads_util[which(rownames(spp.loads_util)=="Tmarus sp.1"), 2] <- -0.335

### Para diferenciar as observações (local e data) pelos métodos de coleta:
pchs <- ifelse(spp.local.data$met2=="batedor", 21, 19) #círculo para batedor e ponto preenchido para noturna e ambos métodos
cores <- ifelse(spp.local.data$met2=="batnot", "gray", "black") #ambos métodos em cinza, o resto preto
jpeg("nmds.jpg", width=15, height=15, units="cm", res=300)
par(mar = c(4, 4, .1, .1))
plot(nmds.escores, bty = "n",  col = cores, pch = pchs,
     xlim = c(-1.6, 1.5),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
text(spp.loads_util*tremida, rownames(spp.loads_util),
     col = "gray20", cex = .5, font = 3)
dev.off()
#### Figura. Ordenação das amostras (n = 39) de aranhas por análise não-métrica de escalas multidimensionais (NMDS) a partir de distâncias Bray-Curtis pela abundância relativa. 
#### Cada amostra (pontos no gráfico) correspondeu a um ponto de amostragem (n = 15 em 8 fragmentos) em uma data (n = 35 entre novembro/2016 e agosto/2017).  
#### A posição do nome indica quanto cada espécie contribuiu para a ordenação das amostras. 
#### Pontos abertos correspondem a amostras obtidas com guarda-chuva entomológico, pontos pretos com busca ativa noturna e pontos cinzas com ambos métodos.

##### Essa ordenação recuperou a diferença em composição de espécies entre os métodos de captura das aranhas. Isso era de se esperar em função da particularidade de cada método. Enquanto com guarda-chuva entomológico aranhas são capturadas na folhagem arbustivo-arbórea durante o dia, com busca ativa noturna todos os substratos do chão a cerca de 2 m de altura são amostrados. 


##########AGORA VAMOS PENSAR SOMENTE NAS AMOSTRAS
View(abund)
abund$nindiv #número de aranhas adultas por amostra
summary(abund$nindiv) #1/4 das amostras tem até 11 adultas; 
                      #50% até 19,5; 
                      #em média 23,59;
                      #3/4 até 33,25;
hist(abund$nindiv, main = NA, xlab = "Número de aranhas adultas", 
     ylab = "Número de amostras") 
abline(v=19.5, lwd=2, lty=2) #mediana do número de aranhas adultas nas amostras
text(80, 30, "Total")

bat <- abund[which(abund$method=="batedor"), ]
View(bat)
bat$nindiv
summary(bat$nindiv)
hist(bat$nindiv, main = NA, xlab = "Número de aranhas adultas", 
     ylab = "Número de amostras")
abline(v=12, lwd=2, lty = 2)

not <- abund[which(abund$method=="noturna"), ]
View(not)
not$nindiv
summary(not$nindiv)
hist(not$nindiv, main = NA, xlab = "Número de aranhas adultas", 
     ylab = "Número de amostras")
abline(v=33.5, lwd=2, lty = 2)
#A distribuição do número de indivíduos nas amostras por busca ativa noturna é mais simétrica do que aquela com guarda-chuva entomológico.

jpeg("distribui_metodo.jpg", width = 15, height = 25, units = "cm", res = 300)
par(mar = c(5.1, 5.1, .1, .1), mfrow = c(3, 1))
hist(not$nindiv, main = NA, xlab = NA, cex.lab = 2, cex.axis = 2,
     ylab = "Número de amostras", xlim = c(0, 100), breaks = 10, xaxt = "n")
abline(v=33.5, lwd=2, lty = 2)
text(80, 15, "Busca ativa\nnoturna", cex = 2)
hist(bat$nindiv, main = NA, xlab = NA, cex.lab = 2, cex.axis = 2, 
     ylab = "Número de amostras", xlim = c(0, 100), breaks = 5, xaxt = "n")
abline(v=12, lwd=2, lty = 2)
text(80, 20, "Guarda-chuva\nentomológico", cex = 2)
hist(abund$nindiv, main = NA, xlab = "Número de aranhas adultas", 
     ylab = "Número de amostras", cex.lab = 2, cex.axis = 2) 
abline(v=19.5, lwd=2, lty=2) #mediana do número de aranhas adultas nas amostras
text(80, 30, "Total", cex = 2)
dev.off()

#mesmo quando excluímos amostras com menos de 5 aranhas:
bat_util <- abund[which(abund$method=="batedor" & abund$nindiv>5), ]
View(bat_util)
bat_util$nindiv
summary(bat_util$nindiv)
hist(bat_util$nindiv, main = NA, xlab = "Número de aranhas adultas", 
     ylab = "Número de amostras")
abline(v=13, lwd=2, lty = 2)

View(spp)
rowSums(spp>0) #número de espécies
summary(rowSums(spp>0))

View(spp_bat) #espécies nas 82 amostras com guarda-chuva entomológico
rowSums(spp_bat>0) #número de espécies
summary(rowSums(spp_bat>0))
hist(rowSums(spp_bat>0))
abline(v=5, lwd=2, lty = 2)

View(spp_not) #espécies nas 78 amostras com busca ativa noturna
rowSums(spp_not>0) #número de espécies
summary(rowSums(spp_not>0))
hist(rowSums(spp_not>0))
abline(v=11, lwd=2, lty = 2)

####### Juntando tudo:
jpeg("distribui_metodo_spp.jpg", width = 20, height = 25, units = "cm", res = 300)
par(mar = c(5.1, 5.1, .1, .1), mfrow = c(3, 2))
hist(not$nindiv, main = NA, xlab = NA, cex.lab = 2, cex.axis = 2,
     ylab = NA, xlim = c(0, 100), breaks = 10)
abline(v=33.5, lwd=2, lty = 2)
text(80, 15, "Busca ativa\nnoturna", cex = 2)
hist(rowSums(spp_not>0), main = NA, xlab = NA, cex.lab = 2, cex.axis = 2,
     ylab = NA, xlim = c(0, 20))
abline(v=11, lwd=2, lty = 2)
hist(bat$nindiv, main = NA, xlab = NA, cex.lab = 2, cex.axis = 2, 
     ylab = "Número de amostras", xlim = c(0, 100), breaks = 5)
abline(v=12, lwd=2, lty = 2)
text(80, 20, "Guarda-chuva\nentomológico", cex = 2)
hist(rowSums(spp_bat>0), main = NA, xlab = NA, cex.lab = 2, cex.axis = 2, 
     ylab = NA, xlim = c(0, 20), breaks = 5)
abline(v=5, lwd=2, lty = 2)
hist(abund$nindiv, main = NA, xlab = "Número de aranhas adultas", 
     ylab = NA, cex.lab = 2, cex.axis = 2) 
abline(v=19.5, lwd=2, lty=2) #mediana do número de aranhas adultas nas amostras
text(80, 30, "Total", cex = 2)
hist(rowSums(spp>0), main = NA, xlab = "Número de espécies de aranhas", 
     ylab = NA, cex.lab = 2, cex.axis = 2) 
abline(v=8, lwd=2, lty=2) #mediana do número de aranhas adultas nas amostras
dev.off()

########GUARDA-CHUVA ENTOMOLÓGICO###########
# Variação em composição de espécies (diversidade Beta) por local amostrado com guarda-chuva entomológico

spp_bat.local <- aggregate(spp_bat, list(abund$site[which(abund$method=="batedor")]), mean)
colnames(spp_bat.local)[1] <- "site"
View(spp_bat.local) #média de abundância das espécies nos batedores
site.bat <- spp_bat.local[, 1] # salva o nome dos locais amostrados com batedor
spp_bat.local <- spp_bat.local[, -1] #tira o nome dos locais da matriz das espécies
spp_bat.local <- spp_bat.local[, -which(colSums(spp_bat.local)==0)] #tira as espécies ausentes em batedor
ncol(spp_bat.local) #95 espécies em guarda-chuva entomológico

## Primeiro transformamos para abundância relativa

bat.rel <- decostand(spp_bat.local, "total")
rowSums(bat.rel) #só pra conferir: todos 1
colMeans(bat.rel) #abundância relativa média (por local) de cada espécie
colSums(bat.rel>0) #frequência de ocorrência das espécies (número de locais em que ocorreram)
rowSums(bat.rel>0) #número de espécies por local


## Ordenar os locais amostrados usando NMDS pelas diferenças Bray-Curtis em composição de espécies: 

nmds.1d.bat <- metaMDS(bat.rel, k=1) #Escalonamento multidimensional não-métrico (NMDS) em uma dimensão
nmds.1d.bat
stressplot(nmds.1d.bat)

nmds.2d.bat <- metaMDS(bat.rel) #Escalonamento multidimensional não-métrico (NMDS) em duas dimensões
nmds.2d.bat
stressplot(nmds.2d.bat)

### Decidimos avaliar o padrão recuperado pela ordenação em duas dimensões.
#### Para isso vamos salvar os escores dessa ordenação (nesse caso, os vetores que definem os eixos do gráfico de ordenação) 

nmds.bat.escores <- scores(nmds.2d.bat)
nmds.bat.escores # NMDS scores ("posição dos pontos no plano da ordenação")

#### Definimos os loadings das espécies com a função wascores do pacote vegan. Os loadings correspondem a correlação das espécies com a ordenação.

spp_bat.loads <- wascores(nmds.bat.escores, bat.rel)
spp_bat.loads #Loadings das espécies ("posição das espécies no plano da ordenação"). Indica quanto cada espécie contribuiu para a ordenação das amostras.

#### Vamos avaliar o gráfico da ordenação com todas as espécies:

plot(nmds.bat.escores, bty = "n",  col = "gray",
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
text(spp_bat.loads,rownames(spp_bat.loads),
     col = "gray20", cex = .6, font = 3)
##### Muitas espécies sobrepostas.
##### Vamos selecionar as espécies com maiores correlações
spp_bat.loads1 <- spp_bat.loads[, 1]
sort(spp_bat.loads1)
spp_bat.loads2 <- spp_bat.loads[, 2]
sort(spp_bat.loads2)
spp_bat.loads[order(abs(spp_bat.loads1)), ]
spp_bat.loads_util <- spp_bat.loads[which(abs(spp_bat.loads1)>.4 | abs(spp_bat.loads2)>.4), ]
View(spp_bat.loads_util)

### Vamos ver no gráfico tentando diminuir a sobreposição dos nomes
plot(nmds.bat.escores, bty = "n",  col = "gray",
     ylim = c(-1.5, 1),
     xlim = c(-1.5, 1.5),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
#tremida.bat <- replicate(nrow(spp_bat.loads_util), jitter(1.3, 10))
text(spp_bat.loads_util*tremida.bat,rownames(spp_bat.loads_util),
     col = "gray20", cex = .6, font = 3)

###### Correção da posição dos pontos no gráfico para evitar sobreposição#######
####### Eu fui olhando no gráfico a seguir já em jpg e fui ajustando a altura de cada espécie para tirar as sobreposições usando os seguintes comandos
sort(rownames(spp_bat.loads_util)) #facilta achar o nome da espécie para colar no comando abaixo
spp_bat.loads_util[
  which(rownames(spp_bat.loads_util)== "Sparassidae sp.1"), 2]
spp_bat.loads_util[
  which(rownames(spp_bat.loads_util)== "Sparassidae sp.1"), 2] <- 0.475

jpeg("nmds_bat.jpg", width=15, height=15, units="cm", res=300)
par(mar = c(4, 4, .1, .1))
plot(nmds.bat.escores, bty = "n",  pch = 19, col = "gray", cex = 1.5,
     ylim = c(-1.5, 1),
     xlim = c(-1.5, 1.5),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
text(spp_bat.loads_util*tremida.bat,rownames(spp_bat.loads_util),
     col = "gray20", cex = .6, font = 3)
dev.off()
#### Figura. Ordenação dos locais (n = 11) pela variação em composição de espécies de aranhas capturadas em guarda-chuva entomológico por análise não-métrica de escalas multidimensionais (NMDS) a partir de distâncias Bray-Curtis pela abundância relativa. 
#### Cada amostra (pontos no gráfico) correspondeu a local de amostragem (n = 11 em 8 fragmentos).  
#### A posição do nome indica quanto cada espécie contribuiu para a ordenação das amostras. 

## Paisagem por local amostrado com guarda-chuva entomológico (11 locais em 8 fragmentos)
View(dados)
dados_bat <- dados[which(dados$method=="batedor"), ]
paisag.bat_35 <- aggregate(dados_bat[, 9:12], list(dados_bat$site), mean)
head(paisag.bat_35) #Proporção de cada componente da paisagem em escala regional (35 km)
colnames(paisag.bat_35) <- c("site", "forest_35", "tillage_35", "soil_35", "water_35")
colnames(paisag.bat_35)
dim(paisag.bat_35)
round(rowSums(paisag.bat[, 2:5])) #100 para cada observação
View(paisag.bat_35)
### Os locais foram amostrados em oito fragmentos com guarda-chuva entomológico:
frags.bat <- c("Balneário", "Clube", "Ecosystem", "Ecosystem", 
               "Fazenda Dânia", "Joel", "Mata do Azulão", "Mata do Azulão", 
               "Mata do Azulão", "Mata do Samu", "Reserva")
paisag.bat_35 <- paisag.bat_35[, -1]
head(paisag.bat_35)
dim(paisag.bat_35)

## PCA da paisagem por local (site) em buffers de 35 km de raio ao redor do ponto de amostragem
### PCA de correlação (argumento scale=TRUE)
pca.bat_35 <- rda(paisag.bat_35, scale=TRUE)
pca.bat_35 
summary(pca.bat_35) # Os dois primeiros eixos da PCA recuperaram 96% da variância na matriz de paisagem, 
# sendo 65,5% no primeiro eixo e 30,5% no segundo (Importance of components).

#### escores dos locais amostrados com guarda-chuva entomológico
site.pca.bat_35 <- pca.bat_35$CA$u
site.pca.bat_35
#### escores dos componentes da paisagem (loadings)
paisag.pca.bat_35 <- pca.bat_35$CA$v
paisag.pca.bat_35
#### Eigenvalues | comprimento do eixos | proporcional a variância
(ev.bat_35 <- pca.bat_35$CA$eig)

plot(site.pca.bat_35[, 1:2]*2.51, bty = "n", #essa correção de 2.51 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1", ylab = "PCA 2", ylim = c(-1, 2.2))
text(paisag.pca.bat_35[, 1], paisag.pca.bat_35[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"))
arrows(0, 0, paisag.pca.bat_35[, 1] * .8, 
       paisag.pca.bat_35[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")


## Paisagem por local amostrado com guarda-chuva entomológico (11 locais em 
## 8 fragmentos) em buffers de 5km de raio ao redor do ponto de amostragem
paisag.bat_5 <- aggregate(dados_bat[, 13:16], list(dados_bat$site), mean)
head(paisag.bat_5) #Proporção de cada componente da paisagem em escala local (5 km)
colnames(paisag.bat_5) <- c("site", "forest_5", "tillage_5", "soil_5", "water_5")
colnames(paisag.bat_5)
dim(paisag.bat_5)
round(rowSums(paisag.bat[, 2:5])) #100 para cada observação
View(paisag.bat_5)
paisag.bat_5 <- paisag.bat_5[, -1] #exclui o nome dos locais da planilha
head(paisag.bat_5)
dim(paisag.bat_5)

## PCA da paisagem por local (site) em buffers de 5 km de raio ao redor do ponto de amostragem
### PCA de correlação (argumento scale=TRUE)
pca.bat_5 <- rda(paisag.bat_5, scale=TRUE)
pca.bat_5 
summary(pca.bat_5) # Os dois primeiros eixos da PCA recuperaram 84,5% da variância na matriz de paisagem, 
# sendo 52% no primeiro eixo e 32,5% no segundo (Importance of components).

#### escores dos locais amostrados com guarda-chuva entomológico
site.pca.bat_5 <- pca.bat_5$CA$u
site.pca.bat_5
#### escores dos componentes da paisagem (loadings)
paisag.pca.bat_5 <- pca.bat_5$CA$v
paisag.pca.bat_5
#### Eigenvalues | comprimento do eixos | proporcional a variância
(ev.bat_5 <- pca.bat_5$CA$eig)

plot(site.pca.bat_5[, 1:2]*2.51, bty = "n", #essa correção de 2.51 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1", ylab = "PCA 2", ylim = c(-1, 2.2))
text(paisag.pca.bat_5[, 1], paisag.pca.bat_5[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"))
arrows(0, 0, paisag.pca.bat_5[, 1] * .8, 
       paisag.pca.bat_5[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")

### Tudo junto: Ordenação das amostras por análise de componentes principais (PCA) da paisagem em buffers
### de 5 km ou 35 km de raio ao redor dos pontos de amostragem com guarda-chuva entomológico
jpeg("paisagem_PCA_bat.jpg", width = 20, height = 25, units = "cm", res = 300)
layout(matrix(c(1, 2), 2, 1, byrow = T))
par(mar = c(5, 5, 1, 4))
plot(site.pca.bat_5[, 1:2]*2.51, bty = "n", #essa correção de 2.51 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1 (52 %)", ylab = "PCA 2 (32,5 %)", ylim = c(-1, 2.2), pch = 19, col = "gray40", 
     cex = 1.5, cex.lab = 1.5)
text(paisag.pca.bat_5[, 1], paisag.pca.bat_5[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"), cex = 1.5)
arrows(0, 0, paisag.pca.bat_5[, 1] * .8, 
       paisag.pca.bat_5[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")
text(1, 2, "Local (5 km)", adj = 1, cex = 1.5, col = "gray")
plot(site.pca.bat_35[, 1:2]*2.51, bty = "n", #essa correção de 2.51 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1 (65,5 %)", ylab = "PCA 2 (30,5 %)", ylim = c(-1, 2.2), xlim = c(-1.5, 2), pch = 19, col = "gray40", 
     cex = 1.5, cex.lab = 1.5)
text(paisag.pca.bat_35[, 1], paisag.pca.bat_35[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"), cex = 1.5)
arrows(0, 0, paisag.pca.bat_35[, 1] * .8, 
       paisag.pca.bat_35[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")
text(2, 2, "Regional (35 km)", adj = 1, cex = 1.5, col = "gray")
dev.off()
#### Figura. Ordenação dos locais por análise de componentes principais (PCA) para correlação entre componentes da paisagem (definidos como a proporção da área de cultivo, floresta, água ou solo exposto).
#### As amostras foram obtidas em 11 locais, entre nov/2016 e set/2017, de oito fragmentos em escala local (buffer com 5 km de raio) e regional (35 km). 


######################################################################
# Decomposição da diversidade [índice de entropia de Rao (Rao 1982)] #
#                       Adonis-permanova                             #
######################################################################
## Detalhes no script RaoRel.R ou RaoAdo.R (de Bello et al. 2011)
source("RaoRel.R")
source("RaoAdo.R")
### Diversidade observada (alfa, beta e gama)
Raospp.mat.bat <- RaoRel(sample=t(spp_bat.local), dfunc=vegdist(decostand(t(spp_bat.local), "total")), dphyl=NULL, 
                     weight=T, Jost=F, structure=NULL)
Raospp.mat.bat$TD$Alpha #diversidade de Simpson em cada observação#
witRao.bat<-Raospp.mat.bat$TD$Mean_Alpha
witRao.bat #ALFA (diversidade de Simpson)
betRao.bat<-Raospp.mat.bat$TD$Beta_add #diversidade Beta#
betRao.bat #BETA
totRao.bat<-Raospp.mat.bat$TD$Gamma #diversidade Gamma#
totRao.bat #GAMA
(betRao.bat+witRao.bat)==totRao.bat #BETA + ALFA = GAMA

### Diversidade esperada (alfa, beta e gama)
RaoPerm.bat<-RaoAdo(sample=t(spp_bat.local), dfunc=vegdist(decostand(t(spp_bat.local), "total")), dphyl=NULL, weight=T, Jost=F, structure=NULL)
RaoPerm.bat$TD$Gamma
RaoPerm.bat$TD$Mean_Alpha
RaoPerm.bat$TD$Beta_add

#Tabela. Partição da diversidade de aranhas na trasição do Cerrado com Florsta Atlântica.
#Fonte de variação      diversidade obs.	%	    diversidade esp.	%	    
#Interna (alfa)          			     0.803  92.1	           0.828  92.3	  obs = esp	
#Entre observações (beta)			     0.068	 7.9	           0.069	 7.7	  obs = esp	
#Total	(gama)		                 0.872	                 0.897		      obs = esp	


# Análise espacial
## Transformação dos dados
### Hellinger
spp.hel.bat <- decostand(spp_bat.local, "hel")
dim(spp.hel.bat) #95 espécies em 11 locais

### Distância métrica para as coordenadas
coord2.bat <- geoXY(coord.local.bat[, 2], coord.local.bat[, 1])
coord2.bat #distâncias euclidianas desde os pontos mais ao sul (X) e mais ao leste (Y)

# Análise de mapas de autovetores de Moran baseados em distância (dbMEM) com a função quickMEM() de Daniel Borcard (Borcard et. al. 2011)
spiders.dbmem.quick.bat <- quickMEM(spp.hel.bat, coord2.bat)
## Essa análise não detectou nenhuma estrutura espacial na comunidade (p > 0,1).

# A comunidade responde a paisagem?
## Primeiro olhamos para a correlação da paisagem com a comunidade
eixos.pca.bat_5 <- site.pca.bat_5[, 1:2] # Dois primeiros eixos da PCA da paisagem por local
e.pca1.bat_5 <- eixos.pca.bat_5[, 1] + abs(min(eixos.pca.bat_5[, 1])) # tranformação para o eixo 1 manter a escala somente com valores positivos 
e.pca2.bat_5 <- eixos.pca.bat_5[, 2] + abs(min(eixos.pca.bat_5[, 2])) # tranformação para o eixo 2 manter a escala somente com valores positivos
pca.loads.bat_5 <- wascores(nmds.bat.escores, 
                            cbind(e.pca1.bat_5, e.pca2.bat_5), expand = T)
pca.loads.bat_5 # Escores dos componentes da paisagem ponderados pela média para a ordenação das amostras pelas espécies de aranhas (NMDS).

eixos.pca.bat_35 <- site.pca.bat_35[, 1:2] # Dois primeiros eixos da PCA da paisagem por local
e.pca1.bat_35 <- eixos.pca.bat_35[, 1] + abs(min(eixos.pca.bat_35[, 1])) # tranformação para o eixo 1 manter a escala somente com valores positivos 
e.pca2.bat_35 <- eixos.pca.bat_35[, 2] + abs(min(eixos.pca.bat_35[, 2])) # tranformação para o eixo 2 manter a escala somente com valores positivos
pca.loads.bat_35 <- wascores(nmds.bat.escores, 
                            cbind(e.pca1.bat_35, e.pca2.bat_35), expand = T)
pca.loads.bat_35 # Escores dos componentes da paisagem ponderados pela média para a ordenação das amostras pelas espécies de aranhas (NMDS).

### Teste de permutação (detalhes dos testes de permutação do pacote vegan 
### em ?permutations)
envfit(nmds.bat.escores, cbind(eixos.pca.bat_5, eixos.pca.bat_35))
#### O primeiro eixo da PCA para escala local (5 km) teve correlação 
#### significante com a ordenação das amostras pela composição de espécies de 
#### aranhas (r2 = 0,82; p = 0,004). As demais correlações não foram 
#### significantes (p > 0,1).

#### Salvar a figura da ordenação com a variação na paisagem
jpeg("nmds_paisag_bat.jpg", width=25, height=15, units="cm", res=300)
layout(matrix(c(1, 3, 2, 3), 2, 2, byrow = T), widths = lcm(c(11, 14)))
par(mar = c(5, 5, 1, 4))
plot(site.pca.bat_5[, 1:2]*2.51, bty = "n", #essa correção de 2.51 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1 (52 %)", ylab = "PCA 2 (32,5 %)", ylim = c(-1, 2.2), pch = 19, col = "gray40", 
     cex = 1.5)
text(paisag.pca.bat_5[, 1], paisag.pca.bat_5[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"))
arrows(0, 0, paisag.pca.bat_5[, 1] * .8, 
       paisag.pca.bat_5[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")
text(1, 2, "Local (5 km)", adj = 1)
plot(site.pca.bat_35[, 1:2]*2.51, bty = "n", #essa correção de 2.51 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1 (65,5 %)", ylab = "PCA 2 (30,5 %)", ylim = c(-1, 2.2), xlim = c(-1.5, 2), pch = 19, col = "gray40", 
     cex = 1.5)
text(paisag.pca.bat_35[, 1], paisag.pca.bat_35[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"))
arrows(0, 0, paisag.pca.bat_35[, 1] * .8, 
       paisag.pca.bat_35[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")
text(2, 2, "Regional (35 km)", adj = 1, col = "gray")
par(mar = c(4, 4, .1, .1))
plot(nmds.bat.escores, bty = "n",  pch = 19, col = "gray", cex = 1.5,
     ylim = c(-1.5, 1),
     xlim = c(-1.5, 1.5),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
text(spp_bat.loads_util*tremida.bat,rownames(spp_bat.loads_util),
     col = "gray20", cex = .6, font = 3)
plot(envfit(nmds.bat.escores, cbind(eixos.pca.bat_5, eixos.pca.bat_35)), 
     labels = c("PCA 1", "PCA 2", "PCA 1", "PCA 2"), 
     col = rep(c("black", "gray"), each = 2), cex = 1.2)
dev.off()

summary(manova(nmds.bat.escores~eixos.pca.bat_5[,1] + eixos.pca.bat_5[,2] + 
                 eixos.pca.bat_35[,1] + eixos.pca.bat_35[,2]))
## A estrutura da comunidade de aranhas amostradas com guarda-chuva 
## entomológico (ordenação por NMDS) respondeu a variação na paisagem em escala local, sendo 
## significativamente relacionada com o primeiro eixo da PCA pelos componentes da paisagem. 

########BUSCA ATIVA NOTURNA###########
# Composição de espécies (diversidade Beta) por local amostrado com busca ativa noturna
spp_not.local <- aggregate(spp_not, list(abund$site[which(abund$method=="noturna")]), mean)
colnames(spp_not.local)[1] <- "site"
View(spp_not.local) #média de abundância das espécies nas coletas noturnas
site.not <- spp_not.local[, 1] # salva o nome dos locais amostrados com busca ativa noturna
spp_not.local <- spp_not.local[, -1] #tira o nome dos locais da matriz das espécies
spp_not.local <- spp_not.local[, -which(colSums(spp_not.local)==0)] #tira as espécies ausentes em coleta noturna
ncol(spp_not.local) #119 espécies em busca ativa noturna

## Primeiro transformamos para abundância relativa
not.rel <- decostand(spp_not.local, "total")
rowSums(not.rel) #só pra conferir: todos 1
colMeans(not.rel) #abundância relativa média (por local) de cada espécie
colSums(not.rel>0) #frequência de ocorrência das espécies (número de locais em que ocorreram)
rowSums(not.rel>0) #número de espécies por local


## Ordenar os locais amostrados usando NMDS pelas diferenças Bray-Curtis em composição de espécies: 
nmds.1d.not <- metaMDS(not.rel, k=1) #Escalonamento multidimensional não-métrico (NMDS) em uma dimensão
nmds.1d.not
stressplot(nmds.1d.not)

nmds.2d.not <- metaMDS(not.rel) #Escalonamento multidimensional não-métrico (NMDS) em duas dimensões
nmds.2d.not
stressplot(nmds.2d.not)

### Decidimos avaliar o padrão recuperado pela ordenação em duas dimensões.
#### Para isso vamos salvar os escores dessa ordenação (nesse caso, os vetores que definem os eixos do gráfico de ordenação) 
nmds.not.escores <- scores(nmds.2d.not)
nmds.not.escores # NMDS scores ("posição dos pontos no plano da ordenação")
#### Definimos os loadings das espécies com a função wascores do pacote vegan. Os loadings correspondem a correlação das espécies com a ordenação.
spp_not.loads <- wascores(nmds.not.escores, not.rel)
spp_not.loads #Loadings das espécies ("posição das espécies no plano da ordenação"). Indica quanto cada espécie contribuiu para a ordenação das amostras.

#### Vamos avaliar o gráfico da ordenação com todas as espécies:
plot(nmds.not.escores, bty = "n",  pch = 19, col = "gray30",
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
text(spp_not.loads,rownames(spp_not.loads),
     col = "gray20", cex = .6, font = 3)
##### Muitas espécies sobrepostas.
##### Vamos selecionar as espécies com maiores correlações
spp_not.loads1 <- spp_not.loads[, 1]
spp_not.loads2 <- spp_not.loads[, 2]
spp_not.loads[order(abs(spp_not.loads1)), ]
spp_not.loads[order(abs(spp_not.loads2)), ]
spp_not.loads_util <- spp_not.loads[which(abs(spp_not.loads1)>.3 | abs(spp_not.loads2)>.3), ]
View(spp_not.loads_util)

### Vamos ver no gráfico tentando diminuir a sobreposição dos nomes
plot(nmds.not.escores, bty = "n",  col = "gray",
     xlim = c(-1.5, 1),
     ylim = c(-1, 1),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
#tremida.not <- replicate(nrow(spp_not.loads_util), jitter(1.3, 10))
text(spp_not.loads_util[, 1], spp_not.loads_util[, 2] * tremida.not, rownames(spp_not.loads_util),
     col = "gray20", cex = .6, font = 3)

###### Correção da posição dos pontos no gráfico para evitar sobreposição#######
####### Eu fui olhando no gráfico a seguir já em jpg e fui ajustando a altura de cada espécie para tirar as sobreposições usando os seguintes comandos
sort(rownames(spp_not.loads_util)) #facilta achar o nome da espécie para colar no comando abaixo
spp_not.loads_util[
  which(rownames(spp_not.loads_util)== "Tmarus sp.1"), 2]
spp_not.loads_util[
  which(rownames(spp_not.loads_util)== "Tmarus sp.1"), 2] <- -0.41

jpeg("nmds_not.jpg", width=15, height=15, units="cm", res=300)
par(mar = c(4, 4, .1, .1))
plot(nmds.not.escores, bty = "n",  pch = 19, col = "gray", cex = 1.5,
     xlim = c(-1.3, .6),
     ylim = c(-.6, 1),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
#tremida.not <- replicate(nrow(spp_not.loads_util), jitter(1.3, 10))
text(spp_not.loads_util[, 1], spp_not.loads_util[, 2] * tremida.not, rownames(spp_not.loads_util),
     col = "gray20", cex = .6, font = 3)
dev.off()
#### Figura. Ordenação dos locais (n = 15) pela variação em composição de espécies de aranhas capturadas em busca ativa noturna por análise não-métrica de escalas multidimensionais (NMDS) a partir de distâncias Bray-Curtis pela abundância relativa. 
#### Cada amostra (pontos no gráfico) correspondeu ao local de amostragem (n = 15 em 8 fragmentos).  
#### A posição do nome indica quanto cada espécie contribuiu para a ordenação das amostras. 

## Paisagem por local amostrado com busca ativa noturna (15 locais em 8 fragmentos)
View(dados)
dados_not <- dados[which(dados$method=="noturna"), ]
paisag.not_35 <- aggregate(dados_not[, 9:12], list(dados_not$site), mean)
head(paisag.not_35) #Proporção de cada componente da paisagem em escala regional (35 km)
colnames(paisag.not_35) <- c("site", "forest_35", "tillage_35", "soil_35", "water_35")
colnames(paisag.not_35)
dim(paisag.not_35)
round(rowSums(paisag.not_35[, 2:5])) #100 para cada observação
View(paisag.not_35)
### Os 15 locais foram amostrados em oito fragmentos com busca ativa noturna:
site.not
frags.not <- c("Balneário", "Clube", "Ecosystem", "Ecosystem", "Ecosystem", 
               "Fazenda Dânia", "Joel", "Joel", "Joel", "Mata do Azulão", 
               "Mata do Azulão", "Mata do Azulão", "Mata do Samu", 
               "Mata do Samu", "Reserva")
paisag.not_35 <- paisag.not_35[, -1]
head(paisag.not_35)
dim(paisag.not_35)

## PCA da paisagem por local (site) em buffers de 35 km de raio ao redor do ponto de amostragem
### PCA de correlação (argumento scale=TRUE)
pca.not_35 <- rda(paisag.not_35, scale=TRUE)
pca.not_35 
summary(pca.not_35) # Os dois primeiros eixos da PCA recuperaram 86,5% da variância na matriz de paisagem, 
# sendo 58% no primeiro eixo e 28,5% no segundo (Importance of components).

#### escores dos locais amostrados com guarda-chuva entomológico
site.pca.not_35 <- pca.not_35$CA$u
site.pca.not_35
#### escores dos componentes da paisagem (loadings)
paisag.pca.not_35 <- pca.not_35$CA$v
paisag.pca.not_35
#### Eigenvalues | comprimento do eixos | proporcional a variância
(ev.not_35 <- pca.not_35$CA$eig)

plot(site.pca.not_35[, 1:2]*2.74, bty = "n", #essa correção de 2.74 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1", ylab = "PCA 2", ylim = c(-2.2, 1))
text(paisag.pca.not_35[, 1], paisag.pca.not_35[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"))
arrows(0, 0, paisag.pca.not_35[, 1] * .8, 
       paisag.pca.not_35[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")


## Paisagem por local amostrado com busca ativa noturna (15 locais em 
## 8 fragmentos) em buffers de 5km de raio ao redor do ponto de amostragem
paisag.not_5 <- aggregate(dados_not[, 13:16], list(dados_not$site), mean)
head(paisag.not_5) #Proporção de cada componente da paisagem em escala local (5 km)
colnames(paisag.not_5) <- c("site", "forest_5", "tillage_5", "soil_5", "water_5")
colnames(paisag.not_5)
dim(paisag.not_5)
round(rowSums(paisag.not_5[, 2:5])) #100 para cada observação
View(paisag.not_5)
paisag.not_5 <- paisag.not_5[, -1] #exclui o nome dos locais da planilha
head(paisag.not_5)
dim(paisag.not_5)

## PCA da paisagem por local (site) em buffers de 5 km de raio ao redor do ponto de amostragem
### PCA de correlação (argumento scale=TRUE)
pca.not_5 <- rda(paisag.not_5, scale=TRUE)
pca.not_5 
summary(pca.not_5) # Os dois primeiros eixos da PCA recuperaram 77,5% da variância na matriz de paisagem, 
# sendo 46% no primeiro eixo e 31,5% no segundo (Importance of components).

#### escores dos locais amostrados com guarda-chuva entomológico
site.pca.not_5 <- pca.not_5$CA$u
site.pca.not_5
#### escores dos componentes da paisagem (loadings)
paisag.pca.not_5 <- pca.not_5$CA$v
paisag.pca.not_5
#### Eigenvalues | comprimento do eixos | proporcional a variância
(ev.not_5 <- pca.not_5$CA$eig)

plot(site.pca.not_5[, 1:2]*2.74, bty = "n", #essa correção de 2.74 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1", ylab = "PCA 2")
text(paisag.pca.not_5[, 1], paisag.pca.not_5[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"))
arrows(0, 0, paisag.pca.not_5[, 1] * .8, 
       paisag.pca.not_5[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")

### Tudo junto: Ordenação das amostras por análise de componentes principais (PCA) da paisagem em buffers
### de 5 km ou 35 km de raio ao redor dos pontos de amostragem com busca ativa noturna
jpeg("paisagem_PCA_not.jpg", width = 20, height = 25, units = "cm", res = 300)
layout(matrix(c(1, 2), 2, 1, byrow = T))
par(mar = c(5, 5, 1, 4))
plot(site.pca.not_5[, 1:2]*2.74, bty = "n", #essa correção de 2.74 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1 (46 %)", ylab = "PCA 2 (31,5 %)",pch = 19, col = "gray40", 
     cex = 1.5, cex.lab = 1.5)
text(paisag.pca.not_5[, 1], paisag.pca.not_5[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"), cex = 1.5)
arrows(0, 0, paisag.pca.not_5[, 1] * .8, 
       paisag.pca.not_5[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")
text(1, 2, "Local (5 km)", adj = 1, cex = 1.5, col = "gray")
plot(site.pca.not_35[, 1:2]*2.74, bty = "n", #essa correção de 2.51 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1 (58 %)", ylab = "PCA 2 (28,5 %)", ylim = c(-2.2, 1), pch = 19, col = "gray40", 
     cex = 1.5, cex.lab = 1.5)
text(paisag.pca.not_35[, 1], paisag.pca.not_35[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"), cex = 1.5)
arrows(0, 0, paisag.pca.not_35[, 1] * .8, 
       paisag.pca.not_35[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")
text(2, .5, "Regional (35 km)", adj = 1, cex = 1.5, col = "gray")
dev.off()
#### Figura. Ordenação dos locais por análise de componentes principais (PCA) para correlação entre componentes da paisagem (definidos como a proporção da área de cultivo, floresta, água ou solo exposto).
#### As amostras foram obtidas em 15 locais, entre nov/2016 e set/2017, de oito fragmentos em escala local (buffer com 5 km de raio) e regional (35 km). 


######################################################################
# Decomposição da diversidade [índice de entropia de Rao (Rao 1982)] #
#                       Adonis-permanova                             #
######################################################################
## Detalhes no script RaoRel.R ou RaoAdo.R (de Bello et al. 2011)
source("RaoRel.R")
source("RaoAdo.R")
### Diversidade observada (alfa, beta e gama)
Raospp.mat.not <- RaoRel(sample=t(spp_not.local), dfunc=vegdist(decostand(t(spp_not.local), "total")), dphyl=NULL, 
                         weight=T, Jost=F, structure=NULL)
Raospp.mat.not$TD$Alpha #diversidade de Simpson em cada observação#
witRao.not<-Raospp.mat.not$TD$Mean_Alpha
witRao.not #ALFA (diversidade de Simpson)
betRao.not<-Raospp.mat.not$TD$Beta_add #diversidade Beta#
betRao.not #BETA
totRao.not<-Raospp.mat.not$TD$Gamma #diversidade Gamma#
totRao.not #GAMA
(betRao.not+witRao.not)==totRao.not #BETA + ALFA = GAMA

### Diversidade esperada (alfa, beta e gama)
RaoPerm.not<-RaoAdo(sample=t(spp_not.local), dfunc=vegdist(decostand(t(spp_not.local), "total")), dphyl=NULL, weight=T, Jost=F, structure=NULL)
RaoPerm.not$TD$Gamma
RaoPerm.not$TD$Mean_Alpha
RaoPerm.not$TD$Beta_add

#Tabela. Partição da diversidade de aranhas na trasição do Cerrado com Florsta Atlântica.
#Fonte de variação      diversidade obs.	%	    diversidade esp.	%	    
#Interna (alfa)          			     0.802  95.5	           0.825  95.8	  obs = esp	
#Entre observações (beta)			     0.038	 4.5	           0.036	 4.2	  obs = esp	
#Total	(gama)		                 0.840	                 0.861		      obs = esp	


# Análise espacial
## Transformação dos dados
### Hellinger para os morcegos
spp.hel.not <- decostand(spp_not.local, "hel")
dim(spp.hel.not) #119 espécies em 15 locais

### Distância métrica para as coordenadas
coord2.not <- geoXY(coord.local.not[, 2], coord.local.bat[, 1])
coord2.not #distâncias euclidianas desde os pontos mais ao sul (X) e mais ao leste (Y)

# Análise de mapas de autovetores de Moran baseados em distância (dbMEM) com a função quickMEM() de Daniel Borcard (Borcard et. al. 2011)
spiders.dbmem.quick.not <- quickMEM(spp.hel.not, coord2.not)
## Essa análise não detectou nenhuma estrutura espacial na comunidade (p > 0,1).

# A comunidade responde a paisagem?
## Primeiro olhamos para a correlação da paisagem com a comunidade
eixos.pca.not_5 <- site.pca.not_5[, 1:2] # Dois primeiros eixos da PCA da paisagem por local
e.pca1.not_5 <- eixos.pca.not_5[, 1] + abs(min(eixos.pca.not_5[, 1])) # tranformação para o eixo 1 manter a escala somente com valores positivos 
e.pca2.not_5 <- eixos.pca.not_5[, 2] + abs(min(eixos.pca.not_5[, 2])) # tranformação para o eixo 2 manter a escala somente com valores positivos
pca.loads.not_5 <- wascores(nmds.not.escores, 
                            cbind(e.pca1.not_5, e.pca2.not_5), expand = T)
pca.loads.not_5 # Escores dos componentes da paisagem ponderados pela média para a ordenação das amostras pelas espécies de aranhas (NMDS).

eixos.pca.not_35 <- site.pca.not_35[, 1:2] # Dois primeiros eixos da PCA da paisagem por local
e.pca1.not_35 <- eixos.pca.not_35[, 1] + abs(min(eixos.pca.not_35[, 1])) # tranformação para o eixo 1 manter a escala somente com valores positivos 
e.pca2.not_35 <- eixos.pca.not_35[, 2] + abs(min(eixos.pca.not_35[, 2])) # tranformação para o eixo 2 manter a escala somente com valores positivos
pca.loads.not_35 <- wascores(nmds.not.escores, 
                             cbind(e.pca1.not_35, e.pca2.not_35), expand = T)
pca.loads.not_35 # Escores dos componentes da paisagem ponderados pela média para a ordenação das amostras pelas espécies de aranhas (NMDS).

### Teste de permutação (detalhes dos testes de permutação do pacote vegan 
### em ?permutations)
envfit(nmds.not.escores, cbind(eixos.pca.not_5, eixos.pca.not_35))
#### Os dois eixos da PCA para escala local (5 km) tiveram correlação 
#### significante com a ordenação das amostras pela composição de espécies de 
#### aranhas (r2 = 0,46 e 0,56; p = 0,025 e 0,022). As demais correlações não 
#### foram significantes (p > 0,1).

#### Salvar a figura da ordenação com a variação na paisagem
jpeg("nmds_paisag_not.jpg", width=25, height=15, units="cm", res=300)
layout(matrix(c(1, 3, 2, 3), 2, 2, byrow = T), widths = lcm(c(11, 14)))
par(mar = c(5, 5, 1, 4))
plot(site.pca.not_5[, 1:2]*2.74, bty = "n", #essa correção de 2.74 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1 (46 %)", ylab = "PCA 2 (31,5 %)",pch = 19, col = "gray40", 
     cex = 1.5)
text(paisag.pca.not_5[, 1], paisag.pca.not_5[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"))
arrows(0, 0, paisag.pca.not_5[, 1] * .8, 
       paisag.pca.not_5[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")
text(1, 2, "Local (5 km)", adj = 1)
plot(site.pca.not_35[, 1:2]*2.74, bty = "n", #essa correção de 2.51 é o "General scaling constant of scores" em summary(land.pca)
     xlab = "PCA 1 (58 %)", ylab = "PCA 2 (28,5 %)", ylim = c(-2.2, 1), pch = 19, col = "gray40", 
     cex = 1.5)
text(paisag.pca.not_35[, 1], paisag.pca.not_35[, 2],
     c("Floresta", "Cultivo", "Solo", "Água"))
arrows(0, 0, paisag.pca.not_35[, 1] * .8, 
       paisag.pca.not_35[, 2] *.8, length = .1)
abline(v = 0, h = 0, lty = 3, lwd = .8, col = "gray")
text(2, .5, "Regional (35 km)", adj = 1, col = "gray")
par(mar = c(4, 4, .1, .1))
plot(nmds.not.escores, bty = "n",  pch = 19, col = "gray", cex = 1.5,
     xlim = c(-1.3, .6),
     ylim = c(-.6, 1),
     xlab = "Dimensão NMDS 1", 
     ylab = "Dimensão NMDS 2")
abline(h=0, v=0, lty=3, col="gray")
#tremida.not <- replicate(nrow(spp_not.loads_util), jitter(1.3, 10))
text(spp_not.loads_util[, 1], spp_not.loads_util[, 2] * tremida.not, rownames(spp_not.loads_util),
     col = "gray20", cex = .6, font = 3)
plot(envfit(nmds.not.escores, cbind(eixos.pca.not_5, eixos.pca.not_35)), 
     labels = c("PCA 1", "PCA 2", "PCA 1", "PCA 2"), 
     col = rep(c("black", "gray"), each = 2), cex = 1.2)
dev.off()

summary(manova(nmds.not.escores~eixos.pca.not_5[,1] + eixos.pca.not_5[,2] + 
                 eixos.pca.not_35[,1] + eixos.pca.not_35[,2]))
## A estrutura da comunidade de aranhas amostradas com busca ativa noturna 
## (ordenação por NMDS) respondeu a variação na paisagem em escala local e regional,
## sendo significativamente relacionada com o primeiro eixo da PCA pelos componentes da paisagem 
## local e com o segundo eixo pelos componentes da paisagem tanto em escala local
## quanto regional. 

