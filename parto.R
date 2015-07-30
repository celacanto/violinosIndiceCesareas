library(gdata)
library(caroline)

minN <- 10
colPlot <- "#2B251F"
bgCol <- "#f0f4f5"
linesCol <- adjustcolor("grey", 0.3)

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

### sp
sp <- read.xls("input/sp.xlsx", sheet=1, stringsAsFactors = FALSE)
sp$TAXAS.DE.CESÁREA <- as.numeric(gsub(pattern="%", "", sp$TAXAS.DE.CESÁREA))
sp$OBSTETRA <- gsub(" \\d+$", "",  sp$OBSTETRA)


### rj
rj <- read.xls("input/rj.xlsx", sheet=1, stringsAsFactors = FALSE)
rj <- rj[,1:3]
select_percentage <- grepl("%", rj$Taxa.de.Cesárea)
rj$Taxa.de.Cesárea <- as.numeric(gsub(pattern="%", "", rj$Taxa.de.Cesárea))
rj$Taxa.de.Cesárea[!select_percentage] <- rj$Taxa.de.Cesárea[!select_percentage] * 100

### merge
sp$CIDADE <- NULL
colnames(rj) <- colnames(sp)
sp$CONVÊNIO <- paste(sp$CONVÊNIO, "- SP")
rj$CONVÊNIO <- paste(rj$CONVÊNIO, "- RJ")
rj_sp <- rbind(sp, rj)


# plot

# formata variáveis
txPerConv <- split(rj_sp$TAXAS.DE.CESÁREA, rj_sp$CONVÊNIO)
nPerConv <- sapply(txPerConv, length)
txPerConv <- txPerConv[nPerConv >= minN]

names(txPerConv) <- sapply(tolower(names(txPerConv)), .simpleCap)
names(txPerConv) <- gsub("p$", "P", names(txPerConv))
names(txPerConv) <- gsub("j$", "J", names(txPerConv))

means <- sapply(txPerConv, median)
txPerConv <- txPerConv[order(means)]

# parametros
par(bty = 'n', bg = bgCol, family = "Palatino", mar = c(5, 5, 5, 10))

# violin plots
boxplot(x=txPerConv, main = "", ylab ="", xlab="", axes = FALSE, outline = TRUE,
        horizontal = TRUE,
        pars = list(boxlty = 0, 
                    staplewex = 0, whisklty = 1, whisklwd =0.7, whiskcol = colPlot,
                    medpch= 19, medcex = 0.9, medcol = colPlot, medlty =0,
                    outpch= 19, outcex = 0.5, outcol = colPlot))
violins(txPerConv, drawRect = FALSE, connectcol = FALSE, add = TRUE, 
        horizontal = TRUE, connect= FALSE,  border=colPlot)

# segmentos no fundo
segments(x0 = seq(0, 100, 10), 
         x1 = seq(0, 100, 10), 
         y0 = 0.5, 
         y1 = length(txPerConv) + 0.5, 
         col = linesCol)
segments(y0 = seq(0.5, length(txPerConv) + 0.5, 1), 
         y1 = seq(0.5, length(txPerConv) + 0.5, 1), 
         x0 = 0, 
         x1 = 100, 
         col = linesCol)

axis(1, seq(0, 100, 10), lwd = 0, line = -2, col.axis = colPlot)
axis(3, seq(0, 100, 10), lwd = 0, line = -2, col.axis = colPlot)
axis(4, at  =  seq(1, length(txPerConv), 1), labels = names(txPerConv), las = 1, lwd = 0, line = -2, col.axis = colPlot)

# n
axis(3, at = -10, labels= c("n"), lwd = 0, line = -2, xpd = TRUE, font = 3, cex.axis = 1.3, 
     col.axis = colPlot)
axis(2, at = seq(1, length(txPerConv), 1), labels = sapply(txPerConv, length), col.axis = colPlot,
     las = 1, lwd = 0, line= 0.5, 
     xpd = TRUE, font = 3, cex.axis = 1.3)

# taxas
segments(x0 = 15, y0 = 0.5, x1 = 15, y1 = length(txPerConv) + 0.5, lty = 2, col = colPlot, lwd = 2)
segments(x0 = 27,5, y0 = 0.5, x1 = 27,5, y1 = length(txPerConv) + 0.5, lty = 2, col = "#D45640", lwd = 2)
segments(x0 = 83, y0 = 0.5, x1 = 83, y1 = length(txPerConv) + 0.5, lty = 2, col = "#37448C", lwd = 2)

# legenda
legend(x=0, y= 0, xpd= TRUE,  title= "Índices:",box.lwd=0, title.adj=c(0.05),
       lty = rep(2, 3), lwd = 2, col = c(colPlot, "#D45640", "#37448C"),
       legend=c("Recomendado pela OMS", "Rede Pública Brasileira", "Rede Privada Brasileira"))

# titulo
mtext(text = "Índices de Cesáreas de Obstetras que Atendem Parto pelos Planos de Saúde", side = 3,
      line = 3.4, cex = 1.3)

# label
mtext(text = "Porcentagem de césarea", side = 3,
      line = 1, cex =1)


# fonte
text(x = 100, y = -1.3, xpd = TRUE,  cex = 0.9, adj= c(1, 0.5),
     labels = "Fonte: goo.gl/Ovt3HW",font = 3)
