#-------------------Установка----------------
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("graph")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")
BiocManager::install("XML")

library(graph)
library(RBGL)
library(Rgraphviz)
library(XML)
#------------------Загрузка примера--------------
data("FileDep")
FileDep
edgeNames(FileDep)
plot(FileDep, attrs=list(node=list(fillcolor="lightgreen"),
                     edge=list(color="darkgreen"),
                     graph=list(rankdir="LR")))
#------------------Алгоритмы BGL----------------
#bfs
#загрузка данных
con <- file(system.file("XML/bfsex.gxl", package="RBGL"))
bf <- fromGXL(con)
close(con)

bf
plot(bf)

bfs.res <- bfs(bf, "s")
bfs.res

newnodes <- vector()
for (i in nodes(bf)) newnodes <- append(newnodes, as.character(which(bfs.res == i)))
names(newnodes) <- nodes(bf)
plot(bf, nodeAttrs = list(label = newnodes))

#алгоритм Дейкстры
#загрузка данных
con <- file(system.file("XML/dijkex.gxl", package="RBGL"))
dijk <- fromGXL(con)
close(con)

dijk

attrs <- list(node=list(shape="ellipse", fixedsize=FALSE), edge = list(color="green"))
eAttrs <- list()
ew <- as.character(unlist(edgeWeights(dijk)))
ew <- ew[setdiff(seq(along=ew), removedEdges(dijk))]
names(ew) <- edgeNames(dijk)
eAttrs$label <- ew
attrs$edge$fontsize = 10
plot(dijk, edgeAttrs=eAttrs, attrs=attrs)

dijkstra.sp(dijk, start = "B")$distances
#------------------Алгоритмы, основанные на BGL------
# min-cut - разбивает граф за наименьшую цену
con <- file(system.file("XML/conn.gxl", package="RBGL"))
coex <- fromGXL(con)
close(con)

coex
plot(coex) #невзвешенный граф

minCut(coex)
#------------------Алгоритмы, независимые от BGL-----
#хордальный граф
con <- file(system.file("XML/hcs.gxl", package="RBGL"))
hcs <- fromGXL(con)
close(con)

hcs
plot(hcs)
is.triangulated(hcs)

plot(coex)
is.triangulated(coex)
