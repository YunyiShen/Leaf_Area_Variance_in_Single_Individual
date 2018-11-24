leafdata = read.csv("leavesstats.csv")
Edge = leafdata$Perimeter_mm/(pi*leafdata$ECD_mm)
leafdata = data.frame(leafdata,Edge)

data.building = leafdata[leafdata$shade=="Building",]
data.norm = leafdata[leafdata$shade!="Building",]
data.building$Orientation = "Building"
leafdata = rbind(data.norm,data.building) # sepcial treatment to building shade data

boxplot(ECD_mm~Orientation,data = leafdata)
boxplot(Edge~Orientation,data = leafdata)

ECD.lm = lm(ECD_mm~Orientation,data = leafdata[leafdata$shade!='Building',])
ECD.anova = anova(ECD.lm)

Edge.lm = lm(Edge~Orientation,data = leafdata[leafdata$shade!='Building',])
Edge.anova = anova(Edge.lm)



data.S = data.norm[data.norm$Orientation=="S",]
data.N = data.norm[data.norm$Orientation=="N",]
data.E = data.norm[data.norm$Orientation=="E",]
data.W = data.norm[data.norm$Orientation=="W",]

n.bootstrap = 1000
n.sample = 15
bootstrap.area.vars = data.frame(matrix(0,nrow = n.bootstrap,ncol = 5))
colnames(bootstrap.area.vars)=c("N","E","S","W","building_shade")

bootstrap.edge.vars = data.frame(matrix(0,nrow = n.bootstrap,ncol = 5))
colnames(bootstrap.edge.vars)=c("N","E","S","W","building_shade")


for(i in 1:n.bootstrap){
  bootstrapsample.N = data.N$ECD_mm[sample.int(nrow(data.N),n.sample)]
  bootstrap.area.vars$N[i] = var(bootstrapsample.N)
  bootstrapsample.E = data.E$ECD_mm[sample.int(nrow(data.E),n.sample)]
  bootstrap.area.vars$E[i] = var(bootstrapsample.E)
  bootstrapsample.S = data.S$ECD_mm[sample.int(nrow(data.S),n.sample)]
  bootstrap.area.vars$S[i] = var(bootstrapsample.S)
  bootstrapsample.W = data.W$ECD_mm[sample.int(nrow(data.W),n.sample)]
  bootstrap.area.vars$W[i] = var(bootstrapsample.W)
  bootstrapsample.building = data.building$ECD_mm[sample.int(nrow(data.building),n.sample)]
  bootstrap.area.vars$building_shade[i] = var(bootstrapsample.building)
  
  
  bootstrapsample.edge.N = data.N$Edge[sample.int(nrow(data.N),n.sample)]
  bootstrap.edge.vars$N[i] = var(bootstrapsample.edge.N)
  bootstrapsample.edge.E = data.E$Edge[sample.int(nrow(data.E),n.sample)]
  bootstrap.edge.vars$E[i] = var(bootstrapsample.edge.E)
  bootstrapsample.edge.S = data.S$Edge[sample.int(nrow(data.S),n.sample)]
  bootstrap.edge.vars$S[i] = var(bootstrapsample.edge.S)
  bootstrapsample.edge.W = data.W$Edge[sample.int(nrow(data.W),n.sample)]
  bootstrap.edge.vars$W[i] = var(bootstrapsample.edge.W)
  bootstrapsample.edge.building = data.building$Edge[sample.int(nrow(data.building),n.sample)]
  bootstrap.edge.vars$building_shade[i] = var(bootstrapsample.edge.building)
  
  }
boxplot(bootstrap.area.vars)
boxplot(bootstrap.edge.vars)

boot.data.reshape = data.frame(ECD.var = matrix( as.matrix(bootstrap.area.vars),nrow = 5*n.bootstrap,ncol = 1)
                               ,Edge.var = matrix( as.matrix(bootstrap.edge.vars),nrow = 5*n.bootstrap,ncol = 1)
                               ,Orientation = rep(colnames(bootstrap.area.vars),each = n.bootstrap))

ECD.var.lm = lm(ECD.var~Orientation,data = boot.data.reshape)
anova(ECD.var.lm)

Edge.var.lm = lm(Edge.var~Orientation,data = boot.data.reshape)
anova(Edge.var.lm)




