leafdata = read.csv("leavesstats.csv")
boxplot(ECD_mm~Orientation,data = leafdata[leafdata$shade!='Building' ,])
ECD.lm = lm(ECD_mm~Orientation,data = leafdata[leafdata$shade!='Building',])

ECD.anova = anova(ECD.lm)

data.building = leafdata[leafdata$shade=="Building",]
data.norm = leafdata[leafdata$shade!="Building",]

data.S = leafdata[data.norm$Orientation=="S",]
data.N = leafdata[data.norm$Orientation=="N",]
data.E = leafdata[data.norm$Orientation=="E",]
data.W = leafdata[data.norm$Orientation=="W",]

n.bootstrap = 1000
n.sample = 15
bootstrap.vars = data.frame(matrix(0,nrow = n.bootstrap,ncol = 5))
colnames(bootstrap.vars)=c("N","E","S","W","building_shade")

for(i in 1:n.bootstrap){
  bootstrapsample.N = data.N$ECD_mm[sample.int(nrow(data.N),n.sample)]
  bootstrap.vars$N[i] = var(bootstrapsample.N)
  bootstrapsample.E = data.E$ECD_mm[sample.int(nrow(data.E),n.sample)]
  bootstrap.vars$E[i] = var(bootstrapsample.E)
  bootstrapsample.S = data.S$ECD_mm[sample.int(nrow(data.S),n.sample)]
  bootstrap.vars$S[i] = var(bootstrapsample.S)
  bootstrapsample.W = data.W$ECD_mm[sample.int(nrow(data.W),n.sample)]
  bootstrap.vars$W[i] = var(bootstrapsample.W)
  bootstrapsample.building = data.building$ECD_mm[sample.int(nrow(data.building),n.sample)]
  bootstrap.vars$building_shade[i] = var(bootstrapsample.building)
}
boxplot(bootstrap.vars)

boot.data.reshape = data.frame(ECD.var = matrix( as.matrix(bootstrap.vars),nrow = 5*n.bootstrap,ncol = 1)
                               ,Orientation = rep(colnames(bootstrap.vars),each = n.bootstrap))

ECD.var.lm = lm(ECD.var~.,data = boot.data.reshape)

anova(ECD.var.lm)
