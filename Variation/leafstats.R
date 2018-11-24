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


n.bootstrap = 1000
n.sample = 15

oris = c("N","E","S","W","Building")
bootstrap.area.vars = data.frame(matrix(0,nrow = n.bootstrap,ncol = 5))
colnames(bootstrap.area.vars)=oris

bootstrap.edge.vars = data.frame(matrix(0,nrow = n.bootstrap,ncol = 5))
colnames(bootstrap.edge.vars)=oris
for(ori in 1:5){
	for(i in 1:n.bootstrap){
	data.temp = leafdata[leafdata$Orientation==oris[ori],]
	bootstrapsample.area = data.temp$ECD_mm[sample.int(nrow(data.temp),n.sample)]
	bootstrap.area.vars[i,ori] = var(bootstrapsample.area)

	bootstrapsample.edge = data.temp$Edge[sample.int(nrow(data.temp),n.sample)]
	bootstrap.edge.vars[i,ori] = var(bootstrapsample.edge)
  
  }
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




