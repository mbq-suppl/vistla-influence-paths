library(vistla)
library(ggplot2)

readRDS('affect.RDS')->A

prm<-function(x){
 for(e in 1:ncol(x)) x[,e]<-sample(x[,e])
 x
}

rbindlist<-function(x) do.call(rbind,x)

set.seed(1)

if(!exists("V")){
 vistla(Affect~.,data=A,estimator='kt')->V
 replicate(500,vistla(Affect~.,data=prm(A),estimator='kt'),simplify=FALSE)->Vd
}

data.frame(hierarchy(V))->VV

lapply(Vd,function(x){ 
 data.frame(hierarchy(x))->x
 x[,c("depth","score")]
})|>rbindlist()->Sp
Sp$Permuted<-TRUE
rownames(Sp)<-NULL

VV[,c("depth","score")]->S
S$Permuted<-FALSE
S<-rbind(Sp,S)
S[S$depth>0,]->S

S$Depth<-paste(S$depth)
S$Score<-S$score

S$Length<-ordered(S$depth+2)

S$Dataset<-factor(ifelse(S$Permuted,'Permuted','Original'),levels=c('Original','Permuted'))

#Identify 99th percentile of points in each boxplot
parts<-sprintf("%s_%s",S$Depth,S$Permuted)
split(S$Score,parts)|>
 lapply(function(x) x>quantile(x,0.99))|>
 unsplit(parts)->S$Top
 
 
pltos<-ggplot(S,aes(x=Length,y=Score,fill=Dataset))+
 geom_hline(yintercept=0.02)+
 geom_boxplot(outlier.shape=NA)+
 geom_point(pch=21,position=position_jitterdodge(),data=S[S$Top|!S$Permuted,])+ 
 ylab("Score [nats]")


pltp<-pltos+theme_classic()+
 theme(legend.position="bottom")+
 xlab("Path length [features]")

if(!interactive()) ggsave("score-datum.pdf",pltp,width=6,height=4)
