library(pspearman)
library(vistla)
library(parallel)

readRDS('affect.RDS')->A

A$Affect<-as.numeric(A$Affect)


expand.grid(a=1:ncol(A),b=1:ncol(A))->p
p[p$a>p$b,]->p

mclapply(1:nrow(p),function(e){
 spearman.test(A[,p$a[e]],A[,p$b[e]])
},mc.cores=detectCores())->pr

p$pv<-sapply(pr,'[[','p.value')
p$cor<-sapply(pr,'[[','estimate')


p$a<-names(A)[p$a]
p$b<-names(A)[p$b]


p->P

P$apv<-p.adjust(P$pv,'fdr')
P[P$apv<0.001,]->P
rownames(P)<-NULL

nice<-function(x){
 x<-gsub("\\.","\n",x)
 x<-gsub('Cpu','CPu',x)
 x<-gsub("_","-",x)
 x
}

gsub("\\..+$","",names(A))->str
sort(unique(str))->us
hsv((1:length(us))/length(us),0.3,1)->scol
names(scol)<-us
setNames(scol[str],names(A))->scol

dropViz<-function(x){
 if(is.null(x$active)) x$active<-TRUE
 x$color<-ifelse(x$cor<0,"red","black")
 x$color[!x$active]<-"gray"
 sprintf('"%s" -- "%s" [color="%s"]',x$a,x$b,x$color)->E
 v<-unique(c(x$a,x$b))
 sprintf('"%s" [label="%s", fillcolor="%s", style="filled"]',v,nice(v),scol[v])->V
 sprintf("graph {\n sep=\"3\"\n overlap=\"prism\"\n splines=\"true\"\n%s\n\n%s \n}",paste(V,collapse="\n"),paste(E,collapse="\n"))
}

pullPos<-function(dot){
 tempfile()->s
 writeLines(dot,s)
 readLines(pipe(sprintf('neato %s -T plain',s)))->z
 strsplit(z[grepl("node",z)],split=" ")->pz
 as.numeric(sapply(pz,'[[',3))->x
 as.numeric(sapply(pz,'[[',4))->y
 gsub('"','',sapply(pz,'[[',2))->id
 ans<-data.frame(x,y) 
 rownames(ans)<-id
 ans
}

dropVizPos<-function(x,pos,vi){
 if(!missing(vi)) vistla2edges(vi)->E
 v<-unique(c(x$a,x$b))
 if(!missing(vi)) vistla2vtx(vi)->v
 if(missing(pos)) pos<-data.frame(x=c(),y=c())
 pos[v,]->pos
 sprintf('"%s" [label="%s", fillcolor="%s", style="filled", pos="%0.5f,%0.5f", pin=true]',v,nice(v),scol[v],pos$x,pos$y)->V
 gsub(', pos="NA,NA", pin=true','',V)->V
 sprintf("digraph {\n sep=\"3\"\n overlap=\"prism\"\n splines=\"true\"\n%s\n\n%s \n}",paste(V,collapse="\n"),paste(E,collapse="\n"))
}

vistla2edges<-function(x){
 if(inherits(x,"vistla")) x<-hierarchy(x)
 stopifnot(inherits(x,"vistla_hierarchy"))
 data.frame(x)->x
 x$score/max(x$score,na.rm=TRUE)->x$rs
 x$rs[is.na(x$rs)]<-0
 x$clr<-gray(0.9-x$rs*0.9)
 x$lw<-0.1+1.5*x$rs
 
 sapply(2:nrow(x),function(e){
  sprintf('"%s" -> "%s" [penwidth="%0.3f",color="%s"]',x$name[x$prv[e]],x$name[e],x$lw[e],x$clr[e])
 })
}

vistla2vtx<-function(x){
 if(inherits(x,"vistla")) x<-hierarchy(x)
 stopifnot(inherits(x,"vistla_hierarchy"))
 unique(x$name)
}

ww<-list(
 shape=function(x) ifelse(x$depth<0,"egg",ifelse(x$leaf,"box","ellipse")),
 label=function(x) sprintf("\"%s\"", nice(x$name)),
 style="filled",
 fillcolor=function(x) sprintf("\"%s\"",scol[x$name])
)
         
make_plots<-function(){
 writeLines(dropViz(P),'g.dot')
 system('neato g.dot -T pdf > graph-cor.pdf')
 pullPos(dropViz(P))->POS
 
 set.seed(1)
 vistla(Affect~.,data=A,estimator='kt')->V

 writeLines(write.dot(V,vstyle=ww,gstyle=list(overlap='"prism"',ratio="0.9",rankdir="LR")),'v-nat.dot')
 system('dot v-nat.dot -T pdf > graph-vistla-nat.pdf')

 writeLines(dropVizPos(P,POS,V),'v.dot')
 system('neato v.dot -T pdf > graph-vistla.pdf')
 
 writeLines(dropVizPos(P,POS,prune(V,iomin=0.02)),'v0.02.dot')
 system('neato v0.02.dot -T pdf > graph-vistla-0.02.pdf')

 writeLines(write.dot(prune(V,iomin=0.02),vstyle=ww,gstyle=list(overlap='"prism"',ratio="0.9",rankdir="LR")),'v-nat-0.02.dot')
 system('dot v-nat-0.02.dot -T pdf > graph-vistla-nat-0.02.pdf')
 
 writeLines(dropVizPos(P,POS,prune(V,iomin=0.01)),'v0.01.dot')
 system('neato v0.01.dot -T pdf > graph-vistla-0.01.pdf')
}

if(!interactive()) make_plots()
