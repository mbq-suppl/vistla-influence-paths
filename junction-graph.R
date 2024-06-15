library(vistla)
library(praznik)
readRDS('junction.RDS')->A

expand.grid(a=1:ncol(A),b=1:ncol(A))->p
p[p$a>p$b,]->p

mi<-miMatrix(A)
mi[cbind(p$a,p$b)]->p$mi

p$a<-names(A)[p$a]
p$b<-names(A)[p$b]

p->P
P[P$mi>0.02,]->P
rownames(P)<-NULL

gsub("^(.).*","\\1",names(A))->str
sort(unique(str))->us
hsv((1:length(us))/length(us),0.3,1)->scol
names(scol)<-us
setNames(scol[str],names(A))->scol

dropViz<-function(x){
 sprintf('"%s" -- "%s" [penwidth=%0.3f]',x$a,x$b,0.5+x$mi/max(x$mi)*2.5)->E
 v<-unique(c(x$a,x$b))
 sprintf('"%s" [label="%s", fillcolor="%s", style="filled"]',v,v,scol[v])->V
 sprintf("graph {\n sep=\"3\"\n overlap=\"prism\"\n splines=\"true\"\n%s\n\n%s \n}",paste(V,collapse="\n"),paste(E,collapse="\n"))
}

make_plots<-function(){
 writeLines(dropViz(P),'junction-graph.dot')
 system('neato junction-graph.dot -T pdf > graph-cor-junction.pdf')
 
 set.seed(1)
 vistla(Y~.,data=A)->V
 vstyle<-list(
  shape=function(x) ifelse(x$depth<0,"egg",ifelse(x$leaf,"box","ellipse")),
  label=function(x) sprintf("\"%s\"",x$name),
  style="filled",
  fillcolor=function(x) sprintf('"%s"',scol[x$name])
 )
 write.dot(V,'junction-vi.dot',vstyle=vstyle)
 system('dot junction-vi.dot -T pdf > junction-vi.pdf')
}

if(!interactive()) make_plots()
