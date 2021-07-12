## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- library-----------------------------------------------------------------
#Install from CRAN
#install.packages("DA")
## or you can get the latest version of HierDpart from github
#library(devtools)
#install_github("xinghuq/DA")

library("DA")
library("kernlab")


## -----------------------------------------------------------------------------
# example genepop file
f <- system.file('extdata',package='DA')
infile <- file.path(f, "Cattle_breeds_allele_frequency.csv")
Cattle_pop=file.path(f, "Cattle_pop.csv")
cattle_geno=read.csv(infile,h=T)
cattle_pop=read.csv(Cattle_pop,h=T)


## ----fig1, fig.height = 5, fig.width = 8.5, fig.align = "center"--------------
cattle_pop$x=factor(cattle_pop$x,levels = unique(cattle_pop$x))
### PCA
cattle_pc=princomp(cattle_geno[,-1])

#plot the data projection on the components
library(plotly)
   cols=rainbow(length(unique(cattle_pop$x)))
   p0 <- plot_ly(as.data.frame(cattle_pc$scores), x =cattle_pc$scores[,1], y =cattle_pc$scores[,2], color = cattle_pop$x,colors=cols[cattle_pop$x],symbol = cattle_pop$x,symbols = 1:15L) %>% 
     add_markers() %>%
     layout(scene = list(xaxis = list(title = 'PC1'),
                         yaxis = list(title = 'PC2')))

  
p0


## ----fig2, fig.height = 5, fig.width = 8.5, fig.align = "center"--------------
library(adegenet)
cattle_pop$x=factor(cattle_pop$x,levels = unique(cattle_pop$x))
###DAPC
cattle_dapc=dapc(cattle_geno[,-1],grp=cattle_pop$x,n.pca=10, n.da=3)

#plot the data projection on the components
library(plotly)
   cols=rainbow(length(unique(cattle_pop$x)))
   p1 <- plot_ly(as.data.frame(cattle_dapc$ind.coord), x =cattle_dapc$ind.coord[,1], y =cattle_dapc$ind.coord[,2], color = cattle_pop$x,colors=cols[cattle_pop$x],symbol = cattle_pop$x,symbols = 1:15L) %>% 
     add_markers() %>%
     layout(scene = list(xaxis = list(title = 'LDA1'),
                         yaxis = list(title = 'LDA2')))

   p1


## ----fig3, fig.height = 5, fig.width = 8.5, fig.align = "center"--------------

cattle_ldakpc=LDAKPC(cattle_geno[,-1],cattle_pop$x,n.pc=3)

 cols=rainbow(length(unique(cattle_pop$x)))
   p2 <- plot_ly(as.data.frame(cattle_ldakpc$LDs), x =cattle_ldakpc$LDs[,1], y =cattle_ldakpc$LDs[,2], color = cattle_pop$x,colors=cols[cattle_pop$x],symbol = cattle_pop$x,symbols = 1:15L) %>% 
     add_markers() %>%
     layout(scene = list(xaxis = list(title = 'LDA1'),
                         yaxis = list(title = 'LDA2')))
p2

## ----fig4, fig.height = 5, fig.width = 8.5, fig.align = "center"--------------

cattle_lfda=LFDA(cattle_geno[,-1],cattle_pop$x,r=3,tol=1E-3)

cols=rainbow(length(unique(cattle_pop$x)))
p3 <- plot_ly(as.data.frame(cattle_lfda$Z), x =cattle_lfda$Z[,1], y =cattle_lfda$Z[,2], color = cattle_pop$x,colors=cols[cattle_pop$x],symbol = cattle_pop$x,symbols = 1:15L) %>% 
     add_markers() %>%
     layout(scene = list(xaxis = list(title = 'LDA1'),
                         yaxis = list(title = 'LDA2')))
p3

## ----fig5, fig.height = 5, fig.width = 8.5, fig.align = "center"--------------


cattle_lfdakpc=LFDAKPC(cattle_geno[,-1],cattle_pop$x,n.pc=3,tol=1E-3)

cols=rainbow(length(unique(cattle_pop$x)))
p4 <- plot_ly(as.data.frame(cattle_lfdakpc$LDs), x =cattle_lfdakpc$LDs[,1], y =cattle_lfdakpc$LDs[,2], color = cattle_pop$x,colors=cols[cattle_pop$x],symbol = cattle_pop$x,symbols = 1:15L) %>% 
     add_markers() %>%
     layout(scene = list(xaxis = list(title = 'LDA1'),
                         yaxis = list(title = 'LDA2')))
p4

## ----fig6, fig.height = 5, fig.width = 8.5, fig.align = "center"--------------
cattle_klfda=KLFDA(as.matrix(cattle_geno[,-1]),as.factor(cattle_pop$x),r=3,tol=1E-10,prior = NULL)

cols=rainbow(length(unique(cattle_pop$x)))
p5 <- plot_ly(as.data.frame(cattle_klfda$Z), x =cattle_klfda$Z[,1], y =cattle_klfda$Z[,2], color = cattle_pop$x,colors=cols[cattle_pop$x],symbol = cattle_pop$x,symbols = 1:15L) %>% 
     add_markers() %>%
     layout(scene = list(xaxis = list(title = 'LDA1'),
                         yaxis = list(title = 'LDA2')))
p5

## ----fig7, fig.height = 5, fig.width = 8.5, fig.align = "center"--------------


library(adegenet)
## asignment plot
compoplot(as.matrix(cattle_klfda$bayes_assigment$posterior),show.lab = TRUE, posi=list(x=5,y=-0.01),txt.leg = unique(cattle_pop$x))


