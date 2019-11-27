## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE, comment=FALSE, echo=FALSE--------------------------
library(tscR)
library(tidyverse)
library(grid)
library(ggplot2)
library(latex2exp)
library(tscR)
library(TSdist)

## ----warning=F, fig.cap="Figure 1", echo=FALSE--------------------------------
a <- c(10, 15, 17, 25)
b <- c(5, 8, 6, 9 )
c <- c(-19, -14, -12, -4)
x <- c(1, 2, 3, 4)

df <- as.data.frame(cbind(x,a,b,c))

ggplot(df, aes(x=x))+ 
  geom_line(aes(y=a), color = "red")+
  geom_point(y=a, col="red")+
  geom_line(aes(y=b))+
  geom_point(y=b)+
  geom_line(aes(y=c), color = "steelblue")+
  geom_point(y=c, color="steelblue")+
  geom_label(aes(x=1.5, y=14, label = TeX("$S_{a1}$", output="character")), parse=TRUE   ) +
  geom_label(aes(x=2.5, y=17, label = TeX("$S_{a2}$", output="character")), parse=TRUE  ) +
  geom_label(aes(x=3.5, y=23, label = TeX("$S_{a3}$", output="character")), parse=TRUE  ) +
  geom_label(aes(x=1.5, y=7.5, label = TeX("$S_{b1}$", output="character")), parse=TRUE  ) +
  geom_label(aes(x=2.5, y=8, label = TeX("$S_{b2}$)", output="character")), parse=TRUE  ) +
  geom_label(aes(x=3.5, y=9, label = TeX("$S_{b3}$", output="character")), parse=TRUE  ) + 
  geom_label(aes(x=1.5, y=-15, label = TeX("$S_{c1}$", output="character")), parse=TRUE  ) +
  geom_label(aes(x=2.5, y=-12, label = TeX("$S_{c2}$", output="character")), parse=TRUE  ) +
  geom_label(aes(x=3.5, y=-7, label = TeX("$S_{c3}$", output="character")), parse=TRUE  ) +
  geom_text(aes(x=1, y=12, label = "Traject. a"))+
  geom_text(aes(x=1, y=4, label = "Traject. b"))+
  geom_text(aes(x=1, y=-17, label = "Traject. c"))+
  theme(legend.position = "none")+
  xlab("") + ylab("")


## ----echo=FALSE---------------------------------------------------------------
dfx <- as.matrix(rbind(a,b,c))
dsE <- round( dist(dfx, method = "euclidean", diag = FALSE, upper = FALSE), 3)

## ----echo=FALSE---------------------------------------------------------------
time <- c(1,2,3,4)
dF <- tscR::frechetDistC(dfx, time)

## ----echo=FALSE---------------------------------------------------------------
dt1 <- DTWDistance(a,b)
dt2 <- DTWDistance(a,c)
dt3 <- DTWDistance(b,c)

## ----echo=FALSE, fig.align='center', fig.cap="Figure 2"-----------------------

df <- data.frame(T1 = c(140,100,75,35), T2=c(120,120,50,48), T3 = c(100,140,35,70))

df1 <- matrix(NA, nrow=10, ncol=3)
df2 <- matrix(NA, nrow=10, ncol=3)
df3 <- matrix(NA, nrow=10, ncol=3)
df4 <- matrix(NA, nrow=10, ncol=3)
for(i in seq(1,10)){
  df1[i,] <- jitter(as.numeric(df[1,]), factor = 1.5)
  df2[i,] <- jitter(as.numeric(df[2,]), factor = 1.5)
  df3[i,] <- jitter(as.numeric(df[3,]), factor = 1.5)
  df4[i,] <- jitter(as.numeric(df[4,]), factor = 1.5)
  }
df <- as.data.frame(rbind(df1,df2,df3,df4))
names(df) <- c("T1","T2","T3")

df <- as.data.frame.table(t(df))
df$Var3 <- rep(c("A","B","C","D"), each=30)

p1 <- df %>% 
ggplot( aes_(~Var1, ~Freq, group=~Var2) ) + 
  geom_line() + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  xlab("(A)") + ylab("") + ggtitle(label = "Raw trajectories")

p2 <- df %>%
  mutate(Var4 = recode(Var3, "B" = "A")) %>%
  mutate(Var4 = recode(Var4, "D" = "C")) %>% 
ggplot( aes_(~Var1, ~Freq, group=~Var2, colour=~Var4) ) + 
  geom_line() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  xlab("(B)") + ylab("") + ggtitle(label = "Frechet based cluster")

p3 <- df %>%
  mutate(Var4 = recode(Var3, "C" = "A")) %>%
  mutate(Var4 = recode(Var4, "D" = "B")) %>% 
ggplot( aes_(~Var1, ~Freq, group=~Var2, colour=~Var4) ) + 
  geom_line() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  xlab("(C)") + ylab("") + ggtitle(label = "Slope based cluster")

p4 <- df %>%
ggplot( aes_(~Var1, ~Freq, group=~Var2, colour=~Var3) ) + 
  geom_line() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  xlab("(D)") + ylab("") + ggtitle(label = "Combined clusters")


gridExtra::grid.arrange(p1,p2,p3,p4, nrow=2,
                        bottom = textGrob("Fig. 1",gp=gpar(fontsize=14,font=3)))


## ----eval = FALSE, echo=TRUE--------------------------------------------------
#  devtools::install_github("fpsanz/tscR")

## ----eval = TRUE--------------------------------------------------------------
library(tscR)

## ----eval =FALSE--------------------------------------------------------------
#  browseVignettes("tscR")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
data(tscR)
df <- tscR
head(df)

## ----echo = F, eval=TRUE------------------------------------------------------
matplot(t(df), type = "l", col = "gray30", lty = 1, ylab = "")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
time <- c(1,2,3)
sDist <- slopeDist(df, time)

## ----eval=TRUE----------------------------------------------------------------
sclust <- getClusters(sDist, k = 3)

## ----eval=T, echo=T-----------------------------------------------------------
plotCluster(data = df, clust = sclust, ncluster = "all")

## ----eval=T, echo=T-----------------------------------------------------------
plotCluster(df, sclust, 1)

## ----eval=T, echo=T-----------------------------------------------------------
plotCluster(df, sclust, c(1:2))

## ----eval = TRUE, echo = TRUE-------------------------------------------------
fdist <- frechetDistC(df, time)
fclust <- getClusters(fdist, 3)
plotCluster(df, fclust, "all")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
ccluster <- combineCluster(sclust, fclust)
plotCluster(df, ccluster, c(1:6))
plotCluster(df, ccluster, "all")

## ----eval=T, echo=T-----------------------------------------------------------
data( "tscR" )
bigDF <- tscR
senators <- imputeSenators( bigDF, k = 100 )

## -----------------------------------------------------------------------------
sdistSen <- frechetDistC( senators$senatorData, time = c( 1, 2, 3 ) ) 
cSenators <- getClusters( sdistSen, k = 4 )

## -----------------------------------------------------------------------------
plotCluster(senators$senatorData, cSenators, "all")
plotCluster(senators$senatorData, cSenators, c(1,2,3,4))

## -----------------------------------------------------------------------------
endCluster <- imputeSenatorToData(senators, cSenators)

## -----------------------------------------------------------------------------
plotClusterSenator(endCluster, "all")
plotClusterSenator(endCluster, c(1,2,3,4))

