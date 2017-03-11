rm(list=ls(all=TRUE))
#Test changes in time series

library(haven)
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(C50)
# coba_dataset <- read_dta("C:/Users/Gabriela A. Werb/OneDrive/coba_dataset.dta")
# coba_dataset <- read_dta("C:/Users/User/OneDrive/coba_dataset.dta")

wd <- "/Users/frederikwisser/Desktop/GSEFM/2016:2017/Big Data/Project/"
# setwd(wd)
# coba_dataset <- read.csv(paste0(wd,"Data/coba_dataset.dta"), header = TRUE, sep = ",")
coba_dataset <- read_dta(paste0(wd,"Data/coba_dataset.dta"))
# coba_dataset <- read_dta("C:/Users/Gabriela A. Werb/OneDrive/coba_dataset.dta")
# coba_dataset <- read_dta("C:/Users/User/OneDrive/coba_dataset.dta")
# colnames(coba_dataset) <- tolower(colnames(coba_dataset));



# Data Cleaning / Variable Selection####
coba_dataset$vers <- rowSums(subset(coba_dataset, select=c(klv,nuv,dmof,rlv,rv,sih,vazrv,vazfrv,vazklv,vazflv, vazrlv,vazbuz,vazriest,vazruer,vazunfal,vazkrank,vazsach)))
coba<-coba_dataset[,c("id","profitq","clv","bkkanzum","bspvohmo","kkboni","gaanzum","krkanzum", "obwanzum", "obwanm","dmanzkau", "dmanzver","bfpkprod","vers", "saf", 
                      "krsaldo","dpdwpwer","dpboerse","anzkubez",	"scdbsalb",	"abwawe",	"kdsegmen", "gschlcht","dmfamsta",	"dmalter",	"altercl",	"akadem",	"stifnkd",	"grdsegid",	
                      "ostfil",	"nordfil",	"beruf",	"zufuekz", "sternz",	"gemschl",	"bpifalfk",	"clfilhb","entfwf")]


#Rename Variables 

setnames(coba, old = c("bspvohmo","gaanzum","krkanzum","obwanzum", "obwanm", "dmanzkau","dmanzver", "bfpkprod", "dpdwpwer","dpboerse",
                       "dmalter","altercl", "gschlcht","dmfamsta","stifnkd","grdsegid","zufuekz","bpifalfk","clfilhb","entfwf","scdbsalb"), 
         new = c("bar_einlagen","atm","creditcard","onlineum","onlinelog","wpkauf","wpver","baufi", "depotwert","depotgewinn",
                 "age","agecl", "gender", "famsta", "kids", "segment","referral", "totkred","closetofil","disttofil","saldospar"))

# Heavy Trader dummy (Y/N)
coba$trade<-coba$wpkauf+coba$wpver
coba$heavytrader<-ifelse(coba$trade>4,1,0)
coba$trade<-NULL # exclude variable trade
tabulate(as.factor(coba$heavytrader)) 
count(unique(coba[coba$heavytrader==1,"id"])) #208 heavy traders

#Baufinanzierung Dummy
coba$baufi<-ifelse(coba$baufi==0,0,1)


# Substitute NA's in profitq and clv ####
coba$clv.mean <- with(coba, ave(clv, id, FUN = function(x) mean(x, na.rm = TRUE)))
coba$profit.mean <- with(coba, ave(profitq, id, FUN = function(x) mean(x, na.rm = TRUE)))
colnames(coba)[colnames(coba)=="clv.mean"] <- "clv.mean" 
colnames(coba)[colnames(coba)=="profit.mean"] <- "profit.mean" 
coba$profitqn<-coba$profitq
coba[which(is.na(coba$profitqn)), "profitqn"] <-coba[which(is.na(coba$profitqn)), "profit.mean"] 
coba$clvn<-coba$clv
coba[which(is.na(coba$clvn)), "clvn"] <-coba[which(is.na(coba$clvn)), "clv.mean"] 

#Check if it worked - only substituting NA's by the avererage of the other observations for the same id
indice<-(unique(coba[which(is.na(coba$profitq)),"id"])) # creates a data frame (list) with the ids, but we need to convert it to a vector before using it
temp<-coba[coba$id %in% unname(unlist(indice)), ]
indice<-(unique(coba[which(is.na(coba$clv)),"id"])) # creates a data frame (list) with the ids, but we need to convert it to a vector before using it
temp<-coba[coba$id %in% unname(unlist(indice)), ]

#Substitute variables by the new ones
coba$clv<-coba$clvn
coba$profitq<-coba$profitqn
coba$profitqn<-coba$clvn<-NULL
#Round
coba$clv<-round(coba$clv,2)
coba$profitq<-round(coba$profitq,2)
coba$clv.mean<-round(coba$clv.mean,2)
coba$profit.mean<-round(coba$profit.mean,2)

# Check all variables for NA's ####
summary(coba)

# Exclude Outliers ####
#Compute quantiles
quantile(coba$profitq, prob = seq(0, 1, length = 11), type = 5)

boxplot(coba$profit.mean,col="lightgray",horizontal=TRUE, main="Average profit", xlab="Euros")
quantile(coba$profit.mean, prob = seq(0, 1, length = 21), type = 5)
plot(coba$depotwert,coba$profitq) 

#Eliminate outliers in profit.mean (upper and lower 2.5%)

quantile(coba$profit.mean,0.975) # 199.01
quantile(coba$profit.mean,0.025) # -1.44
cobac<-coba[which((coba$profit.mean < 199) & (coba$profit.mean >-1.44)),]

length(unique(cobac$id)) # 9497 customers left

# Create dataset with only averages / categorical variables by customer: cobaun ####
lab<-colnames(cobac)

#Group numeric variables
cobaun<-cobac %>% group_by(id) %>%  summarise(bkkkanzum=round(mean(bkkanzum),0), bar_einlagen=round(mean(bar_einlagen),2),kkboni=round(mean(kkboni),0),atm=round(mean(atm),0),creditcard=round(mean(creditcard),0), 
                                              onlineum=round(mean(onlineum),0), onlinelog=round(mean(onlinelog),0), wpkauf=round(mean(wpkauf),0), wpver=round(mean(wpver),0),
                                              baufi=round(mean(baufi),0), vers=round(mean(baufi),0),
                                              saf=round(mean(baufi),0), krsaldo=round(mean(krsaldo),2),
                                              depotwert=round(mean(depotwert),2), depotgewinn=round(mean(depotgewinn),2),
                                              anzkubez=round(mean(anzkubez),0), saldospar=round(mean(saldospar),2),
                                              abwawe=round(mean(abwawe),2), famsta=round(mean(famsta),0),
                                              age=round(mean(age),0), agecl=round(mean(agecl),0),
                                              akadem=round(mean(akadem),0), kids=round(mean(kids),0),
                                              ostfil=round(mean(ostfil),0), nordfil=round(mean(nordfil),0),
                                              beruf=round(mean(beruf),0), gemschl=round(mean(gemschl),0),
                                              totkred=round(mean(totkred),0), closetofil=round(mean(closetofil),0),
                                              disttofil=round(mean(disttofil),0), heavytrader=round(mean(heavytrader),0),
                                              clv=round(mean(clv.mean),2), profit=round(mean(profit.mean),2))

#Add categorical variables
pos<-match(unique(cobac$id),cobac$id) # position of the first observations for each id
prov<-cobac[pos,] #new data set with only the first obs of each id
cobaun$kdsegmen<-prov$kdsegmen
cobaun$gender<-prov$gender
cobaun$segment<-prov$segment
cobaun$referral<-prov$referral
cobaun$sternz<-prov$sternz

rm(pos,prov)




# Define customers as churned ####
# We define a customer as churned when he (in average) has no detected activity in the measured 15 months, has no credit product with the bank, has less
#than 100??? in KK/Sparbuch/Depot and has generated no profit.

cobaun$churn<-ifelse(cobaun$bkkkanzum==0 & cobaun$kkboni==0 & cobaun$atm==0 & cobaun$creditcard==0 & cobaun$onlineum==0 & cobaun$onlinelog==0 &
                       cobaun$wpkauf==0 & cobaun$wpver==0 & cobaun$baufi==0 & cobaun$vers==0 & cobaun$saf==0 & 
                       cobaun$krsaldo==0 & cobaun$krsaldo==0 & cobaun$bar_einlagen<100 & cobaun$depotwert<100 & cobaun$saldospar<100 & cobaun$profit==0,1,0)

# Check results
rbind(levels(as.factor(cobaun$churn)),tabulate(as.factor(cobaun$churn)))
test<-cobaun[which(cobaun$churn==1),]

############################################################################
# cluster analysis 
############################################################################

cobaun <- as.data.frame(cobaun)
data <- cobaun
# many missing values in gemschl
data$gemschl[is.nan(data$gemschl)]<--1

# sapply(cobaun, class)  check class of variable

x <- matrix(data[,1])
for (i in 2:ncol(data)) {
  if (class(data[,colnames(data)[i]]) == "character") {
    coluna <- matrix(as.numeric(as.factor(data[,i])))
    x <- cbind(x, coluna)
  } else {
    coluna <- as.numeric(data[,i])
    x <- cbind(x, coluna)
  }
}

colnames(x) <- colnames(data) 

n_cluster = 5
km.out=kmeans(x[,c("agecl","akadem","kids","ostfil","nordfil","beruf","gemschl","totkred","disttofil","kdsegmen","gender","segment","sternz") ],n_cluster,nstart=20)
cluster <- km.out$cluster
plot(x, col=(km.out$cluster +1), main="K-Means Clustering Results with K=5", xlab="", ylab="", pch=20, cex=2)

# check cluster for churning -> rows=cluster number; 
check <- as.data.frame(list(churn_rate=matrix(rep(0,n_cluster)))); rownames(check) <- 1:n_cluster; 
for (i in 1:n_cluster) {
  check[i,1] <- sum(x[which(cluster==i),c("churn")])/length((x[which(cluster==i),c("churn")]))
}

############################################################################
# logit on churn
############################################################################

# logit on churn
x_vars <- tolower(c("agecl","akadem","kids","ostfil","nordfil","beruf","gemschl","totkred","disttofil","kdsegmen","gender","segment"))
logit <- glm(data$churn ~.,family=binomial(link='logit'),data=data[,x_vars])
write(print(xtable(logit,caption='Baseline Model: Repurchase Logit',digits=c(0,3,3,2,4)),table.placement = "H",caption.placement = 'top',latex.environments = "flushleft"), file=paste0(paste0(wd,"Code"),"TableA11.txt"))


############################################################################
# tree model churning
############################################################################
# tree model
x_vars <- tolower(c("agecl","akadem","kids","ostfil","onlineum","nordfil","beruf","gemschl","totkred","disttofil","kdsegmen","gender","segment","sternz"))

# have to clean some data: why are there 0s in altercl?
data$agecl[which(data$agecl==0)]<--1
# need to have sufficient number of observations for each category in categorical variable
data$beruf[which(data$beruf==11 | data$beruf==58 | data$beruf==58 | data$beruf==78 | data$beruf==88)]<-83
data$totkred[which(data$totkred==6 | data$totkred==7 | data$totkred==8 )]<-5
# create segments for disttofil
data$disttofil[which(data$disttofil>=0 & data$disttofil<=100)] <-50
data$disttofil[which(data$disttofil>100 & data$disttofil<=200)] <-150
data$disttofil[which(data$disttofil>200 & data$disttofil<=300)] <-250
data$disttofil[which(data$disttofil>300 & data$disttofil<=400)] <-350
data$disttofil[which(data$disttofil>400 & data$disttofil<=500)] <-450
data$disttofil[which(data$disttofil>500 & data$disttofil<=692)] <-580
# impute missing values for gender
data$gender[which(data$gender=="?")] <- "M"
data$kdsegmen[which(data$kdsegmen=="?")] <- "NA"
# impute sternz 
data$sternz[which(data$sternz=="?")] <- "ST"

# impute missing for onlien umsatz
data$onlineum[which(data$onlineum>=9)] <- 10

x <- matrix(data[,1])
for (i in 2:ncol(data)) {
  if (class(data[,colnames(data)[i]]) == "character") {
    coluna <- matrix(as.numeric(as.factor(data[,i])))
    x <- cbind(x, coluna)
  } else {
    coluna <- as.numeric(data[,i])
    x <- cbind(x, coluna)
  }
}

colnames(x)<-colnames(data)

tree_model <- C5.0(x[,x_vars], as.factor(x[,c("churn")]), control=C5.0Control(minCases=10),trials=10)




#### Activity Index ####

