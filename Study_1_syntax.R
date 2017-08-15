library(foreign)
library(MplusAutomation)
library(pastecs)
library(ggplot2)
library(psych)
library(psychometric)

masterdata<-read.spss("MR&MD Study 9 - SPSS Data.sav", to.data.frame=T, use.value.labels = FALSE)
names(masterdata)<-tolower(names(masterdata))
setwd(wd)
data<-masterdata

data$mj1<-(101-data$mj1)
data$mj14<-(101-data$mj14)
data$mj36<-(101-data$mj36)
data$mj59<-(101-data$mj59)
data$mj67<-(101-data$mj67)
data$mj79<-(101-data$mj79)
data$mj92<-(101-data$mj92)
data$mj94<-(101-data$mj94)
data$mj96<-(101-data$mj96)
data$mj97<-(101-data$mj97)

#Examine item-total (without item itself) correlations to see which items per subscale have the highest data-driven coherence
mr.scales.list = list("KinshipR" = c("mr1", "mr2", "mr3", "mr4", "mr5", "mr6", "mr7"), "MutualismR" = c("mr8", "mr9", "mr10", "mr11", "mr12", "mr13", "mr14", "mr15", "mr16"), "ReciprocityR" = c("mr17", "mr18", "mr19", "mr20", "mr21", "mr22", "mr23", "mr24", "mr25"), "HawkR" = c("mr26", "mr27", "mr28", "mr29", "mr30", "mr31", "mr32", "mr33"), "DoveR" = c("mr34", "mr35", "mr36", "mr37", "mr38", "mr39", "mr40", "mr41"), "FairnessR" = c("mr42", "mr43", "mr44", "mr45", "mr46", "mr47", "mr48", "mr49", "mr50"), "PossessionR" = c("mr51", "mr52", "mr53", "mr54", "mr55", "mr56", "mr57"))
                   
mj.scales.list = list("KinshipJ" = c("mj1", "mj2", "mj3", "mj4", "mj5", "mj6", "mj7"), "MutualismJ" = c("mj8", "mj9", "mj10", "mj11", "mj12", "mj13", "mj14", "mj15", "mj16", "mj17", "mj18", "mj19", "mj20", "mj21", "mj22", "mj23", "mj24", "mj25", "mj26", "mj27", "mj28", "mj29", "mj30", "mj31", "mj32", "mj33", "mj34", "mj35"), "ReciprocityJ" = c("mj36", "mj37", "mj38", "mj39", "mj40", "mj41", "mj42", "mj43", "mj44", "mj45", "mj46", "mj47", "mj48", "mj49", "mj50", "mj51", "mj52", "mj53", "mj54", "mj55", "mj56", "mj57"), "HawkJ" = c("mj58", "mj59", "mj60", "mj61", "mj62", "mj63", "mj64", "mj65", "mj66", "mj67", "mj68", "mj69", "mj70"), "DoveJ" = c("mj71", "mj72", "mj73", "mj74", "mj75", "mj76", "mj77"), "FairnessJ" = c("mj78", "mj79", "mj80", "mj81", "mj82", "mj83", "mj84", "mj85", "mj86", "mj87", "mj88", "mj89", "mj90"), "PossessionJ" = c("mj91", "mj92", "mj93", "mj94", "mj95", "mj96", "mj97"))

mr.items<-lapply(mr.scales.list, function(items){
  tmpdat<-data[, items]
  tmpitemcors<-item.exam(tmpdat)
  sort(row.names(tmpitemcors[ order(-tmpitemcors[,3]), ])[1:3])
})

mr.items

mj.items<-lapply(mj.scales.list, function(items){
  tmpdat<-data[, items]
  tmpitemcors<-item.exam(tmpdat)
  sort(row.names(tmpitemcors[ order(-tmpitemcors[,3]), ])[1:3])
})

mj.items

#These items were selected based on theoretical concerns and the above item-total correlations: 
macitems<-c("mr2", "mr3", "mr7", "mr10", "mr12", "mr15", "mr18", "mr22", "mr23", "mr27", "mr31", "mr33", "mr36", "mr37", "mr39", "mr43", "mr44", "mr45", "mr54", "mr56", "mr57", "mj4", "mj6", "mj7", "mj10", "mj20", "mj35", "mj39", "mj54", "mj55", "mj60", "mj68", "mj70", "mj75", "mj76", "mj77", "mj84", "mj85", "mj90", "mj94", "mj96", "mj97")

data <- data[, which(names(data) %in% macitems)]

#Descriptives
table(data$sex)
mean(data$age_years)
sd(data$age_years)


#
#Item descriptives
#

itemDesc<-function(data, scales.list=NULL){
  require(psych)
  require(pastecs)
  
  if(is.null(scales.list)){
    scales.list<-list(names(data))
  }
  
  datafiles<-lapply(scales.list, function(x){data[, names(data) %in% x]})
  
  tables<-lapply(datafiles, function(x){data.frame(Item=names(as.data.frame(x)), round(describe(as.data.frame(x))[,c(2,3,4,8,9)], 2), t(round(stat.desc(as.data.frame(x), basic = FALSE, norm = TRUE),2)[c(8,9,10,11),]))})
  return(tables=tables)
}

itemtable <- itemDesc(data)

write.csv(itemtable[[1]], file="Table 1 and 2 item descriptives.csv", row.names = FALSE)



#Do reliability analyses
doReliability<-function(data, keys.list, name, missing=TRUE, impute="none", omega=FALSE, omega.factors=NULL){
  require(psych)
  require(pastecs)
  
    
  scoredatanames<-as.vector(gsub("-", "", unlist(keys.list)))
  scoredatanames<-unique(scoredatanames)
  
  data<-subset(data, select=(names(data) %in% scoredatanames))
  
  keys<-make.keys(length(scoredatanames), keys.list=keys.list,item.labels=scoredatanames)
  
  scores <- scoreItems(keys,data, missing=missing, impute=impute)
  
  if(omega){
    if(!is.null(omega.factors)){
       sapply(1:length(keys.list), function(x){
         if(omega.factors[x]=="pa"){
           return(fa.parallel(data[keys.list[[x]]], fa="fa", 
                              use=ifelse(missing==TRUE, "pairwise.complete.obs", "complete.obs"),
                              plot=FALSE)$nfact)
         } else{
           return(x)
         }
       })
    } else {
       omega.factors<-rep(3, length(keys.list))
    }
    omegas<-unlist(sapply(1:length(keys.list), function(x){
      omega(data[keys.list[[x]]], nfactors=omega.factors[x])$omega.tot
    }))
  }
  
  interpret<-function(reliability=NULL){
    interpretation<-rep("Unacceptable", length(reliability))
    interpretation[reliability>=.5]<-"Poor"
    interpretation[reliability>=.6]<-"Questionable"
    interpretation[reliability>=.7]<-"Acceptable"
    interpretation[reliability>=.8]<-"Good"
    interpretation[reliability>=.9]<-"Excellent"  
    return(interpretation)
  }
  table_descriptives<-data.frame(Subscale=colnames(scores$scores), Items=unlist(lapply(keys.list, length)) , round(describe(scores$scores)[,c(2,3,4,8,9)], 2), t(round(stat.desc(scores$scores, basic = FALSE, norm = TRUE),2)[c(8,9,10,11),]), Alpha=round(as.vector(scores$alpha), 2), Interpret.a=interpret(round(as.vector(scores$alpha), 2)))
  
  if(omega){
    table_descriptives<-data.frame(table_descriptives, Omega=round(omegas, 2), Interpret.O=interpret(round(omegas, 2)))
  }
  
  write.csv(table_descriptives, paste0(name, " scale table.csv"), row.names = F)
  print(table_descriptives)
  cortab<-as.data.frame(round(cor(scores$scores, use=ifelse(missing==TRUE, "pairwise.complete.obs", "complete.obs")), 2))
  cortab[upper.tri(cortab)]<-""
  print(cortab)
  write.csv(cortab, paste0(name, " correlation table.csv"))
  return(list("table_descriptives"=table_descriptives, "Correlations"=cortab, "scores"=scores$scores))
}

macscales<-doReliability(data=data, keys.list = list("KinshipR"=names(data)[c(1:3)], "MutualismR"=names(data)[c(4:6)], "ReciprocityR"=names(data)[c(7:9)], "HawkR"=names(data)[c(10:12)], "DoveR"=names(data)[c(13:15)], "FairnessR"=names(data)[c(16:18)], "PossessionR"=names(data)[c(19:21)],"KinshipJ"=names(data)[c(22:24)], "MutualismJ"=names(data)[c(25:27)], "ReciprocityJ"=names(data)[c(28:30)], "HawkJ"=names(data)[c(31:33)], "DoveJ"=names(data)[c(34:36)], "FairnessJ"=names(data)[c(37:39)], "PossessionJ"=names(data)[c(40:42)]), name="MAC_2-12", omega=TRUE)

meanscores<-as.data.frame(macscales$scores)

newtable<-macscales$table_descriptives
newtable$CL_2.5<-round(newtable$mean-1.96*(newtable$sd/sqrt(newtable$n)),2)
newtable$CL_97.5<-round(newtable$mean+1.96*(newtable$sd/sqrt(newtable$n)),2)

write.csv(newtable, "table3WITHconfidenceintervals.csv", row.names = FALSE)


#
#EFA Assumptions checking
#
det(mrcor21)
det(mjcor21)

kmo.mrcor<-KMO(mrcor21)
round(kmo.mrcor$MSA,2)
round(range(kmo.mrcor$MSAi),2)
kmo.mjcor<-KMO(mjcor21)
round(kmo.mjcor$MSA,2)
round(range(kmo.mjcor$MSAi),2)

cortest.bartlett(mrcor21, n=1392)
cortest.bartlett(mjcor21, n=1392)

#
#Parallel analysis
#
fa.parallel(mrcor21, n.obs=1392)
fa.parallel(mjcor21, n.obs=1392)


mrfa.7<-fa(mrcor21, nfactors=7,  rotate="oblimin", n.obs=1392, fm="ml")
print.psych(mrfa.7, digits=2, cut=.3)
Object<-mrfa.7$loadings
Object<-round(Object, 2)
write.csv(Object, file="mrfa7loadings.csv")

mjfa.7<-fa(mjcor21, nfactors=7,  rotate="oblimin", n.obs=1392, fm="ml")
print.psych(mjfa.7, digits=2, cut=.3)
Object<-mjfa.7$loadings
Object<-round(Object, 2)
write.csv(Object, file="mjfa7loadings.csv")


#
#Conduct CFA's
#
dir.create(paste0(getwd(), "/Combine MR MJ"))
setwd(paste0(getwd(), "/Combine MR MJ"))
prepareMplusData(data, filename="finalmacitems.dat", inpfile = FALSE)
runModels()
modelsums<-extractModelSummaries()
HTMLSummaryTable(modelsums, filename="Model summaries.html", keepCols=c("Filename", "ChiSqM_Value", "ChiSqM_DF", "ChiSqM_ScalingCorrection", "AIC", "aBIC", "RMSEA_Estimate", "CFI", "TLI", "SRMR"), sortBy="RMSEA_Estimate")
getwd()
#
#Put Latent variable and mean score correlations into one table. Lower triangle is mean correlations
#
factorscores<-getSavedata_Data("mr7 with mj7 final.out")
factorscores<-dplyr::select(factorscores, -starts_with("MR"), -starts_with("MJ"), -ends_with("SE"))
latentcors<-as.matrix(cor(factorscores))


meancors<-as.matrix(cor(meanscores))
meancors[upper.tri(meancors)]<-latentcors[upper.tri(latentcors)]

write.csv(meancors, file="Combined Latent and Mean correlations MAC.csv", row.names=F)


#
#Save all this goodness
#
setwd(wd)
save.image("MAC Clean syntax workspace.RData")
load("MAC Clean syntax workspace.RData")

