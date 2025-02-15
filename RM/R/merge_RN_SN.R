library(readxl)
library(dplyr)

# setwd(choose.dir(default=getwd())) # where run data are saved
# d2=getwd()
# d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R' #where (PRM, bgm, group data) are saved
d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM' #where (PRM, bgm, group data) are saved

setwd(d1)

# 
# #linux
d1='/home/ryan/Git/atneus_RM'
# d2='/home/ryan/AtlRuns/20180510a'
setwd(d1)

codes=read.table(file=paste(d1, '/R/coderelations.csv', sep=''), header=T, sep=',', stringsAsFactors = F)
codes=codes[-c(1:2),]



# t=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/RNSN.xlsx', sheet='Sheet1')
# c=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/RNSN.xlsx', sheet='Sheet2')
# v=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/RNSN.xlsx', sheet='Sheet3')
# 
# 
# colnames(c)=c('Code', 'Species')
# tt=merge(t, c, by=('Species'))
# colnames(v)=c('Code', 'li_a', 'li_b')
# tt2=merge(tt, v, by='Code')
# 
# 
# tt2$length=(tt2$`weight (g)`/tt2$li_a)^(1/tt2$li_b)
# tt3=tt2[with(tt2, order(Code, Cohort)),]
# write.csv(tt3, file='length_weight_v15_test.csv', col.names = T, row.names = F, sep=',')
# 
# spp=unique(tt3$Species)
# pdf(nc='length_weight.pdf',paper='A4r',width=11, height=8)
# for(i in 1:length(spp)){
# 
#   plot( tt3$length[which(tt3$Species==spp[i])]~tt3$Cohort[which(tt3$Species==spp[i])], type='b', ylab='length (cm)', xlab='cohort', main=spp[i],
#         ylim=c(0, max(tt3$length[which(tt3$Species==spp[i])])))
#   plot( tt3$`weight (kg)`[which(tt3$Species==spp[i])]~tt3$Cohort[which(tt3$Species==spp[i])], type='b', ylab='weight (kg)', xlab='cohort', main=spp[i],
#         ylim=c(0, max(tt3$`weight (kg)`[which(tt3$Species==spp[i])])))
#   
# }
# dev.off()
# 
# cr=read_xlsx('RNSN.xlsx', sheet='Sheet4')
# tt=read.table('length_weight_v15.csv', header = T, sep=',')

## missing entries for MPF, BPF, FDE, FDF... look into this.


## Read in RN and SN values, xlsx updated 20180709, '____data' tab has values copied from '___calc_doc' tab, which has documentation and calculations
## (von Bertalanffy model, Atlantis calcs for RN SN weight conversions, length_weight relationships, etc.)
# x=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/length_weight_v15.xlsx', sheet='length_weight_v15_data')
# x=read_xlsx(paste(d1, '/R/length_weight_v15.xlsx', sep=''), sheet='length_weight_v15_data_update')
# x$Rname=paste(x$Species, x$Cohort, "_ResN", sep="") #make name same as variable name in netcdf file
# x$Sname=paste(x$Species, x$Cohort, "_StructN", sep="") #make name same as variable name in netcdf file
# ## Subset above to use for replacement in initial conditions netcdf file
# xR=data.frame(Variables=x$Rname,RN=x$RN_mg) # this is the new calculated reserve nitrogen value
# xS=data.frame(Variables=x$Sname,SN=x$SN_mg) # this is the new calculated structural nitrogen value


### edit RN and SN values to compensate for long-lived species being at old age for cohort 1 (e.g. whales, sharks, etc)
# newX=x
# newX[,c('SN', 'RN', 'weight_g')]=NULL
# newX[,c('weight_kg', 'weight_t', 'weight_lbs', 'length_cm')]=NULL
# ### weight of inidividual fish in a cohort should be mean size (age 1.5 in cohort 1 of a fish that spends 2 years in a cohort)
# ### The recruit weight is then that of an age 1 fish
# newX$interval=apply(newX[,'numyrs'], 1, function(x) median(seq(1:x))) # use to get mean age of cohort
# newX$newage=newX$age*newX$interval # this is updated mean age of cohort to use in von Bert calcs instead of below
# newX$newage2=newX$age-(newX$numyrs-1) # use this for von Bertalannfy calculations, li_a li_b (offsets values to start at 1)


#### 20190612 Updated version ###
# remove old static data, update calc of age as beginning of cohort age (i.e. for 4 year numyrageclass cohort age = 1, 5, 9, etc.)
x=read_xlsx(paste(d1, '/R/length_weight_v15.xlsx', sep=''), sheet='length_weight_v15_calc_age_R') # change To to zero for all
x=x[,1:11]
x$Rname=paste(x$Species, x$Cohort, "_ResN", sep="") #make name same as variable name in netcdf file
x$Sname=paste(x$Species, x$Cohort, "_StructN", sep="") #make name same as variable name in netcdf file
newX=x

newX$vbert_cm2=newX$Linf*(1-exp(-newX$K*(newX$cohortAGE-newX$To)))
# newX$vbert_cm2=newX$Linf*(1-exp(-newX$K*(newX$newage-0))) # use if To set to zero
newX$grams2=newX$li_a*(newX$vbert_cm2^newX$li_b)
newX$inches=newX$vbert_cm2*0.393701
newX$lbs=newX$grams2*0.00220462
newX$recruit_cm=newX$Linf*(1-exp(-newX$K*(1-newX$To))) # age 1 fish
# newX$recruit_cm=newX$Linf*(1-exp(-newX$K*(1-0))) # age 1 fish # use if To set to zero
newX$recruit_grams=newX$li_a*(newX$recruit_cm^newX$li_b)

## plot length vs weight to make sure things look OK - inches/lbs
nmc=unique(newX$Code)
pdf(file='weight_length_US_20190612_updated.pdf')
for(i in 1:length(nmc)){
  ii=nmc[i]
plot(newX$inches[newX$Code==ii]~newX$lbs[newX$Code==ii], ylab='inches', xlab='lbs', main=ii)
}
dev.off()
##
## plot length vs weight to make sure things look OK - metric
nmc=unique(newX$Code)
pdf(file='weight_length_metric_20190612_updated.pdf')
cohort=seq(1,10,1)
for(i in 1:length(nmc)){
  ii=nmc[i]
  # plot(newX$vbert_cm2[newX$Code==ii]~newX$grams2[newX$Code==ii], ylab='cm', xlab='grams', main=ii)
  plot(newX$vbert_cm2[newX$Code==ii]~cohort, ylab='cm', xlab='cohort', main=ii)
}
dev.off()

### USE THESE VALUES TO REPLACE RN AND SN IN INITIAL CONDITIONS FILE ### updated 20180711
newX$SN_2=round(newX$grams2/20/5.7/3.65*1000, digits=2) # convert grams wet weight to mg SN
newX$RN_2=round(newX$SN_2*2.65, digits = 2) # convert SN to RN (mg)
### for recruits, use in KWRR_xxx KWSS_xxx in biol file
newX$recruitSN=round(newX$recruit_grams /20/5.7/3.65*1000, digits=2)
newX$recruitRN=round(newX$recruitSN *2.65, digits=2)

### save recruit weights to use in biol file KWRR_XXX and KWSR_XXX ###
recruits=data.frame(newX$recruitRN, newX$recruitSN)
recruits$code=newX$Code
sec=seq(from=1, to=length(recruits$code),by=10)
recruits=recruits[sec,]
recruits$Rname=paste("KWRR_",recruits$code, sep="") 
recruits$Sname=paste("KWSR_",recruits$code, sep="") 
columns=colnames(recruits)
col2=columns[c(3, 4, 1, 5, 2)]
rec=recruits[,col2]
write.table(rec, file='vertebrate_recruit_weights_kwrr_kwsr_20190612.csv', sep=',', col.names = T, row.names = F)


## Subset above to use for replacement in initial conditions netcdf file
xR=data.frame(Variables=newX$Rname,RN=newX$RN_2) # this is the new calculated reserve nitrogen value
xS=data.frame(Variables=newX$Sname,SN=newX$SN_2) # this is the new calculated structural nitrogen value

### reshape and cast long to wide to get individual vertebrate weight by cohort (grams)
library(reshape)
t=newX[,c("Code", "grams2", "Cohort")]
t2=melt.data.frame(t, id.vars=c('Code', 'Cohort'), measure.vars = 'grams2')
t3=cast(t2, Code ~ Cohort)
write.table(t3, file='vertebrate_weights_grams_20190617.csv', sep=',', col.names = T, row.names = F)

# length at age for initial conditions 
t=newX[,c("Code", "Species", "vbert_cm2", "Cohort")]
t2=melt.data.frame(t, id.vars=c('Code', 'Cohort'), measure.vars = 'vbert_cm2')
t3=cast(t2, Code ~ Cohort)
t2=melt.data.frame(t, id.vars=c('Species', 'Cohort'), measure.vars = 'vbert_cm2')
t4=cast(t2, Species ~ Cohort)
write.table(t4, file='vertebrate_init_length_cm_20190110.csv', sep=',', col.names = T, row.names = F)
t5=left_join(t3, codes, by=c('Code'='Child'))
# write.table(t5, file='vertebrate_init_length_cm.csv', sep=',', col.names = T, row.names = F)


t=newX[,c("Code", "Cohort")]
t$RNSN=newX$RN_2 + newX$SN_2
# t2=melt.data.frame(t, id.vars=c("Code", "Cohort")) #, measure.vars = c('RN_2', 'SN_2'))
t3=cast(t, Code ~ Cohort)
write.table(t3, file='vertebrate_sumRNSN_20190612.csv', sep=',', col.names = T, row.names = F)

## write out RN and SN by cohort
t=newX[,c("Code", "Cohort", "SN_2")]
t3=cast(t, Code ~ Cohort)
write.table(t3, file='vertebrate_SN.csv', sep=',', col.names = T, row.names = F)

### get C biomass (mg C per individual), RN+SN, convert to C; used for tuning mum and C
t=newX[,c("Code", "Cohort")]
t$biomass=(newX$RN_2 + newX$SN_2)*20*5.7
t2=melt.data.frame(t, id.vars=c('Code', 'Cohort'), measure.vars = 'biomass')
t3=cast(t2, Code ~ Cohort)
write.table(t3, file='vertebrate_biomass_mgC.csv', sep=',', col.names = T, row.names = F)



## read in initial conditions numbers (saved as output from shinyrAtlantis, nums.df)
n=read.csv(file.path(paste(d1,'/init_nums.csv', sep="")), stringsAsFactors = F)
num=reshape(n,idvar = 'Species', timevar = 'Cohort', direction = 'wide' )
# n2=melt.data.frame(n, id.vars = c("Species", "Cohort"))
# nn=cast(n2, "Species" ~ "Cohort")

# copy scaling factor from run file, drop inverts:
# updated 20180816
#init_scalar       89
scl=c(0.205275,1.597980,3.061782,0.449815,0.110567,0.003238,0.062150,0.153040,0.113941,0.000245,0.172427,0.016489,0.100860,
  1.379642,0.058535,0.097947,0.038402,0.056091,0.220816,6.451870,1.174644,0.152468,0.773771,1.291924,0.001254,0.030864,
  0.048406,0.000504,0.011100,0.000072,1.412003,0.037380,0.000002,0.000309,0.000437,0.000280,0.003929,0.000052,0.699984,
  1.829899,3.618399,0.159980,0.385529,0.018171,0.000515,0.727557,0.232208,0.168540,0.047367,0.241343,0.050138,2.172812,
  0.434166,0.349456,0.180294,0.881796,1.130103,0.012676,0.002091)

names=read_xlsx('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/Scaling_biomass_logfile_output.xlsx', sheet='desiredBiomass')
nm=order(names$Name[1:59])
names2=names$Name[nm]
test=names2 %in% num$Species # skate is not included here???? not sure why
names2[which(test!=1)]


### open initial conditions nc
library(ncdf4)
# nc=nc_open('RMinit_newvalues2017.nc', write=T) # old init, old _FillValues, (used to create file below)
# nc=nc_open('20180710init.nc', write=T) # edited 20180710, new _FillValues, need to update data with _FillValues
# 
# a=data.frame(attributes(nc$var), stringsAsFactors = F) # dataframe
# aa=attributes(nc$var) #list
# print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"Netcdf attributes"))
# 
# ## this looks promising...
# # ncvar_change_missval( nc, varid, missval )
# # aa$names[1]
# # aaR=aa$names[which("_ResN" %in% aa$names)] 
# # ncvar_change_missval( nc, avarid, missval )
# 
# 
# # library(stringr)
# # str_locate_all(aa$names[2], "_ResN")
# 
# # grep("_ResN", aa$names[2]) # search 
# # grep("_StructN", aa$names[2])
# aaR=data.frame(nm=a$names[grep("_ResN", a$names)]) # split names with ResN
# aaS=data.frame(nm=a$names[grep("_StructN", a$names)]) # split names with SN
# # aaR=aa$names[which(grep("_ResN", aa$names)==1)]
# 
# aaR=data.frame(nm=a$names[grep("_ResN", a$names)]) # split names with ResN
# aaS=data.frame(nm=a$names[grep("_StructN", a$names)]) # split names with SN
# 
# aaRS=rbind(aaR, aaS)
# b=which(a$names %in% aaRS$nm)
# c=a[b,1]
# ## can manually edit the initial conditions nc like this...
# # nc$var$Anchovies10_ResN$missval=x$RN_mg[10]
# ## and with indexing like this...
# # nc$var[[2]]$missval # second variable entry of 1917
# # nc$var[[2]]$name # use to match aaR/aaS
# 
# # grep("_ResN", nc$var[[2]]$name) # search 
# # 
# # nc$var[[i]]$missval=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
# # nc$var[[i]]$missval=xS$SN[which(xS$Variables==nc$var[[i]]$name)]
# 
# ## reassign missing values for RN and SN, this works now, but need to replace values for all non vert entries below yellowtail flounder
# ## as this routine causes the _FillValue to be replaced with " " (copy and paste old vals from cdf file, then do ncgen...)
# for (i in 1:dim(a)[1]){ #1:10){
#   # print(nc$var[[i]]$name) # use to match aaR/aaS
#   testNum=grep("_Nums", nc$var[[i]]$name) # search for Nums, skip to next
#   testN=grep("_N", nc$var[[i]]$name)
#   test2=grep("_ResN", nc$var[[i]]$name) # search for RN
#   if (length(testNum) ==1){
#     next
#   } else if (length(testN) ==1){
#     next
#   }  else if (length(test2) == 1) {
#     # print(nc$var[[i]]$missval)
#     # print(xR$RN[which(xR$Variables==nc$var[[i]]$name)])
#     # nc$var[[i]]$missval=xR$RN[which(xR$Variables==nc$var[[i]]$name)]
#     # ncvar_change_missval(nc, a[i,1],xR$RN[which(xR$Variables==nc$var[[i]]$name)]) #nc$var[[i]]$name)])
#     ncatt_put(nc,a[i,1],"_FillValue", xR$RN[which(xR$Variables==nc$var[[i]]$name)])
#     ncvar_put(nc,a[i,1], rep(xR$RN[which(xR$Variables==nc$var[[i]]$name)],150)) # replicate 5x30
#     # print(nc$var[[i]]$missval) 
#   } else if (length(test2) != 1) {
#     # print(nc$var[[i]]$missval)
#     # print(xS$SN[which(xS$Variables==nc$var[[i]]$name)])
#     # nc$var[[i]]$missval=xS$SN[which(xS$Variables==nc$var[[i]]$name)]
#     # ncvar_change_missval(nc, a[i,1],xS$SN[which(xS$Variables==nc$var[[i]]$name)])
#     ncatt_put(nc,a[i,1],"_FillValue", xS$SN[which(xS$Variables==nc$var[[i]]$name)])
#     # print(nc$var[[i]]$missval) #
#     ncvar_put(nc,a[i,1], rep(xS$SN[which(xS$Variables==nc$var[[i]]$name)],150))
#     
#   }
#   nc_sync(nc)
# }
# nc_sync(nc)
# nc_close(nc)


#### 20180711 update - used this to write data to variables for ResN and StructN; (just need xR and xS from routine above)
nc=nc_open('RMinit2_2019.nc', write=T) # 
a=data.frame(attributes(nc$var), stringsAsFactors = F) # dataframe

aaR=data.frame(nm=a$names[grep("_ResN", a$names)], stringsAsFactors = F) # split names with ResN
aaS=data.frame(nm=a$names[grep("_StructN", a$names)], stringsAsFactors = F) # split names with SN
aaNum=data.frame(nm=a$names[grep("_Nums$", a$names)], stringsAsFactors = F) # split names with Nums
aaN=data.frame(nm=a$names[grep("_N$", a$names)], stringsAsFactors = F) # split names with _N
aaN1=data.frame(nm=a$names[grep("_N1", a$names)], stringsAsFactors = F) # split names with _N
aaN2=data.frame(nm=a$names[grep("_N2", a$names)], stringsAsFactors = F) # split names with _N

aaRS=rbind(aaR, aaS, stringsasFactors=F)
b=which(a$names %in% aaRS$nm)
c=a[b,1]

for (i in 1:length(b)){
  vv=as.numeric(b[i]) # index of ResN and StructN in entire list `a` of nc file
  testSN=grep("_StructN", a[vv,1]) 
  testRN=grep("_ResN", a[vv,1])
  
  if (length(testRN) ==1){
    ncatt_put(nc,a[vv,1],"_FillValue", xR$RN[which(xR$Variables==a[vv,1])])
    ncvar_put(nc,a[vv,1], rep(xR$RN[which(xR$Variables==a[vv,1])],150)) # replicate 5x30
  } else if (length(testSN) ==1){
    ncatt_put(nc,a[vv,1],"_FillValue", xS$SN[which(xS$Variables==a[vv,1])])
    ncvar_put(nc,a[vv,1], rep(xS$SN[which(xS$Variables==a[vv,1])],150))
  }
  nc_sync(nc)
}
nc_sync(nc)
nc_close(nc)

# then in terminal:
# ncatted -O -a _FillValue,,d,, RMinit2_2019.nc RMinitnofill_2019.nc



### check new values in RM_NEUS.r initial conditions
library(shinyrAtlantis)
library(tidyverse)
library(stringr)
library(rbgm)
library(bgmfiles)
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
bgm.file <- ("neus_tmerc_RM2.bgm")
# NEUS_15_init=make.sh.init.object(bgm.file, '20180710init2.nc') #'RMinit_newvalues2017.nc')
NEUS_15_init=make.sh.init.object(bgm.file, 'RMinit2_2019.nc') #surfOnly.nc') #'RMinit_newvalues2017.nc')
sh.init(NEUS_15_init)
newN=NEUS_15_init$df.nitrogen
newNums=NEUS_15_init$df.nums
write.csv(t, file='benthic_species_N.csv', sep=',', col.names = T, row.names = F)


### View and CHANGE spatial distributions 
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
bgm.file <- ("neus_tmerc_RM.bgm") #neus30_v15_notsohighvertmix.bgm")
NEUS_15_dist <- make.sh.dist.object(bgm.file)
sh.dist(NEUS_15_dist)



# t2=NEUS_15_init$species.3.data
# t=t(t2)
# colnames(t)=NEUS_15_init$species.2.names.full
# write.csv(t, file='benthic_species_N.csv', sep=',', col.names = T, row.names = F)
