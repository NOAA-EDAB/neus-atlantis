#Plot catch spike summary figure

library(dplyr)
library(ggplot2)
library(gridExtra)

experiment.id = 'fscale_combined'

data.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/data/',experiment.id,'/')
figure.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/figures/',experiment.id,'/')

setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
master.dat = read.csv(here::here('diagnostics','scenario_db','scenario_db_master.csv'),as.is = T) %>%
  filter(experiment_id %in% c('fscale2','fscale3'))

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T) %>%
  filter(IsTurnedOn == T)%>%
  select(Code, LongName)
  
biomass.baseline = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/fishing_sensitivity/reference_run/fishing_sensitivity_baseline/neus_outputBiomIndx.txt',header = T)%>%
  select(Time, all_of(fgs$Code))%>%
  mutate(Time = floor(Time/365))%>%
  tidyr::gather('Code','Biomass',-Time)%>%
  group_by(Code,Time)%>%
  summarise(Biomass = mean(Biomass,na.rm=T))%>%
  mutate(scalar = 1,
         run.id = 'baseline')

biomass = readRDS(paste0(data.dir,'BiomIndx_1_28105_year_fscale_combined.rds')) %>%
  left_join(setup.df, by = 'run.id')%>%
  select(Time,Code,Biomass,scalar,run.id)%>%
  bind_rows(biomass.baseline)
  


spike.start = master.dat$event_start_d
spike.end = master.dat$event_end_d

spp.names = sort(unique(setup.df$target.species))

bio.plot.ls = list()
i=1
for(i in 1:length(spp.names)){
  
  dat.spp = biomass %>%
    filter(Code == spp.names[i])
  
  bio.plot.ls[[i]] = ggplot(dat.spp, aes(x= Time, y= Biomass,color = factor(scalar)))+
    geom_line()+
    theme_bw()+
    ggtitle(spp.names[i])
}

pdf(paste0(figure.dir,experiment.id,'_biomass.pdf'))
for(i in 1:length(bio.plot.ls)){
  if(is.null(bio.plot.ls[[i]])){next()}
  grid.arrange(bio.plot.ls[[i]])}
dev.off()

