#' Script to create plot comparisons between individual runs of the same Run.Group
#' Also produces mean biomass of target species
library(dplyr)
library(ggplot2)
#Specify setup.csv used to generate calibration run set
experiment.id = 'fleet_calibration_2'
setup.df = read.csv(here::here('Setup_Files','cloud_fleet_calibration_2.csv'),as.is=T)
setup.df$file.ID = 1:nrow(setup.df)
# experiment.dir = here::here('Atlantis_Runs',experiment.id,'')
experiment.dir = paste0('/atlantisdisk/fleet_calibration_2/Atlantis_Runs/',experiment.id,'/')
# figure.dir = here::here('Figures',experiment.id,'')
figure.dir = paste0('/model/Joseph.Caracappa/Figures/',experiment.id,'/')

if(!dir.exists(figure.dir)){dir.create(figure.dir)}



#Read in groups file
# fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>%
fgs = read.csv('/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/currentVersion/neus_groups.csv',as.is = T)%>%
  filter(IsTurnedOn == T)
fgs.fished  = fgs$Code[which(fgs$isFished == T)]
fleets.file = read.csv(here::here('currentVersion','neus_fisheries.csv'))
gf.fleets = grep('^gf',fleets.file$Code,value =T)

plot.colors = c(RColorBrewer::brewer.pal(12,'Paired'),
                RColorBrewer::brewer.pal(8,'Dark2'),
                RColorBrewer::brewer.pal(12,'Set3'))

run.groups = sort(unique(setup.df$Run.Group))
i=6

for(i in 1:length(run.groups)){
  
  setup.group = setup.df %>%
    filter(Run.Group == run.groups[i])
  
  j=1
  bio.run.group = list()
  catch.run.group = list()
  catch.fleet.run.group = list()
  for(j in 1:nrow(setup.group)){
    biom.file = paste0(experiment.dir,experiment.id,'_',setup.group$Run.ID[j],'/neus_outputBiomIndx.txt')
    if(!file.exists(biom.file)){next()}
    bio.run.group[[j]] = data.table::fread(paste0(experiment.dir,experiment.id,'_',setup.group$Run.ID[j],'/neus_outputBiomIndx.txt'))%>%
      select(Time,all_of(fgs$Code))%>%
      tidyr::gather(Code,Biomass,-Time)%>%
      mutate(Run.ID = setup.group$Run.ID[j],
             file.ID = setup.group$file.ID[j])
    
    catch.fleet.file = paste0(experiment.dir,experiment.id,'_',setup.group$Run.ID[j],'/neus_outputCatchPerFishery.txt')
    if(!file.exists(catch.fleet.file)){next()}
    catch.fleet.run.group[[j]] = data.table::fread(catch.fleet.file)%>%
      filter(Fishery %in% gf.fleets)%>%
      tidyr::gather(Code,Catch,-Time,-Fishery)%>%
      mutate(Run.ID = setup.group$Run.ID[j],
             file.ID = setup.group$file.ID[j])
    
    catch.tot.file = paste0(experiment.dir,experiment.id,'_',setup.group$Run.ID[j],'/neus_outputCatch.txt')
    if(!file.exists(catch.tot.file)){next()}
    catch.run.group[[j]] = data.table::fread(catch.tot.file)%>%
      select(Time,all_of(fgs.fished))%>%
      tidyr::gather(Code,Catch,-Time)%>%
      mutate(Run.ID = setup.group$Run.ID[j],
             file.ID = setup.group$file.ID[j])
  }
  bio.run.group = bind_rows(bio.run.group)
  catch.run.group = bind_rows(catch.run.group)
  catch.fleet.run.group = bind_rows(catch.fleet.run.group)
  
  spp.names = sort(unique(fgs$LongName))
  s=1

  pdf(paste0(figure.dir,experiment.id,'_',run.groups[i],'_biomass.pdf'))
  for(s in 1:length(spp.names)){
    
    spp.code = fgs$Code[which(fgs$LongName == spp.names[s])]
    bio.spp = bio.run.group %>%
      filter(Code == spp.code)
    
    
    p =ggplot(bio.spp, aes(x=Time,y = Biomass, color = factor(Run.ID)))+
      geom_line()+
      scale_color_manual(name = paste0('Run ID (',run.groups[i],')'),
                         values = plot.colors[1:length(unique(setup.group$Run.ID))])+
      ggtitle(spp.names[s])+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = 'bottom')
    gridExtra::grid.arrange(p)
  }
  dev.off()
  
  pdf(paste0(figure.dir,experiment.id,'_',run.groups[i],'_Catch.pdf'))
  for(s in 1:length(fgs.fished)){
    
    spp.code = fgs.fished[s]
    catch.spp = catch.run.group %>%
      filter(Code == spp.code)
    
    
    p =ggplot(catch.spp, aes(x=Time,y = Catch, color = factor(Run.ID)))+
      geom_line()+
      scale_color_manual(name = paste0('Run ID (',run.groups[i],')'),
                         values = plot.colors[1:length(unique(setup.group$Run.ID))])+
      ggtitle(spp.names[s])+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = 'bottom')
    gridExtra::grid.arrange(p)
  }
  dev.off()
  
  pdf(paste0(figure.dir,experiment.id,'_',run.groups[i],'_biomass.pdf'))
  for(s in 1:length(spp.names)){
    
    spp.code = fgs$Code[which(fgs$LongName == spp.names[s])]
    spp.LongName = fgs$LongName[which(fgs$Code == spp.code)]
    bio.spp = bio.run.group %>%
      filter(Code == spp.code)
    
    
    p =ggplot(bio.spp, aes(x=Time,y = Biomass, color = factor(Run.ID)))+
      geom_line()+
      scale_color_manual(name = paste0('Run ID (',run.groups[i],')'),
                         values = plot.colors[1:length(unique(setup.group$Run.ID))])+
      ggtitle(spp.LongName)+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = 'bottom')
    gridExtra::grid.arrange(p)
  }
  dev.off()
  
  pdf(paste0(figure.dir,experiment.id,'_',run.groups[i],'_CatchFleet.pdf'))
  for(s in 1:length(gf.fleets)){
    
    catch.fleet = catch.fleet.run.group %>%
      filter(Fishery == gf.fleets[s])
    
    catch.fleet.zero = catch.fleet %>%
      group_by(Fishery,Code)%>%
      summarise(totcatch = sum(Catch,na.rm=T))%>%
      filter(totcatch == 0)
    
    catch.fleet = catch.fleet %>%
      filter(!(Code %in% catch.fleet.zero$Code))
    
    fleet.spp = sort(unique(catch.fleet$Code))
    
    for(f in 1:length(fleet.spp)){
      
      catch.fleet.spp = catch.fleet %>%
        filter(Code == fleet.spp[f])
      
      p =ggplot(catch.fleet.spp, aes(x=Time,y = Catch, color = factor(Run.ID)))+
        geom_line()+
        scale_color_manual(name = paste0('Run ID (',run.groups[i],')'),
                           values = plot.colors[1:length(unique(setup.group$Run.ID))])+
        ggtitle(paste0(gf.fleets[s],':',fleet.spp[f]))+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = 'bottom')
      gridExtra::grid.arrange(p)
    }

  }
  dev.off()
}
