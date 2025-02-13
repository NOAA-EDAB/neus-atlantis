#' Script to generate plots relating to catch scalar scenarios including:
#' 1) delta_biomass ~ catch scalar, by species -> PDF
#' 2) mean_age ~ catch scalar, by species -> PDF
#' 3) delta_numbers ~ catch scalar, by species -> PDF
#' 4) numbers_trend ~ biomass_trend by guild

data.dir = 'C:/Users/Joseph.Caracappa/Documents/Atlantis/fishing_sensitivity/data/fscale_combined/'
figure.dir = 'C:/Users/Joseph.Caracappa/Documents/Atlantis/fishing_sensitivity/figures/fscale_combined/'

#make some fake data based on the run.index
experiment.id = 'fscale_combined'


ref.run.dir = 'C:/Users/Joseph.caracappa/Documents/Atlantis/fishing_sensitivity/reference_Run/fishing_sensitivity_baseline/'
data.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/ref_run_summary.rds'))
proj.start = 20805

fgs.file = here::here('currentVersion','neus_groups.csv')
                          

plot_catch_scalar_summary = function(data.dir,figure.dir,setup.df,ref.run.dir,ref.time,fgs.file){
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(ggrepel)
  
  fgs = read.csv(fgs.file,as.is =T) %>%select(Code,LongName,IsTurnedOn)
  
  setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
  
  # setup.df$target.species = sapply(setup.df$Run,function(x) return(strsplit(x,split=paste0(experiment.id,'|_'))[[1]][3]),USE.NAMES = F)
  
  setup.df = setup.df %>% select(run.id,scalar,target.species)
  
  data.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/ref_run_summary.rds'))%>%
    rename(biomass.age.mean.ref = 'biomass.age.mean')
  
  #Read in biomass and numbers data
  biomass = readRDS(paste0(data.dir,'scenario_stats_biomass.rds'))
  numbers = readRDS(paste0(data.dir,'scenario_stats_numbers.rds'))
  
  data.run = biomass %>%
    left_join(numbers)%>%
    tidyr::separate(run.name,c('dum','ID'),'_',remove =F )%>%
    mutate(ID = as.numeric(ID))%>%
    rename(Code = 'species')%>%
    left_join(setup.df,by = c('run.name' = 'run.id'))%>%
    left_join(fgs)%>%
    filter(IsTurnedOn == 1)
    
  #Combine run output and ref data
  data.all = data.run %>% 
    left_join(data.ref, by = 'Code') %>%
    mutate(biomass.rel = biomass.mean/biomass.ref,
           number.rel = number.mean/number.ref)%>%
    left_join(fgs)%>%
    filter(IsTurnedOn == 1)
  
  spp.names = sort(unique(data.run$Code))
  
  biomass.plot.name = paste0(figure.dir,'biomass_catch_scalar_species.pdf')
  number.plot.name = paste0(figure.dir,'numbers_catch_scalar_species.pdf')
  biomass.number.plot.name = paste0(figure.dir,'biomass_v_numbers_catch_scalar_species.pdf')
  number.age.plot.name = paste0(figure.dir,'numbers_age_catch_scalar_species.pdf')
  biomass.age.plot.name = paste0(figure.dir,'biomass_age_catch_scalar_species.pdf')
  
  biomass.plot = number.plot = biomass.number.plot = number.age.plot = biomass.age.plot = list()
  
  biomass.numbers = data.frame(Code = spp.names,biomass.slope = NA,biomass.int = NA, number.slope = NA, number.int = NA)

  i=1
  for(i in 1:length(spp.names)){
    
    data.species = data.all %>% 
      filter(Code == spp.names[i] & Code == target.species)%>%
      arrange(scalar)%>%
      mutate(scalar.log = log(scalar),
             biomass.rel.log = log(biomass.rel),
             number.rel.log = log(number.rel))
    
    data.species.nozero = data.species %>%
      filter(scalar >0)%>%
      filter(is.finite(biomass.rel.log))
    
    if(nrow(data.species)== 0| all(is.na(data.species$biomass.ref))){
      biomass.plot[[i]] = NULL
      biomass.age.plot[[i]] = NULL
      number.age.plot[[i]] = NULL
      number.plot[[i]] = NULL
      next()
    }else{
      #fit a linear model to biomass and numbers vs fishing scalar and extract the slopes
      b.lm = lm(biomass.rel.log~scalar.log,data.species.nozero)
      # b.glm = glm(biomass.rel~scalar.log,data = data.species.nozero, family = gaussian(link = log))
      # print(summary(b.lm))
      biomass.numbers$biomass.slope[i] = coef(b.lm)[2]
      biomass.numbers$biomass.int[i] = coef(b.lm)[1]
      
      # b.nls = nls(biomass.rel~b*scalar^z, start = list(b=1,z=0.33), data = data.species.nozero)
      # 
      # plot(biomass.rel~scalar,data.species.nozero)
      # lines(x= data.species.nozero$scalar,y=fitted(b.glm))
      # curve(coef(b.nls)[1]*x^coef(b.nls)[2],add = T)
      # 
      # plot(biomass.rel.log~scalar.log,data.species)
      # abline(b.lm)
      # points(data.species.nozero$scalar.log, fitted(b.lm), col = 2)
      # 
      
      #Do biomass vs fishing scalar
      biomass.plot[[i]] = ggplot(data = data.species, aes(x= scalar, y= biomass.rel))+
        geom_point()+
        geom_path()+
        # geom_smooth(method = 'glm', se = F, method.args = list(family = 'binomial'))+
        # geom_smooth(method = 'lm')+
        stat_function(fun = function(x) exp(biomass.numbers$biomass.int[i])*x^biomass.numbers$biomass.slope[i], lty = 2)+
        ggtitle(data.species$LongName[1])+
        geom_hline(yintercept = 0,lty =2)+
        theme_bw()+
        xlab('Fishing Scalar')+
        ylab('Relative Biomass')+
        scale_x_continuous(breaks = data.species$scalar)+
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.minor = element_blank())
    }
    

    
    if(all(is.na(data.species$number.ref))){
      biomass.numbers$number.slope[i] = NA
      biomass.age.plot[[i]] = NULL
      number.age.plot[[i]] = NULL
      number.plot[[i]] = NULL
      next()
    }else{

      n.lm = lm(number.rel.log~scalar.log,data =data.species.nozero)
      biomass.numbers$number.slope[i] = coef(n.lm)[2]
      biomass.numbers$number.int[i] = coef(n.lm)[1]

      #Do mean age (abundance-weighted) vs fishing scalar
      number.age.plot[[i]] = ggplot(data = data.species, aes(x= scalar, y= number.age.mean))+
        geom_point()+
        ylim(0,10)+
        # geom_smooth(method = 'lm')+
        ggtitle(data.species$LongName[1])+
        theme_bw()+
        xlab('Fishing Scalar')+
        ylab('Mean age (abundance-weighted)')+
        theme(plot.title = element_text(hjust = 0.5))



      #Do mean age (biomass-weighted) vs fishing scalar
      biomass.age.plot[[i]] = ggplot(data = data.species, aes(x= scalar, y= biomass.age.mean))+
        geom_point()+
        geom_path()+
        ylim(0,10)+
        # geom_smooth(method = 'lm')+
        ggtitle(data.species$LongName[1])+
        theme_bw()+
        xlab('Fishing Scalar')+
        ylab('Mean age (biomass-weighted)')+
        theme(plot.title = element_text(hjust = 0.5))



      #Do numbers vs fishing scalar
      number.plot[[i]] = ggplot(data = data.species, aes(x= scalar, y= number.rel))+
        geom_point()+
        geom_path()+
        # geom_smooth(method = 'glm', se = F, method.args = list(family = 'binomial'))+
        # geom_smooth(method = 'lm')+
        stat_function(fun = function(x) exp(biomass.numbers$number.int[i])*x^biomass.numbers$number.slope[i], lty = 2)+
        ggtitle(data.species$LongName[1])+
        theme_bw()+
        xlab('Fishing Scalar')+
        ylab('Relative Numbers')+
        theme(plot.title = element_text(hjust = 0.5))
    }

  }
  
  
  if(!dir.exists(figure.dir)){
    dir.create(figure.dir)
  }
  pdf(biomass.plot.name)
  for(i in 1:length(biomass.plot)){
    if(is.null(biomass.plot[[i]])){next()}
    print(biomass.plot[[i]])}
  # do.call('grid.arrange',biomass.plot)
  dev.off()
  
  pdf(number.plot.name)
  for(i in 1:length(number.plot)){
    if(is.null(number.plot[[i]])){next()}
    grid.arrange(number.plot[[i]])}
  dev.off()
  
  pdf(biomass.age.plot.name)
  for(i in 1:length(biomass.age.plot)){
    if(is.null(biomass.age.plot[[i]])){next()}
    grid.arrange(biomass.age.plot[[i]])}
  dev.off()
  
  pdf(number.age.plot.name)
  for(i in 1:length(number.age.plot)){
    if(is.null(number.age.plot[[i]])){next()}
    grid.arrange(number.age.plot[[i]])}
  dev.off()
  
  #plot biomass slope vs numbers slope and color code by guild
  
  spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
  
  data.slope = biomass.numbers %>% 
    filter(!is.na(number.slope))%>%
    left_join(spp2guild)%>%
    left_join(data.ref)
    
  
  ggplot(data = data.slope, aes( x= biomass.slope, y = number.slope, color = Guild, label = Code))+
    geom_point(size = 5, alpha = 0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    geom_vline(xintercept = 0,lty =2)+
    stat_function(fun = function(x) x,lty = 1, color = 'black')+
    geom_text_repel()+
    xlab('log (biomass slope)')+
    ylab('log (numbers slope)')+
    theme_bw()+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'/numbers_biomass_guild.png'))

  ggplot(data = data.slope, aes(x = exploit.prop,y = -biomass.slope, color = Guild, label = Code))+
    geom_point(alpha = 0.5,size = 5)+
    geom_text_repel()+
    xlab('Exploitation Rate')+
    ylab('Fishing Sensitivity')+
    theme_bw()+
    theme(legend.position = 'bottom')
    ggsave(paste0(figure.dir,'/biomass_exploitation_guild.png'))
    
  ggplot(data = data.slope, aes(x = recruit.ref,y = -biomass.slope, color = Guild, label = Code))+
      geom_point(alpha = 0.5,size = 5)+
      geom_text_repel()+
      xlab('Reruitment')+
      ylab('Fishing Sensitivity')+
      theme_bw()+
      theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'/recruitment_exploitation_guild.png'))
    
  ggplot(data = data.slope, aes(x = biomass.ref,y = -biomass.slope, color = Guild, label = Code))+
    geom_point(alpha = 0.5,size = 5)+
    geom_text_repel()+
    xlab('Reference Biomass')+
    ylab('Fishing Sensitivity')+
    theme_bw()+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'/reference_biomass_exploitation_guild.png'))
  
  ggplot(data = data.slope, aes(x = catch.ref,y = -biomass.slope, color = Guild, label = Code))+
    geom_point(alpha = 0.5,size = 5)+
    geom_text_repel()+
    xlab('Reference Catch')+
    ylab('Fishing Sensitivity')+
    theme_bw()+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'/reference_catch_exploitation_guild.png'))
    
  #plot maximum scalar
  spp.names2 = sort(unique(data.all$target.species))
  guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv')) %>%
    select(Code,LongName,Guild)
  
  
  data.thresh = data.frame(Code = spp.names2,max.scalar = NA)
  for(i in 1:length(spp.names2)){
    
    data.spp = data.all %>% 
      filter(target.species == spp.names2[i] & Code == spp.names2[i])%>%
      mutate(bio.prop = biomass.mean/biomass.ref)%>%
      filter(bio.prop >= 0.1)
    
    data.thresh$max.scalar[i] =  max(data.spp$scalar,na.rm=T)
  }
  data.thresh = data.thresh %>%
    left_join(fgs)%>%
    left_join(guild2spp)%>%
    left_join(data.ref)
  
  ggplot(data = data.thresh,aes(x= reorder(LongName,max.scalar),y=max.scalar,fill = Guild))+
    geom_bar(stat = 'identity')+
    guides(fill= guide_legend(title.position = 'left',nrow = 1))+
    ylab('Max Scalar that Persists')+
    xlab('')+
    coord_flip()+
    theme_bw()+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'max_scalar_guild.png'),width =10,height =8, units = 'in',dpi =300)
  
  ggplot(data = data.thresh, aes(x= exploit.prop,y = max.scalar, color = Guild))+
    geom_point(size = 3)+
    theme_bw()+
    guides(color = guide_legend(nrow =1,title = ''))+
    xlab('Fishing Mortality')+
    ylab('Maximum Fishing Multiplier')+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'max_scalar_Exploitation_guild.png'))
  
  thresh.prop = data.thresh %>%
    group_by(max.scalar)%>%
    summarise(N =n())%>%
    mutate(pct = N/nrow(data.thresh))
    
  thresh.prop$cum.pct = cumsum(thresh.prop$pct)
  
  nrow(data.thresh %>% filter(exploit.prop < 0.01))/nrow(data.thresh)
}
