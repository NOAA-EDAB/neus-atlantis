

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/'
# roms.files <- list.files(path=roms.dir, pattern="^roms_output_ltl_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
# roms.file = paste0(roms.dir,'roms_output_ltl_statevars_tohydro_1964.nc')

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT_Output/phys_statevars/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars_alternate/'
# roms.file = paste0(roms.dir,'roms_cobalt_v10_statevars_1981.nc')
force.vars = c('temperature','salinity')
final.vars = c('temperature','salinity')
var.units = c('degrees Celcius','PSU')
long.names = c('Temperature','Salinity')
fill.val = c(15,0)
miss.val = c(15,0)
valid.min = c(-2,0)
valid.max = c(999,999)

year.range = 1964:2014


tempsalt.files = list.files(roms.dir,'*.nc',full.names = T)
file.years = as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",tempsalt.files)))
tempsalt.files = tempsalt.files[which(file.years %in% year.range)]
.packages = c("devtools","tidyverse","stringi","RNetCDF", "data.table")
lapply(.packages, require, character.only=TRUE)
source(here::here('R','make_force_statevar.R'))

for(i in 1:length(tempsalt.files)){
  make_force_statevar(roms.dir = roms.dir,
                      roms.file = tempsalt.files[i],
                      out.dir = out.dir,
                      force.vars = force.vars,
                      var.units = var.units,
                      final.vars = final.vars,
                      fill.val = fill.val,
                      long.names = long.names,
                      miss.val = miss.val,
                      valid.min = valid.min,
                      valid.max = valid.max,
                      out.prefix = 'roms_tempsalt_force_')
  print(i)
}

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT_Output/ltl_statevars/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/ltl_statevars/'
# roms.file = paste0(roms.dir,'roms_cobalt_v10_statevars_1981.nc')
# force.vars = c('ndi','nlg','nlgz','nmdz','nsm','nsmz','silg','nbact')
final.vars = c('Diatom_N','Carniv_Zoo_N','Zoo_N','PicoPhytopl_N','MicroZoo_N','Diatom_S','Pelag_Bact_N')
force.vars= final.vars
var.units = c(rep('mg N m-3',6),'mg Si m-3','mg N m-3')
long.names = c('Datiom Nitrogen','Large Zooplankton Nitrogen','Meso-Zooplankton Nitrogent','PicoPhytoplankton Nitrogen','Micro-Zooplankton Nitrogen','Diatom Silicon','Pelagic Bacteria Nitrogen')
fill.val = c(0,0,0,0,0,0,0.2)
miss.val = fill.val
valid.min = rep(0,7)
valid.max = rep(99999,7)

ltl.files = list.files(roms.dir,'*.nc',full.names = T)
file.years = as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",ltl.files)))
ltl.files = ltl.files[which(file.years %in% year.range)]
for(i in 1:length(tempsalt.files)){
  make_force_statevar(roms.dir = roms.dir,
                      roms.file = ltl.files[i],
                      out.dir = out.dir,
                      force.vars = force.vars,
                      var.units = var.units,
                      final.vars = final.vars,
                      fill.val = fill.val,
                      long.names = long.names,
                      miss.val = miss.val,
                      valid.min = valid.min,
                      valid.max = valid.max,
                      out.prefix = 'roms_ltl_force_')
  print(i)
}

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT_Output/nut_statevars/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/nut_statevars/'
# roms.file = paste0(roms.dir,'roms_cobalt_v10_statevars_1981.nc')
force.vars = c('nh4','no3','o2','sio4')
final.vars = c('NH3','NO3','Oxygen','Si')
var.units = c('mg N m-3','mg N m-3','mg O2 m-3','mg Si m-3')
fill.val = c(0,0,8000,0)
miss.val = fill.val
long.names = c('Ammonia','Nitrate','Dissovled Oxygen','Dissolved Silica')
valid.min = rep(0,4)
valid.max = rep(99999,4)

nut.files = list.files(roms.dir,'*.nc',full.names = T)
file.years = as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",nut.files)))
nut.files = nut.files[which(file.years %in% year.range)]

for(i in 1:length(nut.files)){
  make_force_statevar(roms.dir = roms.dir,
                      roms.file = nut.files[i],
                      out.dir = out.dir,
                      force.vars = force.vars,
                      var.units = var.units,
                      final.vars = final.vars,
                      fill.val = fill.val,
                      long.names = long.names,
                      miss.val = miss.val,
                      valid.min = valid.min,
                      valid.max = valid.max,
                      out.prefix = 'roms_nut_force_')
  print(i)
}
