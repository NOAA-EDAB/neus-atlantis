
source(here::here('R','Post_Processing','plot_run_comparisons.R'))
source(here::here('R','Post_Processing','plot_run_catch_comparisons.R'))

run.set.names = paste0('fleet_calibration_2_',1:18)
dev = 'Dev_6681_20240905'

dev.dir = '/net/work3/EDAB/atlantis/Shared_Data/Dev_Runs/Dev_6681_20240905/'
run.set.dirs = here::here('Atlantis_Runs',paste0('fleet_calibration_2_',1:18),'')
# master = '/net/work3/EDAB/atlantis/Andy_Proj/Atlantis_Runs/master_2_2_0/'

figure.dir = here::here('Figures','Run_Comparisons','')

# plot_run_catch_comparisons(model.dirs = c(master,fleet), model.names = c('master','fleet'),
#                            plot.out = paste0(figure.dir,'master_fleet_comparison'),
#                            plot.diff = F,plot.raw = T)
plot_run_comparisons(
  model.dirs = c(dev.dir,devplusfleets.dir,minsize.dir),
  model.names = c(dev,devplusfleets,minsize),
  plot.rel = T,
  plot.diff = F,
  plot.out = paste(figure.dir,'gffleets_minsize_ref'), 
  table.out = F,
  groups = NULL,
  remove.init = F
)


plot_run_catch_comparisons(
  model.dirs = c(dev.dir,run.set.dirs),
  model.names = c(dev,run.set.names),
  plot.diff = F,
  plot.out = paste(figure.dir,'fleet_calibration_2'),
  table.out = F,
  groups = NULL,
  remove.init = F
)
