#' Create Mapping of Species to functional group
#' 
#' Write the mapping to a file format (GitHub flavored markdown) that can used on the wiki. 
#' 
#'  Requires: andybeet/abutils package for "capitalize_first_letter" function
#'            andybeet/dbutils package for create_species_lookup.r and connect_to_database  

# list packages and check to see if needed to be installed
library(magrittr)
# packages <- data.frame(pkgName = c("remotes","dplyr","readr","here","dbutils","utilities","cfdbs","survdat"),
#                        pkgSource = c("remotes","dplyr","readr","here","andybeet/dbutils","andybeet/utilities","andybeet/cfdbs","andybeet/survdat"),
#                        pkgLocation= c("cran","cran","cran","cran","github","github","github","github"))
# 
# # install missing packages                       
# for (apack in packages$pkgName) {
#   if (length(apack[!( apack %in% installed.packages()[, "Package"])])) {
#     pkgInfo <- packages %>% dplyr::filter(pkgName==apack)
#     if (pkgInfo$pkgSource == "cran") {
#       install.packages(apack)
#     } else {
#       remotes::install_github(pkgInfo$pkgSource)
#     }
#   }
# }


create_map_functional_group <- function(channel,writeToFile=F) {

  # read in functional group codes and name from Atlantis input file
  fg <- atlantisom::load_fgs(here::here("currentVersion"),"neus_groups.csv") |> 
    dplyr::rename(Functional_Group = LongName) |> 
    dplyr::select(Code,Functional_Group,isFished)
  
  
  #fg <-  readr::read_csv(here::here("data-raw","initialFunctionalGroupNames.csv"))
  
  # read in species membership to group, then join with functional group names
  data1 <- readr::read_csv(here::here("data-raw/data","Atlantis_1_5_groups_svspp_nespp3.csv")) |> 
    dplyr::mutate(NESPP3 = sprintf("%03d",NESPP3)) |> 
    dplyr::left_join(fg,by="Code")
  

  
  data2 <- readr::read_csv(here::here("data-raw/data","AdditionalSpeciesFromCAS.csv")) |> 
    dplyr::filter(!is.na(Code)) |> 
    dplyr::mutate(NESPP3 = sprintf("%03d",NESPP3)) |> 
    dplyr::select(Code,Name,SVSPP,NESPP3) |> 
    dplyr::left_join(fg, by = "Code") |> 
    dplyr::mutate(Name = stringr::str_to_sentence(Name),
                  isFished = 1) 

  
  data <- rbind(data1,data2)
  
  # get info by looking up by svspp code
  pullBySVSPP <- dbutils::create_species_lookup(channel,species=na.omit(data$SVSPP),speciesType = "SVSPP")
  SVSPPData <- pullBySVSPP$data |> 
    dplyr::select(SVSPPsv,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) |> 
    dplyr::distinct()
  
  # get same data by looking up by nespp3
  pullByNESPP3 <- dbutils::create_species_lookup(channel,species=na.omit(data$NESPP3),speciesType = "NESPP3")
  NESPP3Data <- pullByNESPP3$data |> 
    dplyr::select(NESPP3,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) |> 
    dplyr::distinct()
  
  # merge two results, rename variable
  masterList <- dplyr::left_join(data,SVSPPData, by=c("SVSPP"="SVSPPsv"))  |> 
    dplyr::full_join(NESPP3Data, by="NESPP3") |> 
    dplyr::arrange(Code) |> 
    dplyr::rename(Species = Name,
                  Common_Name = COMNAME.y,
                  Scientific_Name=SCIENTIFIC_NAME.y,
                  Species_Itis=SPECIES_ITIS.y)  |> 
    dplyr::mutate(Common_Name = abutils::capitalize_first_letter(Common_Name),
                  NESPP3=as.numeric(NESPP3),
                  Species_Itis=as.numeric(Species_Itis)) |> 
    dplyr::select(Code,Functional_Group,Species,Scientific_Name,SVSPP,NESPP3,Species_Itis,isFished) |> 
    dplyr::mutate(isFishedSpecies = (Functional_Group==Species) & (isFished==T)) |> 
    dplyr::select(-isFished)
  
  ## Fixes
  # if duplicate SVSPP and NESPP3 values, due to SVSPP code is missing.  due to cfdbs having only genus info.
  # Remove these entries
  removedups <- masterList |>
    dplyr::group_by(SVSPP,NESPP3) |> 
    dplyr::filter((dplyr::n()>1) & !(is.na(NESPP3) & is.na(SVSPP))) |> 
    dplyr::mutate(keep = dplyr::case_when(grepl("\\s+",Scientific_Name) ~ T,
                                          TRUE ~ F)) |> 
    dplyr::filter(keep == F) |> 
    dplyr::select(-keep)
   
  
  masterList <- dplyr::setdiff(masterList,removedups)

  # edits manual
  #change menhaden species_itis
  ind <- masterList$NESPP3 %in% 221 
  masterList[ind,"Species_Itis"] <- 161732
  
  # remove species with 2 nespp3 codes. Remove code deals with bits and pieces
  # cod, goosefish, haddock, pollock, winter flounder
  masterList <- masterList |> 
    dplyr::filter(is.na(NESPP3) | !(NESPP3 %in% c(82,11,148,270,119)) )
  # remove general wolffishes category. Atlantic wolfish covers this
  masterList <- masterList |> 
    dplyr::filter(is.na(NESPP3) | !(is.na(SVSPP) & NESPP3 == 512))
  
  write.csv(masterList,here::here("data-raw/data/Atlantis_2_0_groups_svspp_nespp3.csv"))
  
  # format to markdown table. Copy output to wiki
  # open file and write
  outputFile <- here::here("data","functionalGroupNames.txt")
  fileConn<-file(outputFile,open="w")
  header <- paste0("|",paste0(names(masterList),collapse = "|"),"|")
  cat(header,file=fileConn,append=T)
  cat("\n",file=fileConn,append=T)
  spacer <- paste0("|",paste0(rep("---",ncol(masterList)),collapse = "|"),"|")
  cat(spacer,file=fileConn,append=T)
  cat("\n",file=fileConn,append=T)
  
  for (irow in 1:nrow(masterList)) {
    rowData <- paste0("|",paste0(masterList[irow,],collapse = "|"),"|")
    cat(rowData,file=fileConn,append=T)
    cat("\n",file=fileConn,append=T)
  }
  
  close(fileConn)
  
  if(writeToFile){
    readr::write_csv(masterList,here::here("data","functionalGroupNames.csv"))
  }
  
  return(masterList)
}
