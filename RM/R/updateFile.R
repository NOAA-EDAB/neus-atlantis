###############################################################################################################################################################################
#
#   R script to duplicate parameter values for a given functional group in Atlantis style prm files.
#
#   For example if you have an existing group called "SG" and you wish to add another groups called "SGS" and use the values for SG as a start for the parameterisations of SGS.
#
#   Note as you are adding an additional group into your file there are a number of parameters that you will need to hand edit including the diet matrix. Best to do
#   a search for parameters that have a length of 67 (or the old number of groups). 
#
#   Only change the following values:
#
###############################################################################################################################################################################
d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM' #where (PRM, bgm, group data) are saved
d1='/home/ryan/Git/atneus_RM'
setwd(d1)

CodeRelations <- read.csv(paste(d1,"/R/coderelations.csv", sep=''), stringsAsFactors = F)
cr=CodeRelations[order(CodeRelations$Parent),]
cr=cr[-c(1,2),]
cr$test=cr$Parent==cr$Child # compare names

cr$rpt=NA
cr$new=NA
cr$diff=NA
for (i in 1:length(cr$Parent)){
  cr$rpt[i]=sum(cr$Parent==cr$Parent[i]) # total number of groups from parent
  cr$new[i]=sum(cr$Child==cr$Parent[i]) # if parent is reused (1) or renamed (0)
  cr$diff[i]=cr$rpt[i]-cr$new[i]
}
cr2=cr[cr$diff>0,]


# The name of the existing prm file you wish to update.
# inputFileName = "/data/Atlantis/runFiles/trunk/SETas_model_New/VMPA_setas_biol_fishing_New_BU.prm"
inputFileName = paste(d1,"/at_harvest_neus_v15_DE.prm", sep='')

#The name of the new prm file.
# outputFileName = "/data/Atlantis/runFiles/trunk/SETas_model_New/VMPA_setas_biol_fishing_New_Updated.prm"
outputFileName = paste(d1,"/at_harvest_neus_v15_DE_RM2.prm", sep='')


# The groupCode of the existing group in your inputFileName prm file.
OriginalGroupName = "FVT"

# The groupCode of the new group you want to add.
AdditionalGroupName = "BIL"

### replace XXX with YYY for certain species (example below)
tx  <- readLines("~/Desktop/text.txt")
tx2  <- gsub(pattern = "abc", replace = "ccccccccccccccccccccc", x = tx)
writeLines(tx2, con="~/Desktop/text2.txt")

###############################################################################################################################################################################

text <- readLines(inputFileName,encoding="UTF-8")
sink(outputFileName);

numLines = length(text)

for (lineIndex in 1:numLines){

    if(nchar(text[lineIndex]) > 0){
    
        match = grep(OriginalGroupName, text[lineIndex]);

        if(length(match) > 0){
       
            # Does the next line start with a number?
            startChar = substr(text[lineIndex + 1], 0, 1); # 0, 10 would catch if it starts with #
            
            result = grep("[0-9]", startChar)
                      
            if(length(result) > 0){
            
                # Double line parameter.
                # write(text[lineIndex], "", append = TRUE);
                # write(text[lineIndex + 1], "", append = TRUE);
               
                #Now replace the strings and print out.
                string = gsub(OriginalGroupName, AdditionalGroupName, text[lineIndex]);
                write(string, "", append=T)
                write(text[lineIndex + 1], "", append = TRUE);
            }else{
            
                # Single line parameter.
                write(text[lineIndex], "", append = TRUE);
               
                #Now replace the strings and print out.
                string = gsub(OriginalGroupName, AdditionalGroupName, text[lineIndex]);
                write(string, "", append = TRUE);
            }
        }
        else{
          write(text[lineIndex], "", append = TRUE);
        }
    }else{
        write(text[lineIndex], "", append = "TRUE");
    }
}

sink();

#### testing - currently copies file 66 times...

inputFileName = paste(d1,"/at_harvest_neus_v15_DE.prm", sep='')

outputFileName = paste(d1,"/at_harvest_neus_v15_DE_RM2.prm", sep='')
text <- readLines(inputFileName,encoding="UTF-8")
sink(outputFileName);

numLines = length(text)
for (nn in 1: length(cr2$Parent)){
  
  # The groupCode of the existing group in your inputFileName prm file.
  OriginalGroupName = cr2$Parent[nn] #"FVT"
  
  # The groupCode of the new group you want to add.
  AdditionalGroupName = cr2$Child[nn] #"BIL"
  
  numx=cr2$rpt[nn] # number of times it is repeated
  numnew=cr2$diff[nn] # number of new entries (excludes when parent==child)
  
  # ### replace XXX with YYY for certain species (example below)
  # tx  <- readLines("~/Desktop/text.txt")
  # tx2  <- gsub(pattern = "abc", replace = "ccccccccccccccccccccc", x = tx)
  # writeLines(tx2, con="~/Desktop/text2.txt")
  
  
  
  for (lineIndex in 1:numLines){
    
    if(nchar(text[lineIndex]) > 0){
      
      match = grep(OriginalGroupName, text[lineIndex]);
      
      if(length(match) > 0){
        
        # Does the next line start with a number?
        startChar = substr(text[lineIndex + 1], 0, 1); # 0, 10 would catch if it starts with #
        
        result = grep("[0-9]", startChar)
        
        if(length(result) > 0){
          for (nnn in 1:numx){
            
            # Double line parameter.
            write(text[lineIndex], "", append = TRUE);
            write(text[lineIndex + 1], "", append = TRUE);
            
            #Now replace the strings and print out.
            string = gsub(OriginalGroupName, AdditionalGroupName, text[lineIndex]);
            write(string, "", append = TRUE);
            write(text[lineIndex + 1], "", append = TRUE);
          }
        }else{
          for (nnn in 1:numx){
            # Single line parameter.
            write(text[lineIndex], "", append = TRUE);
            
            #Now replace the strings and print out.
            string = gsub(OriginalGroupName, AdditionalGroupName, text[lineIndex]);
            write(string, "", append = TRUE);
          }
        }
      }
      else{
        write(text[lineIndex], "", append = TRUE);
      }
    }else{
      write(text[lineIndex], "", append = "TRUE");
    }
  }
  
}
sink();


#################################################### this seems to work... 20181210 RM
inputFileName = paste(d1,"/at_harvest_neus_v15_DE.prm", sep='')
outputFileName = paste(d1,"/at_harvest_neus_v15_DE_RM9.prm", sep='')

text <- readLines(inputFileName,encoding="UTF-8")
sink(outputFileName);
numLines = length(text)
topMatch=unique(cr$Parent)
# lowMatch=unique(cr$Parent)

for (lineIndex in 1:numLines){
  # if(nchar(text[lineIndex]) > 0){
  # matchA = grep(topMatch, text[lineIndex]);
  # matchA = grep("BFF|BFS|BML|BMS|CEP|FBP|FDB|FDC|FDD|FDE|FDO|FDS|FMM|FPL|FPS|FVB|FVD|FVS|FVT|PWN|SHB|SHD|SHP|SSK|WHB|WHT",text[lineIndex]);
  excludes=grep("FMN|FVO|BFD|BMD|SP|SHR|WDG|FPO|FDP|FDM|FVV|SHC|WHS", text[lineIndex]);
  if(length(excludes) > 0){
    next
  }
  else{
  matchA = grep("BB|BC|BD|BFF|BFS|BG|BML|BMS|BO|CEP|DC|DCsed|DF|DL|DLsed|DR|DRsed|FBP|FDB|FDC|FDD|FDE|FDF|FDO|FDS|FMM|FPL|FPS|FVB|FVD|FVS|FVT|MA|MB|PB|PIN|PL|PS|PWN|REP|SB|SG|SHB|SHD|SHP|SSK|WHB|WHT|ZG|ZL|ZM|ZS",text[lineIndex]);
  startChar = substr(text[lineIndex + 1], 0, 1); # 0, 10 would catch if it starts with #
  result = grep("[0-9]", startChar)
  if(length(matchA) == 0 & length(result==0)){
    write(text[lineIndex], "", append = "TRUE");
  }
   else{
     
   for (nn in 1: length(cr$Parent)){
      OriginalGroupName = cr$Parent[nn] #"FVT"
      AdditionalGroupName = cr$Child[nn] #"BIL"
      # numx=cr$rpt[nn] # number of times it is repeated
      # numnew=cr$diff[nn] # number of new entries (excludes when parent==child)
      
      if(nchar(text[lineIndex]) > 0){
        
        match = grep(OriginalGroupName, text[lineIndex]);
        
        if(length(match) > 0){
          
          # Does the next line start with a number?
          startChar = substr(text[lineIndex + 1], 0, 1); # 0, 10 would catch if it starts with #
          
          result = grep("[0-9]", startChar)
          
          if(length(result) > 0){
            
            # Double line parameter.
            # write(text[lineIndex], "", append = TRUE);
            # write(text[lineIndex + 1], "", append = TRUE);
            
            #Now replace the strings and print out.
            string = gsub(OriginalGroupName, AdditionalGroupName, text[lineIndex]);
            write(string, "", append=T)
            write(text[lineIndex + 1], "", append = TRUE);
          }else{
            
            # Single line parameter.
            # write(text[lineIndex], "", append = TRUE);
            
            #Now replace the strings and print out.
            string = gsub(OriginalGroupName, AdditionalGroupName, text[lineIndex]);
            write(string, "", append = F);
          }
        }
      }
      }
    }
  }
}
sink()

## now clean up file to remove instances of name changes FPS->HER not caught in above code conversion
test=cr[which((cr$Child != cr$Parent) & (cr$diff<2)),] # find name changes
inputFileName = paste(d1,"/at_harvest_neus_v15_DE_RM9.prm", sep='')
outputFileName = paste(d1,"/at_harvest_neus_v15_DE_RM_10.prm", sep='')

text <- readLines(inputFileName,encoding="UTF-8")
sink(outputFileName);
numLines = length(text)

for (lineIndex in 1:numLines){
  excludes=grep("BFS|BML|BMS|FDB|FDD|FDE|FDF|FDO|FDS|FMM|FPL|FPS|FVD|FVS|SHB", text[lineIndex]);
  excludes2=grep("BFS|BML|BMS|FDB|FDD|FDE|FDF|FDO|FDS|FMM|FPL|FPS|FVD|FVS|SHB", text[lineIndex-1]) #check previous to catch in loop;
  # Does the next line start with a number?
  startChar = substr(text[lineIndex + 1], 0, 1); # 0, 10 would catch if it starts with #
  result = grep("[0-9]", startChar)
  if((length(excludes) > 0) & (length(result)>0)){
    # lineIndex=lineIndex+1
    # string = lineIndex;
    # write(string, "", append = F);
    next
  }
  else if ((length(excludes2) > 0) & (length(result)==0)){
    # lineIndex=lineIndex+1
    # string = lineIndex;
    # write(string, "", append = F);
    next
  }
  else{
    string = text[lineIndex];
    write(string, "", append = F);
  }
}
sink()