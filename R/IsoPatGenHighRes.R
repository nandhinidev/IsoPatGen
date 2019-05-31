source(file.path(getwd(),"R","functions.R"))

# Check to see if required packages are installed and install them if not found
packages = c("data.table","testthat")
check.pack = lapply(packages,function(x) if(!require(x,character.only=TRUE)){install.packages(x,dependancies=TRUE)
  library(x,character.only=TRUE)})

# Read User Input
comp <- read.table(file.path(getwd(),"data","input.txt"),sep="\t",header=FALSE)
comp <- data.frame(comp)
colnames(comp) <- c("carbon","hydrogen","nitrogen","oxygen","sulphur","niso")

carbon = as.integer(comp$carbon)
if(carbon <= 0){
  stop('Number of carbon atoms must be greater than zero')
}

hydrogen = as.integer(comp$hydrogen)
if(hydrogen <= 0){
  stop('Number of hydrogen atoms must be greater than zero')
}

nitrogen = as.integer(comp$nitrogen)

oxygen = as.integer(comp$oxygen)

sulphur = as.integer(comp$sulphur)

niso = comp$niso

# Isotopic envelope
isogrid <- envelope(niso)
pair <- pred.int(isogrid,niso)
final <- pair[which(pair[,2]>0),]
colnames(final) <- c("mz","Intensity")
final <- within(final,
                {RA <- Intensity/max(Intensity)*100})
final <- final[with(final,order(RA,decreasing=TRUE)),]
colnames(final) <- c("#m/z","Intensity","RA")
if(!dir.exists(file.path(getwd(),"results"))){dir.create(file.path(getwd(),"results"))}
write.table(final,file=file.path(getwd(),"results","output.txt"),sep="\t",quote=FALSE,col.names=TRUE,row.names=FALSE)



