# Functions to calculate the isotope distribution for each element

cdist <- function(carbon,ci)
{
  if (carbon >= ci)
  {
    return(dmultinom(c(carbon-ci,ci),carbon,c(0.9893,0.0107)))
  }
  else {return(0)}
}

hdist <- function(hydrogen,hi)
{
  if (hydrogen >= hi)
  {
    return(dmultinom(c(hydrogen-hi,hi),hydrogen,c(0.999885,0.000115)))
  }
  else {return(0)}
}

ndist <- function(nitrogen,ni)
{
  if (nitrogen >= ni)
  {
    return(dmultinom(c(nitrogen-ni,ni),nitrogen,c(0.99632,0.00368)))
  }
  else {return(0)}
}

odist <- function(oxygen,o17i,o18i)
{
  if (oxygen >= (o17i+o18i))
  {
    return(dmultinom(c(oxygen-o17i-o18i,o17i,o18i),oxygen,c(0.99757,0.00038,0.00205)))
  }
  else {return(0)}
}

sdist <- function(sulphur,s33i,s34i)
{
  if (sulphur >= (s33i+s34i))
  {
    return(dmultinom(c(sulphur-s33i-s34i,s33i,s34i),sulphur,c(0.9493,0.0076,0.0429)))
  }
  else {return(0)}
}

# Function to build the isotopic envelope
envelope <- function(niso) {
# Calculate all theoretical possibilities
chcomb <- data.table::CJ(seq(0,niso),seq(0,niso))
#Build isotopic envelope one element at a time
chcomb[,sum:=Reduce(`+`,.SD)]
chcombf <- chcomb[sum<(niso+1),1:2]
chncomb <- chcombf[,seq(0,niso),by=chcombf]
chncomb[,sum:=Reduce(`+`,.SD)]
chncombf <- chncomb[sum<(niso+1),1:3]
oscomb <- CJ(seq(0,niso),seq(0,niso,2))
oscomb[,sum:=Reduce(`+`,.SD)]
oscombf <- oscomb[sum<(niso+1),1:2]
chnocomb <- chncombf[,as.list(oscombf),by=chncombf]
chnocomb[,sum:=Reduce(`+`,.SD)]
chnocombf <- chnocomb[sum<(niso+1),1:5]
chnoscomb <- chnocombf[,as.list(oscombf),by=chnocombf]
chnoscomb[,sum:=Reduce(`+`,.SD)]
chnoscombf <- chnoscomb[sum<(niso+1)]
colnames(chnoscombf) <- c("ciso","hiso","ntiso","o17iso","o18iso","s33iso","s34iso","sum")
return(chnoscombf)
}

# Predict abundances
pred.int <- function(grid.data,niso){
  new <- data.frame(mz=numeric(),Intensity=numeric())
  for (i in seq(1:(niso+1)))
  {
    if((i-1)==0){
      cat("Calculating Monoisotopic Peak...\n")}
    else{
      cat(paste((i-1),"isotope...\n",sep=" "))
    }
    mat <- grid.data[sum==(i-1)]
    mat[,cprob:=mapply(cdist,ci=ciso,carbon=carbon)]
    mat[,cmass:=13.003355 * ciso + 12 * (carbon - ciso)]
    mat[,hprob:=mapply(hdist,hi=hiso,hydrogen=hydrogen)]
    mat[,hmass:=2.014102 * hiso + 1.007825 * (hydrogen - hiso)]
    mat[,nprob:=mapply(ndist,ni=ntiso,nitrogen=nitrogen)]
    mat[,nmass:=15.000109 * ntiso + 14.003074 * (nitrogen - ntiso)]
    mat[,oprob:=mapply(odist,o17i=o17iso,o18i=o18iso/2,oxygen=oxygen)]
    mat[,omass:=16.999132 * o17iso + 17.999160 * (o18iso/2) + 15.994915 * (oxygen - o17iso - (o18iso/2))]
    mat[,sprob:=mapply(sdist,s33i=s33iso,s34i=s34iso/2,sulphur=sulphur)]
    mat[,smass:=32.971458 * s33iso + 33.967867 * (s34iso/2) + 31.972071 * (sulphur - s33iso - (s34iso/2))]
    prob <- mat[,(cprob*hprob*nprob*oprob*sprob)]
    mass <- mat[,(cmass+hmass+nmass+omass+smass)]
    plist <- data.frame(cbind(mass,prob))
    new <- rbind(new,plist)
  }
  return(new)
}