# matrixProfile: Matrix Profile in r
# Published:	2018-08-14
# Author:	Donghwan Kim
# Maintainer:	Donghwan Kim <donhkim9714 at korea.ac.kr, dhkim2@bistel.com>
# A simple and the early stage package for matrix profile based on the paper of ChinChia Michael Yeh, Yan Zhu, Liudmila Ulanova, Nurjahan Begum, Yifei Ding, Hoang Anh Dau, Diego Furtado Silva, Abdullah Mueen, and Eamonn Keogh (2016) <DOI:10.1109/ICDM.2016.0179>. This package calculates all-pairssimilarity for a given window size for time series data.


#### pkgs ####
library(graphics)
library(stats)
library(TTR)
library(zoo)
library(fftw)
library(signal)


#### functions ####
stamp = function(q, t, by = 10, isPlot = FALSE){

  stamp.models = list()
  class(stamp.models) = "stamp.models"
  n = length(t) ; w = length(q) ; excluedZone.Len = round(length(q)*0.5) ; proLen = (n-w+1)
  MP = rep(x = Inf, proLen)
  MPI = rep(-1, proLen)
  anytime = sample(proLen, proLen, replace = F)
  percent = floor(seq(1, proLen, proLen/(100/by)))
  k = 1
  for(i in anytime){
    # make MP
    d = mass(t = t, q = t[anytime[i]:(anytime[i]+w-1)])
    exclueZone.start = max(1, (anytime[i]-excluedZone.Len))
    exclueZone.end = min(proLen, (anytime[i]+excluedZone.Len))
    d[exclueZone.start:exclueZone.end] = Inf
    MP = pmin.int(MP, d) # is slightly faster than MP = pmin(MP, d)
    MPI[which(MP == d)] = anytime[i]

    # check process percentage
    p = which((k == percent) == "TRUE")
    if(length(p)){
      out = floor((percent[p]/proLen)*100)
      cat(paste(out,"% completed", "\n" ), sep = "")
    }
    if(k == proLen){
      cat("100% comleted")
    }

    # for plot
    if(out %in% seq(from = 0, to = 100, by = by) && length(p) && isPlot){
      plot(MP, type = "l", main = paste("mp_", out, "%",sep = ""))
    }else if(k == proLen){
      plot(MP, type = "l", main = paste("mp_100", "%", sep = ""))
    }
    k = k + 1
  }

  motif = order(MP, decreasing = F)[1:7]
  motif1_idx = motif[order(motif[1:2], decreasing = F)]
  stamp.models$MP = MP
  stamp.models$MPI = MPI
  stamp.models$MTI = motif1_idx
  return(stamp.models)
}

