# matrixProfile: Matrix Profile in r
# Published:	2018-08-14
# Author:	Donghwan Kim
# Maintainer:	Donghwan Kim <donhkim9714@korea.ac.kr, dhkim2@bistel.com>
# A simple and the early stage package for matrix profile based on the paper of ChinChia Michael Yeh, Yan Zhu, Liudmila Ulanova, Nurjahan Begum, Yifei Ding, Hoang Anh Dau, Diego Furtado Silva, Abdullah Mueen, and Eamonn Keogh (2016) <DOI:10.1109/ICDM.2016.0179>. This package calculates all-pairssimilarity for a given window size for time series data.


#### pkgs ####
library(graphics)
library(stats)
library(TTR)
library(zoo)
library(fftw)
library(signal)


#### functions ####
movstd = function(x = x, w = w){
  temp.sd = numeric(length(x))
  w = w+1
  for(k in 2:(w-1)){
    temp.sd[k] = std(x[1:k])
  }
  temp.sd = na.omit(c(temp.sd, (runSD(x, w) *sqrt((w-1)/(w)))))

  return(c(0, temp.sd))
}



