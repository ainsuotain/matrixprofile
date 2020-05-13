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
movmean = function(x = x, w = w){
  i = 1
  temp.mean = numeric(length(x))
  w = w+1
  for(k in 1:w){
    if(i < w){
      temp.mean[k] = mean(x[1:i])
    }
    i = i + 1
  }
  temp.mean = c(temp.mean, rollapply(x, w, mean))
  return(temp.mean)
}
