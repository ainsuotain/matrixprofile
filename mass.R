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
mass = function(q, t){
  n = length(t) ; w = length(q)

  meany = mean(q)
  sigmay = std(q)
  meanx = movmean(x = t, w = (w-1))
  sigmax = movstd(x = t, w = (w-1))

  q = rev(q)
  q = append(q, rep(0, (n-w)))
  options(digits = 20)
  z = ifft(fft(t) * (fft(q)))
  z = Re(z)
  options(warn = -1)
  dist = 2*(w - (z[w:n] - w*meanx[w:n]*meany)/(sigmax[w:n]*sigmay))
  dist = (sqrt(dist))
  dist = replace(dist, is.na(dist), 0)

  return(dist)
}
