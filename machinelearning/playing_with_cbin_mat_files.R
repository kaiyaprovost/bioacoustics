library(R.matlab)

mat = R.matlab::readMat("/Users/kprovost/Documents/TweetyNet/032212/gy6or6_baseline_220312_0836.3.cbin.not.mat")

str(mat)


## .cbin files are big endian, 16 bit signed int, hence dtype=">i2" below
to.read = file("/Users/kprovost/Documents/TweetyNet/032212/gy6or6_baseline_220312_0836.3.cbin", "rb")
test=readBin(to.read,what="integer",n=2400,endian="big",signed=T)
