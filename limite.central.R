#-- 中心极限定理. 渐近正态性的图形检验.
#-- 函数参数说明：
#-- 函数默认为         均匀分布.           r = {runif}
#-- 分布区间默认为     [0,1]               distpar = {c(0,1)}
#-- 分布的均值默认为   0.5                 m = {0.5}
#-- 分布的标准差默认为 1/sqrt(12)          s = {1/sqrt(12)}
#-- 样本容量大小默认为 c(1,3,10,30)        n = {c(1, 3, 10, 30)}
#-- 重复次数默认为     1000                N = {1000}
limite.central <- function (r=runif, distpar=c(0,1), m=.5,s=1/sqrt(12),n=c(1,3,10,30), N=1000) {
  for (i in n) {
    if (length(distpar)==2){
      x <- matrix(r(i*N, distpar[1],distpar[2]),nc=i)
    }
    else {
      x <- matrix(r(i*N, distpar), nc=i)
    }
    x <- (apply(x, 1, sum) - i*m )/(sqrt(i)*s)
    hist(x, col='light blue', probability=T, main=paste("n=",i), ylim=c(0,max(.4, density(x)$y))) # 绘制X的直方图
    lines(density(x), col='red', lwd=3)                                                           # 绘制X的核密度估计值线
    curve(dnorm(x), col='blue', lwd=3, lty=3, add=T)                                              # 绘制X处标准正态分布的[概率]密度函数值线.
	legend("topright", legend=c("直方图","核密度估计","概率密度函数"), col=c("light blue","red","blue"), lty=c(1,2,3))
    #	qqnorm(x)		##创建x的基本Q-Q图
    #	qqline(x)       ##添加x的对角线
	# 如果x服从一个完美的正态分布,那么点会精确的落在对角线上.
	# 即: 当样本容量n逐渐增大，所得的随机变量x的和逐渐接近正态分布
	#    落点在对角线上. n逐渐增大时， x是逐渐近似正态的.
	# 对于落点在对角线之上 (向左偏移的x)，或许可以通过指数log(x)变换纠正
    if( N>100 ) {
      rug(sample(x,100))
    }
    else {
      rug(x)
    }
  }  
}



#--  以不同颜色打印基础图形系统plot函数图示.
disp.plot.icons <- function() {
	x <- seq(1:25)
	y <- seq(1:25)
	plot(x,y, pch=x, col=y, cex=3)
	rug(x)
}