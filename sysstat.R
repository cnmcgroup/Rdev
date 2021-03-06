

# To run this example use  Spark
# ./bin/spark-submit sysstat.R


# To run this in Local R.
# Rscript sysstat.R



# Load SparkR
# Set this to where Spark is installed
Sys.setenv(SPARK_HOME="/u03/hadoop/spark-2.1.0")
# This line loads SparkR from the installed directory
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
library(SparkR)

# Load SpartR library from absolute path
#if (nchar(Sys.getenv('SPARK_HOME')<1)){
#        Sys.setenv(SPARK_HOME='/u03/hadoop/spark-2.1.0')
#       library(SparkR,lib.loc=c(file.path(Sys.getenv('SPARK_HOME'),'R','lib')))
#}


#spark 2.1.x Version 
# Initialize SparkSession
sparkR.session(master="local", 
			appName = "R Spark SQL basic example", 
			sparkConfig = list(spark.some.config.option = "some-value", spark.driver.memory="2g")
	)


# Load R library.
library(data.table)
library(lubridate)
library(Rcpp)

# Load data.table colnames
#vmstat_header <- read.table('../log/VMSTAT_HEADERS.log', header=TRUE)
#iostat_header <- read.table('../log/IOSTAT_HEADERS.log', header=TRUE)

# Read statistics data.
#vmstat <- fread('../out/vmstat.out', col.names=names(vmstat_header),)
#iostat <- fread('../out/iostat.out', col.names=names(iostat_header))

# 加载源数据
# # Create a simple local data.frame(data.table)
sysstat <- fread(file='sysstat.out', 
			sep=",", 
			stringsAsFactors=FALSE, 
			header=FALSE, 
			col.names=c("datetime","name","value"), 
			# colClasses=c("date","character","numeric")
			colClasses=c(NA,NA,"numeric")
			)

# 计算单个因子行数
N <- nrow(sysstat) / length(levels(as.factor(sysstat[,name])))

redo.bytes <- as.numeric();
physical.read.bytes <- as.numeric();
physical.write.bytes <- as.numeric();
physical.read.total.bytes <- as.numeric();
physical.write.total.bytes <- as.numeric();



# 向量赋值
redo.value <- sysstat[name=='redo size',value]
physical.read.value <- sysstat[name=='physical read bytes',value]
physical.write.value <- sysstat[name=='physical write bytes',value]
physical.read.total.value <- sysstat[name=='physical read total bytes',value]
physical.write.total.value <- sysstat[name=='physical write total bytes',value]


# 0. Rcpp 加速.
print("Rcpp 调用C++ 语言函数.")
sourceCpp("sysstat.cpp")
redo.bytes <- getbytes(redo.value)
physical.read.bytes <- getbytes(physical.read.value)
physical.write.bytes <- getbytes(physical.write.value)
physical.read.total.bytes <- getbytes(physical.read.total.value)
physical.write.total.bytes <- getbytes(physical.write.total.value)

# C++ 语言数值数组首元素默认赋予0值, 
# R 语言均值计算时,首元素未知,按NA处理(忽略)
redo.bytes[1] <- NA
physical.read.bytes[1] <- NA
physical.write.bytes[1] <- NA
physical.read.total.bytes[1] <- NA
physical.write.total.bytes[1] <- NA

# 1.向量化方法
print("向量化赋值")
system.time(
for (i in 2:(N)) { 
        redo.bytes[i] <- redo.value[i] - redo.value[i-1] 
        physical.read.bytes[i] <- physical.read.value[i] - physical.read.value[i-1] 
        physical.write.bytes[i] <- physical.write.value[i] - physical.write.value[i-1]
        physical.read.total.bytes[i] <- physical.read.total.value[i] - physical.read.total.value[i-1] 
        physical.write.total.bytes[i] <- physical.write.total.value[i] - physical.write.total.value[i-1]
}
)

# 2.向量化并行方法
# parallel processing
#library(foreach)
#library(doSNOW)
#cl <- makeCluster(4, type="SOCK") # for 4 cores machine
#registerDoSNOW (cl)
#system.time(
#foreach (i = 2:N, .combine=c) %dopar%{ 
#        redo.bytes[i] <- redo.value[i] - redo.value[i-1] 
#        physical.read.bytes[i] <- physical.read.value[i] - physical.read.value[i-1] 
#        physical.write.bytes[i] <- physical.write.value[i] - physical.write.value[i-1]
#        physical.read.total.bytes[i] <- physical.read.total.value[i] - physical.read.total.value[i-1] 
#        physical.write.total.bytes[i] <- physical.write.total.value[i] - physical.write.total.value[i-1]
#}
#)


# 1.原始方法
#print("原始方法循环处理.")
#date()
#system.time(
#for (i in 2:(N)) { 
#       redo.bytes[i] <- sysstat[name=='redo size',][i, value] - sysstat[name=='redo size',][i-1, value] 
#       physical.read.bytes[i] <- sysstat[name=='physical read bytes',][i, value] - sysstat[name=='physical read bytes',][i-1, value] 
#       physical.write.bytes[i] <- sysstat[name=='physical write bytes',][i, value] - sysstat[name=='physical write bytes',][i-1, value] 
#       physical.read.total.bytes[i] <- sysstat[name=='physical read total bytes',][i, value] - sysstat[name=='physical read total bytes',][i-1, value] 
#       physical.write.total.bytes[i] <- sysstat[name=='physical write total bytes',][i, value] - sysstat[name=='physical write total bytes',][i-1, value] 
#}
#)
#date()


# 用均值初始化首元素
print("用均值填充首元素")
redo.bytes[1] <- round(mean(redo.bytes, na.rm = TRUE))
physical.read.bytes[1] <- round(mean(physical.read.bytes, na.rm = TRUE))
physical.write.bytes[1] <- round(mean(physical.write.bytes, na.rm = TRUE))
physical.read.total.bytes[1] <- round(mean(physical.read.total.bytes, na.rm = TRUE))
physical.write.total.bytes[1] <- round(mean(physical.write.total.bytes, na.rm = TRUE))


print(paste0("redo.bytes[1]:[",redo.bytes[1],"]"))
print(paste0("physical.read.bytes[1]:[",physical.read.bytes[1],"]"))
print(paste0("physical.write.bytes[1]:[",physical.write.bytes[1],"]"))
print(paste0("physical.read.total.bytes[1]:[",physical.read.total.bytes[1],"]"))
print(paste0("physical.write.total.bytes[1]:[",physical.write.total.bytes[1],"]"))

# data.table 赋值 
sysstat[name=='redo size',bytes:= redo.bytes]
sysstat[name=='physical read bytes',bytes:= physical.read.bytes]
sysstat[name=='physical write bytes',bytes:= physical.write.bytes]
sysstat[name=='physical read total bytes',bytes:= physical.read.total.bytes]
sysstat[name=='physical write total bytes',bytes:= physical.write.total.bytes]


sysstat[,datetime := ymd_hms(datetime)]
head(sysstat)


# Convert local data frame to a SparkDataFrame
sysstatDF  <- createDataFrame(sysstat)

# Print its schema
printSchema(sysstatDF)

# Register this DataFrame as a table.
createOrReplaceTempView(sysstatDF, "sysstat")

# SQL statements can be run by using the sql methods
redo_size <-  sql("select * from sysstat where name = 'redo size'") 

# Call collect to get a local data.frame
redo_df <- collect(redo_size)

# Print the redo size in our dataset
print(redo_df)
#base::head(redo_df)


# Stop the SparkSession now
sparkR.session.stop()




library(ggplot2)
library(Cairo)

# Cairo
#Cairo(600, 600, file="plot.png", type="png", bg="white")

# PNG
#png(file="plot5.png",width=640,height=480)

# Cairo PNG
#CairoPNG(file="Cairo5.png",width=640,height=480)

# SVG  
#svg(file="plot-svg5.svg",width=6,height=6)

# CairoSVG
CairoSVG(file="Cairo-svg5.svg",width=6,height=6)

ggplot(sysstat, mapping= aes(x = datetime, y = bytes/1024, color=factor(name))) + geom_line()
dev.off() 

# ggplot2::ggsave 保存
#ggsave( file = "mtcars_plot.png", width = 5, height = 6, type = "cairo", dpi = 600)
