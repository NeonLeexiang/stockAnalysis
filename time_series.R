# 时间序列分析

# use quantmod
library(quantmod) 
# quantmod是一个金融数据分析的主要R语言包
# getSymbols命令主要从美国股市数据库中获得参数定义的上市公司名称的股价数据，注意括号中的双引号
# chartSeries命令可以用来绘制走势图，subset参数用来定义多长时间的数据展示出来，可以使天，月或者年

getSymbols("000001.ss") # `000001.SS` 为上证指数，ss为上证代码，sz为深圳，hk为港股

# through the plot we can easily conclude that 000001.ss is more suitable for time series research
chartSeries(`000001.SS`, up.col='red', dn.col='green',TA="addVo(); addMACD()")  # 追加MACD线
data = as.data.frame(`000001.SS`) # switch the 'xts' object to the 'dataFrame'

# time_series plot
dates = seq(from = as.Date("2007-01-04"),by=1,length.out = 2853) # set sequence for the x axis of plot
plot(dates,data_000001$`000001.SS.Adjusted`,type = 'l') # plot time_series
# also can use the packages of tseries
library(tseries)
yield = ts(data_000001$`000001.SS.Adjusted`,start = c(2007,1,4),frequency = 365)
plot(yield)
abline(lm(yield~time(yield)),col="red")

# then we preprocess the data
# find that the column has NA so we need to remove them
y = data_000001$`000001.SS.Adjusted`
y_na = na.omit(y)
# new a time series object
yield_na = ts(y_na,start = c(2007,1,4),frequency = 365)

# then we analyze the data
# 为了估计时间序列的趋势的、季节性和不规则部分，输入代码
yield_na_components = decompose(yield_na)
plot(yield_na_components)

# now we do the test of yield_na (the ts object without NA)
acf(yield_na) # 自相关图检验
pacf(yield_na) # 偏自相关图检验
Box.test(yield_na) # 纯随机性检验(白噪声检验) we find that p-value < 2.2e-16 为白噪声序列
# 需要做一个平稳性检验：然后再做白噪声检验 need package:tseries
adf.test(yield_na) # p_value < 0.05是拒绝原假设。原假设是非平稳

# 系统自动定阶数
library(zoo)
library(forecast)
auto.arima(yield_na) # 定阶结果为ARIMA(4,1,4)
# 在论文叙述中应该使用AIC BIC准则去确定阶数，同时也应该做一个一阶差分的操作

# 详细的一阶差分的操作如下
yield_na_diff = diff(yield_na)
plot(yield_na_diff)
# compare the first and the second
var(yield_na) # 588431.9
var(yield_na_diff) # 3087.753
var(diff(yield_na,1,2)) # 5952.11
# then we can conclude that we should use the first order difference
# adf test, Stationality test
adf.test(yield_na_diff)

# then we do more to make sure the order of the model of ARIMA
# we check the ACF and the PACF, then we use the AIC and BIC
acf(yield_na_diff)
pacf(yield_na_diff)

# then we check the model
yield_na_diff_fit = arima(yield_na, order = c(4,1,4))
for(i in 1:2) print(Box.test(yield_na_diff_fit$residuals,lag=6*i)) # p-value > 0.05 obviously it is valid

# at last we do a forecast of 10 days
# need to library zoo and forecast
yield_na_diff_fit_fore = forecast(yield_na_diff_fit,h=10)
plot(yield_na_diff_fit_fore)

# 以上我都犯了一个错误，时间序列不同于机器学习，时间序列具有类似马尔可夫的性质，意味着非常久远的数据
# 并不适合做时间序列分析，所以接下来应该做短期的时间序列分析，将数据量的数量级控制在一定水平。
# 而大数据量的东西拿来做随机森林的处理和预测，短期的数据还是适合时序分析。
# 关于BP神经网络和随机森林，后续分析。


# 失败的一次尝试


# 以下进行小数据量分析预测，按照训练集和测试集的比例1:9，取100个数据集
# 用前90个数据集作为训练集进行模型拟合，用后10个数据集
train = yield_na[2752:2851]
train_na = train[1:90] # training set
test_na = train[91:100]# test set

# tseries
train_na_ts = ts(train_na)
plot(train_na_ts)
abline(lm(train_na_ts~time(train_na_ts)),col="red")

# now we do the test of yield_na (the ts object without NA)
acf(train_na_ts) # 自相关图检验
pacf(train_na_ts) # 偏自相关图检验
Box.test(train_na_ts) # 纯随机性检验(白噪声检验) we find that p-value < 2.2e-16 为白噪声序列
# 需要做一个平稳性检验：然后再做白噪声检验 need package:tseries
adf.test(train_na_ts) # p_value < 0.05是拒绝原假设。原假设是非平稳

# 详细的一阶差分的操作如下
train_na_ts_diff = diff(train_na_ts)
plot(train_na_ts_diff)
# compare the first and the second
var(train_na_ts) 
var(train_na_ts_diff) 
# then we can conclude that we should use the first order difference
# adf test, Stationality test
adf.test(train_na_ts_diff) # 通过了平稳性检验
Box.test(train_na_ts_diff) # 但是没通过Box.test
# 通过以上的检验发现了90个数据量无法做到周期性，所以就想着尝试下更过的数据。


# 用更大数据量的预测
train = yield_na[2000:2851]
train_na = train[1:841]
test_na = train[842:851]

# tseries
train_na_ts = ts(train_na)
plot(train_na_ts)
abline(lm(train_na_ts~time(train_na_ts)),col="red")

# now we do the test of yield_na (the ts object without NA)
acf(train_na_ts) # 自相关图检验
pacf(train_na_ts) # 偏自相关图检验
Box.test(train_na_ts) # 纯随机性检验(白噪声检验) we find that p-value < 2.2e-16 为白噪声序列
# 需要做一个平稳性检验：然后再做白噪声检验 need package:tseries
adf.test(train_na_ts) # p_value < 0.05是拒绝原假设。原假设是非平稳

# 详细的一阶差分的操作如下
train_na_ts_diff = diff(train_na_ts)
plot(train_na_ts_diff)
# compare the first and the second
var(train_na_ts) 
var(train_na_ts_diff) 
# then we can conclude that we should use the first order difference
# adf test, Stationality test
adf.test(train_na_ts_diff) # 通过了平稳性检验
Box.test(train_na_ts_diff) # 通过了Box.test

# 以下开始拟合模型并且进行预测
train_na_diff_fit = arima(train_na, order = c(3,1,5))
for(i in 1:2) print(Box.test(train_na_diff_fit$residuals,lag=6*i)) # p-value > 0.05 obviously it is valid

train_na_diff_fit_fore = forecast(train_na_diff_fit,h=10)
plot(train_na_diff_fit_fore)

# 以下做时间序列预测结果比较
train_mean = train_na_diff_fit_fore$mean
res = (train_mean - test_na)/test_na
plot((train_mean - test_na)/test_na)
