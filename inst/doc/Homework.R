## -----------------------------------------------------------------------------
  library(ggplot2)
  library(stats)
  library(bootstrap)
  library(DAAG)
  library(microbenchmark)
  library(Rcpp)

## ----echo=FALSE---------------------------------------------------------------

set.seed(123)

# 生成自变量 X 和因变量 Y
X <- rnorm(100, mean = 0, sd = 1)
Y <- 2*X + 3 + rnorm(100, mean = 0, sd = 1)

# 创建数据框
data<- data.frame(X, Y)

# 显示数据内容
knitr::kable(head(data, 10), caption = "随机产生的数据集 (前10行)")

## ----include = FALSE----------------------------------------------------------
# 拟合线性回归模型
model <- lm(Y ~ X, data = data)
summary(model)

## ----echo=FALSE---------------------------------------------------------------
# 提取模型系数并转换为表格
coefficients <- summary(model)$coefficients
knitr::kable(coefficients, caption = "线性回归模型系数")

## ----echo=FALSE---------------------------------------------------------------
# 绘制拟合图像
library(ggplot2)
ggplot(data, aes(x = X, y = Y)) +  geom_point() + geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)

## ----echo=FALSE---------------------------------------------------------------

# 生成虚拟数据集
# 设置随机种子
set.seed(123)

# 样本数量
n <- 100  

# 生成符合正态分布的自变量 X
X <- rnorm(n)

# 生成因变量 Y，使用逻辑回归模型生成

Y <- rbinom(n, 1, plogis(2 + 3 * X))  # 生成响应变量，使用逻辑回归模型生成

# 创建数据框
data <- data.frame(X, Y)

# 显示数据内容
knitr::kable(head(data, 10), caption = "随机产生的数据集 (前十行)")

## ----include = FALSE----------------------------------------------------------
# 拟合逻辑回归模型
model <- glm(Y ~ X, data = data, family = binomial(link = "logit"))
summary(model)

## ----echo=FALSE---------------------------------------------------------------
# 提取模型系数并转换为表格
coefficients <- summary(model)$coefficients
knitr::kable(coefficients, caption = "逻辑回归模型系数")

## ----echo=FALSE---------------------------------------------------------------
# 绘制拟合图像
data$prob <- predict(model, type = "response")
ggplot(data, aes(x = X, y = prob)) +
  geom_point(aes(color = factor(Y))) +
  geom_line(aes(y = fitted(model), color = "Fitted")) +
  labs(x = "X", y = "P(Y=1|X)", color = "Legend") +
  theme_minimal()

## ----include=FALSE------------------------------------------------------------
# 生成虚拟数据集
set.seed(123)
n <- 100
X <- seq(0, 10, length.out = n)
Y <- 2*X^2 - 3*X + rnorm(n)
data <- data.frame(X, Y)
head(data)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(head(data, 10), caption = "随机产生的数据集 (前十行)") 

## ----include=FALSE------------------------------------------------------------
# 多项式拟合模型
degree <- 2  # 多项式次数
model <- lm(Y ~ poly(X, degree, raw = TRUE), data = data)
summary(model)

## ----include=FALSE------------------------------------------------------------
# 多项式拟合模型
degree <- 2  # 多项式次数
model <- lm(Y ~ poly(X, degree, raw = TRUE), data = data)
summary(model)

## ----echo=FALSE---------------------------------------------------------------
coefficients <- summary(model)$coefficients
knitr::kable(coefficients)

## ----echo=FALSE---------------------------------------------------------------
# 预测值
data$predicted <- predict(model, data)

# 绘制拟合曲线
library(ggplot2)
ggplot(data, aes(x = X, y = Y)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "red") +
  labs(title = "多项式拟合模型拟合曲线",
       x = "X", y = "Y") +
  theme_minimal()

## ----include=TRUE-------------------------------------------------------------
my_sample <- function(n, a, b, prob = rep(1, length(a:b))) {
  # 生成0到1之间的随机数
  u <- runif(n)
  
  # 计算累积概率
  cum_prob <- cumsum(prob)
  
  # 逆变换生成随机样本
  samples <- a + findInterval(u, cum_prob) * (b - a)
  
  return(samples)
}

# 生成服从自定义概率分布的随机样本
set.seed(123)  # 设置随机种子以重现结果
n <- 10  # 样本数量
a <- 1   # 下限
b <- 10  # 上限
prob <- c(0.2, 0.3, 0.5)  # 自定义概率分布
my_sample(n, a, b, prob)


## ----include=TRUE-------------------------------------------------------------
# 定义逆变换方法生成随机样本的函数
inverse_transform_sample <- function(n) {
  u <- runif(n)  # 生成0到1之间的随机数
  samples <- -sign(u - 0.5) * log(1 - 2 * abs(u - 0.5))  # 逆变换生成随机样本
  return(samples)
}

# 生成标准拉普拉斯分布的随机样本
set.seed(123)  # 设置随机种子以重现结果
n <- 1000  # 样本数量
random_samples <- inverse_transform_sample(n)

# 绘制生成的样本和目标分布的直方图进行比较
hist(random_samples, prob = TRUE, breaks = 50, main = expression(paste(f(x) == frac(1, 2) * e^(-abs(x)))),
     xlab = "x", ylab = "Density", col = "lightblue", border = "white")
curve(0.5 * exp(-abs(x)), add = TRUE, col = "red", lwd = 2, lty = 2, n = 1000, from = -5, to = 5)

## ----include=TRUE-------------------------------------------------------------

# 密度函数 Beta(a, b)
beta_density <- function(x, a, b) {
  ifelse(x >= 0 & x <= 1, x^(a-1) * (1-x)^(b-1) * (gamma(a+b) / (gamma(a) * gamma(b))), 0)
}

# 接受-拒绝法生成 Beta(a, b) 分布的随机样本
accept_reject_sampling <- function(n, a, b) {
  samples <- numeric(n)
  
  for (i in 1:n) {
    accept <- FALSE
    
    while (!accept) {
      x <- runif(1)  # 生成均匀分布的随机变量
      u <- runif(1)  # 生成均匀分布的随机变量
      
      # 如果 u <= f(x)，则接受 x
      if (u <= beta_density(x, a, b)) {
        samples[i] <- x
        accept <- TRUE
      }
    }
  }
  
  return(samples)
}

# 生成 Beta(3, 2) 分布的随机样本
n <- 1000
a <- 3
b <- 2
samples <- accept_reject_sampling(n, a, b)

# 绘制直方图和理论密度曲线
hist(samples, breaks = 50,  prob = TRUE, main = "Beta(3, 2)")
curve(beta_density(x, a, b), add = TRUE, col = "red", lwd = 2, n = 1001)

## ----include=TRUE-------------------------------------------------------------
# 定义密度函数 f_e(x)
f_e <- function(x) {ifelse(abs(x) <= 1, 3/4 * (1 - x^2), 0)}

# 生成f_e(x)随机变量的算法
generate_variable <- function() {
  U1 <- runif(1, -1, 1)
  U2 <- runif(1, -1, 1)
  U3 <- runif(1, -1, 1)
  
  if (abs(U3) >= abs(U2) && abs(U3) >= abs(U1)) {
    return(U2)
  } else {
    return(U3)
  }
}

# 生成一组大样本随机变量
n <- 10000  # 生成随机变量的数量
variables <- replicate(n, generate_variable())

# 统计随机变量的分布
hist(variables,breaks = 100, freq = FALSE, main = expression(f(x)==(3/4)(1-x^2)))
curve(f_e(x), add = TRUE, col = "red", lwd = 2, n = 1001)

## ----include=TRUE-------------------------------------------------------------
# 设置参数
n <- 106  # 样本数量
K <- 100  # 重复模拟次数

rho_1 <- 1.0  # 第一个rho值
rho_2 <- 0.5  # 第二个rho值
rho_3 <- 0.8  # 第三个rho值

# 定义函数，用于模拟pi值
simulate_pi <- function(rho) {
  pi_values <- numeric(K)
  for (i in 1:K) {
    # 模拟m的值，使用binom函数从二项分布中抽样
    m <- rbinom(1, n, rho)
    # 计算p_hat和pi_hat
    p_hat <- n / m
    pi_hat <- 2 * rho * p_hat
    # 将得到的pi_hat存储在pi_values中
    pi_values[i] <- pi_hat
  }
  return(pi_values)
}

# 分别模拟三个rho值下的pi，并计算方差
pi_values_1 <- simulate_pi(rho_1)
var_pi_1 <- var(pi_values_1)

pi_values_2 <- simulate_pi(rho_2)
var_pi_2 <- var(pi_values_2)

pi_values_3 <- simulate_pi(rho_3)
var_pi_3 <- var(pi_values_3)

# 输出结果
print(paste("Variance of pi for rho 1:", var_pi_1))
print(paste("Variance of pi for rho 2:", var_pi_2))
print(paste("Variance of pi for rho 3:", var_pi_3))


## ----include=TRUE-------------------------------------------------------------
m <- 10000  # 样本数量
U <- runif(m)

# 简单蒙特卡罗法估计
T1 <- exp(U) 
theta_simple <- mean(T1)
# 对偶变量法估计
T2 <- (exp(U) + exp(1 - U))/2
theta_antithetic <- mean(T2)

# 计算方差减少百分比的经验估计
var_simple <- var(T1)
var_antithetic <- var(T2)
var_reduction_percentage <- (1 - var_antithetic / var_simple) * 100

# 打印结果
cat("Simple Monte Carlo estimate of theta:", theta_simple, "\n")
cat("Antithetic variate estimate of theta:", theta_antithetic, "\n")
cat("Percent reduction in variance:", var_reduction_percentage, "%\n")

## ----include=TRUE-------------------------------------------------------------
# 生成一组 x 值
x = seq(1,11,0.1)
w <- 2

g = function (x) {
  x ^ 2 / sqrt(2*pi) * exp(-x^2/2)
}

f1 = function(x, sigma){
  x/(sigma)^2 * exp(-x^2/(2 * sigma^2))
}

f2 = function(x, μ){
  1/sqrt(2*pi) * exp(-(x - μ)^2/2)
}

# 计算目标函数在x值上的概率密度值
f = g(x)
# 计算瑞利分布在x值上计算分布参数为1.5的概率密度值
f1 =f1(x,  1.5)
# 计算正态分布在x值上均值参数为1.5的概率密度值
f2 = f2(x, 1.5)

lim = max(c(f, f1, f2))
# 绘制 g(x) 的概率密度函数曲线
plot(x, f, type = "l", main= "",ylab = "", ylim = c(0, lim), lwd = w)

# 分别绘制 瑞利分布 和 正态 的概率密度函数曲线
lines(x, f1, lwd = w, col="red", ylim = c(0, lim))
lines(x, f2, lwd = w,  col="blue", ylim = c(0, lim))

# 正态分布的概率密度函数f2更接近目标函数


## ----include = TRUE-----------------------------------------------------------
sigma.rayleigh <- 1.5
mean <- 1.5
n <- 10000

g <- function (x) {
  x ^ 2 / sqrt(2*pi) * exp(-x^2/2) * (x > 1)
}

f1 = function (x) {
  (x / sigma.rayleigh^2) * exp(-x^2 / (2 * sigma.rayleigh^2)) * (x > 1)
}

f2 <- function (x) {
  dnorm(x, mean = mean) * (x > 1)
}

rf1 <- function () {
  sqrt(-2 * sigma.rayleigh^2 * log(runif(n))) * (runif(n) < 1) + 1
}

rf2 <- function () {
  rnorm(n, mean = mean)
}

is.rayleigh <- function () {
  xs <- rf1()
  return(mean(g(xs)/f1(xs), na.rm = TRUE))  
}

is.norm <- function () {
  xs <- rf2()
  return(mean(g(xs)/f2(xs), na.rm = TRUE))  
}

theta1 <- is.rayleigh()
theta2 <- is.norm()
truth <- 0.400626

theta1  # 输出 theta1 的值
theta2  # 输出 theta2 的值
truth   # 输出真实值

## ----include=TRUE-------------------------------------------------------------

M <- 10000 # 总抽样数
N <- 1000 # 重复抽样的次数
k <- 5 # 分层数

 # 使用逆变换方法生成具有密度函数 f_k(x) 的随机数
inv_fun <- function(n, a, b) {
  -log(exp(-a) - (exp(-a) - exp(-b)) * runif(n))
}


res3 <- sapply(1:N, function(o) {
  x <- inv_fun(M, 0, 1) # 生成满足密度函数 f(x) 的随机数
  M1 <- mean((1 - exp(-1)) / (1 + x^2))  # 使用重要抽样方法计算 M1 的估计值
  M2 <- numeric(k) # 使用重要抽样方法计算 M1 的估计值
  for (j in 0:(k - 1)) {
    a <- j / k
    b <- (j + 1) / k
    xj <- inv_fun(M / k, a, b)  # 生成满足密度函数 f_j(x) 的随机数
    M2[j + 1] <- mean((exp(-a) - exp(-b)) / (1 + xj^2))  # 使用分层重要抽样方法计算 M2 的估计值
  }
  c(M1, sum(M2)) # 使用分层重要抽样方法计算 M2 的估计值
})


c(var(res3[1, ]), var(res3[2, ])) # 使用分层重要抽样方法计算 M2 的估计值

## ----include = TRUE-----------------------------------------------------------
exercise_6_5 <- function(seed = 123) {
  set.seed(seed)
  n <- 20
  c <- qt(0.975, n - 1)  # t-分布的0.975分位数

  # 使用蒙特卡洛模拟计算t区间
  m <- 1000
  cv.t <- sapply(1:m, FUN = function(o) {
    x <- rchisq(n, 2)  # 从卡方分布中生成样本数据x
    m <- mean(x)  # 均值的估计值
    se <- sqrt(var(x))  # 标准误差的估计值
    as.numeric((m - c * se / sqrt(n) < 2) & (m + c * se / sqrt(n) > 2))  # 置信区间
  })
  level1 <- mean(cv.t)  # 蒙特卡洛实验的平均值，即t区间的覆盖概率估计

  # 使用卡方区间估计方差的覆盖概率
  alpha <- 0.05
  UCL <- replicate(1000, expr = {
    x <- rchisq(n, 2)
    (n - 1) * var(x) / qchisq(alpha, df = n - 1)
  })
  level2 <- sum(UCL > 4) / m

  return(data.frame(level1, level2))
}

result <- exercise_6_5(1012)
print(result)
# 我们可以看到结果远小于0.95，因此t-区间更稳健

## ----include = TRUE-----------------------------------------------------------
exercise_6_A <- function(seed){
  set.seed(123) #设置随机种子
  
  num<-c(50,100,200,500,1000)  # 不同样本大小
  m<-10000 # 迭代次数
 
  error<-NULL # 存储结果的空向量
  for (n in num){
    cv<-qt(0.975,n-1) # 计算自由度为 n-1 的 t 分布上的临界值
   
   # 估计卡方分布的经验第一类错误率
  error_1<-mean(sapply(1:m,FUN = function(o){
    x<-rchisq(n,1)
    m<-mean(x)
    se<-sqrt(var(x))
    abs((m-1)*sqrt(n)/se)>=cv
}))  
  
  # 估计均匀分布的经验第一类错误率
  error_2<-mean(sapply(1:m,FUN = function(o){
    x<-runif(n,0,2)
    m<-mean(x)
    se<-sqrt(var(x))
    abs((m-1)*sqrt(n)/se)>=cv
  }))  
  # 估计指数分布的经验第一类错误率
  error_3<-mean(sapply(1:m,FUN = function(o){
    x<-rexp(n,1)
    m<-mean(x)
    se<-sqrt(var(x))
    abs((m-1)*sqrt(n)/se)>=cv 
}))  
  # 将结果添加到结果矩阵中
  error <-cbind(error,c(error_1,error_2,error_3))
}
  colnames(error)<-num # 列名
  rownames(error)<-c("χ2(1)","U(0,2)","exp(1)") # 行名
  return(error) # 返回结果矩阵               
}

result <- exercise_6_A(123) # 调用函数并传入随机种子
print(result)  # 打印结果矩阵

## ----include=TRUE-------------------------------------------------------------
# 设置模拟参数
M <- 1000      # 模拟次数
alpha <- 0.1   # 显著性水平

# 初始化结果变量
FWER_bonf <- 0  # Bonferroni校正的FWER
FDR_bh <- 0    # B-H校正的FDR
TPR_bh <- 0    # B-H校正的TPR

# 进行模拟
for (i in 1:M) {
  # 生成m个p值
  m <- 1000
  p <- rep(0, m)
  
  for (j in 1:m) {
    if (runif(1) <= 0.95) {
      p[j] <- runif(1)
    } else {
      p[j] <- rbeta(1, 0.1, 1)
    }
  }
  
  # Bonferroni校正的p值
  p_bonf <- p.adjust(p, method = "bonferroni")
  
  # B-H校正的p值
  p_bh <- p.adjust(p, method = "BH")
  
  # 计算拒绝原假设的情况
  reject_bonf <- p_bonf <= alpha
  reject_bh <- p_bh <= alpha
  
  # 计算统计指标
  FWER_bonf <- FWER_bonf + sum(reject_bonf)
  FDR_bh <- FDR_bh + sum(reject_bh & !reject_bonf)
  TPR_bh <- TPR_bh + sum(reject_bh)
}

# 计算平均统计指标
FWER_bonf <- FWER_bonf / (M * m)
FDR_bh <- FDR_bh / (M * m)
TPR_bh <- TPR_bh / (M * m)

# 输出结果
result <- matrix(c(FWER_bonf, NA, NA,
                   NA, FDR_bh, TPR_bh),
                 nrow = 2, ncol = 3,
                 dimnames = list(c("Bonferroni", "B-H"),
                                 c("FWER", "FDR", "TPR")))
print(result)

## ----include=TRUE-------------------------------------------------------------
lambda <- 2  # 真实值
sample_sizes <- c(5, 10, 20)  # 样本大小 n
B <- 1000  # 自助重复次数
m <- 1000  # 模拟重复次数
# 初始化存储平均偏差的矩阵
mean_bias <- matrix(0, nrow = m, ncol = length(sample_sizes))  
# 初始化存储标准误的矩阵
std_error <- matrix(0, nrow = m, ncol = length(sample_sizes))  

for (i in 1:m) {
  for (j in 1:length(sample_sizes)) {
    n <- sample_sizes[j]  # 当前样本大小
    X <- rexp(n, rate = lambda)  # 生成指数分布样本
    X_bar <- mean(X)  # 计算样本均值

    # 使用自助法进行重采样，并计算每次重采样的MLE
    bootstrap_estimates <- replicate(B, 1/mean(sample(X, replace = TRUE)))
    # 计算平均偏差
    mean_bias[i, j] <- mean(bootstrap_estimates) - lambda/(n-1)  
    # 计算标准误差
    std_error[i, j] <- sqrt(n-2) * mean(bootstrap_estimates) * (n/(n-1))  
  }
}

mean_bias_theoretical <- lambda/(sample_sizes - 1)  # 理论偏差
std_error_theoretical <- lambda * sample_sizes / ((sample_sizes - 1) * sqrt(sample_sizes - 2))  # 理论标准误差

mean_bias_mean <- apply(mean_bias, 2, mean)  # 平均偏差的均值
std_error_mean <- apply(std_error, 2, mean)  # 标准误的均值

mean_bias_theoretical  # 输出理论偏差
std_error_theoretical  # 输出理论标准误差
mean_bias_mean  # 输出自助法的平均偏差
std_error_mean  # 输出自助法的平均标准误差


## ----include = TRUE-----------------------------------------------------------
library("bootstrap") # 加载bootstrap包

# 设置bootstrap
B <- 200 # 自助法重复次数
n <- nrow(law) # 数据集的观测数

theta.hat <- cor(law$LSAT, law$GPA) # 原始样本的相关系数估计值
theta.hats.b <- numeric(B) # 储存自助法样本相关系数估计值的向量

ts <- numeric(B)  # 存储t统计量的向量

# 自助法估计的R的标准差 
for (b in 1:B) {
  # 从原始样本中有放回地抽取自助样本
  i <- sample(x = 1:n, size = n, replace = TRUE)  
  # 根据自助样本索引获取自助样本
  law.b = law[i,] 
   # 计算自助样本的相关系数估计值
  theta.hats.b[b] <- cor(law.b$LSAT, law.b$GPA) 
  sd.theta.hats.b <- numeric(B)
  
  for(b2 in 1:B) {
    i2 <- sample(x = 1:n, size = n, replace = TRUE)
    law.b2 <- law.b[i2,]
    sd.theta.hats.b[b2] <- cor(law.b2$LSAT, law.b2$GPA)
  }
  
  se.b <- sd(sd.theta.hats.b)
  
  ts[b] <- (theta.hats.b[b] - theta.hat) / se.b
}

alpha <- 0.05 # 置信水平
ts.ordered <- sort(ts) # 对t统计量进行排序
# 计算置信区间的上下界
qs <- quantile(ts.ordered, probs = c(alpha/2, 1-alpha/2)) 
# 原始样本相关系数估计值的标准差
se.hat <- sd(theta.hats.b)  
# 置信区间估计
CI <- c(theta.hat - qs[2]*se.hat, theta.hat - qs[1]*se.hat)  
print(CI)
hist(ts, breaks = 100, xlim = c(-5, 10))

## ----include=TRUE-------------------------------------------------------------
library(boot)
hours = aircondit$hours
n = length(hours)
B = 200

#  MLE 
mle.lambda = function(values) {
  return(length(values) / sum(values))
}


# 初始化保存自助法估计结果的向量
time.b = numeric(B)
ts = numeric(B)

# 计算 lambda 的 MLE 估计值
time.hat = 1 / mle.lambda(hours)

for (b in 1:B) {
  # 从原始数据中进行有放回抽样
  i = sample(1:n, n, replace = TRUE)
  hours.b = hours[i]
  
  # 计算抽样数据的 MLE 估计值
  time.b[b] = 1 / mle.lambda(hours.b)
  
  times.b2 = numeric(B)
  
  # 计算用于后续计算的自助法估计值
  for (b2 in 1:B) {
    i2 = sample(1:n, n, replace = TRUE)
    hours.b2 = hours.b[i2]
    times.b2[b2] = 1 / mle.lambda(hours.b2)
  }
  
  ts[b] = (time.b[b] - time.hat) / sd(times.b2)
}

# 计算估计值的标准误差
se.hat = sd(time.b)
alpha = 0.05
q.probs = c(alpha/2, 1 - alpha/2)

# 定义函数用于设置置信区间的名称
setCINames = function(object) {
  return(setNames(object, c(paste((alpha/2) * 100, '%'), paste((1 - alpha/2) * 100, '%'))))
}

# 标准正态分布置信区间
q = qnorm(1 - alpha/2)
ci.sn = time.hat + c(-1, 1) * q * se.hat
ci.sn = setCINames(ci.sn)

# 基本自助法置信区间
qs.time.hat = quantile(x = time.b, p = q.probs)
ci.basic = rev(2 * time.hat - qs.time.hat)
ci.basic = setCINames(ci.basic)

# 百分位自助法置信区间
ci.percentile = qs.time.hat

# 自助法 t 置信区间
qs.t = quantile(x = ts, p = q.probs)
ci.t = setCINames(rev(time.hat - qs.t * se.hat))

# 输出置信区间
confidence_intervals <- data.frame(Method = c("Standard Normal", "Basic Bootstrap", "Percentile Bootstrap", "Bootstrap t"),
                                   Lower = c(ci.sn[1], ci.basic[1], ci.percentile[1], ci.t[1]),
                                   Upper = c(ci.sn[2], ci.basic[2], ci.percentile[2], ci.t[2]))

print(confidence_intervals)

## ----include=TRUE-------------------------------------------------------------
library("bootstrap")
data("scor")

# 计算 theta 的函数
theta <- function(x) {
  sigma <- cov(x)
  pca.sigma <- prcomp(sigma)
  theta <- pca.sigma$sdev[1] / sum(pca.sigma$sdev)
  return(theta)
}

# 计算 theta.hat
n <- length(scor$mec)
values <- eigen(cov(scor))$values
theta.hat <- values[1] / sum(values)

# 计算theta.jackknife
theta.jack <- numeric(n)

for (i in 1:n) {
  Scor <- scor[-i, ]
  val <- eigen(cov(Scor))$values
  theta.jack[i] <- val[1] / sum(val)
}

# 计算 jackknife 估计的偏差和标准差
bias <- (n - 1) * (mean(theta.jack) - theta.hat)
se.jack <- sqrt((n - 1) * mean((theta.jack - theta.hat)^2))

c(bias, se.jack)

## ----include = TRUE-----------------------------------------------------------
library(DAAG, quietly=TRUE)

# 加载数据集
data(ironslag)

# 将 x 和 y 转换为数据框形式
x <- data.frame(chemical = ironslag$chemical)
y <- data.frame(magnetic = ironslag$magnetic)

# 初始化向量以存储每个模型的均方误差（MSE）
mse_linear <- numeric(nrow(ironslag))
mse_quadratic <- numeric(nrow(ironslag))
mse_exponential <- numeric(nrow(ironslag))
mse_loglog <- numeric(nrow(ironslag))

# 进行交叉验证
for (k1 in 1:(nrow(ironslag)-1)) {
  for (k2 in (k1+1):nrow(ironslag)) {
    # 留出观测 k1 和 k2
    train <- data.frame(x = x[-c(k1, k2), ], y = y[-c(k1, k2), ])
    test <- data.frame(x = x[c(k1, k2), ], y = y[c(k1, k2), ])
    
    ### 模型 1: 线性模型 ###
    model_linear <- lm(y ~ x, data = train)
    prediction_linear <- predict(model_linear, newdata = test)
    mse_linear[k1] <- mse_linear[k1] + mean((test$y - prediction_linear)^2)
    
    ### 模型 2: 二次模型 ###
    model_quadratic <- lm(y ~ x + I(x^2), data = train)
    prediction_quadratic <- predict(model_quadratic, newdata = test)
    mse_quadratic[k1] <- mse_quadratic[k1] + mean((test$y - prediction_quadratic)^2)
    
    ### 模型 3: 指数模型 ###
    model_exponential <- lm(log(y) ~ x, data = train)
    prediction_exponential <- exp(predict(model_exponential, newdata = test))
    mse_exponential[k1] <- mse_exponential[k1] + mean((test$y - prediction_exponential)^2)
    
    ### 模型 4: 对数-对数模型 ###
    model_loglog <- lm(log(y) ~ log(x), data = train)
    prediction_loglog <- exp(predict(model_loglog, newdata = test))
    mse_loglog[k1] <- mse_loglog[k1] + mean((test$y - prediction_loglog)^2)
  }
}

# 计算每个模型的平均 MSE
avg_mse_linear <- mean(mse_linear)
avg_mse_quadratic <- mean(mse_quadratic)
avg_mse_exponential <- mean(mse_exponential)
avg_mse_loglog <- mean(mse_loglog)

# 打印每个模型的平均 MSE
cat("线性模型 MSE:", avg_mse_linear, "\n")
cat("二次模型 MSE:", avg_mse_quadratic, "\n")
cat("指数模型 MSE:", avg_mse_exponential, "\n")
cat("对数-对数模型 MSE:", avg_mse_loglog, "\n")

## ----echo=FALSE---------------------------------------------------------------
model_quadratic
plot(model_quadratic)

## ----include=TRUE-------------------------------------------------------------
attach(chickwts) # 将 chickwts 数据集附加到当前环境中

# 分别提取使用两种饲料类型喂食的鸡的体重并排序
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"])) 

detach(chickwts) # 分离 chickwts 数据集

library(cramer) # 加载 cramer 包

rep <- 1000 # 设置重复次数

z <- c(x, y) # 汇集样本
n1 <- length(x)
n2 <- length(y)
n <- n1 + n2

reps <- numeric(rep) # 存储模拟统计量

T.hat <-  cramer.test(x, y)$statistic

# 进行模拟
for (i in 1:rep) {
  # 从z中无放回地随机抽取n1个样本作为一组
  k <- sample(1:n, n1, replace = FALSE)
  z1 <- z[k]
  z2 <- z[-k]
  reps[i] <- cramer.test(z1, z2)$statistic
}

p.hat <-  mean(abs(T.hat) < abs(reps))
p.hat

## ----include=TRUE-------------------------------------------------------------
hist(reps, main = "", freq = FALSE, xlab = "T (P = 0.75)", breaks = "scott")
points(T.hat, 0, cex = 1, pch = 16)

## ----include=TRUE-------------------------------------------------------------
n1 <- 100
n2 <- 200

# 返回方差不相等的概率，其中 H0 表示方差相等（即 sd1 = sd2）
test.equal.variance <- function(xs1, xs2) {
  # 将 xs1 设置为较小的样本，xs2 设置为较大的样本
  if (length(xs2) < length(xs1)) {
    tmp <- xs1
    xs1 <- xs2
    xs2 <- tmp
  }
  n1 <- length(xs1)
  n2 <- length(xs2)
  
  y <- c(xs1, xs2)
  
  # 如果一个样本中有五个以上的值超出另一个样本的范围，则返回 1。
  count5 <- function(x1, x2) {
    stopifnot(length(x1) == length(x2))
    extr1 <- sum((x1 < min(x2))) + sum((x1 > max(x2)))
    extr2 <- sum((x2 < min(x1))) + sum((x2 > max(x1)))
    
    out <- max(extr1, extr2)
    return(as.integer(out > 5))
  }
  
  # 运行置换测试。
  # 如果 H0 成立，count 5 在从 y 中随机选取的 "大多数"（取决于所需的 p 值）大小相等的样本对中应返回 0。
  rep <- 1000
  n.min <- n1
  n <- length(y)
  c5s <- numeric(rep)
  size <- floor(n / 2)
  
  for (i in 1:rep) {
    k <- sample(1:n, size, replace = FALSE)
    y1 <- y[k]
    y2 <- y[-k]
    # 计算 count 5 测试返回 1 的次数（即方差不同）
    c5s[i] <- count5(y1, y2)
  }
  
  # 绘制 count 5 结果的直方图
  hist(c5s, probability = TRUE)
  
  # 根据 count 5 测试计算方差不同的频率
  return(mean(c5s))
}

mean <- 0
# 使用不同标准差的随机正态分布样本进行三次测试，检验算法的效果。
p_value1 <- test.equal.variance(rnorm(n1, mean, 1), rnorm(n2, mean, 1))
p_value2 <- test.equal.variance(rnorm(n1, mean, 10), rnorm(n2, mean, 1))
p_value3 <- test.equal.variance(rnorm(n1, mean, 100), rnorm(n2, mean, 1))

print(p_value1)
print(p_value2)
print(p_value3)

## ----include=TRUE-------------------------------------------------------------
# 定义计算a的函数
calculate_a <- function(N, b1, b2, b3, f0) {
  # 生成 X1, X2, X3
  X1 <- rpois(1, 1)
  X2 <- rexp(1, 1)
  X3 <- rbinom(1, N, 0.5)
  
  # 计算 a
  a <- log(f0) - log(1 - f0) - b1 * X1 - b2 * X2 - b3 * X3
  
  return(a)
}

N <- 10^6
b1 <- 0
b2 <- 1
b3 <- -1
f0_values <- c(0.1, 0.01, 0.001, 0.0001)

# 计算a的结果
a_values <- calculate_a(N, b1, b2, b3, f0_values)

# 绘制结果图表
plot(-log(f0_values), a_values, type = "b", col = "blue",
     xlab = "-log f0", ylab = "a", main = "-log f0 vs a")

## ----include=TRUE-------------------------------------------------------------
# 定义标准拉普拉斯分布的概率密度函数
laplace_pdf <- function(x) {
  return(0.5 * exp(-abs(x)))
}

# 实现随机行走Metropolis采样器
random_walk_metropolis <- function(N, sigma) {
  # 初始化采样链和接受计数
  chain <- numeric(N)
  accept_count <- 0
  
  # 设置初始状态
  x <- 0
  
  # 进行采样
  for (i in 1:N) {
    # 从提议分布中抽取增量
    increment <- rnorm(1, 0, sigma)
    
    # 计算接受率
    alpha <- min(1, laplace_pdf(x + increment) / laplace_pdf(x))
    
    # 决定是否接受增量
    if (runif(1) < alpha) {
      x <- x + increment
      accept_count <- accept_count + 1
    }
    
    # 记录当前状态到采样链中
    chain[i] <- x
  }
  
  # 计算接受率
  acceptance_rate <- accept_count / N
  
  return(list(chain = chain, acceptance_rate = acceptance_rate))
}

# 设置参数
N <- 10000
sigma_values <- c(0.1, 0.5, 1, 2)

# 生成不同方差下的链并计算接受率
results <- list()
for (sigma in sigma_values) {
  result <- random_walk_metropolis(N, sigma)
  results[[as.character(sigma)]] <- result
}

# 输出接受率
for (sigma in sigma_values) {
  acceptance_rate <- results[[as.character(sigma)]]$acceptance_rate
  cat("Acceptance rate for sigma =", sigma, ":", acceptance_rate, "\n")
}

# 绘制不同方差下的链
for (sigma in sigma_values) {
  chain <- results[[as.character(sigma)]]$chain
  plot(chain, type = "l", main = paste("Chain (sigma =", sigma, ")"))
}

## ----include=TRUE-------------------------------------------------------------
m = 5000
burn = 1000

x = matrix(0, m, 2)

rho = 0.9
mu1 = 0
mu2 = 0
sigma1 = 1
sigma2 = 1
s1 = sqrt(1-rho^2)*sigma1
s2 = sqrt(1-rho^2)*sigma2

mean12 = function (x2) mu1 + rho*sigma1/sigma2*(x2 - mu2)
mean21 = function (x1) mu2 + rho*sigma2/sigma1*(x1 - mu1)

x[1,] = c(mu1, mu2)

for (i in 2:m) {
  xt = x[i-1,]
  xt[1] = rnorm(1, mean12(xt[2]), s1)
  xt[2] = rnorm(1, mean21(xt[1]), s2)
  x[i,] = xt
}

x = x[(burn+1):m,]

x = data.frame(x)
lin.reg = lm(X1 ~ X2, data = x)

par(mfrow=c(1,2))
plot(x, cex = 0.5, main = "generated data")
hist(lin.reg$residuals, main = "residuals of linear model")
par(mfrow=c(1,1))

## ----include=TRUE-------------------------------------------------------------
# 定义瑞利分布的概率密度函数
rayleigh_pdf <- function(x, sigma) {
  if (any(x < 0)) return(0)
  stopifnot(sigma > 0)
  return(x / sigma^2 * exp(-x^2 / (2 * sigma^2)))
}

# Metropolis-Hastings采样算法
metropolis_hastings_sampler <- function(n, sigma, initial_value) {
  samples <- numeric(n)
  x <- initial_value
  
  for (i in 1:n) {
    # 从建议分布中生成新样本
    y <- rnorm(1, mean = x, sd = 1)
    
    # 计算接受比率
    alpha <- rayleigh_pdf(y, sigma) / rayleigh_pdf(x, sigma)
    
    # 决定是否接受新样本
    u <- runif(1)
    if (u <= alpha) {
      x <- y
    }
    
    # 保存样本值
    samples[i] <- x
  }
  
  return(samples)
}

# 计算Gelman-Rubin统计量
gelman_rubin <- function(chains) {
  m <- length(chains)  # 链的数量
  n <- min(sapply(chains, length))  # 所有链中的最小长度
  
  # 截取所有链到相同的长度
  chains <- lapply(chains, function(x) x[1:n])
  
  # 计算每条链的均值
  chain_means <- sapply(chains, mean)
  
  # 计算所有链的均值
  all_means <- colMeans(do.call(rbind, chains))
  
  # 计算B
  B <- n * var(chain_means)
  
  # 计算W
  W <- mean(sapply(chains, var))
  
  # 计算var_hat
  var_hat <- ((n - 1) / n) * W + (1 / n) * B
  
  # 计算Gelman-Rubin统计量
  R_hat <- sqrt(var_hat / W)
  
  return(R_hat)
}

# 设置参数
sigma <- 4  # 瑞利分布的尺度参数
initial_value <- 1  # 初始样本值
target_R_hat <- 1.2  # 目标Gelman-Rubin统计量

# 初始化链
chains <- list()

while (TRUE) {
  # 生成样本
  samples <- metropolis_hastings_sampler(n = 10000, sigma = sigma, initial_value = initial_value)
  
  # 将样本添加到链中
  chains <- c(chains, list(samples))
  
  # 计算Gelman-Rubin统计量
  R_hat <- gelman_rubin(chains)
  
  # 检查是否达到收敛条件
  if (!is.na(R_hat) && R_hat < target_R_hat) {
    break
  }
  
  # 更新初始样本值
  initial_value <- samples[length(samples)]
}

# 打印最终的Gelman-Rubin统计量
print(paste("Final R_hat:", R_hat))

## ----include=TRUE-------------------------------------------------------------
# 观测数据
data <- matrix(c(11, 12,
                 8, 9,
                 27, 28,
                 13, 14,
                 16, 17,
                 0, 1,
                 23, 24,
                 10, 11,
                 24, 25,
                 2, 3), ncol = 2, byrow = TRUE)

# 极大化观测数据的似然函数
mlogL <- function(lambda) {
  u <- data[, 1]
  v <- data[, 2]
  
  # 计算似然函数的对数值
  log_likelihood <- sum(log(exp(-lambda * u) - exp(-lambda * v)))
  
  return(-log_likelihood) # 使用负对数似然函数进行最小化
}

# 使用optim函数进行最大化
fit <- optim(par = 1, fn = mlogL, method = "Brent", lower = 0, upper = 10)

# 输出结果
cat("直接极大似然估计结果：", fit$par, "\n")

# EM算法求解MLE
em_algorithm <- function(data, max_iter = 10000, tol = 1e-6) {
  n <- nrow(data)
  lambda <- 1
  
  for (iter in 1:max_iter) {
    lambda_prev <- lambda
    
    # E步
    z <- (exp(-lambda * data[, 1]) - exp(-lambda * data[, 2])) / (exp(-lambda * data[, 1]) - exp(-lambda * data[, 2]))
    
    # M步
    lambda <- optimize(f = function(x) {
      sum(z * log(1 / x) + (1 - z) * log(-exp(-x * data[, 2]) + exp(-x * data[, 1])))
    }, interval = c(0, 10), maximum = FALSE)$minimum
    
    if (abs(lambda - lambda_prev) < tol) {
      break
    }
  }
  
  return(lambda)
}

# 使用EM算法求解
result_em <- em_algorithm(data)

# 输出结果
cat("EM算法估计结果：", result_em, "\n")

## ----include=TRUE-------------------------------------------------------------
library(boot)
# 创建支付矩阵A
A <- matrix(c( 0,-2,-2,3,0,0,4,0,0,
               2,0,0,0,-3,-3,4,0,0,
               2,0,0,3,0,0,0,-4,-4,
               -3,0,-3,0,4,0,0,5,0,
               0,3,0,-4,0,-4,0,5,0,
               0,3,0,0,4,0,-5,0,-5,
               -4,-4,0,0,0,5,0,0,6,
               0,0,4,-5,-5,0,0,0,6,
               0,0,4,0,0,5,-6,-6,0), 9, 9)
B <- A + 2

solve.game <- function(A){
  # 使用单纯形法解决两个玩家的零和博弈
  # 先优化玩家1，在优化玩家2
  # 最大化v，服从...
  
  # 预处理 
  min.A <- min(A)
  A <- A - min.A # v >= 0
  max.A <- max(A)
  A <- A / max(A)
  
  m <- nrow(A)
  n <- ncol(A)
  it <- n^3
  
  # 玩家1的优化
  a <- c(rep(0, m), 1)  # 目标函数
  A1 <- -cbind(t(A), rep(-1, n)) # <=约束
  b1 <- rep(0, n) # 约束条件右侧常数
  A3 <- t(as.matrix(c(rep(1, m), 0))) # 等式约束 sum(x) = 1
  b3 <- 1
  sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3, maxi=TRUE, n.iter=it)
  
  # 玩家2的优化
  a <- c(rep(0, n), 1)  # 目标函数
  A1 <- cbind(A, rep(-1, m))  # <= 约束
  b1 <- rep(0, m)  # 约束条件右侧常数
  A3 <- t(as.matrix(c(rep(1, n), 0)))  # 等式约束 sum(y) = 1
  b3 <- 1
  sy <- simplex(a = a, A1 = A1, b1 = b1, A3 = A3, b3 = b3, maxi = FALSE, n.iter = it)
  
  # 构造结果
  soln <- list(
    "A" = A * max.A + min.A,  # 恢复原始范围的支付矩阵A
    "x" = sx$soln[1:m],  # 玩家1的策略向量x
    "y" = sy$soln[1:n],  # 玩家2的策略向量y
    "v" = sx$soln[m + 1] * max.A + min.A,  # 博弈的值v
    "optimal_point" = cbind(sx$soln[1:m], sy$soln[1:n])  # 极值点
  )
  
  return(soln)
}

soln_A <- solve.game(A)
soln_B <- solve.game(B)
round(soln_A$optimal_point, 7)
round(soln_A$v, 7)
round(soln_B$optimal_point, 7)
round(soln_B$v, 7)

## ----include=TRUE-------------------------------------------------------------
list <- list(1, 2, list(3, 4))
as.vector(list)
unlist(list)

## ----include=TRUE-------------------------------------------------------------
dim(c(1, 2, 3))

## ----include=TRUE-------------------------------------------------------------
# 创建一个矩阵
mat <- matrix(1:9, nrow = 3, ncol = 3)
print(mat)

# 检查是否为矩阵
is_mat <- is.matrix(mat)
print(is_mat)

# 检查是否为数组
is_arr <- is.array(mat)
print(is_arr)

## ----include=TRUE-------------------------------------------------------------
frame1 = data.frame(col1=character(0),col2=numeric(0),col3=logical(0))
str(frame1)
frame1

## ----include=TRUE-------------------------------------------------------------
frame2 <- data.frame(matrix(nrow = 3, ncol = 0))
str(frame2)
frame2

## ----include=TRUE-------------------------------------------------------------
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE) 
  (x - rng[1]) / (rng[2] - rng[1])
  }

## ----include=TRUE-------------------------------------------------------------
# 定义函数
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# 创建一个示例数据框
df <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 6),
  C = c(7, 8, 9)
)

# 对数据框的每一列应用scale01函数
scaled_df1 <- lapply(df, scale01)

# 仅对数值列应用scale01函数
numeric_cols <- df[sapply(df, is.numeric)]  # 选择只包含数值的列
scaled_df2 <- lapply(numeric_cols, scale01)  # 对数值列应用函数

scaled_df1
scaled_df2

## ----include=TRUE-------------------------------------------------------------
# 计算数值数据框中每一列的标准差
numeric_iris <- iris[-5]
vapply(numeric_iris, sd, numeric(1))

# 计算混合数据框中每个数值列的标准差
df_sd <- function(df) vapply(df[vapply(df, is.numeric, logical(1))], sd, numeric(1))
df_sd(iris)
df_sd(mtcars)

## -----------------------------------------------------------------------------

GibbsR <- function(a,b,n,N){
  X = matrix(0,N,2)
  X[1,] = c(0,0.5)
  for (i in 2:N){
    y = X[i-1,2]
    X[i,1] = rbinom(1,n,y)
    x = X[i,1]
    X[i,2] = rbeta(1,x+a,n-x+b)
  }
  return(X)
}

n <- 100
a <- 30
b <- 60
N <- 1000

X = GibbsR(a,b,n,N)
plot(X[,1],X[,2])

## -----------------------------------------------------------------------------
# Rcpp

sourceCpp(code='
#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix GibbsC(double a, double b, int n, int N) {
  NumericMatrix X(N,2);
  double x,y;
  X(0,0)=0;
  X(0,1)=0.5;
  for(int i=1;i<N;i++){
    y = X(i-1,1);
    X(i,0) = rbinom(1,n,y)[0];
    x = X(i,0);
    X(i,1) = rbeta(1,x+a,n-x+b)[0];
  }
  return X;
}
')
XC = GibbsC(a,b,n,N)
plot(XC[,1],XC[,2])

## -----------------------------------------------------------------------------
ts <- microbenchmark(GibbsR=GibbsR(a,b,n,N),GibbsC=GibbsC(a,b,n,N))
summary(ts)[,c(1,3,5,6)]

