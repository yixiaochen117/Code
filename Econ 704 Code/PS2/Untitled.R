# 加载必要的库
library(plm)
library(lmtest)
library(sandwich)

# 读取数据
rental_data <- read.csv("rental.csv")

# (a) 混合OLS
pooled_model <- lm(lrent ~ y90 + lpop + lavginc + pctstu, data = rental_data)
summary(pooled_model)

# 同方差假设下的标准误
coef_homosked <- coeftest(pooled_model)
cat("Pooled OLS Results (Homoskedastic SE):\n")
print(coef_homosked)

# 异方差稳健标准误
coef_heterosked <- coeftest(pooled_model, vcov = vcovHC(pooled_model, type = "HC1"))
cat("\nPooled OLS Results (Heteroskedastic-robust SE):\n")
print(coef_heterosked)

# (b) 固定效应模型
fe_model <- plm(lrent ~ y90 + lpop + lavginc + pctstu, 
                data = rental_data,
                index = c("city"),
                model = "within")

# 简单固定效应结果
summary(fe_model)

# 在城市层面上聚类的标准误
fe_cluster <- coeftest(fe_model, vcov = vcovHC(fe_model, type = "HC1", cluster = "group"))
cat("\nFixed Effects Results (clustered SE):\n")
print(fe_cluster)

# (d) 同时包含城市和年份固定效应的模型
both_fe_model <- plm(lrent ~ lpop + lavginc + pctstu, 
                     data = rental_data,
                     index = c("city", "year"),
                     model = "within",
                     effect = "twoways")

# 双向固定效应结果
cat("\nBoth City and Year Fixed Effects Results:\n")
summary(both_fe_model)

# 正确比较(b)和(d)的系数:
fe_coefs <- fe_model$coefficients[c("lpop", "lavginc", "pctstu")]
both_fe_coefs <- both_fe_model$coefficients

comparison_df <- data.frame(
  Coefficient = c("lpop", "lavginc", "pctstu"),
  FE_Model = fe_coefs,
  Both_FE_Model = both_fe_coefs
)

print(comparison_df)

# 输出解释分析结果
cat("\n分析结果解释:\n")
cat("1. Pooled OLS中，y90的系数为", round(pooled_model$coefficients["y90"], 4), 
    "，表示从1980年到1990年租金平均增加了", 
    round((exp(pooled_model$coefficients["y90"])-1)*100, 2), "%\n")

cat("2. Pooled OLS中，pctstu的系数为", round(pooled_model$coefficients["pctstu"], 4), 
    "，表示学生比例每增加1个百分点，租金平均增加", 
    round((exp(pooled_model$coefficients["pctstu"])-1)*100, 2), "%\n")

cat("3. 固定效应模型中，y90的系数为", round(fe_model$coefficients["y90"], 4), 
    "，表示控制城市特定效应后，从1980年到1990年租金平均增加了", 
    round((exp(fe_model$coefficients["y90"])-1)*100, 2), "%\n")

cat("4. 固定效应模型中，pctstu的系数为", round(fe_model$coefficients["pctstu"], 4), 
    "，表示控制城市特定效应后，学生比例每增加1个百分点，租金平均增加", 
    round((exp(fe_model$coefficients["pctstu"])-1)*100, 2), "%\n")

cat("5. 双向固定效应模型与带有时间虚拟变量的城市固定效应模型在lpop、lavginc和pctstu的系数上完全一致，证明在两期面板数据中，这两种方法是等效的\n")