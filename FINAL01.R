library(readr)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(gpairs)
library(corrplot)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, d3heatmap)
# german = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
# 
# colnames(german) = c("chk_acct", "duration", "credit_his", "purpose", 
#                             "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
#                             "present_resid", "property", "age", "other_install", "housing", "n_credits", 
#                             "job", "n_people", "telephone", "foreign", "response")
# 
# german$response = german$response - 1
# german$response <- as.factor(german$response) # 信用優劣，0代表good，1代表bad
# german <- read.table("german.txt")
# colnames(german) <- c("現有支票帳戶狀態", "月存續期間","信用紀錄","用途","信貸額度","存款","就業狀態","分期費率在可支配收入的百分比","個人狀態和性別","其他債務人和擔保人","目前居住時間","財產","年齡","其他分期付款計劃","住宅","在本銀行現有的信貸數量","工作","供養人數","手機","外籍勞工","客戶的信用優劣")
# 
# german$chk_acct <- rec(german$chk_acct, rec="A11=0DM以下; A12=0DM~200DM; A13=200DM以上; A14=無支票帳戶", as.num = F)
# german$credit_his <- rec(german$credit_his, rec="A30=無貸款記錄或所有貸款都及時償還; A31=這家銀行的所有貸款都及時償還; A32=現有貸款維持償還到現在; A33=過去有延遲償還過; A34=存在其他貸款", as.num = F)
# german$purpose <- rec(german$purpose, rec="A40=新車; A41=二手車; A42=家具/設備; A43=收音機/電視; A44=家用電器; A45=維修; A46=教育; A47=假期(不存在); A48=重新教育; A49=業務;A410=其他", as.num = F)
# german$saving_acct <- rec(german$saving_acct, rec="A61=100DM內; A62=100~500DM; A63=500~1000DM; A64=於1000DM以上; A65=未知/無存款", as.num = F)
# german$present_emp <- rec(german$present_emp , rec="A71=失業; A72=1年內; A73=1~4年; A74=4~7年; A75=7年以上", as.num = F)
# german$sex <- rec(german$sex , rec="A91=男(離異/分居); A92=女(離異/分居/已婚); A93=男(單身); A94=男(已婚/喪偶); A95=女(單身)", as.num = F)
# german$other_debtor <- rec(german$other_debtor , rec="A101=無; A102=共同申請人; A103=擔保人", as.num = F)
# german$property <- rec(german$property , rec="A121=房產; A122=儲蓄/壽險; A123=汽車或其他(不在6之內者); A124=未知/無財產", as.num = F)
# german$other_install<- rec(german$other_install , rec="A141=銀行; A142=商品; A143=無", as.num = F)
# german$housing <- rec(german$housing , rec="A151=租房; A152=自有; A153=免費租賃", as.num = F)
# german$job <- rec(german$job , rec="A171= 失業/不熟練且非本地居民; A172=不熟練的本地居民; A173=有技能的員工/公務員; A174=管理人員/自家經營者/高階僱員與職員", as.num = F)
# german$telephone <- rec(german$telephone , rec="A191= 無; A192=是,且以客戶名稱註冊", as.num = F)
# german$foreign <- rec(german$foreign , rec="A201= 是; A202=無", as.num = F)


# #save to rda file
# save(german, file = "germandata.rda")
load(file = "germandata.rda")


