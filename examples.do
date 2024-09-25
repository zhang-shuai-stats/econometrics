clear 
cd /Users/zhangshuai/Desktop/econometrics/Data // 修改默认目录

*————————————————————————————————————————————————————————————————————————————————————
* chapter 2
*————————————————————————————————————————————————————————————————————————————————————
* 2.3
use CEOSAL1, clear
keep salary roe
reg salary roe

* 2.4
use WAGE1, clear
reg wage educ

* 2.5 
use VOTE1, clear 
reg voteA shareA

* 2.6
use CEOSAL1, clear
keep roe salary

reg salary roe
predict salaryhat, xb
gen uhat = salary - salaryhat

* 2.10
use WAGE1, clear
gen log_wage = log(wage)
reg log_wage educ

* 2.11
use CEOSAL1, clear
gen log_salary = log(salary)
gen log_sales = log(sales)
reg log_salary log_sales

2.12
use MEAP93, clear
reg math10 lnchprg

*————————————————————————————————————————————————————————————————————————————————————
* chapter 3
*————————————————————————————————————————————————————————————————————————————————————
* 3.1
use GPA1, clear
reg colGPA hsGPA ACT

* 3.2
use WAGE1, clear
gen log_wage = log(wage)
reg log_wage educ exper tenure

* 3.3 
use 401K, clear
reg prate mrate age
ereturn list 
local beta1 = _b[mrate]
local beta2 = _b[age]
di "`beta1' `beta2'"

* 如果省略age
reg prate mrate
local beta1_tilde = _b[mrate]
di "`beta1_tilde'"

reg age mrate
local delta = _b[mrate]
di "`delta'"

local right = `beta1' + `beta2'*`delta'
di "`beta1_tilde' `right'"

* 3.4 
use GPA1, clear
reg colGPA hsGPA ACT

* 3.5 
use CRIME1, clear
table narr86, stat(frequency) stat(percent)
reg narr86 pcnv ptime86 qemp86
reg narr86 pcnv avgsen ptime86 qemp86

* 3.6
use WAGE1, clear
gen log_wage = log(wage)
reg log_wage educ

*————————————————————————————————————————————————————————————————————————————————————
* chapter 6
*————————————————————————————————————————————————————————————————————————————————————
* 表 6-1
use BWGHT, clear 

* 使用collect命令收集回归结果，并输出最终表格
collect clear 
collect: reg bwght cigs faminc
collect: reg bwghtlbs cigs faminc
collect: reg bwght packs faminc 

collect dims
collect levelsof colname
collect levelsof result
collect layout (colname[cigs packs faminc _cons]#result[_r_b _r_se] result[N rss rmse]) (cmdset)

* 行名称
collect style header colname, level(value)
collect style header colname[_cons], level(label)
collect style header result[_r_b _r_se], level(hide)
collect label levels result N "观测次数" r2 "R2" rss "SSR" rmse "SER", modify

* 列名称
collect label levels cmdset 1 "(1) bwght" 2 "(2) bwghtlbs" 3 "(3) bwght", modify

* 其他格式
collect style cell result[_r_b]#result[_r_se], warn nformat(%9.4f)
collect style cell result[_r_se], warn sformat("(%s)")

collect layout (colname[cigs packs faminc _cons]#result[_r_b _r_se] result[N r2 rss rmse]) (cmdset)

* 例 6.5
use GPA2, clear 
reg colgpa sat hsperc hsize hsizesq

gen sat0 = sat - 1200
gen hsperc0 = hsperc - 30
gen hsize0 = hsize - 5
gen hsizesq0 = hsizesq - 25
reg colgpa sat0 hsperc0 hsize0 hsizesq0

* 例 6.7
use CEOSAL2, clear 
reg lsalary lsales lmktval ceoten
predict uhat, residual  // 残差
gen exp_uhat = exp(uhat)
sum exp_uhat
local alpha0 = r(mean)  // exp(uhat)的均值
di "`alpha0'"
local lsalary_hat = _b[_cons] + _b[lsales] * log(5000) + _b[lmktval] * log(10000) + _b[ceoten]*10 
di "`lsalary_hat'"
local salary_hat = `alpha0' * exp(`lsalary_hat')
di "`salary_hat'"

*————————————————————————————————————————————————————————————————————————————————————
* chapter 7
*————————————————————————————————————————————————————————————————————————————————————
* 例 7.1
use WAGE1, clear 
reg wage female educ exper tenure
reg wage female
reg educ female // 验证协变量之间的关系
reg exper female 
reg tenure female

* 例 7.6
use WAGE1, clear 
gen marrmale = married * (1 - female)
gen marrfem = married * female
gen singfem = (1 - married)* female
reg lwage marrmale marrfem singfem educ exper expersq tenure tenursq
* 一种更快捷的方式
reg lwage i.female#i.married educ exper expersq tenure tenursq

* p200 （7.33）
use JTRAIN, clear 
reg lscrap grant lsales lemploy if year == 1988

* p201 （7.35）
use FERTIL2, clear 
reg children age educ 

*————————————————————————————————————————————————————————————————————————————————————
* chapter 8
*————————————————————————————————————————————————————————————————————————————————————
* 例 8.1
use WAGE1, clear 
gen marrmale = married * (1 - female)
gen marrfem = married * female
gen singfem = (1 - married)* female
* 普通回归
reg lwage marrmale marrfem singfem educ exper expersq tenure tenursq
* 异方差-文件标准误
reg lwage marrmale marrfem singfem educ exper expersq tenure tenursq, robust 


* 例 8.6
use 401KSUBS, clear 
keep if fsize == 1 
gen age25 = (age - 25)^2

collect clear 
collect: reg nettfa inc  // ols
collect: reg nettfa inc [aweight=1/inc]
collect: reg nettfa inc age25 male e401k
collect: reg nettfa inc age25 male e401k [aweight=1/inc]

collect dims
collect layout (colname#result[_r_b _r_se] result[N r2]) (cmdset)

* 行名称
collect style header colname, level(value)
collect style header colname[_cons], level(label)
collect style header result[_r_b _r_se], level(hide)
collect label levels result N "观测次数" r2 "R2", modify

* 列名称
collect label levels cmdset 1 "(1) OLS" 2 "(2) WLS" 3 "(3) OLS" 4 "(4) WLS", modify

* 其他格式
collect style cell result[_r_b _r_se r2], warn nformat(%9.3f)
collect style cell result[_r_se], warn sformat("(%s)")
collect layout (colname#result[_r_b _r_se] result[N r2]) (cmdset)

