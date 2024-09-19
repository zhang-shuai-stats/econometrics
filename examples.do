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