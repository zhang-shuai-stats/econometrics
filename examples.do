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