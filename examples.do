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
* chapter 3
*————————————————————————————————————————————————————————————————————————————————————
*********
* 例 4.2
*********
use MEAP93, clear 
reg math10 totcomp staff enroll

*********
* 例 4.3
*********
use GPA1, clear 
reg colGPA hsGPA ACT skipped

***************
* 4.4小节的例子
***************
use TWOYEAR, clear

* (4.21)
reg lwage jc univ exper 

* (4.27)
reg lwage jc totcoll exper 

* 使用test直接检验系数的线性组合
reg lwage jc univ exper 
test jc-univ=0
local t = sqrt(r(F))
di "`t'"

***************
* 4.5小节的例子
***************
use MLB1, clear

* (4.31) 不受约束模型
reg lsalary years gamesyr bavg hrunsyr rbisyr 
ereturn list 
local SSR_ur = e(rss)
* (4.33) 受约束模型
reg lsalary years gamesyr 
local SSR_r = e(rss)
* F的计算
local F = ( (`SSR_r'-`SSR_ur')/3 ) / (`SSR_ur'/347 )
di "`F'"

global test = 3
di "$test"

* 使用test直接检验系数的线性组合
reg lsalary years gamesyr bavg hrunsyr rbisyr 
test bavg hrunsyr rbisyr


*————————————————————————————————————————————————————————————————————————————————————
* chapter 6
*————————————————————————————————————————————————————————————————————————————————————
*********
* 表 6-1
*********

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

*********
* 例 6.3
*********
use ATTEND, clear 
qui: sum priGPA
local avg = r(mean)
reg stndfnl atndrte priGPA ACT c.priGPA#c.priGPA c.ACT#c.ACT c.priGPA#c.atndrte
margins, dydx(atndrte) at(priGPA=`avg')

*********
* 例 6.5
*********
use GPA2, clear 
reg colgpa sat hsperc hsize hsizesq

gen sat0 = sat - 1200
gen hsperc0 = hsperc - 30
gen hsize0 = hsize - 5
gen hsizesq0 = hsizesq - 25
reg colgpa sat0 hsperc0 hsize0 hsizesq0

*********
* 例 6.7
*********
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
*********
* 例 7.1
*********
use WAGE1, clear 
reg wage female educ exper tenure
reg wage female
reg educ female // 验证协变量之间的关系
reg exper female 
reg tenure female

*********
* 例 7.2
*********
use GPA1, clear
reg colGPA PC hsGPA ACT

*********
* 例 7.3
*********
use JTRAIN, clear
reg hrsemp grant lsales lemploy if year == 1988

*********
* 例 7.4
*********
use HPRICE1, clear
reg lprice llotsize lsqrft bdrms colonial 

*********
* 例 7.5
*********
use WAGE1, clear 
reg lwage female educ exper c.exper#c.exper tenure c.tenure#c.tenure

*********
* 例 7.6
*********
use WAGE1, clear 
gen marrmale = married * (1 - female)
gen marrfem = married * female
gen singfem = (1 - married)* female
reg lwage marrmale marrfem singfem educ exper expersq tenure tenursq
* 一种更快捷的方式
reg lwage  i.female#i.married educ exper expersq tenure tenursq

* p200 （7.33）
use JTRAIN, clear 
reg lscrap grant lsales lemploy if year == 1988

* p201 （7.35）
use FERTIL2, clear 
reg children age educ 

***************
* logit模型的例子
***************
sysuse auto, clear
keep foreign weight mpg
logit foreign weight mpg
margins, dydx(weight mpg)

*————————————————————————————————————————————————————————————————————————————————————
* chapter 8
*————————————————————————————————————————————————————————————————————————————————————
*********
* 例 8.1
*********
use WAGE1, clear 
gen marrmale = married * (1 - female)
gen marrfem = married * female
gen singfem = (1 - married)* female
* 普通回归
reg lwage marrmale marrfem singfem educ exper expersq tenure tenursq
* 异方差-稳健标准误
reg lwage marrmale marrfem singfem educ exper expersq tenure tenursq, robust 

********
* 例 8.6
********
use 401KSUBS, clear 
keep if fsize == 1 
gen age25 = (age - 25)^2

collect clear 
collect: reg nettfa inc  // ols
collect: reg nettfa inc [aweight=1/inc]

gen inc_sq = inc^0.5
gen y_star = nettfa / inc_sq
gen x_star = inc/ inc_sq
gen cons = 1
gen cons_star = 1 / inc_sq
reg y_star x_star cons_star, noconstant

reg nettfa inc
reg nettfa inc cons, noconstant

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

*********
* 例 8.7 
*********
use SMOKE, clear 
reg cigs lincome lcigpric educ age agesq restaurn
estat hettest
estat hettest, rhs
estat hettest, rhs iid

* 书上bp检验的详细步骤： estat hettest, rhs iid
* （1）得到残差
reg cigs lincome lcigpric educ age agesq restaurn
predict uhat, residual
gen uhat2 = uhat^2
* (2) 辅助回归得到r2
reg uhat2 lincome lcigpric educ age agesq restaurn
scalar r2 = e(r2)
* (3) 构建lm统计量
scalar lm = e(N) * r2
scalar p = chi2tail(6,lm)
scalar list lm p

* stata上bpg检验的详细步骤: estat hettest
* （1）得到一个新统计量
reg cigs lincome lcigpric educ age agesq restaurn
scalar sig2 = e(rss)/e(N)
gen phat = uhat2/sig2
predict yhat, xb
* (2) 辅助回归得到ess
reg phat yhat
scalar ess = e(mss)
* (3) 构建lm统计量
scalar lm =  ess / 2
scalar p = chi2tail(1,lm)
scalar list lm p

* stata上bpg检验的详细步骤: estat hettest, rhs
* （1）得到一个新统计量phat，与上述步骤一致
* (2) 辅助回归得到ess
reg phat lincome lcigpric educ age agesq restaurn
scalar ess = e(mss)
* (3) 构建lm统计量
scalar lm =  ess / 2
scalar p = chi2tail(1,lm)
scalar list lm p

* 加权回归
gen log_uhat2 = log(uhat2)
reg log_uhat2 lincome lcigpric educ age agesq restaurn
predict gi, xb
gen hi = exp(gi)
reg cigs lincome lcigpric educ age agesq restaurn [aweight=1/hi]

********
* 表8-2
********
use 401KSUBS, clear 
keep if fsize == 1 
gen age25 = (age - 25)^2

collect clear 
collect: reg nettfa inc age25 male e401k [aweight=1/inc]
collect: reg nettfa inc age25 male e401k [aweight=1/inc], robust

collect dims
collect layout (colname#result[_r_b _r_se] result[N r2]) (cmdset)

* 行名称
collect style header colname, level(value)
collect style header colname[_cons], level(label)
collect style header result[_r_b _r_se], level(hide)
collect label levels result N "观测次数" r2 "R2", modify

* 列名称
collect label levels cmdset 1 "(1) 使用非稳健标准误" 2 "(2) 使用稳健标准误", modify

* 其他格式
collect style cell result[_r_b _r_se r2], warn nformat(%9.3f)
collect style cell result[_r_se], warn sformat("(%s)")
collect title "表8-2 nettfa方程的wls估计"

collect layout (colname#result[_r_b _r_se] result[N r2]) (cmdset)

********
* 例 8.9
********
use GPA1, clear
egen parcoll = anymatch(fathcoll mothcoll), values(1)
* OLS 
reg PC hsGPA ACT parcoll
estimates store OLS
reg PC hsGPA ACT parcoll, robust
estimates store OLS_Robust
predict yhat, xb

* WLS
gen hi = yhat*(1-yhat)
reg PC hsGPA ACT parcoll [aweight=1/hi]
estimates store WLS

etable, estimates(OLS OLS_Robust WLS) mstat(N) mstat(r2) column(estimates) novarlabel showstars

*————————————————————————————————————————————————————————————————————————————————————
* chapter 9
*————————————————————————————————————————————————————————————————————————————————————
*********
* 例 9.1
*********
use CRIME1, clear
reg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 black hispan
estimates store no_squared
reg narr86 pcnv pcnvsq avgsen tottime ptime86 pt86sq qemp86 inc86 inc86sq black hispan
estimates store squared 
test pcnvsq pt86sq inc86sq  // f检验

etable, estimates(no_squared squared) mstat(N) mstat(r2,nformat(%9.4f))  novarlabel showstars

*********
* 例 9.2
*********
use HPRICE1, clear

* 第一个方程
reg price lotsize sqrft bdrms
predict yhat, xb 
gen yhat2 = yhat^2 
gen yhat3 = yhat^3 
reg price lotsize sqrft bdrms yhat2 yhat3 
test yhat2 yhat3   

* 第二个方程对数方程
reg lprice llotsize lsqrft bdrms
predict lyhat, xb
gen lyhat2 = lyhat^2
gen lyhat3 = lyhat^3
reg lprice llotsize lsqrft bdrms lyhat2 lyhat3 
test lyhat2 lyhat3

* 还可以使用ovtest命令直接得出结果，但是需要添加四次项
reg price lotsize sqrft bdrms
estat ovtest

* ovtest的原理
gen yhat4 = yhat^4  //四次项
reg price lotsize sqrft bdrms yhat2 yhat3 yhat4
test yhat2 yhat3 yhat4  // 存在问题

* 自己计算
qui: reg price lotsize sqrft bdrms
local r_r = e(r2)
qui: reg price lotsize sqrft bdrms yhat2 yhat3 yhat4
local r_ur = e(r2)
local df = e(df_r)
local f = (`r_ur'-`r_r')/3/(1-`r_ur')*`df'
di "`f'"  // 与estat ovtest结果一致

*********
* 例 9.3
*********
use WAGE2, clear
reg lwage educ exper tenure married south urban black
estimates store c1 
reg lwage educ exper tenure married south urban black IQ 
estimates store c2 
reg lwage educ exper tenure married south urban black IQ c.edu#c.IQ
estimates store c3 

etable, estimates(c1 c2 c3) mstat(N) mstat(r2,nformat(%9.3f)) novarlabel ///
stars(0.10 "*" .05 "**" .01 "***", attach(_r_b))  showstars showstarsnote column(index) ///
export("test", as(xlsx) replace)

*********
* 例 9.4
*********
use CRIME2, clear
reg lcrmrte unem llawexpc lcrmrt_1
estimates store c2 
reg lcrmrte unem llawexpc if e(sample)
estimates store c1

etable, estimates(c1 c2 ) mstat(N) mstat(r2,nformat(%9.3f)) novarlabel ///
stars(0.10 "*" .05 "**" .01 "***", attach(_r_b))  showstars showstarsnote column(index)

*********
* 例 9.8
*********
use RDCHEM, clear
reg rdintens sales profmarg 
estimates store c1 
sum sales
reg rdintens sales profmarg if sales < 20000
estimates store c2
etable, estimates(c1 c2 ) cstat(_r_b, nformat(%9.6f)) cstat(_r_se, nformat(%9.6f)) mstat(N) mstat(r2,nformat(%9.3f)) novarlabel ///
stars(0.10 "*" .05 "**" .01 "***", attach(_r_b))  showstars showstarsnote column(index)

scatter rdintens sales

* 学生化残差：销售额最大的观测值
egen sales_max = max(sales)
gen id = (sales == sales_max)
reg rdintens sales profmarg id
gen st_sales = _b[id]/_se[id]

* 学生化残差:stata命令
reg rdintens sales profmarg 
predict st, rstudent

*********
* 例 9.10
*********
use INFMRT, clear
keep if year == 1990
reg infmort lpcinc lphysic lpopul  
estimates store c1 
reg infmort lpcinc lphysic lpopul  if DC == 0
estimates store c2
etable, estimates(c1 c2 )  mstat(N) mstat(r2,nformat(%9.3f)) novarlabel ///
stars(0.10 "*" .05 "**" .01 "***", attach(_r_b))  showstars showstarsnote column(index)

*————————————————————————————————————————————————————————————————————————————————————
* chapter 13
*————————————————————————————————————————————————————————————————————————————————————
*********
* 例 13.1
*********
use FERTIL1, clear
reg kids educ age agesq black east northcen west farm othrural town smcity ///
y74 y76 y78 y80 y82 y84
test y74 y76 y78 y80 y82 y84 // F检验

*********
* 例 13.2
*********
use CPS78_85, clear 
reg lwage y85 educ y85educ exper expersq union female y85fem

*********
* 例 13.3
*********
use KIELMC, clear 
reg rprice nearinc if year==1981  // 1981年回归结果
reg rprice nearinc if year==1978  // 1978年回归结果

* 表13-2
reg rprice y81 nearinc y81nrinc 
estimates store c1 
reg rprice y81 nearinc y81nrinc age agesq
estimates store c2
reg rprice y81 nearinc y81nrinc age agesq intst land area rooms baths
estimates store c3
etable, estimates(c1 c2 c3 ) keep(y81 nearinc y81nrinc _cons) cstat(_r_b, nformat(%9.2f)) mstat(N) mstat(r2,nformat(%9.3f)) novarlabel ///
stars(0.10 "*" .05 "**" .01 "***", attach(_r_b))  showstars showstarsnote column(index)

*************
* 13.3节的例子
*************
use CRIME2, clear 
egen id = fill(1 1 2 2)  // 生成个人编码
order id year  // 放在数据最前面
xtset id year, delta(5)  // 声明面板数据
reg crmrte unem if year == 87  // 简单回归

* 简单一阶差分回归
gen d_crmte = crmrte - l.crmrte
gen d_unem = unem - l.unem 
reg d_crmte d_unem
reg d.crmrte d.unem  

*********
* 例 13.5
*********
use SLP75_81, clear 
reg cslpnap ctotwrk ceduc cmarr cyngkid cgdhlth 
test ceduc cmarr cyngkid cgdhlth  // 除ctotwrk外的f检验

*********
* 例 13.6
*********
use CRIME3, clear 
reg clcrime cclrprc1 cclrprc2

*************
* 13.4节的例子
*************
use JTRAIN , clear 
egen id = fill(1 1 1 2 2 2)  // 生成企业编码
order id year  // 放在数据最前面
xtset id year // 声明面板数据
* 简单一阶差分
reg d.scrap d.grant if inlist(year,1987,1988)
reg d.lscrap d.grant if inlist(year,1987,1988)  // 对数

*********
* 例 13.7
*********
use TRAFFIC1, clear 
reg cdthrte copen cadmn

*********
* 例 13.8
*********
use EZUNEM, clear 
egen id = fill(1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 )  // 生成城市编码
gen index = _n  // 生成城市编码的另一种方式
bys year (index): gen id1 = _n
order id id1 year  // 放在数据最前面
xtset id year // 声明面板数据

* 简单一阶差分
reg guclms cez d82 d83 d84 d85 d86 d87 d88
reg d.luclms d.ez i.year

*********
* 例 13.9
*********
use CRIME4, clear 
xtset county year 
reg d.lcrmrte d.lprbarr d.lprbconv d.lprbpris d.lavgsen d.lpolpc i.year
reg d.lcrmrte d.lprbarr d.lprbconv d.lprbpris d.lavgsen d.lpolpc i.year, cluster(county)  // 聚类稳健标准误

*————————————————————————————————————————————————————————————————————————————————————
* chapter 14
*————————————————————————————————————————————————————————————————————————————————————
*********
* 例 14.2
*********
use wagepan, clear
xtset nr year
xtdescribe
xtreg lwage expersq married union ib1980.year c.edu#c.d81 c.edu#c.d82 c.edu#c.d83 ///
      c.edu#c.d84 c.edu#c.d85 c.edu#c.d86 c.edu#c.d87, fe
xtreg lwage expersq married union ib1980.year c.edu#ib1980.year, fe
* 注意：结果与书不同

*********
* 例 14.4
*********
use wagepan, clear
xtset nr year

reg lwage educ black hisp exper expersq married union i.year
estimates store c1 
xtreg lwage educ black hisp exper expersq married union i.year, re
estimates store c2 
xtreg lwage educ black hisp exper expersq married union i.year, fe
estimates store c3 
etable, estimates(c1 c2 c3 ) keep(educ black hisp exper expersq married union) ///
novarlabel stars(0.10 "*" .05 "**" .01 "***", attach(_r_b))  ///
showstars showstarsnote column(index)

*********************
* 相关随机效应的一个例子
*********************
* webuse nlswork, clear 
use nlswork, clear 
xtset idcode year 

xtreg ln_wage tenure age i.race, fe   // 固定效应模型
estimates store fe
xtreg ln_wage tenure age i.race, re   // 随机效应模型
estimates store re

* 相关随机效应
egen tenure_m = mean(tenure), by(idcode)
egen age_m = mean(age), by(idcode)
xtreg ln_wage tenure age i.race tenure_m age_m, re
estimates store cre 

etable, estimates(fe re cre ) novarlabel stars(0.10 "*" .05 "**" .01 "***", attach(_r_b))  ///
showstars showstarsnote column(estimates)

* 豪斯曼检验
hausman fe re  // 拒绝随机效应

* cre的检验
xtreg ln_wage tenure age i.race tenure_m age_m, re
test tenure_m age_m  // 拒绝随机效应

**********************************
* 固定效应向量分解方法（FEDV）的一个例子
**********************************
* webuse nlswork, clear 
use nlswork, clear 
xtset idcode year 
xtdescribe

* 第一步估算固定效应模型，并计算每一个截矩项
xtreg ln_wage tenure age i.race, fe   // 固定效应模型
estimates store step1
* 求每个截矩项
local beta1 = _b[tenure] 
local beta2 = _b[age]
egen y_mean = mean(ln_wage) if e(sample), by(idcode)
egen x1_mean = mean(tenure) if e(sample), by(idcode)
egen x2_mean = mean(age) if e(sample), by(idcode)
gen a = y_mean - `beta1'*x1_mean - `beta2'*x2_mean

* 第二步截矩项对时间不变向量回归
collapse (mean) a race, by(idcode)
reg a i.race
estimates store step2
predict h, residual
keep idcode h
save temp, replace

* 第三步将h放回原方程
use nlswork, clear 
merge n:1 idcode using temp
reg ln_wage tenure age i.race h 
estimates store step3

etable, estimates(step1 step2 step3 ) cstat(_r_b, nformat(%9.5f)) cstat(_r_se, nformat(%9.5f)) ///
novarlabel stars(0.10 "*" .05 "**" .01 "***", attach(_r_b))  ///
showstars showstarsnote column(estimates)

*********************
* 聚类标准误的一个例子
*********************
use nlswork, clear 
xtset idcode year 

xtreg ln_wage tenure age i.race, fe  vce(cluster idcode) // 固定效应模型
xtreg ln_wage tenure age i.race, re  vce(cluster idcode) // 随机效应模型
