clear 
cd /Users/zhangshuai/Desktop/econometrics/Data // 修改默认目录

*————————————————————————————————————————————————————————————————————————————————————
* chapter 1 
*————————————————————————————————————————————————————————————————————————————————————
*****************
*  C1
*****************
use WAGE1, clear 

* (i)
summarize educ 
local wage = r(mean)
di "平均受教育程度为：`r(mean)'"
di "最低受教育年数为：`r(min)'"
di "最高受教育年数为：`r(max)'"

* (ii)
summarize wage
di "平均小时工资为：`r(mean)'"

* (iii)
local cpi1976 = 56.9  // cpi 1976
local cpi2010 = 218.1  // cpi 2010

* (iv)
local wage2010 = `wage' * `cpi2010' / `cpi1976'
di "以2010年美元度量的平均小时工资为：`wage2010'"

* (v)
count if female == 1
di "女性人数为：`r(N)'"
count if female == 0
di "男性人数为：`r(N)'"

*****************
*  C2
*****************
use BWGHT, clear 

* (i)
qui: count 
di "样本中有`r(N)'个妇女"
qui: count if cigs > 0 
di "在怀孕期间抽烟的妇女人数为：`r(N)'"

* (ii)
qui: sum cigs
di "平均抽烟数量为：`r(mean)'"

* (iii)
qui: sum cigs if cigs > 0
di "怀孕期间抽烟的妇女中，平均抽烟数量为：`r(mean)'"

* (iv)
sum fatheduc
di "父亲平均受教育年限为：`r(mean)'"

* (v)
qui: sum faminc
local income = r(mean) * 1000
local std = r(sd) * 1000
di "平均家庭收入为：`income'美元,标准差为：`std'美元"

*****************
*  C3
*****************
use MEAP01, clear 

* (i)
qui: sum math4 
di "math4的最大值和最小值分别为: `r(max)'和`r(min)'"

* (ii)
qui: count if math4 == 100
local math4 = r(N)
di "数学测试中通过率为100%的学校个数为：`r(N)'"

qui: count 
local obs = r(N) // 总观测值
local pct = `math4'/`obs'*100
di "通过率为100%的学校占比为：`pct'"

* (iii)
qui: count if math4 == 50
di "数学测试中通过率为50%的学校个数为：`r(N)'"

* (iv)
qui: sum math4
local math4 = r(mean)
qui: sum read4
local read4 = r(mean)
di "数学和阅读平均通过率分别为：`math4',`read4'"

* (v)
corr math4 read4
di "数学通过率与阅读通过率的相关系数为：`r(rho)'"

* (vi)
qui: sum exppp
local exp = r(mean)
local std = r(sd)
display as text "平均每个学生支出为" as result %6.2f `exp' ///
as text "; 标准差为" %6.2f `std'

* (vii)
local a = 6000
local b = 5500
local pct1 = (`a'/`b' - 1) * 100
local pct2 = (log(`a')-log(`b')) * 100
display as text "学校a的支出超出学校b的支出" as result %3.2f `pct1' as text "%"
display as text "自然对数之差近似的百分比为" as result %3.2f `pct2' as text "%"

*****************
*  C4
*****************
use JTRAIN2, clear 

* (i)
qui: sum train
di as text "得到工作培训的男性比例为" as result %4.2f r(mean)*100 as text "%"

* (ii)
ttest re78, by(train)
di as text "得到工作培训的男性平均工资为" as result %6.2f r(mu_2)*1000
di as text "未得到工作培训的男性平均工资为" as result %6.2f r(mu_1)*1000

* (iii)
ttest unem78, by(train)
di as text "得到工作培训的男性失业比例为" as result %4.2f r(mu_2)*100 as text "%"
di as text "未得到工作培训的男性失业比例为" as result %4.2f r(mu_1)*100 as text "%"

*****************
*  C5
*****************
use FERTIL2, clear 

* (i)
qui: sum children
di as text "孩子数量最大值为" as result %2.0f r(max) _newline  ///
as text "孩子数量最小值为" as result %2.0f r(min) _newline  ///
as text "平均孩子数量为" as result %3.2f r(mean)

* (ii)
qui: sum electric
di as text "家里有电的女性占比为" as result %4.2f r(mean)*100 as text "%"

* (iii)
ttest children, by(electric)
di as text "家里没电的平均孩子数量为" as result %4.2f r(mu_1)
di as text "家里有电的平均孩子数量为" as result %4.2f r(mu_2)

*****************
*  C6
*****************
use COUNTYMURDERS, clear 
keep if year == 1996  // 仅使用1996年数据

* (i)
qui: count 
di as text "数据集中城镇数量为" as result %4.0f r(N)
qui: count if murders == 0
di as text "0起谋杀案的城镇数量为" as result %4.0f r(N)
gen death = execs == 0
qui: sum death 
di as text "0个死刑的城镇占比为" as result %4.2f r(mean)*100 as text "%"

* (ii)
qui: sum murders
di as text "谋杀案的最大值为" as result %4.0f r(max)
qui: sum execs
di as text "判处死刑的最大值为" as result %2.0f r(max)

* (iii)
qui: corr murders execs
di as text "murders和execs的相关系数为" as result %4.2f r(rho)

*****************
*  C7
*****************
use ALCOHOL, clear 

* (i)
qui: sum abuse 
di as text "样本报告酗酒的男性比例为" as result %4.2f r(mean)*100 as text "%"
qui: sum employ
di as text "就业率为" as result %4.2f r(mean)*100 as text "%"

* (ii) and (iii)
ttest employ, by(abuse)
di as text "不酗酒男性的就业率为" as result %4.2f r(mu_1)*100 as text "%"
di as text "酗酒男性的就业率为" as result %4.2f r(mu_2)*100 as text "%"

















