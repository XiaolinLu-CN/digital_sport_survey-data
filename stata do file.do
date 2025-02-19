/* 7/5/2023 体育社会资本网络调查 */
cd "D:\big data\data"
use "D:\big data\data\sports sc_2101.dta", clear
count // 2101  样本

// 变量清理
*因变量
*health
tab B1
label var B1 "health"
gen health=B1
tab health
recode health (1 2 3 =0)(4 5=1),gen (health01)
recode health (1 2 =0)(3=1)(4 5=2),gen (health3)


*自变量
*智慧健身
recode C4_1 (2=0),gen (onlinefit) 
recode C4_2 C4_3 C4_4 C4_5 (2=0)
gen devices= C4_2 
gen onlinebook = C4_3
gen onlinewatch =  C4_4 
gen onlineshare =  C4_5

tab onlinefit
tab devices
tab onlinebook
tab onlinewatch
tab onlineshare
pwcorr onlinefit devices onlinebook onlinewatch onlineshare,sig

gen digitsport=onlinefit+devices+onlinebook+onlineshare
sum digitsport
label variable digitsport "智慧健身"


gen digitsport2=onlinefit+devices+onlinebook+onlineshare
tab digitsport2
sum digitsport2

*数字素养
factor C5_1 C5_2 C5_3 C5_4 C5_5  C5_6 C5_7, pcf factor(1) blanks(.40)
rotate, blanks(.40)
estat kmo
predict ict
sum ict
alpha C5_1 C5_2 C5_3 C5_4 C5_5  C5_6 C5_7


egen maxValue = max(ict)
egen minValue = min(ict)
replace ict= (10-1)/(maxValue-minValue)*(ict-minValue)+1
drop maxValue minValue
sum ict

sum ict,detail
recode ict (1/7.678556=0)(7.678556/10=1),gen(ict2)
tab ict2

sum C5_1 C5_2 C5_3 C5_4 C5_5  C5_6 C5_7

************************************************************************************************************************************************ 
*控制变量
* 性别
recode D1 2=0
label def D1 0 "女" 1 "男"
gen gender=D1
tab gender

* 年龄(1=青年，2=中老年)
tab D2
recode D2 (1/4=1)(else=2), gen(age)
recode age (1=0)(2=1)
tab age


* 教育
tab D3
gen edu=D3
recode edu (4 5=1)(else=0),gen(edu01)
label define  edu01 1 "大学" 0 "未上大学"
label values edu01 edu01
recode edu (5=2) (4 =1)(else=0),gen(edu3)

* D4职业
tab D4
recode D4 (1 2=3 "管理")(3=2 "白领")(4 5 6 10 11=1 "蓝领")(else=0 "未就业"), gen(job)
tab job 


*D6 收入
tab D6

histogram D6

recode D6 (1 2=0)(3/max=1),gen (inc)
label define  inc 1 "中高收入" 0 "低收入"
label values inc inc


*D9 婚姻
tab D9
recode D9 (2=1)(else=0),gen(married)
tab married


*D11 省份
tab D11
tab D11,gen(province)

*D12 城乡
tab D12
recode D12 (2=0),gen (urban)
tab urban

*东中西地区
gen area=.
replace area=1 if D11==2
replace area=1 if D11==4
replace area=1 if D11==6
replace area=1 if D11==9
replace area=1 if D11==10
replace area=1 if D11==16
replace area=1 if D11==19
replace area=1 if D11==24
replace area=1 if D11==25
replace area=1 if D11==30
replace area=1 if D11==34

replace area=2 if D11==1
replace area=2 if D11==11
replace area=2 if D11==12 
replace area=2 if D11==14
replace area=2 if D11==15
replace area=2 if D11==17 
replace area=2 if D11==18
replace area=2 if D11==26 

replace area=3 if D11==3
replace area=3 if D11==5
replace area=3 if D11==7
replace area=3 if D11==8
replace area=3 if D11==21
replace area=3 if D11==22
replace area=3 if D11==23
replace area=3 if D11==27
replace area=3 if D11==28
replace area=3 if D11==31
replace area=3 if D11==32
replace area=3 if D11==33

label var area "地区"
label def area 1 "东部" 2 "中部" 3 "西部"
label val area area

tab area

*recode D11  

recode D11 (12=11)(11=12)(14=13)(15=14)(18=15)(19=18)(21=19)(22=20)(23=21)(24=22)(26=23)(27=24)(28=26)(30=27)(32=28)(31=29)(33=30)(34=31)
tab D11

global control " i.job  edu01  inc gender i.age i.area"
sum health  ict ict2 digitsport  $control 

        
tab gender
tab age
tab job
tab inc 
tab edu01  
tab married 
tab area


*model 1
* 数字素养——数字体育参与——健康  
* 身体健康  
reg  health ict $control 
outreg2 using reg.docx, bdec(3) tdec(3) stats(coef se)   alpha(0.001, 0.01, 0.05) replace
est store m1

reg health digitsport2 ict $control 
outreg2 using reg.docx, bdec(3) tdec(3) stats(coef se)   alpha(0.001, 0.01, 0.05)  append
est store m2

reg digitsport2 ict $control 
outreg2 using reg.docx, bdec(3) tdec(3) stats(coef se)   alpha(0.001, 0.01, 0.05)  append
est store m3

khb reg  health ict || digitsport2 , concomitant($control ) vce(bootstrap)


**************************************************************************

*因果中介 
*身体健康

*二分   rho=0.23
medeff (regress digitsport2 ict2 gender age  job  edu01  inc area) (regress health  ict2 digitsport2 gender age  job  edu01  inc area) , mediate(digitsport2) treat(ict2)  sims(1000) seed(123)  

medsens (regress digitsport2 ict2 gender age  job  edu01  inc area)  (regress health  ict2 digitsport2  gender age  job  edu01  inc area), mediate(digitsport2) treat(ict2) sims(1000)  eps(.01)

* Fig 1
twoway rarea _med_updelta0 _med_lodelta0 _med_rho, bcolor(gs14) || line _med_delta0 _med_rho , lcolor(black) ytitle("Average mediation effect") xtitle("Sensitivity parameter: p") legend(off) title("ACME(p)") scheme(s1mono)


