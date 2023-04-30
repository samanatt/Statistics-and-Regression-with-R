

################################### 1-Regression Analysis#######################################
#### تحلیل رگرسیون --------------------------------------------------------------


######## تحلیل رگرسیون سنجش رضایت مشتریان  : ---------------------------

##Set work directory-----------------------------
# setwd          
setwd("C:/Users/saman/Desktop/Statistics and Regression with R/Data Frames")  # تعیین مسیر دایرکتوری فایل
getwd()                             # نمایش دایرکتوری ست شده

##Required libraries-----------------------------
install.packages("car")
library("car")
library("moments")

##Read data from file-------------------------
# Assessment Data Set:

data <- read.csv("assessment.csv", header = TRUE)
names(data)       # نام ستون
dim(data)         # ابعاد دیتا ست
head(data)        # نمایش شش ستون اول


### فاز اول : بررسی اولیه داده ها --------------
### Data inspection ---------------------------

str(data)         # Data Structure
summary(data)     # نمایش اطلاعات آماری هر ستون
names(data)       # نمایش نام ستون

## نمایش هیستوگرام
#Continuous variables : در اینجا از متغیرهای پیوسته ستون چهار تا نه استفاده می کنیم

# par = parameter : تابع
par(mfrow = c(2, 3))  # 2 rows 3 columns

## با استفاده از دستور زیر  شش نمودار را در یک پنجره نمایش میدهیم
for (i in 4:9) {
  hist(data[,i], xlab = "", main = paste("Histogram of", names(data)[i]))
}

## در صورتیکه نمودار شما از تنظیمات خارج از کد ذیل استفاده گردد
par("mar")
par(mar=c(1,1,1,1))
##

#-------------------------------
# به حالت عادی بر می گردد
par(mfrow=c(1,1))

## تعیین ضریب همبستگی -----------

cor(data$Main_SF, data$environment, method = "pearson")

## نمایش منحنی  همبستگی دو معیار رضایت و پاکیزگی
plot(data$environment, data$Main_SF)

# نمایش جدول میزان  همبستگی هریک از معیارها
cor_table <- round(cor(data[,c(5:9)]),2)    # round=2   با دو رقم اعشار
cor_table

## بررسی متغییرهای قیاسی
#Categorical variable
str(data)
table(data$weekends)
table(data$no.kids)



## فاز دوم : ----------------------
## برآورد خط رگرسیون ساده : -----------------------------
##Single variable linear regression----------------------

# نکته# در این مدل تنها از یک متغییر جهت سنجش سطح رضایت استفاده می کنیم
# متغییر : transfer

m1 <- lm(Main_SF ~ transfer, data = data)    # lm= تابع تحلیل رگرسیون
m1
summary(m1)



## گام اول : چک می کنیم که مدل چقدر بر روی نمونه ما فیت شده است --------
## ضریب تعیین
##model fit: r-squared
summary(m1)$r.squared               # مستقیما مقدار ضریب تعیین را فراخوانی میکنیم

# چنانچه ضریب همبستگی را به توان دو برسانیم مقدار آن با ضریب تعیین برابر میشود
cor(data$Main_SF, data$transfer)^2     

# ضریب تعیین چه می گوید:
# حدود بیست و درصد واریانس که ما در میزان رضایت می بینیم توسط شاخص سواری توضیح دادخه شده
# در اینجا مقدار ضریب تعیین خیلی عدد خوبی نیست


## با توچه به گزارش summary : 
## گام دوم: آزمون فرض جهت اطمینان از توسعه مدل بر روی کل جامعه ---------
## بررسی نتایج آزمون فرض F و T
#F-test and t-test result


summary(m1)             # نمایش نتایج آزمونهای  F و T :
# 1 )  F-Test : 
# H0 >0.05 : فرض صفر که می گوید هیچ رابطه خطی بین متغیرها وجود ندارد
# p-value: < 0.05 : رد فرض صفر که تایید میکند حداقل یک رابطه خطی بین متغییرها وجود دارد


# 2 ) T-Test : 
# H0 >0.05 : ضریب پیش بینی کننده صفر است
# p-value: < 0.05 : رد فرض صفر که تایید میکند حداقل یک رابطه خطی بین متغییرها وجود دارد

## transfer : ضرایب این متغیر  مخالف صفر است و در نتیجه معنی دار می باشد





### برسی مفروضات رگرسیون با استفاده از نتایج آزمونهای T و F -----
#Assumptions of regression-----------------------


#Residuals: خطاها
#Min      1Q  Median      3Q     Max 
#-3.7042 -0.9452 -0.0006  0.7030  3.7030 : کاهش خطای کرانه بالا و کرانه پائین باید صورت بگیرد
# Residual standard error:انحراف معیار خطاها : با توجه به طیف لیکرت یک تا 10 مقایسه میشود


## Normality of residuals بررسی نرمال بودن خطاها ----------------------
# حالت ایده آل : میانگین صفر و انحراف معیار ثابت

# 1 ) Histogram :
hist(m1$residuals, probability = TRUE)
lines(density(m1$residuals), col = "red")         # برازش با تابع چگالی



# 2 ) QQ-plot :
qqnorm(m1$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m1$residuals, col = "red")

# 3 ) Test for Skewness and Kurtosis آزمون چولگی و کشیدگی -------------------
#Good for sample size > 25

#install.packages("moments")
library(moments)

# 3-1 ) Jarque-Bera Test (Skewness = 0 ?) : آزمون ضریب چولگی ---------
#p-value < 0.05 reject normality assumption
jarque.test(m1$residuals)

# 3-2 ) Anscombe-Glynn Test (Kurtosis = 3 ?) : آزمون ضریب کشیدگی ------
#p-value < 0.05 reject normality assumption
anscombe.test(m1$residuals)

# با توجه به چهار گام بالا میتوان پذیرفت که خطاها از توزیع نرمال پیروی می کنند



## 4 Diagnostic plots : ---------------------------
plot(m1)  # 4 steps in consol 

# 1 : نمودار خطاها بر اساس برآورده هاس ( Y hat)
# 2 : QQ-plot
# 3 : جذر قدر مطلق استاندار خطاها بر اساس برآورد ها
# 4 : خطاهای استاندارد شده را بر حسب شاخص لوریج نمایش میدهد (Leverage)

## Leverage : 
#نشان میدهد بودن یا نبودن نقاط پرت چه اثری در مجموع بر روی مدل پیش بینی ما از متغیر وابسته دارد

## Cook's way فاصله کوک ----------
# معیاری است برای شناسایی داده های پرت (Outliers)
# زمانیکه مقدار لوریج در محور افقی زیاد باشد و مقدار خطا در محور عمودی
# مشاهداتی که وجود یا عدم وجود آنها تاثیر بسزایی بر مدل رگرسیون میگذارد

#---------------------------------------------------
# مثال فاصله کوک با داده ای پرت ( Cook's way) : 

# ایجاد دو بردار با داده های پرت جهت نمایش بهتر فاصله کوک

x <- c(seq(1,19,1),50)        # نمایش یک تا نوزده  و یک داده پرت  پنجاه
x
y <- c(seq(11,29,1),1)        # نمایش یک تا بیست و نه و یک داده پرت یک
y

# نمایش نمودار
plot(x,y)

## حال مدل رگرسیون را می سازیم :
sample_model <- lm(y ~ x)
summary(sample_model)
sample_model
#(Intercept)            x  
#   19.92273     -0.07273    # بدلیل اثر داده پرت ضریب رگرسیون مدل ساخته شده منفی شده است

# مشاهده خط رگرسیون برازش شده
plot(x,y)
abline(sample_model, col = "red") # abline : تابع رسم خط مستقیم بر روی منحنی

# 4 diagnostic plot :
plot(sample_model)    


# مشاهده مستقیم فاصیه کوک
#Cook's way > 1            # بزرگتر از یک به منزله داده پرت است
cooks.way(sample_model)
#remove case #20                # در اینجا داده بیستم بزرگتر از یک است


##  Prediction in Regression--------------------------------------
# پیش بینی در رگرسیون

m1$coefficients     # نمایش ضرایب رگرسیون
#(Intercept)       transfer 
#-6.960434    1.407183


plot(data$transfer, data$Main_SF, main = "Main_SF Satisfaction vs transfer Sat.", 
     ylab = "Main_SF Satisfaction", xlab = "Satisfaction with transfer")

abline(m1, col = "red", lwd = 2)  # lwd : ضخامت خط

# پیش بینی سطح رضایت مشتری بر اساس متغییر transfer:
# Y=b1x+b0
# Transer = 9
#predictive model: Main_SF = -6.960434 + 1.407183 * transfer
# فرض کنیم : transfer=9


-6.960434 + 1.407183 * 9
# پیشبینی سطح رضایت : 5.704213










# حالت دوم :
# فرض کنیم : transfer=2

-6.960434 + 1.407183 * 2   # در اینحالت سطح رضایت مشتری منفی می گردد
# این فرض را نیتوانیم بپذیریم زیرا بازه داده های ما از هفا تا ده میباشد
# بنا بر این عدد دو خارج از بازه داده های ماست


####       Multivariable regression  رگرسیون خطی چند متغیره ### -----------------------------------

# استفاده از سایر متغیرها جهت برآورد متغیر وابسته :

names(data)
head(data)
m2 <- lm(Main_SF ~ transfer + entertainment + pending + environment, data = data)

# coefficients :
m2$coefficients

#model fit: r-squared               #  بررسی ضریب تعیین
summary(m2)$r.squared

summary(m2)
summary(m1)  # مقایسه مدل قبل


#F-test and t-test result : بررسی آزمون ها

#Assumptions of regression m2 -----------------------

# Normality of residuals
hist(m2$residuals, probability = TRUE)
lines(density(m2$residuals), col = "red")

#QQ-plot
qqnorm(m2$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m2$residuals, col = "red")

##Test for Skewness and Kurtosis -----------------
#Good for sample size > 25

##Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m2$residuals)
# p-value = 0.5096 : فرضیه چولگی تفاوت معنی داری با صفر ندارد  

##Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m2$residuals)
#p-value = 0.5694 : فرضیه کشیدگی تفاوت معنا داری با 3 ندارد

# نتیجه : فرضیه نرمال بودن خطاها را نمیتوانیم رد بکنیم


## 4 Diagnostic plots تست تشخیصی چهار منحنی --------
plot(m2)    # console
# نتیجه : در مجموع چهار منحنی ارتباط متغییرها را با متغییر وابسته تایید می کنند


## Check for Multicollinearity بررسی همخطی چندگانه :--------------------
#compare coefficient of transfer in m1 & m2
# بررسی وابستگی متغیرها به هم و خطاهای احتمال

# مقایسه مدل اول با دوم : ---------------
# چرا ضرایب در دو مدل تغییر کرده ؟ 
m1$coefficients
m2$coefficients
cor_table  # متغیر حمل و نقل با سایر متغیرهای همبستگی دارد




#> m1$coefficients : ضریب در حالت اول
# (Intercept)       transfer 
# -6.960434    1.407183 

# > m2$coefficients : ضریب در حالت دوم
#(Intercept)       transfer       entertainment        pending       environment 
#-11.7333965   0.5661151   0.1804956   0.5722112   0.7451711 

# مقدار ضرایب شاخص رایدز در مدل دوم کمتر شده است
#علت : طبق جدول همبستگی زیر در مدل دوم بین این متغییر و متغیرهای مستقل دیگر ارتباط دارد
# لذا سایر متغیرها اثر ضریب متغیر رایدز را کاهش میدهند
#------------------------------------------------------------------


# مساله هم خطی :
# چنانچه این اثر خیلی زیاد باشد ما نمیتوانی بصورت مستقل اثرات هر یک را شناسایی بکنیم
# برای اینکه حد قابل پذیرش و غیر قابل پذیرش ارتباط متغیرها را اندازه گیری کنیم از شاخص زیر استفاده میشود:

##Calculate " variance inflation factor "  (VIF):
# VIF = 1/(1-R^2) : نحوه محاسبه
##If VIF > 10       # then multicollinearity is high
libreray(car)
car :: vif(m2)            # تابع را مستقیما از کتابخانه فراخوانی میکنیم


# car

# یا :
install.packages("car")
library("car")
vif(m2)

# طبق گزارش اعداد هیچ یک از متغیرهای مستقل از شاخص ده کوچکتر نیستند
#transfer    entertainment     pending    environment 
#1.764914 1.270797 1.135040 1.811616


# نکته : در صورتیکه هنگانم نصب پکیج با پیغام خطا مواجه شدید پیکیج ذیل نصب شود
install.packages("stringi")
library("stringi")
install.packages("car",dependencies = T)



# #Prediction پیش بینی --------------------------------------

coef(m2)
# فرض کنیم اعداد داخل بردار میزان رضایت یک شخص از هر یک از شاخصهاست 
coef(m2) %*% c(1, 8, 9, 9, 10) 



# یا به روش قدیم
# Y=B0+B1X1+B2X2+...

#  8, 9, 9, 10
-11.7333965+(0.5661151*8+0.1804956*9+0.5722112*9+0.7451711*10)

## سطح رضایت مشتری نسبت به شاخصها = 7.021597
#-----------------------------------------------------------

# inspection -----------
summary(m1)
summary(m2)
# Residual standard error: 1.16 : در اینجا انحراف معیار خطاها هم کم شدها است
#Residuals: بازه خطاها نیز کم شده است
#  Min      1Q  Median      3Q     Max 
#-3.0898 -0.7649  0.0546  0.8012  3.6629
#------------------------------------------


## Standardizing the predictors استاندارد سازی پیش بینی ---------------------

#Main_SF satisfaction vs way سطح رضایت را با متغیر مسافت می سنجیم
names(data)
cor(data$Main_SF, data$way)    # correlation : 0.07249728 : همبستگی پائین
hist(data$way)   # در اینجا برخلاف طیف لیکرت که از یک تا ده بود رنج از صفر تا سیصد است

summary(m2) # مقايسه ضرايب و اهميت ضرايب بالا در شکل دادن مدل

# برای اینکه اثر رنج بالا خنثی شود همانند مدل استاندارد سازی عمل می کنیم
# که اصطلاحا رنج کردن مینامند
#  هم سطح کردن=(x-mean)/sd
scale(data$way)

# متغيرها را هم سطح کرده و داخل ديتا فريم قرار مي دهيم
data2 <- as.data.frame(scale(data[,c(4:9)]))
head(data2)

## regression model m3 حال رگرسیون را اعمال می کنیم -------------------

m3 <- lm(Main_SF ~ ., data = data2)
summary(m2)
summary(m3)    # مقايسه گزارش مدل با حالتهاي قبلي
head(data)
# تفسير : مقدار ضريب تعيين کمي بهتر شد ولي ضريب تعيين تعديل شده فرقي نکرد



# پس از ساختن مدل سه ميبايست مجددا تمامي فرضيات چک شود ----------------------

head(data)

### بهمبود مدل با اضافه کردن سایر متغیرهای مستقل ------------------------------
#Using factors as predictors: استفاده از متغيرهاي فاکتور بعنوان پيش بيني کننده
# متغییرهای باینری : weekends
# متغیرهای کتگوریکال : no.kids


class(data$weekends)         #  "factor"   :متغیر پایان هفته
levels(data$weekends)        # "no"  "yes"  ( o 1)
class(data$no.kids)       # "integer": change to factor

data$num_child_factor <- factor(data$no.kids) # متغییر تعداد فرزندان را هم قیاسی می کنیم
head(data)
str(data)
class(data$num_child_factor)
levels(data$num_child_factor)

##regression model
names(data)
m4 <- lm(Main_SF ~ way + transfer + entertainment + pending + environment + num_child_factor + weekends, data = data)
summary(m4)
summary(m3)
## ضرایب  ساختگی تعدا فرزندان همگی مجاز شدند
## ظبق گزارش ضرایب شاخص آخر هفته منفی شد


#weekends= میانگین حالت  وسط هفته و آخر هفته تفاوت چندانی ندارد
mean(data$Main_SF [data$weekends == "yes"])
mean(data$Main_SF [data$weekends == "no"])

## Homework : 
#t-test for two-sample mean # home work



#remove weekends  : شاخص آخر هفته را در مدل نمی آوریم
m5 <- lm(Main_SF ~ way + transfer + entertainment + pending + environment + num_child_factor, data = data)
summary(m5)

#reduce the level of num_child categories : تعیین شاخص فرزند به دو حالت با فرزند و بدون فرزند
data$has_child <- factor(data$no.kids > 0)
levels(data$has_child)

m6 <- lm(Main_SF ~ way + transfer + entertainment + pending + environment + has_child, data = data)
summary(m6)

## Interaction effects----------------------------
##  اثر متقابل ------------------------------------

## اثر یک متغیر بر روی مدل میتواند در کنار یک متغیر دیگر تشدید یا تضعیف شود 

# مثال شاخص انتظار در صف برای افراد بدون بچه با افراد بچه دار یکی نیست
# در نتیجه بر روی نرخ رضایتمندی تاثیر می گذارد
# مثال : pending:weekends : اثر متقابل متغیر معطلی بر روی متغر آخر هفته

m7 <- lm(Main_SF ~ way + transfer + entertainment + pending + environment +
           weekends + has_child +
           transfer:has_child + entertainment:has_child + pending:has_child +
           environment:has_child + transfer:weekends + entertainment:weekends +
           pending:weekends + environment:weekends, data = data) 


summary(m7)
# تحلیل گزارش
# انحراف معیار خطاها کمتر شد  ضریب تعیین بیشتر شد
## بعضی از ضرایب اثرشان منفی و از حالت مجاز خارج شد
# بدلیل حالت هم خطی که که اثر متغیرها تغییر کند و اثرات معکوس بشوند


# بررسی هم خطی : -------------------------------------------------------

# VIF>10 critical
car :: vif(m7)
# با توجه به نتايج vif : 
# بدلیل هم خطی مقدار این شاخص خیلی زیاد شده است


##Reduce multicollinearity effect with " scale "  method ----------------------
## رفع هم خطی چند گانه

data3 <- data

names(data3)

data3 [,c(4:8)] <- scale(data3 [,c(4:8)])
head(data3)

m7 <- lm(Main_SF ~ way + transfer + entertainment + pending + environment +
           weekends + has_child +
           transfer:has_child + entertainment:has_child + pending:has_child +
           environment:has_child + transfer:weekends + entertainment:weekends +
           pending:weekends + environment:weekends, data = data3)

summary(m7)

# مشاهده نتایج=
# مقدار ضریب تعیین تغییری نمیکند
# بعضی ضرایب همچنان مقدارشان مجاز نشده
# بعضی ضرایب منفی هستند

car :: vif(m7)  # همگی کمتر از ده شدند
# ----------------------------------------------


##Final regression model-------------------------
## مدل رگرسیون نهایی :

# با توجه به گزارش میآئیم اثرات متقابلی که مقدار کفایت نداشتند را حذف می کنیم
#  و تنها اثر متقابل زمان انمتظار با تعداد فرزندان را در نظر میگیریم

m8 <- lm(Main_SF ~ way + transfer + entertainment + pending + environment + 
           has_child + pending:has_child, data = data3)

summary(m8)


#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         4.10856    0.07058  58.211  < 2e-16 ***    # Significant
#  way            0.06824    0.03887   1.756 0.079731 .    # NOT Significant
#transfer               0.32812    0.05145   6.378 4.15e-10 ***    # Significant
#  entertainment               0.13165    0.04370   3.012 0.002726 **   # Significant
#  pending                0.26617    0.07048   3.777 0.000178 ***  # Significant
#  environment               0.44284    0.05222   8.480 2.64e-16 ***  # Significant
# has_childTRUE       1.56089    0.08456  18.459  < 2e-16 ***   # Significant
#  pending:has_childTRUE  0.54665    0.08390   6.515 1.80e-10 ***  # Significant

## با توجه به اینکه متغیر مسافت اثر معناداری ندارد از مدل حذف میگردد
m8 <- lm(Main_SF ~ transfer + entertainment + pending + environment + 
           has_child + pending:has_child, data = data3)

summary(m8)
names(data3)
# گزارش :
# انحراف معیار خطاها کم شده
# میزان کرانه بالا و پائین خطاها کم شده
#model fit: r-squared
summary(m8)$r.squared

#Residual standard error: 0.87


## تمامی فرضیات را باید چک کنیم
#F-test and t-test result


## بررسی فرضیات مدل رگرسون نهائی
##Assumptions of regression-----------------------

#1-  آزمون نرمال بودن خطاها
#Normality of residuals
hist(m8$residuals, probability = TRUE)
lines(density(m8$residuals), col = "red")

# 2- منحنی چارک به چارک
#QQ-plot
qqnorm(m8$residuals, main = "QQ Plot of residuals", pch = 20)
qqline(m8$residuals, col = "red")

## آزمون چولگی و کشیدگی
#Test for Skewness and Kurtosis
#Good for sample size > 25

library("moments")

## آزمون چولگی :------------------------
##  3- Jarque-Bera Test (Skewness = 0 ?)
#p-value < 0.05 reject normality assumption
jarque.test(m8$residuals)

## آزمون کشیدگی :--------------
# 4- Anscombe-Glynn Test (Kurtosis = 3 ?)
#p-value < 0.05 reject normality assumption
anscombe.test(m8$residuals)

## رسم چهار منحنی تشخیصی : ---------------------
##Diagnostic plots
plot(m8)


## بررسی همخطی چندگانه -------------------------
#Check for Multicollinearity
car :: vif(m8)

# مقادیر قابل قبول هستند :
#transfer          entertainment           pending          environment      has_child 
#1.765956       1.273606       3.314716       1.816491       1.003406 
#pending:has_child 
#3.238225 
# -----------------------------------------------



## prediction : پیش بینی : ---------------------

# ضرایب متغیرها بصورت پیش فرض : ----------------------
#transfer = 10, entertainment = 10, pending = 8, environment = 10, has.child = 1
# ضرایب را هم معیار می کنیم : Scale
#  هم مقیایس کردن=(x-mean)/sd

raw_inputs <- c(10, 10, 8, 10)
names(data3)
scaled_inputs <- (raw_inputs - apply(data[,c(5:8)], 2, mean))/apply(data[,c(5:8)], 2, sd)


coef(m8) %*% c(1,scaled_inputs, 1, scaled_inputs[3] * 1)



## آخرین گام : مقایسه مدل پیش بینی با  واقعی : ---------------------------------------
##prediction vs actual:

# fitted : مقدار پیش بینی شده بر اساس مدل

plot(data$Main_SF, fitted(m8), col = "blue",
     xlim = c(0,10), ylim = c(0,10),
     xlab = "Actual Main_SF Satisfaction", ylab = "Fitted Main_SF Satisfaction")

abline(a = 0, b = 1, col = "red") # a & b = intercept and slope

################################### 2- رگرسیون لجستیک #######################################
###-Logistic Regression -------------------------------------------------------------------



##Set work directory-----------------------------
setwd("C:/Users/gsstech/Desktop")
##Required libraries-----------------------------
##No library required    # در اینجا کتابخانه خاصی مورد نیاز نیست
# Bundle Project

#### مساله : سنجش اثر باندل کردن بر روی رفتار خرید مشتریان :

##Read data from file----------------------------

# marketing_prpmotion


setwd("C:/Users/gsstech/Desktop")
data <- read.csv("marketing_promotion.csv", header = TRUE)
names(data)
dim(data)
head(data)
tail(data)
levels(data$Offer)

## بررسی داده ها
##Data inspection--------------------------------
str(data)
summary(data)
levels(data$Offer)  # در انجا بصورت پیش فرض پایه باندل در نظر گرفته می شود

# تغییر پیش فرض به بدون باندل:
data$Offer <- factor(data$Offer, levels = c("NoBundle","Bundle"))
data$Channel<-as.factor(data$Channel)

levels(data$Channel)
summary(data)
View(data)
# برش متغیر وابسته با بغیه متغیرها: 

table(data$Used_ticket, data$Channel)
table(data$Used_ticket, data$Offer)
table(data$Channel, data$Offer)
table(data$Channel, data$Offer, data$Used_ticket)  # 2 table : No and yes


#----------------------
##Cross Tabulation Analysis تحلیل جدول متقاطع : -----------------------
# با استفاده از روشهای توصیفی و تحلیلهای مبتنی بر جداول :

#Offer : No bundel , bundle
# Used_ticket : Yes , No
cross_tab <- table(data$Offer, data$Used_ticket)
cross_tab       # جدول توزیع  دو متغیر

# نمایش مارجین
margin.table(cross_tab, 1) #over rows   جمع ردیف
margin.table(cross_tab, 2) #over columns جمع سطر

# نمایش درصد مقادیر در جدول
prop.table(cross_tab)   # percengage of totall
prop.table(cross_tab, 1) #over rows  درصد کسانیکه باندل به آنها پیشنهاد شده خرید کردند و یا نکردند
prop.table(cross_tab, 2) #over columns درصد کسانیکه خردید کردند و به آنها باندل پیشنهاد شده با نشده


# # آزمون های آماری : --------------------
##Chi-Square Test : آزمون کای دو 
#H0: Used_ticket is independent of Offer استفاده از کوپن مستقل از عرضه باندل است
#H1: Used_ticket is related to Offer فرضیه مخالف
#If p-value < 0.05 reject H0 رد فرض صفر - تاثیر گذای عرضه باندل بر روی استفاده از کوپن
chisq.test(cross_tab)

# نتیجه : p-value = 6.752e-08 پس فرض صفر را رد می کنیم
# ازائه باندل بر روی خرید یا مصرف کوپن موثر بوده است


# ساخت مدل مبتنی بر رگرسیونن لجستیک
##General Linear Model (GLM)---------------------
##model1-----------------------------------------

# glm= تابع ساخت رگرسیون لجستیک : 
# family = binominal : جهت اجرای رگرسیون لجستیک
## بررسی اثر استفاده از باندل بر روی اینکه از کوپن استفاده می کنند یا خیر؟ :

m1 <- glm(Used_ticket ~ Offer, data = data, family = "binomial")
summary(m1)


## Main_SF fit تست می کنیم آیا مدل به درستی بر روی داده ها فیت شده است؟ -------------------------------------

# 1) deviance statistic : روش  مقایسه دویانس
# با استفاده از آزمون کای دو
#H0: the model is not better than chance at predicting the outcome
#H0 :  مدل را با مدل بیس که هیچ متغیر پیش بینی کننده ای ندارد مقایسه میکند
#p-value < 0.05 reject H0 : بهتر از مدل 

# ابتدا آماره آزمون را میسازیم : ------------------------
# modelchi1= دویانس مدل ساخته شده - دویانس مدل بیس
# m1$null.deviance : دویانس مدل بیس
# m1$deviance : دویانس مدل ساخته شده

modelChi1 <- m1$null.deviance - m1$deviance   
modelChi1

# درجه آزادی : -----------------------------------------
Chidf1    <- m1$df.null - m1$df.residual
Chidf1 
# با استفاده از توزیع کای دو احتمال آنرا بدست می آوریم : ---------------
Chisq_prob1 <- 1 - pchisq(modelChi1, Chidf1)
Chisq_prob1

# P -Value = 5.403966e-08 : مقدار احتمال بسیار کوچک هست پس فرضیه صفر را رد می کنیم

# Null deviance: 4375.0  on 3155  degrees of freedom
# Residual deviance: 4345.4  on 3154  degrees of freedom در مدل پیش بینی مقدار دویانس بهبود یافته

#AIC (Akaike Information Criteria)
#AIC: 4349.4
# ------------------------------------------------
summary(m1)

