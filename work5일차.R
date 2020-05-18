#
# 5일차 
#
# 1. Review
# 
# Text File 읽기

setwd( "C:\\workspaceR" )
df <- read.table( file = "airquality.txt" , header = T )
df

class ( df )

#
# Excel 파일 읽기
#
# Excel 파일 읽기용 패키지 설치 
#

install.packages( "xlsx" )  # Excel 파일 읽기 패키지
install.packages( "rJava" ) # Java 실행 패키지 

# 기본 패키지 외에 설치된 패키지 사용 ( library load )
# library 설치 순서는 반드시 아래 보이는대로 ㅇㅋ?
library( rJava )
library( xlsx )

setwd( "C:\\workspaceR" )   # 파일 저장 경로 설정
df.xlsx <- read.xlsx( file = "airquality.xlsx",
                      sheetIndex = 1,
                      encoding = "UTF-8" )

df.xlsx


# 파일 이름에는 공백을 띄우면 안된다!!!!!!!!!!!!!!! 
class( df.xlsx )
str ( df.xlsx )
head( df.xlsx )
tail( df.xlsx )

score <- c( 76, 55, 24, 7, 99, 73, 23, 47, 60, 34 )
which ( score == 99 )       # 조건에 만족하는 위치의 index
which ( score >= 55 )
max ( score )
which.max ( score )         # 최고값의 index
min ( score )              
which.min ( score )         # 최저값의 index

idx <-  which ( score >= 55 )
score [ idx ] <- 47
score 

idx <- which ( df.xlsx[ , 1:2] == "NA", arr.ind =  TRUE )
idx                #arr.ind = TRUE : 해당 조건의 행/열 값을 확인할때















