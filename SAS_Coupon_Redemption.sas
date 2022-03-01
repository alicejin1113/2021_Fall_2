


PROC IMPORT datafile="D:\Benazir\School\Fall1\CRM\Project\final2.csv"
out=x1
dbms=csv
replace;
getnames=yes;
RUN;

PROC IMPORT datafile="D:\Benazir\School\Fall1\CRM\Project\Train.csv"
out=Train
dbms=csv
replace;
getnames=yes;
run;

PROC SQL;
create table train_1 as 
select *
from Train
RUN;
QUIT;

PROC IMPORT
datafile="D:\Benazir\School\Fall1\CRM\Project\Test.csv"
out=Test
dbms=csv
replace;
getnames=yes;
run;

PROC CONTENTS data = Train_1;
run;

ods rtf file="D:\Benazir\School\Fall1\CRM\Project\RESULT.rtf" style =analysis;

PROC HPSPLIT data=Train_1 seed=42;
CLASS   age_range marital_status  rented  family_size income_bracket brand_type category  redemption_status;
*CODE FILE=scoring ;
*GROW GINI ;
*ID variables ;
MODEL redemption_status = coupon_discount duration age_range 
	                       marital_status  rented  family_size 
	                       income_bracket brand_type category quantity selling_price other_discount  ;
*CODE FILE='D:/Benazir/School/Fall1/CRM/Project/R1.sas';
OUTPUT OUT=score1 ;
PRUNE costcomplexity;
PARTITION FRACTION(VALIDATE=0.3 SEED=42);
run;
ods rtf close;
