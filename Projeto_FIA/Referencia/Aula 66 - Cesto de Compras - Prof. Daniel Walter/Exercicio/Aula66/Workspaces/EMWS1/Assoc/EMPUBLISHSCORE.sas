*------------------------------------------------------------*;
* Assoc: Score Code;
* To run this score code as stand alone uncomment the code below and set the ASSOCDATA and EM_SCORE_OUTPUT macro variables:;
*;
* %let EM_SCORE_OUTPUT=;
* %let ASSOCDATA =;
* data &EM_SCORE_OUTPUT;
* set &ASSOCDATA;
* run;
*------------------------------------------------------------*;
*------------------------------------------------------------*;
* &nodeid: Creating RULES data set;
*------------------------------------------------------------*;
data WORK.RULEID;
  length   SET_SIZE                             8
           EXP_CONF                             8
           CONF                                 8
           SUPPORT                              8
           LIFT                                 8
           COUNT                                8
           RULE                             $  23
           _LHAND                           $   9
           _RHAND                           $   9
           ITEM1                            $   9
           ITEM2                            $   9
           ITEM3                            $   9
           index                                8
           ruleid                               8
           ;

  label    SET_SIZE="Relations"
           EXP_CONF="Expected Confidence(%)"
           CONF="Confidence(%)"
           SUPPORT="Support(%)"
           LIFT="Lift"
           COUNT="Transaction Count"
           RULE="Rule"
           _LHAND="Left Hand of Rule"
           _RHAND="Right Hand of Rule"
           ITEM1="Rule Item 1"
           ITEM2="Rule Item 2"
           ITEM3="Rule Item 3"
           index="Rule Index"
           ;
  format   SET_SIZE 6.
           EXP_CONF 6.2
           CONF 6.2
           SUPPORT 6.2
           LIFT 6.2
           COUNT 6.2
           ;
SET_SIZE=2; EXP_CONF=5; CONF=20; SUPPORT=5; LIFT=4; COUNT=1; RULE="PERFUME ==> BLUSA"; _LHAND="PERFUME"; _RHAND="BLUSA"; ITEM1="PERFUME"; ITEM2="========>"; ITEM3="BLUSA"; index=5; ruleid=1;
output;
SET_SIZE=2; EXP_CONF=5; CONF=20; SUPPORT=5; LIFT=4; COUNT=1; RULE="CALCA ==> BLUSA"; _LHAND="CALCA"; _RHAND="BLUSA"; ITEM1="CALCA"; ITEM2="========>"; ITEM3="BLUSA"; index=7; ruleid=2;
output;
SET_SIZE=2; EXP_CONF=25; CONF=100; SUPPORT=5; LIFT=4; COUNT=1; RULE="SAPATO ==> CALCA"; _LHAND="SAPATO"; _RHAND="CALCA"; ITEM1="SAPATO"; ITEM2="========>"; ITEM3="CALCA"; index=3; ruleid=3;
output;
SET_SIZE=2; EXP_CONF=25; CONF=100; SUPPORT=5; LIFT=4; COUNT=1; RULE="BLUSA ==> CALCA"; _LHAND="BLUSA"; _RHAND="CALCA"; ITEM1="BLUSA"; ITEM2="========>"; ITEM3="CALCA"; index=8; ruleid=4;
output;
SET_SIZE=2; EXP_CONF=25; CONF=100; SUPPORT=5; LIFT=4; COUNT=1; RULE="SAPATO ==> PERFUME"; _LHAND="SAPATO"; _RHAND="PERFUME"; ITEM1="SAPATO"; ITEM2="========>"; ITEM3="PERFUME"; index=1; ruleid=5;
output;
SET_SIZE=2; EXP_CONF=25; CONF=100; SUPPORT=5; LIFT=4; COUNT=1; RULE="BLUSA ==> PERFUME"; _LHAND="BLUSA"; _RHAND="PERFUME"; ITEM1="BLUSA"; ITEM2="========>"; ITEM3="PERFUME"; index=6; ruleid=6;
output;
SET_SIZE=2; EXP_CONF=5; CONF=20; SUPPORT=5; LIFT=4; COUNT=1; RULE="PERFUME ==> SAPATO"; _LHAND="PERFUME"; _RHAND="SAPATO"; ITEM1="PERFUME"; ITEM2="========>"; ITEM3="SAPATO"; index=2; ruleid=7;
output;
SET_SIZE=2; EXP_CONF=5; CONF=20; SUPPORT=5; LIFT=4; COUNT=1; RULE="CALCA ==> SAPATO"; _LHAND="CALCA"; _RHAND="SAPATO"; ITEM1="CALCA"; ITEM2="========>"; ITEM3="SAPATO"; index=4; ruleid=8;
output;
SET_SIZE=2; EXP_CONF=15; CONF=50; SUPPORT=5; LIFT=3.33333333333333; COUNT=1; RULE="SOMBRA ==> BOLSA"; _LHAND="SOMBRA"; _RHAND="BOLSA"; ITEM1="SOMBRA"; ITEM2="========>"; ITEM3="BOLSA"; index=9; ruleid=9;
output;
SET_SIZE=2; EXP_CONF=15; CONF=50; SUPPORT=5; LIFT=3.33333333333333; COUNT=1; RULE="BASE ==> BOLSA"; _LHAND="BASE"; _RHAND="BOLSA"; ITEM1="BASE"; ITEM2="========>"; ITEM3="BOLSA"; index=10; ruleid=10;
output;
SET_SIZE=2; EXP_CONF=10; CONF=33.3333333333333; SUPPORT=5; LIFT=3.33333333333333; COUNT=1; RULE="BOLSA ==> BASE"; _LHAND="BOLSA"; _RHAND="BASE"; ITEM1="BOLSA"; ITEM2="========>"; ITEM3="BASE"; index=12; ruleid=11;
output;
SET_SIZE=2; EXP_CONF=10; CONF=33.3333333333333; SUPPORT=5; LIFT=3.33333333333333; COUNT=1; RULE="BOLSA ==> SOMBRA"; _LHAND="BOLSA"; _RHAND="SOMBRA"; ITEM1="BOLSA"; ITEM2="========>"; ITEM3="SOMBRA"; index=11; ruleid=12;
output;
SET_SIZE=2; EXP_CONF=10; CONF=25; SUPPORT=5; LIFT=2.5; COUNT=1; RULE="BATON ==> BASE"; _LHAND="BATON"; _RHAND="BASE"; ITEM1="BATON"; ITEM2="========>"; ITEM3="BASE"; index=17; ruleid=13;
output;
SET_SIZE=2; EXP_CONF=20; CONF=50; SUPPORT=5; LIFT=2.5; COUNT=1; RULE="SOMBRA ==> BATON"; _LHAND="SOMBRA"; _RHAND="BATON"; ITEM1="SOMBRA"; ITEM2="========>"; ITEM3="BATON"; index=15; ruleid=14;
output;
SET_SIZE=2; EXP_CONF=20; CONF=50; SUPPORT=5; LIFT=2.5; COUNT=1; RULE="BASE ==> BATON"; _LHAND="BASE"; _RHAND="BATON"; ITEM1="BASE"; ITEM2="========>"; ITEM3="BATON"; index=18; ruleid=15;
output;
SET_SIZE=2; EXP_CONF=5; CONF=12.5; SUPPORT=5; LIFT=2.5; COUNT=1; RULE="TENIS ==> BLUSA"; _LHAND="TENIS"; _RHAND="BLUSA"; ITEM1="TENIS"; ITEM2="========>"; ITEM3="BLUSA"; index=13; ruleid=16;
output;
SET_SIZE=2; EXP_CONF=10; CONF=25; SUPPORT=5; LIFT=2.5; COUNT=1; RULE="BATON ==> SOMBRA"; _LHAND="BATON"; _RHAND="SOMBRA"; ITEM1="BATON"; ITEM2="========>"; ITEM3="SOMBRA"; index=16; ruleid=17;
output;
SET_SIZE=2; EXP_CONF=40; CONF=100; SUPPORT=5; LIFT=2.5; COUNT=1; RULE="BLUSA ==> TENIS"; _LHAND="BLUSA"; _RHAND="TENIS"; ITEM1="BLUSA"; ITEM2="========>"; ITEM3="TENIS"; index=14; ruleid=18;
output;
SET_SIZE=2; EXP_CONF=10; CONF=20; SUPPORT=10; LIFT=2; COUNT=2; RULE="MEIA ==> BASE"; _LHAND="MEIA"; _RHAND="BASE"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="BASE"; index=21; ruleid=19;
output;
SET_SIZE=2; EXP_CONF=10; CONF=20; SUPPORT=5; LIFT=2; COUNT=1; RULE="CALCA ==> BASE"; _LHAND="CALCA"; _RHAND="BASE"; ITEM1="CALCA"; ITEM2="========>"; ITEM3="BASE"; index=25; ruleid=20;
output;
SET_SIZE=2; EXP_CONF=5; CONF=10; SUPPORT=5; LIFT=2; COUNT=1; RULE="MEIA ==> BLUSA"; _LHAND="MEIA"; _RHAND="BLUSA"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="BLUSA"; index=23; ruleid=21;
output;
SET_SIZE=2; EXP_CONF=15; CONF=30; SUPPORT=15; LIFT=2; COUNT=3; RULE="MEIA ==> BOLSA"; _LHAND="MEIA"; _RHAND="BOLSA"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="BOLSA"; index=19; ruleid=22;
output;
SET_SIZE=2; EXP_CONF=25; CONF=50; SUPPORT=5; LIFT=2; COUNT=1; RULE="BASE ==> CALCA"; _LHAND="BASE"; _RHAND="CALCA"; ITEM1="BASE"; ITEM2="========>"; ITEM3="CALCA"; index=26; ruleid=23;
output;
SET_SIZE=2; EXP_CONF=50; CONF=100; SUPPORT=15; LIFT=2; COUNT=3; RULE="BOLSA ==> MEIA"; _LHAND="BOLSA"; _RHAND="MEIA"; ITEM1="BOLSA"; ITEM2="========>"; ITEM3="MEIA"; index=20; ruleid=24;
output;
SET_SIZE=2; EXP_CONF=50; CONF=100; SUPPORT=10; LIFT=2; COUNT=2; RULE="BASE ==> MEIA"; _LHAND="BASE"; _RHAND="MEIA"; ITEM1="BASE"; ITEM2="========>"; ITEM3="MEIA"; index=22; ruleid=25;
output;
SET_SIZE=2; EXP_CONF=50; CONF=100; SUPPORT=5; LIFT=2; COUNT=1; RULE="BLUSA ==> MEIA"; _LHAND="BLUSA"; _RHAND="MEIA"; ITEM1="BLUSA"; ITEM2="========>"; ITEM3="MEIA"; index=24; ruleid=26;
output;
SET_SIZE=2; EXP_CONF=20; CONF=37.5; SUPPORT=15; LIFT=1.875; COUNT=3; RULE="GRAVATA ==> CAMISA"; _LHAND="GRAVATA"; _RHAND="CAMISA"; ITEM1="GRAVATA"; ITEM2="========>"; ITEM3="CAMISA"; index=27; ruleid=27;
output;
SET_SIZE=2; EXP_CONF=40; CONF=75; SUPPORT=15; LIFT=1.875; COUNT=3; RULE="CAMISA ==> GRAVATA"; _LHAND="CAMISA"; _RHAND="GRAVATA"; ITEM1="CAMISA"; ITEM2="========>"; ITEM3="GRAVATA"; index=28; ruleid=28;
output;
SET_SIZE=2; EXP_CONF=15; CONF=25; SUPPORT=5; LIFT=1.66666666666666; COUNT=1; RULE="CAMISA ==> BOLSA"; _LHAND="CAMISA"; _RHAND="BOLSA"; ITEM1="CAMISA"; ITEM2="========>"; ITEM3="BOLSA"; index=29; ruleid=29;
output;
SET_SIZE=2; EXP_CONF=15; CONF=25; SUPPORT=5; LIFT=1.66666666666666; COUNT=1; RULE="BATON ==> BOLSA"; _LHAND="BATON"; _RHAND="BOLSA"; ITEM1="BATON"; ITEM2="========>"; ITEM3="BOLSA"; index=30; ruleid=30;
output;
SET_SIZE=2; EXP_CONF=20; CONF=33.3333333333333; SUPPORT=5; LIFT=1.66666666666666; COUNT=1; RULE="BOLSA ==> BATON"; _LHAND="BOLSA"; _RHAND="BATON"; ITEM1="BOLSA"; ITEM2="========>"; ITEM3="BATON"; index=32; ruleid=31;
output;
SET_SIZE=2; EXP_CONF=20; CONF=33.3333333333333; SUPPORT=5; LIFT=1.66666666666666; COUNT=1; RULE="BOLSA ==> CAMISA"; _LHAND="BOLSA"; _RHAND="CAMISA"; ITEM1="BOLSA"; ITEM2="========>"; ITEM3="CAMISA"; index=31; ruleid=32;
output;
SET_SIZE=2; EXP_CONF=25; CONF=40; SUPPORT=10; LIFT=1.6; COUNT=2; RULE="PERFUME ==> CALCA"; _LHAND="PERFUME"; _RHAND="CALCA"; ITEM1="PERFUME"; ITEM2="========>"; ITEM3="CALCA"; index=33; ruleid=33;
output;
SET_SIZE=2; EXP_CONF=25; CONF=40; SUPPORT=10; LIFT=1.6; COUNT=2; RULE="CALCA ==> PERFUME"; _LHAND="CALCA"; _RHAND="PERFUME"; ITEM1="CALCA"; ITEM2="========>"; ITEM3="PERFUME"; index=34; ruleid=34;
output;
SET_SIZE=2; EXP_CONF=25; CONF=37.5; SUPPORT=15; LIFT=1.5; COUNT=3; RULE="TENIS ==> CALCA"; _LHAND="TENIS"; _RHAND="CALCA"; ITEM1="TENIS"; ITEM2="========>"; ITEM3="CALCA"; index=35; ruleid=35;
output;
SET_SIZE=2; EXP_CONF=40; CONF=60; SUPPORT=15; LIFT=1.5; COUNT=3; RULE="CALCA ==> TENIS"; _LHAND="CALCA"; _RHAND="TENIS"; ITEM1="CALCA"; ITEM2="========>"; ITEM3="TENIS"; index=36; ruleid=36;
output;
SET_SIZE=2; EXP_CONF=10; CONF=12.5; SUPPORT=5; LIFT=1.25; COUNT=1; RULE="GRAVATA ==> BASE"; _LHAND="GRAVATA"; _RHAND="BASE"; ITEM1="GRAVATA"; ITEM2="========>"; ITEM3="BASE"; index=39; ruleid=37;
output;
SET_SIZE=2; EXP_CONF=40; CONF=50; SUPPORT=5; LIFT=1.25; COUNT=1; RULE="BASE ==> GRAVATA"; _LHAND="BASE"; _RHAND="GRAVATA"; ITEM1="BASE"; ITEM2="========>"; ITEM3="GRAVATA"; index=40; ruleid=38;
output;
SET_SIZE=2; EXP_CONF=50; CONF=62.5; SUPPORT=25; LIFT=1.25; COUNT=5; RULE="TENIS ==> MEIA"; _LHAND="TENIS"; _RHAND="MEIA"; ITEM1="TENIS"; ITEM2="========>"; ITEM3="MEIA"; index=37; ruleid=39;
output;
SET_SIZE=2; EXP_CONF=40; CONF=50; SUPPORT=25; LIFT=1.25; COUNT=5; RULE="MEIA ==> TENIS"; _LHAND="MEIA"; _RHAND="TENIS"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="TENIS"; index=38; ruleid=40;
output;
SET_SIZE=2; EXP_CONF=25; CONF=30; SUPPORT=15; LIFT=1.2; COUNT=3; RULE="MEIA ==> CALCA"; _LHAND="MEIA"; _RHAND="CALCA"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="CALCA"; index=43; ruleid=41;
output;
SET_SIZE=2; EXP_CONF=50; CONF=60; SUPPORT=15; LIFT=1.2; COUNT=3; RULE="PERFUME ==> MEIA"; _LHAND="PERFUME"; _RHAND="MEIA"; ITEM1="PERFUME"; ITEM2="========>"; ITEM3="MEIA"; index=41; ruleid=42;
output;
SET_SIZE=2; EXP_CONF=50; CONF=60; SUPPORT=15; LIFT=1.2; COUNT=3; RULE="CALCA ==> MEIA"; _LHAND="CALCA"; _RHAND="MEIA"; ITEM1="CALCA"; ITEM2="========>"; ITEM3="MEIA"; index=44; ruleid=43;
output;
SET_SIZE=2; EXP_CONF=25; CONF=30; SUPPORT=15; LIFT=1.2; COUNT=3; RULE="MEIA ==> PERFUME"; _LHAND="MEIA"; _RHAND="PERFUME"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="PERFUME"; index=42; ruleid=44;
output;
SET_SIZE=2; EXP_CONF=20; CONF=20; SUPPORT=5; LIFT=1; COUNT=1; RULE="PERFUME ==> BATON"; _LHAND="PERFUME"; _RHAND="BATON"; ITEM1="PERFUME"; ITEM2="========>"; ITEM3="BATON"; index=55; ruleid=45;
output;
SET_SIZE=2; EXP_CONF=20; CONF=20; SUPPORT=5; LIFT=1; COUNT=1; RULE="CALCA ==> BATON"; _LHAND="CALCA"; _RHAND="BATON"; ITEM1="CALCA"; ITEM2="========>"; ITEM3="BATON"; index=57; ruleid=46;
output;
SET_SIZE=2; EXP_CONF=25; CONF=25; SUPPORT=10; LIFT=1; COUNT=2; RULE="GRAVATA ==> CALCA"; _LHAND="GRAVATA"; _RHAND="CALCA"; ITEM1="GRAVATA"; ITEM2="========>"; ITEM3="CALCA"; index=51; ruleid=47;
output;
SET_SIZE=2; EXP_CONF=25; CONF=25; SUPPORT=5; LIFT=1; COUNT=1; RULE="BATON ==> CALCA"; _LHAND="BATON"; _RHAND="CALCA"; ITEM1="BATON"; ITEM2="========>"; ITEM3="CALCA"; index=58; ruleid=48;
output;
SET_SIZE=2; EXP_CONF=20; CONF=20; SUPPORT=10; LIFT=1; COUNT=2; RULE="MEIA ==> CAMISA"; _LHAND="MEIA"; _RHAND="CAMISA"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="CAMISA"; index=49; ruleid=49;
output;
SET_SIZE=2; EXP_CONF=40; CONF=40; SUPPORT=20; LIFT=1; COUNT=4; RULE="MEIA ==> GRAVATA"; _LHAND="MEIA"; _RHAND="GRAVATA"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="GRAVATA"; index=45; ruleid=50;
output;
SET_SIZE=2; EXP_CONF=40; CONF=40; SUPPORT=10; LIFT=1; COUNT=2; RULE="CALCA ==> GRAVATA"; _LHAND="CALCA"; _RHAND="GRAVATA"; ITEM1="CALCA"; ITEM2="========>"; ITEM3="GRAVATA"; index=52; ruleid=51;
output;
SET_SIZE=2; EXP_CONF=50; CONF=50; SUPPORT=20; LIFT=1; COUNT=4; RULE="GRAVATA ==> MEIA"; _LHAND="GRAVATA"; _RHAND="MEIA"; ITEM1="GRAVATA"; ITEM2="========>"; ITEM3="MEIA"; index=46; ruleid=52;
output;
SET_SIZE=2; EXP_CONF=50; CONF=50; SUPPORT=10; LIFT=1; COUNT=2; RULE="CAMISA ==> MEIA"; _LHAND="CAMISA"; _RHAND="MEIA"; ITEM1="CAMISA"; ITEM2="========>"; ITEM3="MEIA"; index=50; ruleid=53;
output;
SET_SIZE=2; EXP_CONF=50; CONF=50; SUPPORT=5; LIFT=1; COUNT=1; RULE="SOMBRA ==> MEIA"; _LHAND="SOMBRA"; _RHAND="MEIA"; ITEM1="SOMBRA"; ITEM2="========>"; ITEM3="MEIA"; index=53; ruleid=54;
output;
SET_SIZE=2; EXP_CONF=25; CONF=25; SUPPORT=10; LIFT=1; COUNT=2; RULE="TENIS ==> PERFUME"; _LHAND="TENIS"; _RHAND="PERFUME"; ITEM1="TENIS"; ITEM2="========>"; ITEM3="PERFUME"; index=47; ruleid=55;
output;
SET_SIZE=2; EXP_CONF=25; CONF=25; SUPPORT=5; LIFT=1; COUNT=1; RULE="BATON ==> PERFUME"; _LHAND="BATON"; _RHAND="PERFUME"; ITEM1="BATON"; ITEM2="========>"; ITEM3="PERFUME"; index=56; ruleid=56;
output;
SET_SIZE=2; EXP_CONF=10; CONF=10; SUPPORT=5; LIFT=1; COUNT=1; RULE="MEIA ==> SOMBRA"; _LHAND="MEIA"; _RHAND="SOMBRA"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="SOMBRA"; index=54; ruleid=57;
output;
SET_SIZE=2; EXP_CONF=40; CONF=40; SUPPORT=10; LIFT=1; COUNT=2; RULE="PERFUME ==> TENIS"; _LHAND="PERFUME"; _RHAND="TENIS"; ITEM1="PERFUME"; ITEM2="========>"; ITEM3="TENIS"; index=48; ruleid=58;
output;
SET_SIZE=2; EXP_CONF=40; CONF=37.5; SUPPORT=15; LIFT=0.9375; COUNT=3; RULE="TENIS ==> GRAVATA"; _LHAND="TENIS"; _RHAND="GRAVATA"; ITEM1="TENIS"; ITEM2="========>"; ITEM3="GRAVATA"; index=59; ruleid=59;
output;
SET_SIZE=2; EXP_CONF=40; CONF=37.5; SUPPORT=15; LIFT=0.9375; COUNT=3; RULE="GRAVATA ==> TENIS"; _LHAND="GRAVATA"; _RHAND="TENIS"; ITEM1="GRAVATA"; ITEM2="========>"; ITEM3="TENIS"; index=60; ruleid=60;
output;
SET_SIZE=2; EXP_CONF=15; CONF=12.5; SUPPORT=5; LIFT=0.83333333333333; COUNT=1; RULE="GRAVATA ==> BOLSA"; _LHAND="GRAVATA"; _RHAND="BOLSA"; ITEM1="GRAVATA"; ITEM2="========>"; ITEM3="BOLSA"; index=61; ruleid=61;
output;
SET_SIZE=2; EXP_CONF=40; CONF=33.3333333333333; SUPPORT=5; LIFT=0.83333333333333; COUNT=1; RULE="BOLSA ==> GRAVATA"; _LHAND="BOLSA"; _RHAND="GRAVATA"; ITEM1="BOLSA"; ITEM2="========>"; ITEM3="GRAVATA"; index=62; ruleid=62;
output;
SET_SIZE=2; EXP_CONF=20; CONF=12.5; SUPPORT=5; LIFT=0.625; COUNT=1; RULE="TENIS ==> BATON"; _LHAND="TENIS"; _RHAND="BATON"; ITEM1="TENIS"; ITEM2="========>"; ITEM3="BATON"; index=65; ruleid=63;
output;
SET_SIZE=2; EXP_CONF=20; CONF=12.5; SUPPORT=5; LIFT=0.625; COUNT=1; RULE="TENIS ==> CAMISA"; _LHAND="TENIS"; _RHAND="CAMISA"; ITEM1="TENIS"; ITEM2="========>"; ITEM3="CAMISA"; index=63; ruleid=64;
output;
SET_SIZE=2; EXP_CONF=40; CONF=25; SUPPORT=5; LIFT=0.625; COUNT=1; RULE="CAMISA ==> TENIS"; _LHAND="CAMISA"; _RHAND="TENIS"; ITEM1="CAMISA"; ITEM2="========>"; ITEM3="TENIS"; index=64; ruleid=65;
output;
SET_SIZE=2; EXP_CONF=40; CONF=25; SUPPORT=5; LIFT=0.625; COUNT=1; RULE="BATON ==> TENIS"; _LHAND="BATON"; _RHAND="TENIS"; ITEM1="BATON"; ITEM2="========>"; ITEM3="TENIS"; index=66; ruleid=66;
output;
SET_SIZE=2; EXP_CONF=20; CONF=10; SUPPORT=5; LIFT=0.5; COUNT=1; RULE="MEIA ==> BATON"; _LHAND="MEIA"; _RHAND="BATON"; ITEM1="MEIA"; ITEM2="========>"; ITEM3="BATON"; index=67; ruleid=67;
output;
SET_SIZE=2; EXP_CONF=50; CONF=25; SUPPORT=5; LIFT=0.5; COUNT=1; RULE="BATON ==> MEIA"; _LHAND="BATON"; _RHAND="MEIA"; ITEM1="BATON"; ITEM2="========>"; ITEM3="MEIA"; index=68; ruleid=68;
output;
;
run;
*------------------------------------------------------------*;
* Assoc: Creating RULEMAP and Output data sets;
*------------------------------------------------------------*;
%let _scoreDs = &EM_SCORE_OUTPUT;
proc sort data=&_scoreDs;
by Cupon;
run;
proc mbscore data=&_scoreDs out=score_ruleid INCLUDE ALL_ID
;
customer Cupon;
target Transacao;
;
rules data=work.ruleid;
run;
data &_scoreDs;
set score_ruleid;
array _r{68} _r1-_r68 (68*0);
by Cupon;
if first.Cupon then do;
do i=1 to 68;
_r[i]=0;
end;
end;
if ruleid ne . then _r[ruleid]=1;
if last.Cupon then output;
drop i ruleid;
run;
%let _lib=%str();
%let _ds=%str();
%macro _dsname;
%let _lib =%scan(&EM_SCORE_OUTPUT, 1, .);
%let _ds =%scan(&EM_SCORE_OUTPUT, 2, .);
%if "&_ds" = "" %then %do;
%let _lib=WORK;
%let _ds=%scan(&EM_SCORE_OUTPUT, 1, .);
%end;
%mend _dsname;
%_dsname;
data _null_;
set ruleid end = eof;
if _N_=1 then do;
call execute("proc datasets lib=&_lib nolist;");
call execute("   modify &_ds;");
end;
call execute("   rename _r"!!strip(put(_N_, best.))!!"= RULE"!!strip(put(INDEX, best.))!!";");
call execute("   label  RULE"!!strip(put(INDEX, best.))!!'='!!quote(RULE)!!";");
if eof then do;
call execute("run;");
call execute("quit;");
end;
run;
proc datasets lib=work nolist;
delete score_ruleid ruleid;
run;
quit;
