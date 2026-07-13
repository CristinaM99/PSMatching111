/* Extended common-support / caliper derivation from data_generation.sas
   (the three-way-matching block, lines 653-711): the per-center min/max of
   the multinomial-logit probabilities are taken with PROC MEANS (output
   dataset), collapsed across centers with a second PROC MEANS, and the
   pooled common-support bounds are lifted into macro variables with
   CALL SYMPUTX -- exactly how the paper builds the caliper limits used to
   select subjects inside the common support. The out_mlr_wide input is a
   small reproducible draw (fixed CALL STREAMINIT) standing in for the
   PROC LOGISTIC glogit output; the PROC MEANS output-dataset syntax and
   the SYMPUTX resolution are unchanged from the source. */
data out_mlr_wide;
    call streaminit(20260313);
    do id_pz = 1 to 300;
        center = rand("Table",1/3,1/3,1/3)-1;
        prob1 = rand("Uniform");
        prob2 = rand("Uniform")*(1-prob1);
        output;
    end;
    keep id_pz center prob1 prob2;
run;

proc means data=out_mlr_wide min max;
var prob1 prob2;
class center;
output out=common_support(keep=center min_p1 min_p2 max_p1 max_p2) min=min_p1 min_p2 max=max_p1 max_p2;
run;

proc means data=common_support min max;
var min_p1 min_p2 max_p1 max_p2;
output out=common_support2 min=min_min_p1 min_min_p2 min_max_p1 min_max_p2 max=max_min_p1 max_min_p2 max_max_p1 max_max_p2;
run;

data common_support2;
set common_support2;
call symputx('min_cs_p1',max_min_p1);
call symputx('max_cs_p1',min_max_p1);
run;

data _null_;
put "Common-support bounds for p1: min=&min_cs_p1 max=&max_cs_p1";
run;

proc print data=common_support;
title "Per-center support (prob1, prob2)";
run;
