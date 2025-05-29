
Results of 
inp = pd.read_csv(Xnumfum2_repF_with_NEG_UrTest_seqn_age_Anemia_aged_15-49_8907_with_Av_FPro_plus_11_socioeconom_and_62_SUMS_per_day_foods_vars_ends_in_7864_cases.csv', sep=',')


>>> exec(open("T-score_UVE_PLS_Logistic_Regr.py").read())
Accuracy(train) =  0.7339776195320448
Accuracy(CV) =  0.731943031536114
Balanced accuracy(train) =  0.6812149064430092
Balanced accuracy(CV) =  0.6762832267792072
Precision(train) =  0.26813471502590674
Precision(CV) =  0.2644770959377701
Recall(train) =  0.6100196463654224
Recall(CV) =  0.6011787819253438
Specificty(train) =  0.752410166520596
Specificity(CV) =  0.7513876716330704
Dice (i.e. F1 score) (train) =  0.37252549490101977
Dice (i.e. F1 score) (CV) =  0.3673469387755102
AUROC(train) =  0.7492763904409498
AUROC(CV) =  0.7450676028966192
Determined binary (0/1) classification threshold via argmax(F1_score(CV)) =  0.1335
                                                       
            Predicted CV
               P      N 
Actual    P   612     406  as TP    FN
                                                       
Class     N   1702     5144  as FP    TN
                                                       
                                                       
            Predicted TRAIN
               P      N 
Actual    P   621     397  as TP    FN
                                                       
Class     N   1695     5151  as FP    TN
                                                       
                                                       
Selected variable names are:  ['ridageyr' 'Av_FPro' 'Much_alco_1' 'Smoked_rec_1' 'indfmpir' 'bmxbmi'
 'R1' 'R3' 'R4' 'T2' 'Carbohydrate' 'Theobromine' 'Magnesium' 'Zinc'
 'Vitamin.E..alpha.tocopherol.' 'Riboflavin' 'Vitamin.B.12'
 'Choline..total' 'X4.00' 'X12.00' 'X18.02' 'X18.03' 'X22.01'
 'Fatty.acids..total.polyunsaturated']
Selected variables positions are:  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]
Number of PLS components: 3
Logistic regression coefficients of PLS components [Logistic_PLS_comps_coefs]: [[0.56289595 0.14092662 0.15321   ]]
Intercept (1st elem.) of PLS comp coef:  -2.1867978433870583
Standard errors of Intercept and Logistic PLS comp coef:  [0.04151389 0.02535221 0.01741833 0.03003086]
PLS regression coefficient vector: [[ 0.02599048  0.00890548 -0.00634013 -0.02055064 -0.01088066  0.01000059
  -0.00760253 -0.03993668  0.05858992  0.00418628  0.00689296 -0.00641791
  -0.00031639 -0.00826742 -0.00691063 -0.00556839 -0.005212   -0.00240041
  -0.00160809  0.01007928  0.003208    0.00315772 -0.01129883  0.00318269]]
Explained variance in X: [0.11224746 0.30784502 0.37319491]
Variable importances calculated as PLS-Log regression coefficients [Logistic_PLS_comps_coefs x INV(W(t)xL)xW(t), W,L are PLS x-Weights and x-Loadings matrices (t-transpose)]:  [ 0.21596023  0.08072626 -0.05459851 -0.17162222 -0.09542599  0.09029621
 -0.06269201 -0.34419962  0.5001257   0.038447    0.05491253 -0.05607548
 -0.01010106 -0.0748617  -0.06040327 -0.05610686 -0.04933889 -0.02436442
 -0.01908632  0.08128495  0.02793954  0.02662712 -0.09314504  0.02764812]
Correlation between PLS regression vector and Logistic regression vector: 0.9996712790637593
Variable stabilities as mean/stdev of PLS-Log regression coefficients:  [ 39.85681871  16.8128954   13.91462875  34.10243708  16.68518496
  18.62299316  12.53365621  90.58054237 112.29434101  11.69912491
  12.09715138  14.827761     2.93272617  20.35753226  17.74671034
  15.80410811  14.4102806    5.28501242   4.89099679  12.92903363
   9.59801816   8.01288173  22.04253372  10.47207397]
Training set dimensions:  (7864, 24)
Training number of positives:  1018
Absolute difference 95% prediction half-margin in binary probability scale:  0.9283912373025007
Absolute difference 80% prediction half-margin in binary probability scale:  0.32586730185137425
Chisquare test significance for the CV of the training set based on balanced accuracy (50% margin)):  Power_divergenceResult(statistic=977.1088504577823, pvalue=1.6983776429885553e-214)
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of Balanced accuracy =  0.6760440324236774
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of Accuracy =  0.7307985757884028
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of F1 score =  0.36673646425366435
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of TP,TN,FP,FN =  613 5134 1712 405





Results of 
inp = pd.read_csv('Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_plus_8_socioeconom_and_62_SUMS_per_day_foods_vars_8907_cases.csv', sep=',')
>>> exec(open("Output_of_T-score_UVE_PLS_Logistic_Regr.py").read())
Accuracy(train) =  0.7844392051195689
Accuracy(CV) =  0.7832042213988998
Balanced accuracy(train) =  0.655689136516763
Balanced accuracy(CV) =  0.6546015572734536
Precision(train) =  0.28836962294211366
Precision(CV) =  0.2864693446088795
Recall(train) =  0.4835262689225289
Recall(CV) =  0.4826357969723954
Specificty(train) =  0.8278520041109969
Specificity(CV) =  0.8265673175745119
Dice (i.e. F1 score) (train) =  0.36127744510978044
Dice (i.e. F1 score) (CV) =  0.35953565505804314
AUROC(train) =  0.74011489193075
AUROC(CV) =  0.735641940588224
Determined binary (0/1) classification threshold via argmax(F1_score(CV)) =  0.201
                                                       
            Predicted CV
               P      N 
Actual    P   542     581  as TP    FN
                                                       
Class     N   1350     6434  as FP    TN
                                                       
                                                       
            Predicted TRAIN
               P      N 
Actual    P   543     580  as TP    FN
                                                       
Class     N   1340     6444  as FP    TN
                                                       
                                                       
Selected variable names are:  ['ridageyr' 'Av_FPro' 'indfmpir' 'bmxbmi' 'R1' 'R3' 'R4' 'T2' 'Protein'
 'Carbohydrate' 'Caffeine' 'Theobromine' 'Magnesium' 'Sodium' 'Zinc'
 'Riboflavin' 'Vitamin.B.12' 'Vitamin.E..added' 'X4.00' 'X12.00' 'X18.02'
 'X18.03' 'X18.04' 'X22.01' 'Fatty.acids..total.polyunsaturated']
Selected variables positions are:  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
Number of PLS components: 4
Logistic regression coefficients of PLS components [Logistic_PLS_comps_coefs]: [[ 0.50596812  0.13769528  0.15322011 -0.06952647]]
Intercept (1st elem.) of PLS comp coef:  -2.193846688250394
Standard errors of Intercept and Logistic PLS comp coef:  [0.03883652 0.02296    0.01595309 0.0275517  0.03618765]
PLS regression coefficient vector: [[ 0.02958997  0.00919101 -0.00734275  0.00839262 -0.00857126 -0.03689437
   0.05703697  0.00264991 -0.00424748  0.00717026 -0.00610317 -0.00534416
   0.00359733 -0.00598613 -0.00991256 -0.00397776 -0.00620863 -0.00409917
   0.00053013  0.01020818  0.00225095  0.00508823 -0.00525795 -0.00958364
   0.0025873 ]]
Explained variance in X: [0.11925796 0.3165934  0.38219339 0.42761104]
Variable importances calculated as PLS-Log regression coefficients [Logistic_PLS_comps_coefs x INV(W(t)xL)xW(t), W,L are PLS x-Weights and x-Loadings matrices (t-transpose)]:  [ 0.24551501  0.08365425 -0.06669511  0.07634379 -0.07193794 -0.31854153
  0.48819162  0.02560116 -0.04007379  0.05723115 -0.05766984 -0.04750351
  0.02239973 -0.05231833 -0.08901593 -0.04303623 -0.05789056 -0.03787961
 -0.00132181  0.08243713  0.02018892  0.0428702  -0.04432643 -0.07931776
  0.02289394]
Correlation between PLS regression vector and Logistic regression vector: 0.9996147446243016
Variable stabilities as mean/stdev of PLS-Log regression coefficients:  [ 39.90544691  14.98845256  13.56502453  14.34873121  15.17062346
  74.55715176 111.61837634   5.75610075   7.46456828  10.35115092
  11.48101107  11.22519544   5.56860719  12.30448761  22.30038389
   8.58807448  12.6614345    9.6021099    1.16728939  12.33763323
   7.20922629  10.58708783  10.08105076  22.81541823   9.44792884]
Training set dimensions:  (8907, 25)
Training number of positives:  1123
Absolute difference 95% prediction half-margin in binary probability scale:  0.9273917506926319
Absolute difference 80% prediction half-margin in binary probability scale:  0.30954372666212443
Chisquare test significance for the CV of the training set based on balanced accuracy (50% margin)):  Power_divergenceResult(statistic=852.7603116358441, pvalue=1.825870241915304e-187)
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of Balanced accuracy =  0.653454148016023
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of Accuracy =  0.7825305939148984
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of F1 score =  0.3579714948624461
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of TP,TN,FP,FN =  540 6430 1354 583








Results of 
inp = pd.read_csv('Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars_and_62_SUMS_per_day_foods_vars_9928_cases.csv', sep=',')
>>> exec(open("Output_of_T-score_UVE_PLS_Logistic_Regr.py").read())
Accuracy(train) =  0.7885777598710717
Accuracy(CV) =  0.7883763094278807
Balanced accuracy(train) =  0.6330617712608311
Balanced accuracy(CV) =  0.6329491205905596
Precision(train) =  0.23319755600814665
Precision(CV) =  0.23296032553407936
Recall(train) =  0.4357754519505233
Recall(CV) =  0.4357754519505233
Specificty(train) =  0.8303480905711389
Specificity(CV) =  0.8301227892305959
Dice (i.e. F1 score) (train) =  0.30381426202321726
Dice (i.e. F1 score) (CV) =  0.30361286045740804
AUROC(train) =  0.7053145284958499
AUROC(CV) =  0.7001367778499842
Determined binary (0/1) classification threshold via argmax(F1_score(CV)) =  0.146
                                                       
            Predicted CV
               P      N 
Actual    P   458     593  as TP    FN
                                                       
Class     N   1508     7369  as FP    TN
                                                       
                                                       
            Predicted TRAIN
               P      N 
Actual    P   458     593  as TP    FN
                                                       
Class     N   1506     7371  as FP    TN
                                                       
                                                       
Selected variable names are:  ['ridageyr' 'Av_FPro' 'Smoked_rec_1' 'indfmpir' 'R2' 'R3' 'R4' 'T2'
 'Theobromine' 'Magnesium' 'Phosphorus' 'Selenium' 'Thiamin' 'X20.04'
 'X22.01' 'X22.5.n.3']
Selected variables positions are:  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
Number of PLS components: 6
Logistic regression coefficients of PLS components [Logistic_PLS_comps_coefs]: [[ 0.45661767  0.19008459 -0.15340405  0.12045292  0.10050211 -0.10266359]]
Intercept (1st elem.) of PLS comp coef:  -2.3485459755751017
Standard errors of Intercept and Logistic PLS comp coef:  [0.0385897  0.02388836 0.02461055 0.03158738 0.06559587 0.04584824
 0.05914388]
PLS regression coefficient vector: [[ 0.02014205  0.00950221 -0.02299013 -0.01274678 -0.00608933 -0.01282302
   0.04740906  0.00949159 -0.0056804  -0.0201823   0.02177349 -0.01612373
  -0.00521529  0.00675864 -0.00344366 -0.00473802]]
Explained variance in X: [0.1547197  0.29931475 0.40171727 0.43045926 0.48462964 0.52954665]
Variable importances calculated as PLS-Log regression coefficients [Logistic_PLS_comps_coefs x INV(W(t)xL)xW(t), W,L are PLS x-Weights and x-Loadings matrices (t-transpose)]:  [ 0.22063424  0.0739357  -0.24340709 -0.1167688  -0.1061931  -0.14050815
  0.45314332  0.11495675 -0.0743837  -0.24818878  0.25917828 -0.17757116
 -0.06279609  0.03421931 -0.08105096 -0.03117506]
Correlation between PLS regression vector and Logistic regression vector: 0.9914994325363533
Variable stabilities as mean/stdev of PLS-Log regression coefficients:  [33.08667179 14.06325278 51.82129667 19.00732483 15.40545007 18.31349727
 71.13460399 27.60856686 15.88120621 21.64441425 16.61294523 18.48857658
  9.60737017  4.05924917 11.30517294  5.71617076]
Training set dimensions:  (9928, 16)
Training number of positives:  1051
Absolute difference 95% prediction half-margin in binary probability scale:  0.9215263720653435
Absolute difference 80% prediction half-margin in binary probability scale:  0.22381529694880659
Chisquare test significance for the CV of the training set based on balanced accuracy (50% margin)):  Power_divergenceResult(statistic=702.0145044319097, pvalue=1.0905305111384276e-154)
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of Balanced accuracy =  0.6309586550603249
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of Accuracy =  0.7870668815471394
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of F1 score =  0.30092592592592593
In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of TP,TN,FP,FN =  455 7359 1518 596


