import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.model_selection import train_test_split, RandomizedSearchCV, StratifiedKFold
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, accuracy_score, roc_auc_score

#  exec(open("RF_attempt_to_reproduce_MLProc_42nutr.py").read())

# 1. Load and preprocess data
df_train = pd.read_csv("Train_42nutr.csv")
df_train = df_train.drop(df_train.columns[0], axis=1)
df_test = pd.read_csv("Test_42nutr.csv")
df_test = df_test.drop(df_test.columns[0], axis=1)

# Optional: visualize correlation matrix
#sns.clustermap(df_train.corr())
#plt.show()

# 2. Encode features
features = list(df_train.columns)
features.remove('target')  # 'target' assumed to be the class column

X = pd.get_dummies(df_train[features])
y = df_train['target']

# Align test set to training columns
X_test_final = pd.get_dummies(df_test[features])
X_test_final = X_test_final.reindex(columns=X.columns, fill_value=0)

# 3. Train/test split
X_train, X_val, y_train, y_val = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)

# 4. Define parameter grid
random_grid = {
    'n_estimators': np.linspace(200, 2000, 10, dtype=int),
    'max_features': ['sqrt', 'log2'],
    'max_depth': np.linspace(100, 500, 10, dtype=int),
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4],
    'bootstrap': [True, False]
}

# 5. Random Forest with hyperparameter search
rf = RandomForestClassifier(random_state=42)

rf_random = RandomizedSearchCV(
    estimator=rf,
    param_distributions=random_grid,
    n_iter=100,
    cv=5,
    verbose=2,
    random_state=42,
    n_jobs=-1,
    scoring='accuracy'
)

rf_random.fit(X_train, y_train)

# 6. Best parameters
print("Best Parameters:", rf_random.best_params_)

# 7. Final model with manually selected best values (as per description)

final_model = RandomForestClassifier(
    n_estimators=1600,
    max_features='sqrt',
    max_depth=322,
    min_samples_split=2,
    min_samples_leaf=1,
    random_state=42
)

final_model.fit(X_train, y_train)

# 8. Predict on validation set and compute AUC
y_val_proba = final_model.predict_proba(X_val)
y_val_preds = final_model.predict(X_val)

# Metrics
print("Validation Accuracy:", accuracy_score(y_val, y_val_preds))
print(classification_report(y_val, y_val_preds))

#Validation Accuracy: 0.9058219178082192

auc_macro = roc_auc_score(y_val, y_val_proba, multi_class='ovr', average='macro')
print(f"Macro-Averaged AUC (Validation): {auc_macro:.4f}")

#Macro-Averaged AUC (Validation): 0.9815

# 9. Optional: manual 5-fold CV AUC evaluation
print("\n5-Fold Stratified CV AUC Evaluation:")
cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)
auc_scores = []

for fold, (train_idx, val_idx) in enumerate(cv.split(X, y)):
    X_train_cv, X_val_cv = X.iloc[train_idx], X.iloc[val_idx]
    y_train_cv, y_val_cv = y.iloc[train_idx], y.iloc[val_idx]
    model_cv = RandomForestClassifier(
        n_estimators=1600, # rf_random.best_params_['n_estimators'],
        max_features='sqrt', #rf_random.best_params_['max_features'],
        max_depth=322, # rf_random.best_params_['max_depth'],
        random_state=42
    )
    model_cv.fit(X_train_cv, y_train_cv)
    y_val_proba_cv = model_cv.predict_proba(X_val_cv)
    auc = roc_auc_score(y_val_cv, y_val_proba_cv, multi_class='ovr', average='macro')
    auc_scores.append(auc)
    print(f"  Fold {fold + 1} AUC: {auc:.4f}")
    print(f"\nDimension val:{X_val_cv.shape}")
    print(f"\nDimension cal:{X_train_cv.shape}")

print(f"\nMean AUC over 5 folds: {np.mean(auc_scores):.4f} ± {np.std(auc_scores):.4f}")

# Mean AUC over 5 folds: 0.9786 ± 0.0043

# 10. Predict on test data (probability vector output)
test_probs = final_model.predict_proba(X_test_final)
train_probs = final_model.predict_proba(X)
test_pred=final_model.predict(X_test_final)

test_class=df_test['target']

roc_auc_score(test_class, test_probs, multi_class='ovr', average='macro')
#0.9845555449325092

# 11. Format submission
submission = pd.DataFrame(test_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro=(1-submission.iloc[:,0]+submission.iloc[:,3])/2
submission_train = pd.DataFrame(train_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_train=(1-submission_train.iloc[:,0]+submission_train.iloc[:,3])/2

FPro_train.to_csv("FPro_train_42_nutr_model.csv", index=False)
FPro.to_csv("FPro_test_42_nutr_model.csv", index=False)



df_prediction = pd.read_csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn23_24rc.csv")

df_prediction = df_prediction.drop(df_prediction.columns[0], axis=1)
df_prediction = df_prediction.drop(df_prediction.columns[-1], axis=1)
X_prediction_final = pd.get_dummies(df_prediction[features])
X_prediction_final = X_prediction_final.reindex(columns=X.columns, fill_value=0)
prediction_probs=final_model.predict_proba(X_prediction_final)
prediction_pred=final_model.predict(X_prediction_final)
submission_prediction = pd.DataFrame(prediction_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_prediction=(1-submission_prediction.iloc[:,0]+submission_prediction.iloc[:,3])/2

FPro_prediction.to_csv("FPro_prediction_F_codes_nutr_cont_of_hn23_24rc.csv", index=False)



df_prediction = pd.read_csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn22_24rc.csv")

df_prediction = df_prediction.drop(df_prediction.columns[0], axis=1)
df_prediction = df_prediction.drop(df_prediction.columns[-1], axis=1)
X_prediction_final = pd.get_dummies(df_prediction[features])
X_prediction_final = X_prediction_final.reindex(columns=X.columns, fill_value=0)
prediction_probs=final_model.predict_proba(X_prediction_final)
prediction_pred=final_model.predict(X_prediction_final)
submission_prediction = pd.DataFrame(prediction_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_prediction=(1-submission_prediction.iloc[:,0]+submission_prediction.iloc[:,3])/2

FPro_prediction.to_csv("FPro_prediction_F_codes_nutr_cont_of_hn22_24rc.csv", index=False)



df_prediction = pd.read_csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn21_24rc.csv")

df_prediction = df_prediction.drop(df_prediction.columns[0], axis=1)
df_prediction = df_prediction.drop(df_prediction.columns[-1], axis=1)
X_prediction_final = pd.get_dummies(df_prediction[features])
X_prediction_final = X_prediction_final.reindex(columns=X.columns, fill_value=0)
prediction_probs=final_model.predict_proba(X_prediction_final)
prediction_pred=final_model.predict(X_prediction_final)
submission_prediction = pd.DataFrame(prediction_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_prediction=(1-submission_prediction.iloc[:,0]+submission_prediction.iloc[:,3])/2

FPro_prediction.to_csv("FPro_prediction_F_codes_nutr_cont_of_hn21_24rc.csv", index=False)



df_prediction = pd.read_csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn20_24rc.csv")

df_prediction = df_prediction.drop(df_prediction.columns[0], axis=1)
df_prediction = df_prediction.drop(df_prediction.columns[-1], axis=1)
X_prediction_final = pd.get_dummies(df_prediction[features])
X_prediction_final = X_prediction_final.reindex(columns=X.columns, fill_value=0)
prediction_probs=final_model.predict_proba(X_prediction_final)
prediction_pred=final_model.predict(X_prediction_final)
submission_prediction = pd.DataFrame(prediction_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_prediction=(1-submission_prediction.iloc[:,0]+submission_prediction.iloc[:,3])/2

FPro_prediction.to_csv("FPro_prediction_F_codes_nutr_cont_of_hn20_24rc.csv", index=False)



df_prediction = pd.read_csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn19_24rc.csv")

df_prediction = df_prediction.drop(df_prediction.columns[0], axis=1)
df_prediction = df_prediction.drop(df_prediction.columns[-1], axis=1)
X_prediction_final = pd.get_dummies(df_prediction[features])
X_prediction_final = X_prediction_final.reindex(columns=X.columns, fill_value=0)
prediction_probs=final_model.predict_proba(X_prediction_final)
prediction_pred=final_model.predict(X_prediction_final)
submission_prediction = pd.DataFrame(prediction_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_prediction=(1-submission_prediction.iloc[:,0]+submission_prediction.iloc[:,3])/2

FPro_prediction.to_csv("FPro_prediction_F_codes_nutr_cont_of_hn19_24rc.csv", index=False)



df_prediction = pd.read_csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn18_24rc.csv")

df_prediction = df_prediction.drop(df_prediction.columns[0], axis=1)
df_prediction = df_prediction.drop(df_prediction.columns[-1], axis=1)
X_prediction_final = pd.get_dummies(df_prediction[features])
X_prediction_final = X_prediction_final.reindex(columns=X.columns, fill_value=0)
prediction_probs=final_model.predict_proba(X_prediction_final)
prediction_pred=final_model.predict(X_prediction_final)
submission_prediction = pd.DataFrame(prediction_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_prediction=(1-submission_prediction.iloc[:,0]+submission_prediction.iloc[:,3])/2

FPro_prediction.to_csv("FPro_prediction_F_codes_nutr_cont_of_hn18_24rc.csv", index=False)



df_prediction = pd.read_csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn17_24rc.csv")

df_prediction = df_prediction.drop(df_prediction.columns[0], axis=1)
df_prediction = df_prediction.drop(df_prediction.columns[-1], axis=1)
X_prediction_final = pd.get_dummies(df_prediction[features])
X_prediction_final = X_prediction_final.reindex(columns=X.columns, fill_value=0)
prediction_probs=final_model.predict_proba(X_prediction_final)
prediction_pred=final_model.predict(X_prediction_final)
submission_prediction = pd.DataFrame(prediction_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_prediction=(1-submission_prediction.iloc[:,0]+submission_prediction.iloc[:,3])/2

FPro_prediction.to_csv("FPro_prediction_F_codes_nutr_cont_of_hn17_24rc.csv", index=False)


df_prediction = pd.read_csv("log_ie_FPro-ready_F_codes_nutr_cont­_of_hn16_24rc.csv")

df_prediction = df_prediction.drop(df_prediction.columns[0], axis=1)
df_prediction = df_prediction.drop(df_prediction.columns[-1], axis=1)
X_prediction_final = pd.get_dummies(df_prediction[features])
X_prediction_final = X_prediction_final.reindex(columns=X.columns, fill_value=0)
prediction_probs=final_model.predict_proba(X_prediction_final)
prediction_pred=final_model.predict(X_prediction_final)
submission_prediction = pd.DataFrame(prediction_probs, columns=[f"class_{i}_prob" for i in final_model.classes_])
FPro_prediction=(1-submission_prediction.iloc[:,0]+submission_prediction.iloc[:,3])/2

FPro_prediction.to_csv("FPro_prediction_F_codes_nutr_cont_of_hn16_24rc.csv", index=False)






