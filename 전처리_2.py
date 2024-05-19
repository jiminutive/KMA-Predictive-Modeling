import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import roc_auc_score

# Step 1: Load CSV file
data = pd.read_csv('final_ulsan_busan.csv')

# Step 2: Partition the data
train_data, test_data = train_test_split(data, test_size=0.3, random_state=42)
test_data, val_data = train_test_split(test_data, test_size=0.5, random_state=42)

# Step 3: Select columns
numeric_cols = ['lat_N', 'long_E', 'sog', 'cog', 'hdf', 'year', 'month', 'day', 'hour', 'minute',
                'khoa_ws', 'khoa_wd', 'khnp_ws', 'khnp_wd', 'kma_mac_wh', 'kma_sig_wh', 'kma_mean_wh']
categorical_cols = ['khoa_stn_name', 'khoa_wd_point', 'kma_stn_name', 'area']
target_col = 'drag_0_1'
selected_cols = numeric_cols + categorical_cols + [target_col]

# Step 4: Rescale numeric inputs
scaler = MinMaxScaler()
train_data[numeric_cols] = scaler.fit_transform(train_data[numeric_cols])
test_data[numeric_cols] = scaler.transform(test_data[numeric_cols])
val_data[numeric_cols] = scaler.transform(val_data[numeric_cols])

# Step 5: Recode categorical inputs as indicator variables
train_data = pd.get_dummies(train_data, columns=categorical_cols)
test_data = pd.get_dummies(test_data, columns=categorical_cols)
val_data = pd.get_dummies(val_data, columns=categorical_cols)

# Step 7: Modeling using Random Forest
# Step 8: Set up the Random Forest classifier
clf = RandomForestClassifier(n_estimators=300, max_features=6, random_state=42)

# Fit the model on the training data
clf.fit(train_data[selected_cols], train_data[target_col])

# Step 9: Evaluation using ROC AUC
# Predict on the validation set
val_predictions = clf.predict_proba(val_data[selected_cols])[:, 1]

# Calculate ROC AUC score
roc_auc = roc_auc_score(val_data[target_col], val_predictions)
print("ROC AUC:", roc_auc)
