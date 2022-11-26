import pickle
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt

from scipy.interpolate import interp1d
from scipy import interp
from scipy.special import logit, expit

from sklearn.linear_model import LogisticRegressionCV
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import Imputer
from sklearn.model_selection import train_test_split, StratifiedKFold
from sklearn.preprocessing import PolynomialFeatures
from sklearn.metrics import roc_curve, auc, accuracy_score, confusion_matrix

plt.ion()

precip_ts = [0,12,24,48,72,96,120]

#############################
## Load training data
#############################

def load_data(f_d_rf, f_d_mwra_base, f_d_dcr, f_d_logan, f_d_d_aberjona, f_d_h_alewife, f_l_locationtype, f_d_model_list):
	"""
	Load relevant datasets given filenames specified as input
	"""
	print("Loading data")
	
	## Load main dataset
	d_rf = pd.read_csv(f_d_rf, parse_dates=['DateHour'], encoding='latin1')

	## Load MWRA historical data
	d_mwra_base = pd.read_csv(f_d_mwra_base, parse_dates=['DateHour'], encoding='latin1')

	## Load DCR beach historical data
	d_dcr = pd.read_csv(f_d_dcr, parse_dates=['DateHour'], encoding='latin1')

	## Load rainfall data
	d_logan = pd.read_csv(f_d_logan, index_col='datetime', parse_dates=True, encoding='latin1')

	## Load streamflow data
	d_d_aberjona = pd.read_table(f_d_d_aberjona, comment='#', index_col='datetime', encoding='latin1').iloc[1:]
	d_h_alewife = pd.read_table(f_d_h_alewife, comment='#', index_col='datetime', encoding='latin1').iloc[1:]
	d_d_aberjona.index = pd.to_datetime(d_d_aberjona.index) # parse_dates does not work for this date format
	d_h_alewife.index = pd.to_datetime(d_h_alewife.index)

	## Lookup tables
	l_locationtype = pd.read_table(f_l_locationtype, sep='\t', index_col = 'ID', encoding='latin1')
	
	## Model 
	d_model_list = pd.read_excel(f_d_model_list)
	
	return d_rf, d_mwra_base, d_dcr, d_logan, d_d_aberjona, d_h_alewife, l_locationtype, d_model_list

#############################
## Transform data
#############################

def hourly_flow_interp(d_d, cols):
	"""
	Generate interpolated hourly streamflow from daily data
	
	Takes dataframe of streamflow data indexed on datetime as input and a list of columns (site names) to generate flow interpolations for.
	
	Outputs equivalent dataframe interpolated to hourly resolution
	"""
	print("Interpolating streamflow data")
	
	hour_series = pd.date_range(np.min(d_d.index.values), np.max(d_d.index.values), freq=pd.Timedelta(1, 'h'))
	d_h = d_d.loc[hour_series]
	for col in ['agency_cd','site_no']:
		d_h[col] = d_h[col].fillna(method='ffill')

	d_h['datetime'] = d_h.index
	
	for col in cols:
		d_h[col] = d_h[col].astype(float).interpolate()
	
	return d_h



def prepare_data(db_dfs, flow_dfs, l_locationtype, col_cat):
	"""
	Prepare data for feature engineering
	
	Expects two dictionaries as input -
	* db_dfs: Dictionary containing all result databases as dataframes.  Keys can be anything; values are dataframes.
	* flow_dfs: Dictionary containing all HOURLY flow databases as dataframes.  Keys should be the waterbody name; values should be (dataframe, column) pairs, where the column is the one containing relevant streamflow data.
	
	The dataframes in both these dictionaries will be transformed in place (the input dictionary will be edited directly)
	
	col_cat is a listing of categorical variable columns that will be transformed into dummy columns in the dataframes.
	
	Returns col_cat_dummies, list of dummy categorical variables to pass to engineer_features
	"""
	print("Preparing data")

	## Times
	for name in db_dfs:
		db_dfs[name].DateHour = pd.to_datetime(db_dfs[name].DateHour)

	## Not needed - using parse_dates in read_table
	#for name in flow_dfs:
		#flow_dfs[name].datetime = pd.to_datetime(flow_dfs[name].datetime)

	#d_logan.datetime = pd.to_datetime(d_logan.datetime)
	#d_logan.index = d_logan.datetime

	## LocationTypeID names
	for name in db_dfs:
		db_dfs[name]['LocationTypeID'] = db_dfs[name]['LocationTypeID'].apply(lambda x: l_locationtype['LocationTypeName'].loc[x]).values

	col_cat_dummies = []
	for name in db_dfs:
		for col in col_cat:
			df_dum = pd.get_dummies(db_dfs[name][col], prefix=col, drop_first=1, dummy_na=1)
			for new_col in df_dum.columns:
				db_dfs[name][new_col] = df_dum[new_col]
			col_cat_dummies += df_dum.columns.tolist()

	col_cat_dummies = np.unique(col_cat_dummies)

	## Calculate streamflow derivative
	for key in flow_dfs:
		df, col = flow_dfs[key]
		df['flow_deriv'] = np.nan
		df['flow_deriv'].loc[1:] = df[col].astype(float).values[1:] - df[col].astype(float).values[:-1]
	
	return col_cat_dummies

def gen_flow_rainfall(start_time, end_time, flow_dfs, rain_series, freq = (4, 'h'), precip_ts = [0,12,24,48,72,96,120]):
	"""
	Generate flow and rainfall data between some start_time and end_time for simulation purposes
	
	Parameters:
	* start_time: Start point for simulated range
	* end_time: End point for simulated range
	* flow_dfs: dictionary in the same format as that passed to prepare_data
	* rain_series is rainfall data to be passed to sum_rain
	* freq: (numeric, unit) tuple specifying frequency interval for simulated data range, to be passed to pd.Timedelta
	* precip_ts: Defaults to same list as engineer_features - needs to match
	"""
	print("Generating flow and rainfall data for simulation")
	
	hrs_sim = pd.date_range(pd.Timestamp(start_time), pd.Timestamp(end_time), freq=pd.Timedelta(*freq))
	df_sim = pd.DataFrame(data = {'DateHour':hrs_sim.values})
	
	for key in flow_dfs:
		df, col = flow_dfs[key]
		## Load hourly flow data
		df_sim['flow_'+key+'_current'] = df[col].loc[hrs_sim].values
		df_sim['flow_'+key+'_deriv'] = df['flow_deriv'].loc[hrs_sim].values
		## Missing some values, so interpolate
		df_sim['flow_'+key+'_current'].interpolate('linear', inplace=True)
		df_sim['flow_'+key+'_current'].fillna(0, inplace=True)
		df_sim['flow_'+key+'_deriv'].interpolate('linear', inplace=True)
		df_sim['flow_'+key+'_deriv'].fillna(0, inplace=True)
	
	## Calculate hourly precipitation
	for i in range(len(precip_ts)-1):
		df_sim['precip_'+str(precip_ts[i])+'_'+str(precip_ts[i+1])] = \
			df_sim['DateHour'].apply(lambda x: sum_rain(x, [precip_ts[i], precip_ts[i+1]], rain_series))
	return df_sim, hrs_sim


## Join rainfall data on different timescales
def sum_rain(timestamp, hours, rain_series):
	sel = (rain_series.index >= timestamp - pd.Timedelta(hours[1], 'h')) & (rain_series.index < timestamp - pd.Timedelta(hours[0], 'h'))
	return rain_series[sel].sum()


def engineer_features(result_db, CharacteristicIDs, Unit, flow_dfs, col_cat_dummies, d_logan, 
					  precip_ts = [0,12,24,48,72,96,120], DV = 'ResultValue'):
	"""
	Transform prepared data to engineer features for modeling
	
	Parameters:
	* result_db is the results database to operate on 
	* CharacteristicID is a list of measurement types to filter on units (e.g. ['ECOLI']) , and...
	* Unit is the unit to select.
	* flow_dfs is a dictionary in the same format as that passed to prepare_data
	* col_cat_dummies is the list of categorical dummy variables output by prepare_data
	* d_logan: Rainfall dataframe
	
	Non-required parameters are,
	* precip_ts is the series of rainfall timescales to calculate using sum_rain
	* DV is the dependent variable name in result_db
	
	This function also filters out rows with non_null 'FlagID' fields in the 
	"""
	print("Engineering features")
	
	## Filter to CharactericID=='ECOLI' and Units=='MPN/100ml'
	sel = np.any(
		[(result_db["CharacteristicID"]==char) & (result_db['Units']==Unit) for char in CharacteristicIDs],
			axis = 0)
	result_db_f = result_db[sel]

	## Filter out flagged rows
	result_db_f = result_db_f[result_db_f['FlagID'].isnull()]

	## Join streamflow data and derivatives
	flow_vars = []
	for key in flow_dfs:
			df, col = flow_dfs[key]
			result_db_f['flow_'+key+'_current'] = df[col].loc[result_db_f.DateHour].values
			result_db_f['flow_'+key+'_deriv'] = df['flow_deriv'].loc[result_db_f.DateHour].values
			flow_vars += ['flow_'+key+'_current', 'flow_'+key+'_deriv']

	for i in range(len(precip_ts)-1):
		result_db_f['precip_'+str(precip_ts[i])+'_'+str(precip_ts[i+1])] = result_db_f.DateHour.apply(
			lambda x: sum_rain(x, [precip_ts[i], precip_ts[i+1]], d_logan['prcp_in'])
			)

	## Gather variables
	IVs = flow_vars + list(col_cat_dummies) + ['precip_'+str(precip_ts[i])+'_'+str(precip_ts[i+1]) for i in range(len(precip_ts)-1)]
	#IVs = [iv for iv in IVs if iv in result_db_f] # some columns came from other dataframes - eliminate them
	IVs = [iv for iv in IVs if iv.startswith('Qualifier')==0] # does not make causal sense to include the bacterial results qualifier
	
	return result_db_f, IVs


def gen_train_test(result_db_f, standards_dic, IVs, DV = 'ResultValue', test_size=0.2, random_state=101):
	"""
	Generate training and testing data matrices
	
	Parameters:
	* result_db_f: Output from engineer_features
	* standards_dic is a dictionary with CharacteristicIDs as keys and the relevant swimming or boating standard level as values (must match Unit passed to engineer_features)
	* IVs is the independent variable list from engineer_features
	* DV is the dependent variable name in result_db and should match that passed to engineer_features
	* test_size: Fraction specifying size of test set
	* random_state: Set this to fix the sate of the random number generator
	"""
	print("Splitting train and test data")
	
	# Instantiate matrices
	X = result_db_f.loc[:,IVs].astype(float).values
	Y = np.any(
		[((result_db_f[DV].values > standards_dic[key]) & (result_db_f['CharacteristicID'] == key)) for key in standards_dic]
		, axis =0).astype(float)
	
	## Turns out some flow values are null.  Fill with mean
	imp = Imputer(missing_values=np.nan, strategy='mean', axis=0)
	imp.fit(X)
	X_imp = imp.transform(X)

	X_train, X_test, y_train, y_test = \
		train_test_split(X_imp, Y, test_size=test_size, random_state=random_state, stratify=Y)

	baseline = 1-y_test.mean()
	
	return X_imp, Y, X_train, X_test, y_train, y_test, baseline



#############################
## Train model
#############################

def train_model(X_train, y_train, Cs = 10, n_jobs = 4, cv = 7, class_weight=None):
	"""
	Train the logistic regression model, using sklearn's LogisticRegressionCV
	
	Parameters:
	* Cs: Number of regularization parameters to test in grid search
	* n_jobs: Number of CPU cores to parallelize over
	* cv: Number of cross validation folds to use
	* class_weight: Class weighting value to pass (None by default; may also want to use 'balanced')
	"""
	print("Fitting model")

	ss_X = StandardScaler().fit(X_train)

	clf_lr = LogisticRegressionCV(Cs=10, n_jobs=4, cv=7, class_weight=class_weight).fit(ss_X.transform(X_train), y_train)

	### With interactions
	#pf_int = PolynomialFeatures(degree=2)
	#pf_int.fit(ss_X.transform(X_train))
	
	#IVs_int = ['x'.join(['{}^{}'.format(pair[0],pair[1]) for pair in tuple if pair[1]!=0]) for tuple in [zip(IVs,p) for p in pf_int.powers_]]

	#clf_lri = LogisticRegressionCV(Cs=10, n_jobs=4, cv=7).fit(pf_int.transform(ss_X.transform(X_train)), y_train)

	return ss_X, clf_lr



#############################
## Bacteria prediction model class
#############################

def auc_calc(model, ytest, xtest):
		fpr, tpr, thresholds = roc_curve(ytest, model.predict_proba(xtest)[:, 1])
		return auc(fpr, tpr), fpr, tpr, thresholds

class bacteria_model():
	"""
	"""
	
	def __init__(self, name, X_all, y_all, X_train, X_test, y_train, y_test, IVs, 
			  standard, CharacteristicID, Unit,
			  DV = 'ResultValue', precip_ts = [0,12,24,48,72,96,120], 
			  model_Cs = 10, model_n_jobs = 4, model_cv = 7, model_class_weight=None,
			  sensitivity = None, locations = None):
		self.name = name
		self.model = None
		self.standardizer = None
		self.X_all = X_all
		self.y_all = y_all
		self.X_train = X_train
		self.X_test = X_test
		self.y_train = y_train
		self.y_test = y_test
		self.IVs = IVs
		self.standard = standard
		self.CharacteristicID = CharacteristicID
		self.Unit = Unit
		self.DV = DV
		self.precip_ts = precip_ts
		self.Cs = model_Cs
		self.n_jobs = model_n_jobs
		self.cv = model_cv
		self.class_weight = model_class_weight
		
		self.sensitivity = sensitivity
		self.threshold = 0.5
		self.locations = locations
		
		## Train model with all data by default
		self.train_model(X_all, y_all, 
				   Cs = self.Cs, n_jobs = self.n_jobs, cv = self.cv, class_weight=self.class_weight)
	
	def train_model(self, X, Y, sensitivity = None, **kwargs):
		self.standardizer, self.model = train_model(X, Y, **kwargs)
		if sensitivity is not None:
			self.sensitivity = sensitivity
		if self.sensitivity is not None:
			self.pick_threshold(self.sensitivity)
	
	def retrain_model(self, X, Y):
		"""
		Retrains in place and returns the model property of the object, but does NOT change the threshold property.
		"""
		return self.model.fit(X, Y)
	
	def predict(self, X, threshold=None):
		"""
		Predict a binary outcome from the logistic regression model for some data *X* with a probability threshold *threshold*
		
		If a threshold is not specified, the object's threshold parameter (e.g. as set by the pick_threshold method) is used.
		"""
		if threshold is None:
			threshold = self.threshold
		return (self.model.predict_proba(self.standardizer.transform(X)) > threshold).astype(int)[:,1]
	
	def pick_threshold(self, sensitivity):
		"""
		Use the ROC curve to pick a threshold that aligns to the specified *sensitivity* (true positive rate)
		
		Uses linear interpolation to smooth the ROC curve
		"""
		## Retrain on testing data if not already done
		if 'auc' not in self.__dict__:
			self.get_auc()
		
		if self.sensitivity != sensitivity:
			self.sensitivity = sensitivity
		
		auc, fpr, tpr, logit_thresholds, std_auc, std_tpr = self.auc
		
		self.fpr_interp = np.linspace(0,1,10000)
		self.tpr_interp = interp1d(fpr, tpr, kind='linear', fill_value='extrapolate')(self.fpr_interp)
		self.logit_thresholds_interp = interp1d(fpr, logit_thresholds, kind='linear', fill_value='extrapolate')(self.fpr_interp)
		
		nearest = np.abs(self.tpr_interp - sensitivity).argmin()
		self.threshold = expit(self.logit_thresholds_interp[nearest])
	
	def get_auc(self, K=5):
		"""
		Get AUC on held out testing data
		
		Uses cross validation to construct predictions on the entire dataset as held out data
		
		Based on: http://scikit-learn.org/stable/auto_examples/model_selection/plot_roc_crossval.html
		"""
		cv = StratifiedKFold(n_splits = K)
		tprs = []
		aucs = []
		threshs = []
		mean_fpr = np.linspace(0, 1, 10000)
		for train, test in cv.split(self.X_all, self.y_all):
			probas_ = self.retrain_model(self.X_all[train], self.y_all[train]).predict_proba(self.X_all[test])
			fpr, tpr, thresholds = roc_curve(self.y_all[test], probas_[:, 1])
			tprs.append(interp(mean_fpr, fpr, tpr))
			tprs[-1][0] = 0.0
			roc_auc = auc(fpr, tpr)
			aucs.append(roc_auc)
			threshs.append(interp(mean_fpr, fpr, thresholds))

		
		mean_tpr = np.mean(tprs, axis=0)
		mean_tpr[-1] = 1.0
		mean_auc = auc(mean_fpr, mean_tpr)
		std_auc = np.std(aucs)
		std_tpr = np.std(tprs, axis=0)

		self.auc = mean_auc, mean_fpr, mean_tpr, np.mean(threshs, axis=0), std_auc, std_tpr
		
		return self.auc
	
	def summarize_performance(self, Nsim=100):
		"""
		Print out a summary of performance criteria
		"""
		orig_sensitivity = self.sensitivity
		self.pick_threshold(orig_sensitivity)
		self.get_auc()
		
		print("Model configured for target specificity of: ", self.sensitivity)
		print("...using optimized threshold of: ",self.threshold)
		
		print("Test set AUC: ", self.auc[0])
		
		print("Test set Accuracy: ", accuracy_score(self.y_test, self.predict(self.X_test)))
		
		confmat = confusion_matrix(self.y_test, self.predict(self.X_test))
		print("Test set Sensitivity (TPR): ", confmat[1][1] / float((y_all == 1).sum()))
		print("Test set Specificity (TNR): ", confmat[0][0] / float((y_all == 0).sum()))
		
		## Generate sensitivity / threshold plot
		sens = 1 - np.logspace(-3, -0.0001, Nsim)
		threshs = np.zeros(Nsim)
		tpr1 = np.zeros(Nsim)
		for i, s in enumerate(sens):
			self.pick_threshold(s)
			threshs[i] = self.threshold
			tpr1[i] = confusion_matrix(self.y_all, self.predict(self.X_all))[1][1] / float(sum(self.y_all))

		plt.figure()
		plt.plot(sens, tpr1, label='TPR', marker='o')
		plt.plot(sens, threshs, label='Threshold', marker='o')
		plt.plot([0, 1], [0,1], color='.5', zorder=-2, label='Perfect calibration')
		plt.xlabel('Sensitivity (target TPR)')
		plt.title('Sensitivity response (applied to train+test data)')
		plt.legend()
		
		
		## Reset threshold
		self.pick_threshold(orig_sensitivity)
		
		
		## ROC curve
		plt.figure()
		plt.plot(self.auc[1], self.auc[2])

		plt.axis([0,1,0,1])
		plt.plot([0, 1], [0,1], color='.5', zorder=-2, ls='dashed')
		plt.xlabel('False Positive Rate')
		plt.ylabel('True Positive Rate')
		plt.title('Performance on held out testing set')
		plt.legend()




def load_model(f):
	return pickle.load(f)

