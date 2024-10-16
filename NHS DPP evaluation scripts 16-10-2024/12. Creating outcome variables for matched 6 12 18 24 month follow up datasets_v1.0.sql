---NHS DPP evaluation ---
---Deriving outcome variables for matched datasets - 6 / 12 / 18 / 24 month follow-ups. Author: Paul Chappell ---

---Missing values and min & max outcome dates---

SELECT count([NHS_Number]) AS NHS_NUMBER, count([f_date_plus_6]) as f_date_plus_6,
count([new_f_date]) AS F_Date, count([PERSON_ID]) AS PERSON_ID,
 min([new_f_date]) as minFDate,  max([new_f_date]) as maxFDate,
  min([f_date_plus_6]) as min_6_Date,  max([f_date_plus_6]) as max_6_Date	
	  FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_6M_19_April]

---Missing values and min & max outcome dates---

SELECT count([NHS_Number]) AS NHS_NUMBER, count([f_date_plus_12]) as f_date_plus_12,
count([new_f_date]) AS F_Date, count([PERSON_ID]) AS PERSON_ID,
 min([new_f_date]) as minFDate,  max([new_f_date]) as maxFDate,
  min([f_date_plus_12]) as min_12_Date,  max([f_date_plus_12]) as max_12_Date	
	  FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_12M_19_April]

---Missing values and min & max outcome dates---

SELECT count([NHS_Number]) AS NHS_NUMBER, count([f_date_plus_18]) as f_date_plus_18,
count([new_f_date]) AS F_Date, count([PERSON_ID]) AS PERSON_ID,
 min([new_f_date]) as minFDate,  max([new_f_date]) as maxFDate,
  min([f_date_plus_18]) as min_18_Date,  max([f_date_plus_18]) as max_18_Date	
	  FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_18M_19_April]

---Missing values and min & max outcome dates---

SELECT count([NHS_Number]) AS NHS_NUMBER, count([f_date_plus_24]) as f_date_plus_24,
count([new_f_date]) AS F_Date, count([PERSON_ID]) AS PERSON_ID,
 min([new_f_date]) as minFDate,  max([new_f_date]) as maxFDate,
  min([f_date_plus_24]) as min_24_Date,  max([f_date_plus_24]) as max_24_Date	
	  FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_24M_19_April]

---- Testing 1patient per row for ndh----

SELECT [NHS_Number]
FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_6M]
GROUP BY [NHS_Number]
HAVING COUNT(1) > 1

SELECT [NHS_Number]
FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_12M]
GROUP BY [NHS_Number]
HAVING COUNT(1) > 1

SELECT [NHS_Number]
FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_18M]
GROUP BY [NHS_Number]
HAVING COUNT(1) > 1

SELECT [NHS_Number]
FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_24M]
GROUP BY [NHS_Number]
HAVING COUNT(1) > 1


--- Converting to date format---

select
(SELECT FORMAT( EOMONTH([new_f_date]), 'yyyyMMdd') as date) as f_date_end_month
,(SELECT FORMAT( EOMONTH([f_date_plus_6]), 'yyyyMMdd') as date) as outcome_6_date_end_month
	,[NHS_Number]
	,[new_f_date]
	,[f_date_plus_6]
	,[PERSON_ID]
	INTO [ ].[dbo].[Multimorbidityphase3_outcomes_6_1] 
	FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_6M_19_April]
	

select
(SELECT FORMAT( EOMONTH([new_f_date]), 'yyyyMMdd') as date) as f_date_end_month
,(SELECT FORMAT( EOMONTH([f_date_plus_12]), 'yyyyMMdd') as date) as outcome_12_date_end_month
	,[NHS_Number]
	,[new_f_date]
	,[f_date_plus_12]
	,[PERSON_ID]
	INTO [ ].[dbo].[Multimorbidityphase3_outcomes_12_1] 
	FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_12M_19_April]


select
(SELECT FORMAT( EOMONTH([new_f_date]), 'yyyyMMdd') as date) as f_date_end_month
,(SELECT FORMAT( EOMONTH([f_date_plus_18]), 'yyyyMMdd') as date) as outcome_18_date_end_month
	,[NHS_Number]
	,[new_f_date]
	,[f_date_plus_18]
	,[PERSON_ID]
	INTO [ ].[dbo].[Multimorbidityphase3_outcomes_18_1] 
	FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_18M_19_April]


select
(SELECT FORMAT( EOMONTH([new_f_date]), 'yyyyMMdd') as date) as f_date_end_month
,(SELECT FORMAT( EOMONTH([f_date_plus_24]), 'yyyyMMdd') as date) as outcome_24_date_end_month
	,[NHS_Number]
	,[new_f_date]
	,[f_date_plus_24]
	,[PERSON_ID]
	INTO [ ].[dbo].[Multimorbidityphase3_outcomes_24_1] 
	FROM [ ].[dbo].[Multimorbidityphase3_Matched_Sample_IDs_24M_19_April]
		
---Look at top 1k---

		select top(1000)
	f_date_end_month
	,outcome_6_date_end_month
	,[new_f_date]
	,[f_date_plus_6]
	,[PERSON_ID]
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_1] 


		select top(1000)
	f_date_end_month
	,outcome_12_date_end_month
	,[new_f_date]
	,[f_date_plus_12]
	,[PERSON_ID]
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_12_1] 


		select top(1000)
	f_date_end_month
	,outcome_18_date_end_month
	,[new_f_date]
	,[f_date_plus_18]
	,[PERSON_ID]
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_18_1] 


		select top(1000)
	f_date_end_month
	,outcome_6_date_end_month
	,[new_f_date]
	,[f_date_plus_6]
	,[PERSON_ID]
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_1] 

	
---Adding in subsegment combination ID from the segmentation fact_model based on f dates---

	SELECT
	f_date_end_month
	,outcome_6_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_6]
	, f.[PERSON_ID] 
	,s.Date_Id
      , s.[Subsegment_Combination_Id] AS [outcome_6_Subsegment_Combination_Id]
	INTO  [ ].[dbo].[Multimorbidityphase3_outcomes_6_2] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_1] f
	LEFT JOIN [ ].[obh].[FACT_MODEL] s ON f.[outcome_6_date_end_month]  = s.[Date_Id] AND f.[Person_Id] = s.[Person_Id]

	SELECT
	f_date_end_month
	,outcome_12_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_12]
	, f.[PERSON_ID] 
	,s.Date_Id
      , s.[Subsegment_Combination_Id] AS [outcome_12_Subsegment_Combination_Id]
	INTO  [ ].[dbo].[Multimorbidityphase3_outcomes_12_2] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_12_1] f
	LEFT JOIN [ ].[obh].[FACT_MODEL] s ON f.[outcome_12_date_end_month]  = s.[Date_Id] AND f.[Person_Id] = s.[Person_Id]

	
	SELECT
	f_date_end_month
	,outcome_18_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_18]
	, f.[PERSON_ID] 
	,s.Date_Id
      , s.[Subsegment_Combination_Id] AS [outcome_18_Subsegment_Combination_Id]
	INTO  [ ].[dbo].[Multimorbidityphase3_outcomes_18_2] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_18_1] f
	LEFT JOIN [ ].[obh].[FACT_MODEL] s ON f.[outcome_18_date_end_month]  = s.[Date_Id] AND f.[Person_Id] = s.[Person_Id]


	SELECT
	f_date_end_month
	,outcome_24_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_24]
	, f.[PERSON_ID] 
	,s.Date_Id
      , s.[Subsegment_Combination_Id] AS [outcome_24_Subsegment_Combination_Id]
	INTO  [ ].[dbo].[Multimorbidityphase3_outcomes_24_2] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_24_1] f
	LEFT JOIN [ ].[obh].[FACT_MODEL] s ON f.[outcome_24_date_end_month]  = s.[Date_Id] AND f.[Person_Id] = s.[Person_Id]

	---Adding in subsegment combination ID from the segmentation fact_model based on 6,12,18, 24 month after f-dates---

	SELECT
	f_date_end_month
	,outcome_6_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_6]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_6_Subsegment_Combination_Id]
	 ,s.[Subsegment_Combination_Id] AS [f_date_Subsegment_Combination_Id]
	INTO [ ].[dbo].[Multimorbidityphase3_outcomes_6_3] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_2]  f
	LEFT JOIN [ ].[obh].[FACT_MODEL] s ON f.[f_date_end_month]  = s.[Date_Id] AND f.[Person_Id] = s.[Person_Id]

		SELECT
	f_date_end_month
	,outcome_12_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_12]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_12_Subsegment_Combination_Id]
	 ,s.[Subsegment_Combination_Id] AS [f_date_Subsegment_Combination_Id]
	INTO [ ].[dbo].[Multimorbidityphase3_outcomes_12_3] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_12_2]  f
	LEFT JOIN [ ].[obh].[FACT_MODEL] s ON f.[f_date_end_month]  = s.[Date_Id] AND f.[Person_Id] = s.[Person_Id]

		SELECT
	f_date_end_month
	,outcome_18_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_18]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_18_Subsegment_Combination_Id]
	 ,s.[Subsegment_Combination_Id] AS [f_date_Subsegment_Combination_Id]
	INTO [ ].[dbo].[Multimorbidityphase3_outcomes_18_3] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_18_2]  f
	LEFT JOIN [ ].[obh].[FACT_MODEL] s ON f.[f_date_end_month]  = s.[Date_Id] AND f.[Person_Id] = s.[Person_Id]

		SELECT
	f_date_end_month
	,outcome_24_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_24]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_24_Subsegment_Combination_Id]
	 ,s.[Subsegment_Combination_Id] AS [f_date_Subsegment_Combination_Id]
	INTO [ ].[dbo].[Multimorbidityphase3_outcomes_24_3] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_24_2]  f
	LEFT JOIN [ ].[obh].[FACT_MODEL] s ON f.[f_date_end_month]  = s.[Date_Id] AND f.[Person_Id] = s.[Person_Id]


	---Missing values analysis---


SELECT count([NHS_Number]) AS NHS_NUMBER, count(PERSON_ID) AS PERSON_ID,
count([f_date_Subsegment_Combination_Id]) AS F_SUBSEGMENT,
count([outcome_6_Subsegment_Combination_Id]) AS SUBSEGMENT_6_MONTH
	  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_3]


SELECT count([NHS_Number]) AS NHS_NUMBER, count(PERSON_ID) AS PERSON_ID,
count([f_date_Subsegment_Combination_Id]) AS F_SUBSEGMENT,
count([outcome_12_Subsegment_Combination_Id]) AS SUBSEGMENT_12_MONTH
	  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_12_3]


	  SELECT count([NHS_Number]) AS NHS_NUMBER, count(PERSON_ID) AS PERSON_ID,
count([f_date_Subsegment_Combination_Id]) AS F_SUBSEGMENT,
count([outcome_18_Subsegment_Combination_Id]) AS SUBSEGMENT_18_MONTH
	  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_18_3]


	  SELECT count([NHS_Number]) AS NHS_NUMBER, count(PERSON_ID) AS PERSON_ID,
count([f_date_Subsegment_Combination_Id]) AS F_SUBSEGMENT,
count([outcome_24_Subsegment_Combination_Id]) AS SUBSEGMENT_24_MONTH
	  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_24_3]


	---Adding in LTCs and LTC count for f dates---
	
				SELECT 
	f_date_end_month
	,outcome_6_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_6]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_6_Subsegment_Combination_Id]
	 ,f.[f_date_Subsegment_Combination_Id]
      ,[Subsegment_Combination_Code] AS [LTC_f_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_f_Combination_Name]
      ,[Healthy_Well] AS [LTC_f_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_f_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_f_Alcohol_Dependence]
      ,[Asthma] AS [LTC_f_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_f_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_f_Bronchiectasis]
      ,[Cancer] AS [LTC_f_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_f_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_f_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_f_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_f_Chronic_Pain]
      ,[COPD] AS [LTC_f_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_f_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_f_Cystic_Fibrosis]
      ,[Depression] AS [LTC_f_Depression]
      ,[Diabetes] AS [LTC_f_Diabetes]
      ,[Epilepsy] AS [LTC_f_Epilepsy]
      ,[Heart_Failure] AS [LTC_f_Heart_Failure]
      ,[Hypertension] AS [LTC_f_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_f_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_f_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_f_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_f_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_f_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_f_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_f_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_f_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_f_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_f_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_f_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_f_Autism]
      ,[Learning_Disability] AS [LTC_f_Learning_Disability]
      ,[Physical_Disability] AS [LTC_f_Physical_Disability]
      ,[Dementia] AS [LTC_f_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_f_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_f_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_f_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_f_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_f_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_f_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_f_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_f_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_f_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_f_CONDITION_COUNT]
	  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_6_4] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_3]  f 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON f.[f_date_Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	
	
				SELECT 
	f_date_end_month
	,outcome_12_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_12]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_12_Subsegment_Combination_Id]
	 ,f.[f_date_Subsegment_Combination_Id]
      ,[Subsegment_Combination_Code] AS [LTC_f_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_f_Combination_Name]
      ,[Healthy_Well] AS [LTC_f_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_f_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_f_Alcohol_Dependence]
      ,[Asthma] AS [LTC_f_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_f_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_f_Bronchiectasis]
      ,[Cancer] AS [LTC_f_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_f_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_f_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_f_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_f_Chronic_Pain]
      ,[COPD] AS [LTC_f_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_f_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_f_Cystic_Fibrosis]
      ,[Depression] AS [LTC_f_Depression]
      ,[Diabetes] AS [LTC_f_Diabetes]
      ,[Epilepsy] AS [LTC_f_Epilepsy]
      ,[Heart_Failure] AS [LTC_f_Heart_Failure]
      ,[Hypertension] AS [LTC_f_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_f_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_f_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_f_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_f_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_f_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_f_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_f_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_f_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_f_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_f_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_f_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_f_Autism]
      ,[Learning_Disability] AS [LTC_f_Learning_Disability]
      ,[Physical_Disability] AS [LTC_f_Physical_Disability]
      ,[Dementia] AS [LTC_f_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_f_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_f_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_f_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_f_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_f_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_f_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_f_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_f_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_f_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_f_CONDITION_COUNT]
	  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_12_4] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_12_3]  f 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON f.[f_date_Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	

	
				SELECT 
	f_date_end_month
	,outcome_18_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_18]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_18_Subsegment_Combination_Id]
	 ,f.[f_date_Subsegment_Combination_Id]
      ,[Subsegment_Combination_Code] AS [LTC_f_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_f_Combination_Name]
      ,[Healthy_Well] AS [LTC_f_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_f_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_f_Alcohol_Dependence]
      ,[Asthma] AS [LTC_f_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_f_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_f_Bronchiectasis]
      ,[Cancer] AS [LTC_f_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_f_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_f_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_f_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_f_Chronic_Pain]
      ,[COPD] AS [LTC_f_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_f_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_f_Cystic_Fibrosis]
      ,[Depression] AS [LTC_f_Depression]
      ,[Diabetes] AS [LTC_f_Diabetes]
      ,[Epilepsy] AS [LTC_f_Epilepsy]
      ,[Heart_Failure] AS [LTC_f_Heart_Failure]
      ,[Hypertension] AS [LTC_f_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_f_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_f_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_f_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_f_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_f_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_f_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_f_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_f_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_f_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_f_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_f_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_f_Autism]
      ,[Learning_Disability] AS [LTC_f_Learning_Disability]
      ,[Physical_Disability] AS [LTC_f_Physical_Disability]
      ,[Dementia] AS [LTC_f_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_f_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_f_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_f_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_f_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_f_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_f_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_f_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_f_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_f_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_f_CONDITION_COUNT]
	  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_18_4] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_18_3]  f 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON f.[f_date_Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	

	
				SELECT 
	f_date_end_month
	,outcome_24_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_24]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_24_Subsegment_Combination_Id]
	 ,f.[f_date_Subsegment_Combination_Id]
      ,[Subsegment_Combination_Code] AS [LTC_f_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_f_Combination_Name]
      ,[Healthy_Well] AS [LTC_f_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_f_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_f_Alcohol_Dependence]
      ,[Asthma] AS [LTC_f_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_f_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_f_Bronchiectasis]
      ,[Cancer] AS [LTC_f_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_f_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_f_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_f_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_f_Chronic_Pain]
      ,[COPD] AS [LTC_f_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_f_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_f_Cystic_Fibrosis]
      ,[Depression] AS [LTC_f_Depression]
      ,[Diabetes] AS [LTC_f_Diabetes]
      ,[Epilepsy] AS [LTC_f_Epilepsy]
      ,[Heart_Failure] AS [LTC_f_Heart_Failure]
      ,[Hypertension] AS [LTC_f_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_f_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_f_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_f_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_f_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_f_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_f_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_f_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_f_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_f_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_f_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_f_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_f_Autism]
      ,[Learning_Disability] AS [LTC_f_Learning_Disability]
      ,[Physical_Disability] AS [LTC_f_Physical_Disability]
      ,[Dementia] AS [LTC_f_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_f_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_f_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_f_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_f_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_f_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_f_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_f_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_f_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_f_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_f_CONDITION_COUNT]
	  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_24_4] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_24_3]  f 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON f.[f_date_Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	
	---Adding in LTCs for fdate + 6, 12, 18, 24 months---


					SELECT 
	f_date_end_month
	,outcome_6_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_6]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_6_Subsegment_Combination_Id]
	 ,f.[f_date_Subsegment_Combination_Id]
      ,[LTC_f_Combination_Code]
      ,[LTC_f_Combination_Name]
      ,[LTC_f_Healthy_Well]
      ,[LTC_f_Maternal_and_Infant_Health]
      ,[LTC_f_Alcohol_Dependence]
      ,[LTC_f_Asthma]
      ,[LTC_f_Atrial_Fibrillation]
      ,[LTC_f_Bronchiectasis]
      ,[LTC_f_Cancer]
      ,[LTC_f_Cerebrovascular_Disease]
      ,[LTC_f_Chronic_Kidney_Disease]
      ,[LTC_f_Chronic_Liver_Disease]
      ,[LTC_f_Chronic_Pain]
      ,[LTC_f_COPD]
      ,[LTC_f_Coronary_Heart_Disease]
      ,[LTC_f_Cystic_Fibrosis]
      ,[LTC_f_Depression]
      ,[LTC_f_Diabetes]
      ,[LTC_f_Epilepsy]
      ,[LTC_f_Heart_Failure]
      ,[LTC_f_Hypertension]
      ,[LTC_f_Inflammatory_Bowel_Disease]
      ,[LTC_f_Multiple_Sclerosis]
      ,[LTC_f_Osteoarthritis]
      ,[LTC_f_Osteoporosis]
      ,[LTC_f_Parkinsons_Disease]
      ,[LTC_f_Peripheral_Vascular_Disease]
      ,[LTC_f_Pulmonary_Heart_Disease]
      ,[LTC_f_Rheumatoid_Arthritis]
      ,[LTC_f_Sarcoidosis]
      ,[LTC_f_Serious_Mental_Illness]
      ,[LTC_f_Sickle_Cell_Disease]
      ,[LTC_f_Autism]
      ,[LTC_f_Learning_Disability]
      ,[LTC_f_Physical_Disability]
      ,[LTC_f_Dementia]
      ,[LTC_f_Intermediate_Frailty_Risk_HFRS]
      ,[LTC_f_High_Frailty_Risk_HFRS]
      ,[LTC_f_End_Stage_Renal_Failure]
      ,[LTC_f_Severe_Interstitial_Lung_Disease]
      ,[LTC_f_Liver_Failure]
      ,[LTC_f_Neurological_Organ_Failure]
      ,[LTC_f_Severe_COPD]
      ,[LTC_f_Severe_Heart_Failure]
      ,[LTC_f_Incurable_Cancer]
	  ,[LTC_f_CONDITION_COUNT]
	,[Subsegment_Combination_Code] AS [LTC_6_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_6_Combination_Name]
      ,[Healthy_Well] AS [LTC_6_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_6_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_6_Alcohol_Dependence]
      ,[Asthma] AS [LTC_6_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_6_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_6_Bronchiectasis]
      ,[Cancer] AS [LTC_6_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_6_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_6_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_6_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_6_Chronic_Pain]
      ,[COPD] AS [LTC_6_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_6_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_6_Cystic_Fibrosis]
      ,[Depression] AS [LTC_6_Depression]
      ,[Diabetes] AS [LTC_6_Diabetes]
      ,[Epilepsy] AS [LTC_6_Epilepsy]
      ,[Heart_Failure] AS [LTC_6_Heart_Failure]
      ,[Hypertension] AS [LTC_6_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_6_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_6_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_6_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_6_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_6_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_6_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_6_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_6_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_6_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_6_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_6_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_6_Autism]
      ,[Learning_Disability] AS [LTC_6_Learning_Disability]
      ,[Physical_Disability] AS [LTC_6_Physical_Disability]
      ,[Dementia] AS [LTC_6_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_6_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_6_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_6_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_6_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_6_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_6_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_6_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_6_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_6_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_6_CONDITION_COUNT]
	  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_6_R_19_April] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_4]   f 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON f.[outcome_6_Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	

	
					SELECT 
	f_date_end_month
	,outcome_12_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_12]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_12_Subsegment_Combination_Id]
	 ,f.[f_date_Subsegment_Combination_Id]
      ,[LTC_f_Combination_Code]
      ,[LTC_f_Combination_Name]
      ,[LTC_f_Healthy_Well]
      ,[LTC_f_Maternal_and_Infant_Health]
      ,[LTC_f_Alcohol_Dependence]
      ,[LTC_f_Asthma]
      ,[LTC_f_Atrial_Fibrillation]
      ,[LTC_f_Bronchiectasis]
      ,[LTC_f_Cancer]
      ,[LTC_f_Cerebrovascular_Disease]
      ,[LTC_f_Chronic_Kidney_Disease]
      ,[LTC_f_Chronic_Liver_Disease]
      ,[LTC_f_Chronic_Pain]
      ,[LTC_f_COPD]
      ,[LTC_f_Coronary_Heart_Disease]
      ,[LTC_f_Cystic_Fibrosis]
      ,[LTC_f_Depression]
      ,[LTC_f_Diabetes]
      ,[LTC_f_Epilepsy]
      ,[LTC_f_Heart_Failure]
      ,[LTC_f_Hypertension]
      ,[LTC_f_Inflammatory_Bowel_Disease]
      ,[LTC_f_Multiple_Sclerosis]
      ,[LTC_f_Osteoarthritis]
      ,[LTC_f_Osteoporosis]
      ,[LTC_f_Parkinsons_Disease]
      ,[LTC_f_Peripheral_Vascular_Disease]
      ,[LTC_f_Pulmonary_Heart_Disease]
      ,[LTC_f_Rheumatoid_Arthritis]
      ,[LTC_f_Sarcoidosis]
      ,[LTC_f_Serious_Mental_Illness]
      ,[LTC_f_Sickle_Cell_Disease]
      ,[LTC_f_Autism]
      ,[LTC_f_Learning_Disability]
      ,[LTC_f_Physical_Disability]
      ,[LTC_f_Dementia]
      ,[LTC_f_Intermediate_Frailty_Risk_HFRS]
      ,[LTC_f_High_Frailty_Risk_HFRS]
      ,[LTC_f_End_Stage_Renal_Failure]
      ,[LTC_f_Severe_Interstitial_Lung_Disease]
      ,[LTC_f_Liver_Failure]
      ,[LTC_f_Neurological_Organ_Failure]
      ,[LTC_f_Severe_COPD]
      ,[LTC_f_Severe_Heart_Failure]
      ,[LTC_f_Incurable_Cancer]
	  ,[LTC_f_CONDITION_COUNT]
	,[Subsegment_Combination_Code] AS [LTC_12_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_12_Combination_Name]
      ,[Healthy_Well] AS [LTC_12_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_12_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_12_Alcohol_Dependence]
      ,[Asthma] AS [LTC_12_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_12_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_12_Bronchiectasis]
      ,[Cancer] AS [LTC_12_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_12_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_12_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_12_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_12_Chronic_Pain]
      ,[COPD] AS [LTC_12_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_12_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_12_Cystic_Fibrosis]
      ,[Depression] AS [LTC_12_Depression]
      ,[Diabetes] AS [LTC_12_Diabetes]
      ,[Epilepsy] AS [LTC_12_Epilepsy]
      ,[Heart_Failure] AS [LTC_12_Heart_Failure]
      ,[Hypertension] AS [LTC_12_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_12_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_12_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_12_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_12_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_12_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_12_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_12_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_12_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_12_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_12_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_12_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_12_Autism]
      ,[Learning_Disability] AS [LTC_12_Learning_Disability]
      ,[Physical_Disability] AS [LTC_12_Physical_Disability]
      ,[Dementia] AS [LTC_12_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_12_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_12_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_12_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_12_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_12_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_12_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_12_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_12_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_12_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_12_CONDITION_COUNT]
	  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_12_R_19_April] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_12_4]   f 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON f.[outcome_12_Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	

	
					SELECT 
	f_date_end_month
	,outcome_18_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_18]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_18_Subsegment_Combination_Id]
	 ,f.[f_date_Subsegment_Combination_Id]
      ,[LTC_f_Combination_Code]
      ,[LTC_f_Combination_Name]
      ,[LTC_f_Healthy_Well]
      ,[LTC_f_Maternal_and_Infant_Health]
      ,[LTC_f_Alcohol_Dependence]
      ,[LTC_f_Asthma]
      ,[LTC_f_Atrial_Fibrillation]
      ,[LTC_f_Bronchiectasis]
      ,[LTC_f_Cancer]
      ,[LTC_f_Cerebrovascular_Disease]
      ,[LTC_f_Chronic_Kidney_Disease]
      ,[LTC_f_Chronic_Liver_Disease]
      ,[LTC_f_Chronic_Pain]
      ,[LTC_f_COPD]
      ,[LTC_f_Coronary_Heart_Disease]
      ,[LTC_f_Cystic_Fibrosis]
      ,[LTC_f_Depression]
      ,[LTC_f_Diabetes]
      ,[LTC_f_Epilepsy]
      ,[LTC_f_Heart_Failure]
      ,[LTC_f_Hypertension]
      ,[LTC_f_Inflammatory_Bowel_Disease]
      ,[LTC_f_Multiple_Sclerosis]
      ,[LTC_f_Osteoarthritis]
      ,[LTC_f_Osteoporosis]
      ,[LTC_f_Parkinsons_Disease]
      ,[LTC_f_Peripheral_Vascular_Disease]
      ,[LTC_f_Pulmonary_Heart_Disease]
      ,[LTC_f_Rheumatoid_Arthritis]
      ,[LTC_f_Sarcoidosis]
      ,[LTC_f_Serious_Mental_Illness]
      ,[LTC_f_Sickle_Cell_Disease]
      ,[LTC_f_Autism]
      ,[LTC_f_Learning_Disability]
      ,[LTC_f_Physical_Disability]
      ,[LTC_f_Dementia]
      ,[LTC_f_Intermediate_Frailty_Risk_HFRS]
      ,[LTC_f_High_Frailty_Risk_HFRS]
      ,[LTC_f_End_Stage_Renal_Failure]
      ,[LTC_f_Severe_Interstitial_Lung_Disease]
      ,[LTC_f_Liver_Failure]
      ,[LTC_f_Neurological_Organ_Failure]
      ,[LTC_f_Severe_COPD]
      ,[LTC_f_Severe_Heart_Failure]
      ,[LTC_f_Incurable_Cancer]
	  ,[LTC_f_CONDITION_COUNT]
	,[Subsegment_Combination_Code] AS [LTC_18_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_18_Combination_Name]
      ,[Healthy_Well] AS [LTC_18_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_18_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_18_Alcohol_Dependence]
      ,[Asthma] AS [LTC_18_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_18_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_18_Bronchiectasis]
      ,[Cancer] AS [LTC_18_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_18_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_18_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_18_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_18_Chronic_Pain]
      ,[COPD] AS [LTC_18_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_18_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_18_Cystic_Fibrosis]
      ,[Depression] AS [LTC_18_Depression]
      ,[Diabetes] AS [LTC_18_Diabetes]
      ,[Epilepsy] AS [LTC_18_Epilepsy]
      ,[Heart_Failure] AS [LTC_18_Heart_Failure]
      ,[Hypertension] AS [LTC_18_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_18_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_18_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_18_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_18_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_18_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_18_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_18_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_18_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_18_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_18_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_18_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_18_Autism]
      ,[Learning_Disability] AS [LTC_18_Learning_Disability]
      ,[Physical_Disability] AS [LTC_18_Physical_Disability]
      ,[Dementia] AS [LTC_18_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_18_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_18_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_18_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_18_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_18_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_18_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_18_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_18_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_18_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_18_CONDITION_COUNT]
	  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_18_R_19_April] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_18_4]   f 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON f.[outcome_18_Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	

	
					SELECT 
	f_date_end_month
	,outcome_24_date_end_month
	,f.[NHS_Number]
	,f.[new_f_date]
	,f.[f_date_plus_24]
	, f.[PERSON_ID] 
	,f.Date_Id
     ,f.[outcome_24_Subsegment_Combination_Id]
	 ,f.[f_date_Subsegment_Combination_Id]
      ,[LTC_f_Combination_Code]
      ,[LTC_f_Combination_Name]
      ,[LTC_f_Healthy_Well]
      ,[LTC_f_Maternal_and_Infant_Health]
      ,[LTC_f_Alcohol_Dependence]
      ,[LTC_f_Asthma]
      ,[LTC_f_Atrial_Fibrillation]
      ,[LTC_f_Bronchiectasis]
      ,[LTC_f_Cancer]
      ,[LTC_f_Cerebrovascular_Disease]
      ,[LTC_f_Chronic_Kidney_Disease]
      ,[LTC_f_Chronic_Liver_Disease]
      ,[LTC_f_Chronic_Pain]
      ,[LTC_f_COPD]
      ,[LTC_f_Coronary_Heart_Disease]
      ,[LTC_f_Cystic_Fibrosis]
      ,[LTC_f_Depression]
      ,[LTC_f_Diabetes]
      ,[LTC_f_Epilepsy]
      ,[LTC_f_Heart_Failure]
      ,[LTC_f_Hypertension]
      ,[LTC_f_Inflammatory_Bowel_Disease]
      ,[LTC_f_Multiple_Sclerosis]
      ,[LTC_f_Osteoarthritis]
      ,[LTC_f_Osteoporosis]
      ,[LTC_f_Parkinsons_Disease]
      ,[LTC_f_Peripheral_Vascular_Disease]
      ,[LTC_f_Pulmonary_Heart_Disease]
      ,[LTC_f_Rheumatoid_Arthritis]
      ,[LTC_f_Sarcoidosis]
      ,[LTC_f_Serious_Mental_Illness]
      ,[LTC_f_Sickle_Cell_Disease]
      ,[LTC_f_Autism]
      ,[LTC_f_Learning_Disability]
      ,[LTC_f_Physical_Disability]
      ,[LTC_f_Dementia]
      ,[LTC_f_Intermediate_Frailty_Risk_HFRS]
      ,[LTC_f_High_Frailty_Risk_HFRS]
      ,[LTC_f_End_Stage_Renal_Failure]
      ,[LTC_f_Severe_Interstitial_Lung_Disease]
      ,[LTC_f_Liver_Failure]
      ,[LTC_f_Neurological_Organ_Failure]
      ,[LTC_f_Severe_COPD]
      ,[LTC_f_Severe_Heart_Failure]
      ,[LTC_f_Incurable_Cancer]
	  ,[LTC_f_CONDITION_COUNT]
	,[Subsegment_Combination_Code] AS [LTC_24_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_24_Combination_Name]
      ,[Healthy_Well] AS [LTC_24_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_24_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_24_Alcohol_Dependence]
      ,[Asthma] AS [LTC_24_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_24_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_24_Bronchiectasis]
      ,[Cancer] AS [LTC_24_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_24_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_24_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_24_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_24_Chronic_Pain]
      ,[COPD] AS [LTC_24_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_24_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_24_Cystic_Fibrosis]
      ,[Depression] AS [LTC_24_Depression]
      ,[Diabetes] AS [LTC_24_Diabetes]
      ,[Epilepsy] AS [LTC_24_Epilepsy]
      ,[Heart_Failure] AS [LTC_24_Heart_Failure]
      ,[Hypertension] AS [LTC_24_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_24_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_24_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_24_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_24_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_24_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_24_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_24_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_24_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_24_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_24_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_24_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_24_Autism]
      ,[Learning_Disability] AS [LTC_24_Learning_Disability]
      ,[Physical_Disability] AS [LTC_24_Physical_Disability]
      ,[Dementia] AS [LTC_24_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_24_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_24_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_24_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_24_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_24_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_24_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_24_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_24_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_24_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_24_CONDITION_COUNT]
	  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_24_R_19_April] 
	FROM [ ].[dbo].[Multimorbidityphase3_outcomes_24_4]   f 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON f.[outcome_24_Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	
---Checking---

SELECT count([NHS_Number]) AS NHS_NUMBER, 
count([outcome_6_Subsegment_Combination_Id]) AS SUBSEGMENT_6, 
count([LTC_6_Combination_Code]) AS SUBSEGMENT_CODE_6, 
count([LTC_6_Combination_Name]) AS  SUBSEGMENT_NAME_6,
count([LTC_6_Healthy_Well]) AS HEALTHY_WELL6_,
count([LTC_6_Diabetes]) AS DIABETES_6,
count([LTC_6_Epilepsy]) AS EPILEPSY_6,
count([f_date_Subsegment_Combination_Id]) AS f_SUBSEGMENT, 
count([LTC_f_Combination_Code]) AS f_SUBSEGMENT_CODE, 
count([LTC_f_Combination_Name]) AS  f_SUBSEGMENT_NAME,
count([LTC_f_Healthy_Well]) AS f_HEALTHY_WELL,
count([LTC_f_Diabetes]) AS f_DIABETES,
count([LTC_f_Epilepsy]) AS f_EPILEPSY
	  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_R_19_April] 


	  SELECT count([NHS_Number]) AS NHS_NUMBER, 
count([outcome_12_Subsegment_Combination_Id]) AS SUBSEGMENT_12, 
count([LTC_12_Combination_Code]) AS SUBSEGMENT_CODE_12, 
count([LTC_12_Combination_Name]) AS  SUBSEGMENT_NAME_12,
count([LTC_12_Healthy_Well]) AS HEALTHY_WELL6_,
count([LTC_12_Diabetes]) AS DIABETES_12,
count([LTC_12_Epilepsy]) AS EPILEPSY_12,
count([f_date_Subsegment_Combination_Id]) AS f_SUBSEGMENT, 
count([LTC_f_Combination_Code]) AS f_SUBSEGMENT_CODE, 
count([LTC_f_Combination_Name]) AS  f_SUBSEGMENT_NAME,
count([LTC_f_Healthy_Well]) AS f_HEALTHY_WELL,
count([LTC_f_Diabetes]) AS f_DIABETES,
count([LTC_f_Epilepsy]) AS f_EPILEPSY
	  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_12_R_19_April] 


	  SELECT count([NHS_Number]) AS NHS_NUMBER, 
count([outcome_18_Subsegment_Combination_Id]) AS SUBSEGMENT_18, 
count([LTC_18_Combination_Code]) AS SUBSEGMENT_CODE_18, 
count([LTC_18_Combination_Name]) AS  SUBSEGMENT_NAME_18,
count([LTC_18_Healthy_Well]) AS HEALTHY_WELL6_,
count([LTC_18_Diabetes]) AS DIABETES_18,
count([LTC_18_Epilepsy]) AS EPILEPSY_18,
count([f_date_Subsegment_Combination_Id]) AS f_SUBSEGMENT, 
count([LTC_f_Combination_Code]) AS f_SUBSEGMENT_CODE, 
count([LTC_f_Combination_Name]) AS  f_SUBSEGMENT_NAME,
count([LTC_f_Healthy_Well]) AS f_HEALTHY_WELL,
count([LTC_f_Diabetes]) AS f_DIABETES,
count([LTC_f_Epilepsy]) AS f_EPILEPSY
	  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_18_R_19_April] 


	  SELECT count([NHS_Number]) AS NHS_NUMBER, 
count([outcome_24_Subsegment_Combination_Id]) AS SUBSEGMENT_24, 
count([LTC_24_Combination_Code]) AS SUBSEGMENT_CODE_24, 
count([LTC_24_Combination_Name]) AS  SUBSEGMENT_NAME_24,
count([LTC_24_Healthy_Well]) AS HEALTHY_WELL6_,
count([LTC_24_Diabetes]) AS DIABETES_24,
count([LTC_24_Epilepsy]) AS EPILEPSY_24,
count([f_date_Subsegment_Combination_Id]) AS f_SUBSEGMENT, 
count([LTC_f_Combination_Code]) AS f_SUBSEGMENT_CODE, 
count([LTC_f_Combination_Name]) AS  f_SUBSEGMENT_NAME,
count([LTC_f_Healthy_Well]) AS f_HEALTHY_WELL,
count([LTC_f_Diabetes]) AS f_DIABETES,
count([LTC_f_Epilepsy]) AS f_EPILEPSY
	  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_24_R_19_April] 
