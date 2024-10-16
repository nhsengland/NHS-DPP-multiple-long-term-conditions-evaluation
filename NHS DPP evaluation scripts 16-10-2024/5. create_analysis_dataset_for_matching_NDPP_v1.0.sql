--- NDPP Evaluation. Code author: Paul Chappell ---
--- Add variables from Seg. dataset to prepare for matching ---


----How many cases in [Multimorbidityphase3_PC_IH_after_clean] ---

SELECT count ([nhs_number])
from [ ].[dbo].[Multimorbidityphase3_PC_IH_after_clean_19_April]

----How many cases in [Multimorbidityphase3_PC_IH_after_clean] that fall between '20160401' AND '20190831'? ---

SELECT count ([nhs_number])
from [ ].[dbo].[Multimorbidityphase3_PC_IH_after_clean_19_April]
  WHERE [h_date] BETWEEN '20160401' AND '20190831'
 
 
 ----How many cases in [Multimorbidityphase3_PC_IH_after_clean] that fall between '20160401' AND '20190831'? ---

SELECT count ([nhs_number])
from [ ].[dbo].[Multimorbidityphase3_PC_IH_after_clean_19_April]
  WHERE [h_date] BETWEEN '20160401' AND '20200229'
  

---Merge with demographic information---


SELECT 
h.[nhs_number] AS [NHS_Number_Flag]
, [h_date], [f_date], [provider_name],[pop_group],[ndh_date_valid]    
	, p.[PERSON_ID]
    ,  p.[NHS_NUMBER]
     , p.[DATE_OF_BIRTH]
      ,p.[DATE_OF_DEATH]
      ,p.[GENDER_DESCRIPTION]
      ,p.[ETHNICITY_CATEGORY_DESCRIPTION]
      ,p.[PSEUDO_UPRN]
	  ,p.[IMD_QUINTILE]
      ,p.[IMD_DECILE]
	  INTO [ ].[dbo].[Multimorbidityphase3merge_PC_demographics] 
	FROM		[ ].[dbo].[Multimorbidityphase3_PC_IH_after_clean_19_April] h
	LEFT JOIN [ ].[obh].[DIM_PERSON] p ON h.[nhs_number] = p.NHS_NUMBER
	
	
 ---Testing for missing data---
 
   
 select count([NHS_Number_Flag]) as ID, count([PERSON_ID]) AS PERSON_ID, count([DATE_OF_BIRTH]) as DOB, count([GENDER_DESCRIPTION]) AS GENDER,
 count([ETHNICITY_CATEGORY_DESCRIPTION]) AS ETHNICITY, count(IMD_QUINTILE) AS IMD
 FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics] 
 

---SELECT ALL CASES WHERE MATERNAL FLAG IS FLAGGED BETWEEN 20160401 and 20190831---
	
	SELECT f.[Date_Id]
      ,f.[Person_Id]
      ,f.[Maternal_Health_Flag]
	,	d.[NHS_Number_Flag]
      ,d.[h_date]
	  INTO [ ].[dbo].[Multimorbidityphase3merge_pregnant_flag_PC]
	  	FROM [ ].[obh].[FACT_MODEL] f
		FULL OUTER JOIN  [ ].[dbo].[Multimorbidityphase3merge_PC_demographics] d 
	ON d.[PERSON_ID] = f.[Person_Id]
	  WHERE [Date_Id] BETWEEN '20160401' AND '20190831' 
	  AND d.[NHS_Number_Flag] IS NOT NULL 
	  AND Maternal_Health_Flag = 1
	  	  ORDER BY [Person_Id], [Date_Id]
	
---Identify duplicates and therefore get a list of everyone pregnant at any point during the study---

SELECT [NHS_Number_Flag] 
      ,[Maternal_Health_Flag]
	  ,COUNT(*) AS [CNT]
INTO [ ].[dbo].[Multimorbidityphase3merge_pregnant_flag_PC2]
FROM [ ].[dbo].[Multimorbidityphase3merge_pregnant_flag_PC]
GROUP BY [NHS_Number_Flag] 
      ,[Maternal_Health_Flag]
HAVING COUNT(*) > 1
ORDER BY [CNT]


---Add pregnant at any point variable to sample---


SELECT
d.[NHS_Number_Flag]
      ,[h_date], [f_date], [provider_name],[pop_group],[ndh_date_valid]    
	, [PERSON_ID]
    ,  [NHS_NUMBER]
     , [DATE_OF_BIRTH]
      ,[DATE_OF_DEATH]
      ,[GENDER_DESCRIPTION]
      ,[ETHNICITY_CATEGORY_DESCRIPTION]
      ,[PSEUDO_UPRN]
	  ,[IMD_QUINTILE]
      ,[IMD_DECILE]
	  , [Maternal_Health_Flag]
	  INTO [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_pregnant]
	FROM	[ ].[dbo].[Multimorbidityphase3merge_PC_demographics] d
	LEFT JOIN [ ].[dbo].[Multimorbidityphase3merge_pregnant_flag_PC2] preg ON d.[NHS_Number_Flag] = preg.[NHS_Number_Flag] 
	

 ---Testing for missing data---
 
   
 select count([NHS_Number_Flag]) as ID, count([DATE_OF_BIRTH]) as DOB, count([GENDER_DESCRIPTION]) AS GENDER,
 count([ETHNICITY_CATEGORY_DESCRIPTION]) AS ETHNICITY, count(IMD_QUINTILE) AS IMD, count(Maternal_Health_Flag) AS MATERNAL, count([h_date]) AS h_date
 FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_pregnant]
 
	
----- Adding column for end of the month rolled forward date---

	select
	convert(varchar, h_date, 23) AS h_date_date_format
	, convert(varchar, h_date, 112) AS h_date_yyyyMMdd
	,[NHS_Number_Flag]
      ,[h_date], [f_date], [provider_name],[pop_group],[ndh_date_valid]    
          ,(SELECT FORMAT( h_date, 'yyyyMMdd') as date)  as h_datePyyMMdd_2
		 ,(SELECT FORMAT( EOMONTH(h_date), 'yyyyMMdd') as date) as h_date_end_month
	, [PERSON_ID]
    ,  [NHS_NUMBER]
     , [DATE_OF_BIRTH]
      ,[DATE_OF_DEATH]
      ,[GENDER_DESCRIPTION]
      ,[ETHNICITY_CATEGORY_DESCRIPTION]
      ,[PSEUDO_UPRN]
	  ,[IMD_QUINTILE]
      ,[IMD_DECILE]
	  , [Maternal_Health_Flag]
	  INTO [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_pregnant_date_fixed]
	FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_pregnant]
		
---Testing missing values---

		 select count([h_date]) as H_date, count([DATE_OF_BIRTH]) as DOB, count(h_datePyyMMdd_2) as H_date_new_format, count(h_date_end_month) as END_MONTH_H,
		  count(IMD_QUINTILE) AS IMD, count(Maternal_Health_Flag) AS MATERNAL 
 FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_pregnant_date_fixed]


---- Testing 1patient per row for ndh----

SELECT [NHS_Number_Flag]
FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_pregnant_date_fixed]
GROUP BY [NHS_Number_Flag]
HAVING COUNT(1) > 1


	---Adding in information from the segmentation fact_model based on dates. ---

	SELECT
h_date_yyyyMMdd
	,[NHS_Number_Flag]
      ,[h_date], [f_date], [provider_name],[pop_group],[ndh_date_valid]    
,h_datePyyMMdd_2
 ,h_date_end_month
	, d.[PERSON_ID]
    ,  [NHS_NUMBER]
     , [DATE_OF_BIRTH]
      ,[DATE_OF_DEATH]
      ,[GENDER_DESCRIPTION]
      ,[ETHNICITY_CATEGORY_DESCRIPTION]
      ,[PSEUDO_UPRN]
	  ,[IMD_QUINTILE]
      ,[IMD_DECILE]
	   ,d.[Maternal_Health_Flag]
	  ,f.[Date_Id]
      ,f.[Age_Id]
      ,f.[Gp_Id]
      ,f.[Segment_Combination_Id]
      ,f.[Subsegment_Combination_Id]
      ,f.[Maternal_Health_Flag] AS [Maternal_Health_Flag_at_h_date]
      ,f.[Additional_Characteristic_Combination_ID]
	INTO  [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model]
	FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_pregnant_date_fixed] d
	LEFT JOIN [ ].[obh].[FACT_MODEL] f ON d.[h_date_end_month] = f.[Date_Id] AND d.[Person_Id] = f.[Person_Id]
	
	
---Testing for missing data---

  select count([NHS_Number_Flag]) as ID, count([DATE_OF_BIRTH]) as DOB, count([Maternal_Health_Flag]) AS MATERNAL, count([Maternal_Health_Flag_at_h_date]) AS MATERNAL_AT_H_DATE,
 count([PERSON_ID]) AS PERSON_ID, count([Subsegment_Combination_Id]) AS SUBSEG_ID
 FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model]



---Maternal_Health_Flag (at h-index point) ---

select  count([Maternal_Health_Flag_at_h_date]) AS N_MATERNAL
 FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model]
 GROUP BY ([Maternal_Health_Flag_at_h_date])

	
	---Adding in gp practice code---
	
	SELECT 
[NHS_Number_Flag]
      ,[h_date], [f_date], [provider_name],[pop_group],[ndh_date_valid]    
	  ,h_datePyyMMdd_2
 ,h_date_end_month
    ,  [NHS_NUMBER]
     , [DATE_OF_BIRTH]
      ,[DATE_OF_DEATH]
      ,[GENDER_DESCRIPTION] AS GENDER_PERSON 
      ,[ETHNICITY_CATEGORY_DESCRIPTION] AS ETHNICITY_PERSON
      ,[PSEUDO_UPRN]
	  	  ,f.[IMD_QUINTILE] AS [IMD_QUINTILE_PERSON]
     ,f.[IMD_DECILE] AS [IMD_DECILE_PERSON]
	  ,[Date_Id]
      ,[Person_Id]
      ,[Age_Id]
      ,f.[Gp_Id]
      ,[Segment_Combination_Id]
      ,[Subsegment_Combination_Id]
      ,[Maternal_Health_Flag]
	  ,[Maternal_Health_Flag_at_h_date]
      ,[Additional_Characteristic_Combination_ID]
	  ,[GP_ORG_CODE]
      ,[GP_NAME]
      ,[GP_POST_CODE]
	  INTO [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model_GP_code]
	FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model] f
	LEFT JOIN  [ ].[obh].[DIM_GP_PRACTICE] gp ON f.[Gp_Id] = gp. [Gp_Id]
	
---Testing for missing data---

  select count([NHS_Number_Flag]) as ID, count([GP_ORG_CODE]) as GP_CODE
 FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model_GP_code]


	---Adding in segments---
		
		SELECT 
[NHS_Number_Flag]
      ,[h_date], [f_date], [provider_name],[pop_group],[ndh_date_valid]    
	  ,h_datePyyMMdd_2
 ,h_date_end_month
    ,  [NHS_NUMBER]
     , [DATE_OF_BIRTH]
      ,[DATE_OF_DEATH]
      ,GENDER_PERSON 
      ,ETHNICITY_PERSON
      ,[PSEUDO_UPRN]
	  ,[IMD_QUINTILE_PERSON]
     ,[IMD_DECILE_PERSON]
	  ,[Date_Id]
      ,[Person_Id]
      ,[Age_Id]
      ,[Gp_Id]
      ,gp.[Segment_Combination_Id]
      ,[Subsegment_Combination_Id]
      ,[Maternal_Health_Flag]
	  ,[Maternal_Health_Flag_at_h_date]
      ,[Additional_Characteristic_Combination_ID]
	  ,[GP_ORG_CODE]
      ,[GP_NAME]
      ,[GP_POST_CODE]
	  ,[COMBINATION_CODE] AS [SEG_COMBINATION_CODE]
      ,[COMBINATION_NAME] AS [SEG_COMBINATION_NAME]
      ,[HEALTHY_WELL] AS [SEG_HEALTHY_WELL]
      ,[LTC] AS [SEG_LTC]
      ,[DISABILITY] AS [SEG_DISABILITY]
      ,[FRAILTY_DEMENTIA] AS [SEG_FRAILTY_DEMENTIA]
      ,[ORGAN_FAILURE] AS [SEG_ORGAN_FAILURE]
      ,[INCURABLE_CANCER] AS [SEG_INCURABLE_CANCER]
      ,[HIGHEST_ACUITY_SEGMENT_CODE] AS [SEG_HIGHEST_ACUITY_SEGMENT_CODE]
      ,[HIGHEST_ACUITY_SEGMENT_NAME] AS [SEG_HIGHEST_ACUITY_SEGMENT_NAME]
	  INTO [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model_GP_code_seg]
	FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model_GP_code] gp
	LEFT JOIN [ ].[obh].[DIM_SEGMENT_COMBINATIONS] s  ON gp.[Segment_Combination_Id] = s.[COMBINATION_ID]
	
	
	---Testing for missing data---
	select count([NHS_Number_Flag]) as ID, count([SEG_COMBINATION_NAME]) as COMBINATION_NAME
 FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model_GP_code_seg]
	

	
---Adding in LTCs and LTC count---


				SELECT 
[NHS_Number_Flag]
      ,[h_date], [f_date], [provider_name],[pop_group],[ndh_date_valid]    
	  ,h_datePyyMMdd_2
 ,h_date_end_month
    ,  [NHS_NUMBER]
     , [DATE_OF_BIRTH]
      ,[DATE_OF_DEATH]
      ,GENDER_PERSON 
      ,ETHNICITY_PERSON
      ,[PSEUDO_UPRN]
	  ,[IMD_QUINTILE_PERSON]
     ,[IMD_DECILE_PERSON]
	  ,[Date_Id]
      ,[Person_Id]
      ,[Age_Id]
      ,[Gp_Id]
      ,seg.[Segment_Combination_Id]
      ,seg.[Subsegment_Combination_Id]
      ,[Maternal_Health_Flag]
	  ,[Maternal_Health_Flag_at_h_date]
      ,[Additional_Characteristic_Combination_ID]
	  ,[GP_ORG_CODE]
      ,[GP_NAME]
      ,[GP_POST_CODE]
	  ,[SEG_COMBINATION_CODE]
      ,[SEG_COMBINATION_NAME]
      ,[SEG_HEALTHY_WELL]
      ,[SEG_LTC]
      ,[SEG_DISABILITY]
      ,[SEG_FRAILTY_DEMENTIA]
      ,[SEG_ORGAN_FAILURE]
      ,[SEG_INCURABLE_CANCER]
      ,[SEG_HIGHEST_ACUITY_SEGMENT_CODE]
      ,[SEG_HIGHEST_ACUITY_SEGMENT_NAME]
	  ,ltc.[Subsegment_Combination_ID] AS [LTC_Combination_ID]
      ,[Subsegment_Combination_Code] AS [LTC_Combination_Code]
      ,[Subsegment_Combination_Name] AS [LTC_Combination_Name]
      ,[Healthy_Well] AS [LTC_Healthy_Well]
      ,[Maternal_and_Infant_Health] AS [LTC_Maternal_and_Infant_Health]
      ,[Alcohol_Dependence] AS [LTC_Alcohol_Dependence]
      ,[Asthma] AS [LTC_Asthma]
      ,[Atrial_Fibrillation] AS [LTC_Atrial_Fibrillation]
      ,[Bronchiectasis] AS [LTC_Bronchiectasis]
      ,[Cancer] AS [LTC_Cancer]
      ,[Cerebrovascular_Disease] AS [LTC_Cerebrovascular_Disease]
      ,[Chronic_Kidney_Disease] AS [LTC_Chronic_Kidney_Disease]
      ,[Chronic_Liver_Disease] AS [LTC_Chronic_Liver_Disease]
      ,[Chronic_Pain] AS [LTC_Chronic_Pain]
      ,[COPD] AS [LTC_COPD]
      ,[Coronary_Heart_Disease] AS [LTC_Coronary_Heart_Disease]
      ,[Cystic_Fibrosis] AS [LTC_Cystic_Fibrosis]
      ,[Depression] AS [LTC_Depression]
      ,[Diabetes] AS [LTC_Diabetes]
      ,[Epilepsy] AS [LTC_Epilepsy]
      ,[Heart_Failure] AS [LTC_Heart_Failure]
      ,[Hypertension] AS [LTC_Hypertension]
      ,[Inflammatory_Bowel_Disease] AS [LTC_Inflammatory_Bowel_Disease]
      ,[Multiple_Sclerosis] AS [LTC_Multiple_Sclerosis]
      ,[Osteoarthritis] AS [LTC_Osteoarthritis]
      ,[Osteoporosis] AS [LTC_Osteoporosis]
      ,[Parkinsons_Disease] AS [LTC_Parkinsons_Disease]
      ,[Peripheral_Vascular_Disease] AS [LTC_Peripheral_Vascular_Disease]
      ,[Pulmonary_Heart_Disease] AS [LTC_Pulmonary_Heart_Disease]
      ,[Rheumatoid_Arthritis] AS [LTC_Rheumatoid_Arthritis]
      ,[Sarcoidosis] AS [LTC_Sarcoidosis]
      ,[Serious_Mental_Illness] AS [LTC_Serious_Mental_Illness]
      ,[Sickle_Cell_Disease] AS [LTC_Sickle_Cell_Disease]
      ,[Autism] AS [LTC_Autism]
      ,[Learning_Disability] AS [LTC_Learning_Disability]
      ,[Physical_Disability] AS [LTC_Physical_Disability]
      ,[Dementia] AS [LTC_Dementia]
      ,[Intermediate_Frailty_Risk_HFRS] AS [LTC_Intermediate_Frailty_Risk_HFRS]
      ,[High_Frailty_Risk_HFRS] AS [LTC_High_Frailty_Risk_HFRS]
      ,[End_Stage_Renal_Failure] AS [LTC_End_Stage_Renal_Failure]
      ,[Severe_Interstitial_Lung_Disease] AS [LTC_Severe_Interstitial_Lung_Disease]
      ,[Liver_Failure] AS [LTC_Liver_Failure]
      ,[Neurological_Organ_Failure] AS [LTC_Neurological_Organ_Failure]
      ,[Severe_COPD] AS [LTC_Severe_COPD]
      ,[Severe_Heart_Failure] AS [LTC_Severe_Heart_Failure]
      ,[Incurable_Cancer] AS [LTC_Incurable_Cancer]
	  ,[CONDITION_COUNT] AS [LTC_CONDITION_COUNT]
	
	  INTO [ ].[dbo].[Multimorbidityphase3merge_for_matching_19_April]
	FROM [ ].[dbo].[Multimorbidityphase3merge_PC_demographics_fact_model_GP_code_seg] seg 
	LEFT JOIN [ ].[obh].[DIM_SUBSEGMENT_COMBINATIONS] ltc  ON seg.[Subsegment_Combination_Id] = ltc.[Subsegment_Combination_ID]
	
---Testing for missing values---

	select count([NHS_Number_Flag]) as ID, count(LTC_Combination_ID) AS LTC_COMB, count([LTC_CONDITION_COUNT]) AS [LTC_CONDITION_COUNT],
	count([IMD_QUINTILE_PERSON]) AS IMD
 FROM [ ].[dbo].[Multimorbidityphase3merge_for_matching_19_April]
 

---Drop Pseudo_UPRN variable that was stopping data pulling through to R---
ALTER TABLE [ ].[dbo].[Multimorbidityphase3merge_for_matching_19_April]
DROP COLUMN       [PSEUDO_UPRN]