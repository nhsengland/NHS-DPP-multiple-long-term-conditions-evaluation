---NDPP PROJECT. DERIVING A NEW TYPE II DIABETES MEASURE---
---Code author: Izzy Hatfield & Paul Chappell---

---- SELECTING ONLY PEOPLE FROM LATEST AUDIT----
SELECT [NCDR_Core_IDENT]
      ,[Pseudo_NHS_Number]
      ,[AUDIT_YEAR]
      ,[AGE]
      ,[DERIVED_CLEAN_SEX]
      ,[DERIVED_CLEAN_BIRTH_YEAR]
      ,[DERIVED_CLEAN_ETHNICITY]
      ,[CLEAN_DIAGNOSIS_DATE]
      ,[DERIVED_CLEAN_DIAGNOSIS_YEAR]
      ,[DERIVED_CLEAN_DIABETES_TYPE]
      ,[SMOKING_DATE]
      ,[SMOKING]
      ,[Frailty]
      ,[Frailty_Date]
	  INTO [ ].[dbo].[Multimorbidityphase3_NDA_Core_Latest_Audit]
FROM  [ ].[dbo].[Multimorbidityphase3_NDA_Core]
WHERE [AUDIT_YEAR] LIKE '2022_23_E1'


---CHECK NULLS ON VARIOUS VARIABLES IN NDA CORE---

SELECT count ([Pseudo_NHS_Number]) AS NHS_NO,
count ([NCDR_Core_IDENT]) AS CORE_IDENT,
count ([CLEAN_DIAGNOSIS_DATE]) AS DIAGNOSIS_DATE
FROM [ ].[dbo].[Multimorbidityphase3_NDA_Core_Latest_Audit]


---- Testing 1 patient per row in NDA CORE----

SELECT [Pseudo_NHS_Number]
FROM [ ].[dbo].[Multimorbidityphase3_NDA_Core_Latest_Audit]
GROUP BY [Pseudo_NHS_Number]
HAVING COUNT(1) > 1

---MERGE NDA CORE AND ANALSYIS TABLE FROM PREVIOUS SCRIPT---

SELECT [NHS_Number]
      ,[Person_Id]
	  ,[Pseudo_NHS_Number] 
      ,[new_f_date]
      ,[f_date_plus_6]
	  ,[DERIVED_CLEAN_SEX]
      ,[DERIVED_CLEAN_BIRTH_YEAR]
      ,[DERIVED_CLEAN_ETHNICITY]
      ,[CLEAN_DIAGNOSIS_DATE]
      ,[DERIVED_CLEAN_DIAGNOSIS_YEAR]
      ,[DERIVED_CLEAN_DIABETES_TYPE]
      ,[SMOKING_DATE]
      ,[SMOKING]
  INTO [ ].[dbo].[Multimorbidityphase3_outcomes_6_R_T2D_19_APRIL] 
  FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_R_19_APRIL] o
  LEFT JOIN [ ].[dbo].[Multimorbidityphase3_NDA_Core_Latest_Audit] c ON  o.[NHS_Number] = c.[Pseudo_NHS_Number] 
  
 --- CHECK NUMBERS WHO ARE PRESENT IN BOTH ---

SELECT 
COUNT ([NHS_Number]) AS NHS_number, count([f_date_plus_6])AS f_date_plus_6, count([CLEAN_DIAGNOSIS_DATE]) AS Diag_date,  
count([Pseudo_NHS_Number]) AS NHS_Number_Core
FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_R_T2D_19_APRIL] 
 

 SELECT [Pseudo_NHS_Number]
FROM [ ].[dbo].[Multimorbidityphase3_outcomes_6_R_T2D_19_APRIL] 
GROUP BY [Pseudo_NHS_Number]
HAVING COUNT(1) > 1
