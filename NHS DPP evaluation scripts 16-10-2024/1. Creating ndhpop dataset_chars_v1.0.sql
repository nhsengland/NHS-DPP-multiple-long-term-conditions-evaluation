---Document Author Neha Kashyap, edits by Paul Chappell, edits by Izzy Hatfield. Versions of tables used at 19th April 2023---
--To create a table which has all NDPP data and NDH data ---

USE [ ];

SELECT Der_Pseudo_NHS_Number
,Date_Of_Referral_Receipt
,Referral_HbA1C_Reading
,[Delivery Mode]
,Date_Of_Discharge
,Source_of_Referral
,Finishers_new
,Completers_New
,Date_Reached_IV
,Provider_Name
,[Ethnic Group clean]
,[Age_Group_clean]
,[GenderDescription clean] as [sexDescription_clean]
,(SELECT FORMAT ( Date_Of_Referral_Receipt, 'yyyyMMdd') AS DATE)  as n_DATEID                       ---Reformat the date
,(select FORMAT( EOMONTH(Date_Of_Referral_Receipt), 'yyyyMMdd') AS DATE) as End_dateid             --- End of the month date format
INTO [ ].[dbo].[NDPP_Analysis_Table_duplicates_included_PC_IH_19_April] 
FROM [ ].[dbo].[NDPP_Analysis_Table]
WHERE  ISNULL(Date_of_Discharge, GETDATE()) <> '01 JAN 9999' 
       AND Der_Pseudo_NHS_Number IS NOT NULL


--------------------------------------------------------------------------------------------------------------------------------------------------

---COUNT NUMBER OF INDIVIDUALS---

SELECT count(*) 
FROM  [ ].[dbo].[NDPP_Analysis_Table_duplicates_included_PC_IH_19_April] 


---CALCULATING FREQUENCIES OF CASES WHERE MORE THAN ONE REFERRAL---

SELECT count(Der_Pseudo_NHS_Number) AS N_ENTRIES
INTO [ ].[dbo].[NDPP_Analysis_Table_duplicates_PC_IH]
FROM  [ ].[dbo].[NDPP_Analysis_Table_duplicates_included_PC_IH_19_April] 
GROUP BY Der_Pseudo_NHS_Number
HAVING COUNT(1) > 1


---GENERATING FREQUENCY TABLE---
 Select N_ENTRIES, count(*)
 From   [ ].[dbo].[NDPP_Analysis_Table_duplicates_PC_IH]
 Group By  N_ENTRIES


------NDH DATA TABLE (NDH table is named tbl_DPP_GoldenRecord) with column date rolled forward to end of month ------

SELECT  Pseudo_NHS_Number
	   ,MIN(BirthYear) As birth_year
	   ,MIN(IMDRank) As imd_rank
	   ,MIN(Sex) As sex
	   ,MIN(Ethnicity) as ethnicity
       ,MIN([NonDiabeticHyperglycaemiaDiagMinDate]) As NDHMinDate
	   ,MIN([NonDiabeticHyperglycaemiaDiagMaxDate]) As NDHMaxDate				
INTO [ ].[dbo].[NDH_dates_PC_IH_19_April]
FROM    [ ].[qa].[tbl_DPP_GoldenRecord]
WHERE Pseudo_NHS_Number is not null and NonDiabeticHyperglycaemiaDiagMinDate is not null
GROUP BY Pseudo_NHS_Number



---HOW MANY ROWS?---

select count(*)
FROM  [ ].[dbo].[NDH_dates_PC_IH_19_April]


----- Adding column for end of the month rolled forward date---
SELECT  Pseudo_NHS_Number
		,NDHMaxDate
	      , NDHMinDate
		  ,birth_year
		  ,imd_rank
		  ,sex
		  ,ethnicity
          ,(SELECT FORMAT( NDHMinDate, 'yyyyMMdd') as date)  as n_DATEIDNDH
		 ,(SELECT FORMAT( EOMONTH(NDHMinDate), 'yyyyMMdd') as date) as End_dateidNDH
INTO [ ].[dbo].[NDH_dates_rolled_forward_PC_IH_19_April]
FROM  [ ].[dbo].[NDH_dates_PC_IH_19_April]
 ------------------------------------------------------------------------------------------------------------------------------------    

---- Testing 1patient per row for ndh----
SELECT Pseudo_NHS_Number
FROM [ ].[dbo].[NDH_dates_rolled_forward_PC_IH_19_April]
GROUP BY Pseudo_NHS_Number
HAVING COUNT(1) > 1

		

---HOW MANY ROWS?---


select count(*)
FROM [ ].[dbo].[NDH_dates_rolled_forward_PC_IH_19_April]


-----------------------------------------------------------------------------------------------
		
----NDPP + NDH Data----
SELECT ISNULL(Der_Pseudo_NHS_Number,Pseudo_NHS_Number) As NHS_Number_Flag -- TO JOIN THE TABLES
       ,ISNULL(End_dateid,End_dateidNDH) As Date_NDPP_NDH_FLAG -- TO JOIN OBH FACT MODEL ON DATE
	   ,ISNULL(n_dateid,h.n_DATEIDNDH) As Date_ID_Flag --- To join the OBH DATE
	   ,N.Der_Pseudo_NHS_Number
       ,N.Date_Of_Referral_Receipt
       ,N. Referral_HbA1C_Reading
       ,N.[Delivery Mode]
       ,N. Date_Of_Discharge
      ,N.Source_of_Referral
      ,N.Finishers_new
      ,N.Completers_New
      ,N.n_DATEID
      ,N.End_dateid
	  ,N.Date_Reached_IV
	  ,N.Provider_Name
	  ,N.[Ethnic Group clean]
	  ,N.[Age_Group_clean]
	  ,N.[SexDescription clean]
      ,h.Pseudo_NHS_Number
      ,h.NDHMinDate
		,h.NDHMaxDate
      ,h.n_DATEIDNDH
      ,h.End_dateidNDH
	  ,h.birth_year
	  ,h.imd_rank
	  ,h.sex
	  ,h.ethnicity
INTO [ ].dbo.Multimorbidityphase3_PC_IH_19_April
FROM  [ ].[dbo].[NDPP_Analysis_Table_duplicates_included_PC_IH_19_April]  N
FULL OUTER JOIN  [ ].[dbo].[NDH_dates_rolled_forward_PC_IH_19_April] h
             ON n.Der_Pseudo_NHS_Number =h.Pseudo_NHS_Number

---Count N in joined table---

 select count(*)
FROM [ ].dbo.Multimorbidityphase3_PC_IH_19_April


-------------------------------