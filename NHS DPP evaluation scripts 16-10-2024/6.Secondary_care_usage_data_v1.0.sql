-- Document author Izzy Hatfield 02.03.2023 --
-- To create dataframe with secondary care usage variables --

SELECT Der_Pseudo_NHS_Number
,Arrival_Date
INTO [ ].[dbo].[NDPP_IH_AEA_attendances]
FROM [ ].[dbo].[tbl_Data_SEM_AEA]
WHERE Arrival_Date BETWEEN '2015-01-01 00:00:00.000' AND '2020-01-01 00:00:00.000'

SELECT Der_Pseudo_NHS_Number
,Admission_Date
,Discharge_Date
,Der_Management_Type
INTO [ ].[dbo].[NDPP_IH_APCS_attendances]
FROM [ ].[dbo].[tbl_Data_SEM_APCS]
WHERE Admission_Date BETWEEN '2015-01-01 00:00:00.000' AND '2020-01-01 00:00:00.000'

SELECT Der_Pseudo_NHS_Number
,Appointment_Date
,Der_Appointment_Type
,Attendance_Status
INTO [ ].[dbo].[NDPP_IH_OPA_attendances]
FROM [ ].[dbo].[tbl_Data_SEM_OPA]
WHERE Appointment_Date BETWEEN '2015-01-01 00:00:00.000' AND '2020-01-01 00:00:00.000'

ALTER TABLE [ ].[dbo].[NDPP_IH_AEA_attendances]
ADD Admission_Date date
,Discharge_Date date
,Der_Management_Type varchar
,Appointment_Date date
,Der_Appointment_Type varchar 
,Attendance_Status varchar

ALTER TABLE [ ].[dbo].[NDPP_IH_APCS_attendances]
ADD Arrival_Date date
,Appointment_Date date
,Der_Appointment_Type varchar
,Attendance_Status varchar

ALTER TABLE [ ].[dbo].[NDPP_IH_OPA_attendances]
ADD Admission_Date date
,Discharge_Date date
,Der_Management_Type varchar
,Arrival_Date date

SELECT COUNT (Arrival_Date)
FROM [ ].[dbo].[NDPP_IH_AEA_attendances]�

SELECT COUNT (Admission_Date)
FROM [ ].[dbo].[NDPP_IH_APCS_attendances]�


SELECT COUNT (Appointment_Date)
FROM [ ].[dbo].[NDPP_IH_OPA_attendances]�


SELECT COUNT (NHS_Number_Flag)
FROM [ ].[dbo].[Multimorbidityphase3_PC_IH_19_April]�


-- checking size of apce vs apcs
SELECT Der_Pseudo_NHS_Number
,Admission_Date
,Discharge_Date
,Der_Management_Type
INTO [ ].[dbo].[NDPP_IH_APCE_attendances]
FROM [ ].[dbo].[tbl_Data_SEM_APCE]
WHERE Admission_Date BETWEEN '2015-01-01 00:00:00.000' AND '2020-01-01 00:00:00.000'

SELECT COUNT (Admission_Date)
FROM [ ].[dbo].[NDPP_IH_APCE_attendances]


SELECT Der_Pseudo_NHS_Number, Arrival_Date, Admission_date, Discharge_Date, Der_Management_Type, Appointment_Date, Der_Appointment_Type, Attendance_Status
INTO [ ].[dbo].[NDPP_IH_AEA_APCS_OPA]
FROM [ ].[dbo].[NDPP_IH_AEA_attendances]
UNION
SELECT Der_Pseudo_NHS_Number, Arrival_Date, Admission_date, Discharge_Date, Der_Management_Type, Appointment_Date, Der_Appointment_Type, Attendance_Status
FROM [ ].[dbo].[NDPP_IH_APCS_attendances]
UNION
SELECT Der_Pseudo_NHS_Number, Arrival_Date, Admission_date, Discharge_Date, Der_Management_Type, Appointment_Date, Der_Appointment_Type, Attendance_Status
FROM [ ].[dbo].[NDPP_IH_OPA_attendances]



SELECT d.NHS_Number_Flag
,t.Arrival_Date
,t.Admission_Date
,t.Discharge_Date
,t.Der_Management_Type
,t.Appointment_Date
,t.Der_Appointment_Type
,t.Attendance_Status
INTO [ ].[dbo].[NDPP_IH_AEA_APCS_OPA_NDPPpop_19_April]
FROM [ ].dbo.[Multimorbidityphase3_PC_IH_19_April] d
LEFT JOIN [ ].[dbo].[NDPP_IH_AEA_APCS_OPA] t
ON d.NHS_Number_Flag= t.Der_Pseudo_NHS_Number


-----------------------------------------------------------------------------------
DROP TABLE IF EXISTS  [ ].[dbo].[NDPP_IH_AEA_attendances]
DROP TABLE IF EXISTS  [ ].[dbo].[NDPP_IH_APCS_attendances]
DROP TABLE IF EXISTS  [ ].[dbo].[NDPP_IH_OPA_attendances]
DROP TABLE IF EXISTS  [ ].[dbo].[NDPP_IH_AEA_APCS_OPA]
DROP TABLE IF EXISTS  [ ].[dbo].[NDPP_IH_AEA_APCS_OPA_NDPPpop]

SELECT COUNT (Der_Pseudo_NHS_Number)
FROM [ ].[dbo].[NDPP_IH_AEA_APCS_OPA]


SELECT COUNT (NHS_Number_Flag)
FROM [ ].[dbo].[NDPP_IH_AEA_APCS_OPA_NDPPpop_19_April]



--file to bring through to data science machine [ ].[dbo].[NDPP_IH_AEA_APCS_OPA_NDPPpop] contains all the AEA APCS OPA attendances for individuals in the NDH sample