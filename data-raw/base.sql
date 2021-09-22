/*
INT602 LIS UCD
LIS Dataset
Version 5.2

Created by Steven Buckley
Created on 2nd June 2021


AMENDMENTS:
    08-JUN-2021 :   Added gender proxy based on the applicants title (e.g. Mr = MALE, Mrs = FEMALE)
    08-JUN-2021 :   Linked on SCD2_INDEX_OF_MULTIPLE_DEPRIVATIONS to get the additional scores/ranks/deciles for income and health deprivation
                    Copy of this table has been created in STBUC as the SCD2 copy is not exposed to the DWCP-DALP database link
    08-JUN-2021 :   Added reference to the CENSUS_OUTPUT_AREA linked to the postcode
                    This is based on the latest DALL_REF.POSTCODE_LOOKUP data and therefore some postcodes may not be mapped
    08-JUN-2021 :   Added additional geography linked to the patients postcode
                    The geography ranges are stacked to produce a hierarchy
                        +   LSOA is mapped to postcode
                        +   WARD and CCG are mapped to LSOA
                        +   LAD (Local Authority) is mapped to WARD
                        +   REGION is mapped to LAD
    09-JUN-2021 :   Added fields to show the latest status event for each CASEREF and to include a proxy field for "claim withdrawn"
                    There are two status events for formally withdrawn claims, although many claims essentially sit open if no response from customer
    10-JUN-2021 :   Added a field to show the number of individuals covered by the certificate (applicant + any dependants)
                    The dependants only include partners (not children) and therefore a certificate will only cover 1 or 2 people (excluding errors)
	14-JUN-2021 :   Added some logic to the age calculations to exclude values that are most likely to be errors (under 15 or over 100)
                    Where the age is outside of this range the AGE_AT_REGISTRATION will be replaced with a NULL value
    15-JUN-2021 :   Included process to get the latest certificate information for each CASEREF rather than linking using the LASTASSESSMENTREC
                    This resolves issues where no certificate is being matched when additional assessment activites are taking place once issued
    23-JUN-2021 :   Ensured that the ONS Census classification groups are pulled through to the final output
                    Added a new classification field CERT_GROUP to collate certificates to show the type of support provided
    24-JUN-2021 :   Reordered case statement in "case_status" sub-query.  This will ensure that the correct outcome is captured when there are options
                        + Initially check if cerificate has been printed (issued)
                        + Check for withdrawn cases (this ensure reopened cases are captured and withdrawn after issue are still counted as issued)
                        + If no certificate issued and not withdrawn check when the case was registered to determine if this should be open/abandoned
    28-JUN-2021 :   Added IMD_AVERAGE_SCORE and IMD_RANK to allow general deprivation of areas to be compared.  
                        + Added for the following geographies: Ward / Local Authority / CCG
                        + Please note these figures are for context and are not directly relatable (e.g. a score of 20 is not twice as deprived as a score of 10)
    28-JUN-2021 :   Amended the COMPOSITE_ID field to handle instances where the sub-string being used was blank
                        + For example the composite excluded non-alphabetic characters, however, some names are being recorded as ...
                        + If the substr is blank then the composite should be "na"
    30-JUN-2021 :   Created records for LIS11 responses.  These show as HC3 certificates but as they are above the £128 threshold they offer no support
                        Adjusted the RECTYPE column to show as LIS11 for these records and ensure these are passed to correct value for CERT_GROUP
    30-JUN-2021 :   Added a CERT_LENGTH_MONTHS column to show the number of months between the VALIDFROM and VALIDTO dates
    09-JUL-2021 :   Applied some basic filters and calculated fields based on logic being typically applied by users
                        + Limit to England only (based on postcode linking to English region
                        + Limit to 201504 onwards
                        + Added financial year (based on application month)
                        + Added new outcome classifications
                        + Included clientgroup descriptions
    12-JUL-2021 :   Applied logic to handle potential duplicate records where 2 or more CASEREFs for a COMPOSITE_ID are created on the same date
                        + priority will be granted based on the outcome of the application (e.g HC2 > HC3 > No assistance > Abandoned/Withdrawn > Ongoing)
                        + where an applicant has multiple cases with the same outcome additional tie-breakers will be used to take the record with the most useful data available
    20-JUL-2021 :   Applied an additional classification for the client group (CLIENTGROUP_DESC_FORMAT)
                        + this will combine the "Benefit" and "Other" groups into a single category to reflect the potential overlap
    21-JUL-2021 :   Added identifer for digital applications
                        Digital applications are not classified directly in the database, however assessors are expected/required to add a reference
                        to the digital application as a note.  This is typically stored as a single 8 character string in its own note record.
                        Unfortunately there is no set pattern for the digital reference so any 8 character string is a potential match
                        The only viable option is to include all potential matches and exclude any where the string has appeared in multiple notes, as this
                        assumes that other terms (e.g. payslips, acounts, reissued) are repeatedly used
                        NB: this will mean that some records are incorrectly classified as a digital application if they have a note containing a single (unique) 8 character string
    28-JUL-2021 :   Added details of the date the certificate was issued (based on date PRINT status was created)
                        + this is only for issued certificates, where no certificate/response was issued this will be NULL
    28-JUL-2021 :   Added a field to show the academic year
    04-AUG-2021:    Applied an additional filter to limit to only return records up to 202104
                        + created 4 fields to capture how beneficial partial help, HELP_WITH_BAND_1 / 2 / 3, HELP_WITH_SIGHT_TEST
    [date]      :   [details]


DESCRIPTION:
This script is a potential starting point for a dataset showing LIS data
This will include some of the main common fields used in previous LIS initiatives


NOTES:
This script includes some hard coded logic to say that any application registered prior to 2021 should have been resolved
This is based on data being live as of May-2021.
This will need to be amended to be a suitable date if the data is refreshed.


DEPENDENCIES: (DALP)
    DALP
        LIS.DATAAPPLICANT
        LIS.DATASTATUS
        LIS.DATACERTIFICATE
        LIS.DATACASE
        LIS.CUEXTRACT
        LIS.DATADEPENDANTS
        LIS.DATANOTE
    
        DALL_REF.POSTCODE_LOOKUP
        DALL_REF.ONS_GEOGRAPHY_MAPPING
        DALL_REF.LKP_AGEBANDS
        DALL_REF.ONS_IMD_RANK_AVG_SCORE_BY_AREA
    
    DWCP (via @dwcpb)
        DIM.ONS_POSTCODE_DATA_DIM
        STBUC.SCD2_INDEX_OF_MULTIPLE_DEPRIVATIONS   (to be replaced with reference to SCD2 schema which is not currently available via @dwcpb)
        
    

*/

------------------------------------------------------------------------------------------------------------------------------------------------------
----------SCRIPT START--------------------------------------------------------------------------------------------------------------------------------

CREATE TABLE INT602_FINAL_DATA_V5 as

with


-----SECTION START: POSTCODE CLASSIFICATION-----------------------------------------------------------------------------------------------------------
--find the latest available classification for each postcode
--identify postcode data to find:
--  the IMD decile 
--  the WARD/CCG  (NB other location relationships can be identified
--  
pcd as
(
select  /*+ materialize */
            opdd.POSTCODE,
        --GEOGRAPHY MAPPINGS
            opdd.LSOA_CODE          as PCD_LSOA_CODE,
            ogm1.PARENT_ONS_CODE    as PCD_WARD_CODE,
            ogm1.PARENT_NAME        as PCD_WARD_NAME,
            ogm2.PARENT_ONS_CODE    as PCD_CCG_CODE,
            ogm2.PARENT_ALT_CODE    as PCD_CCG_ID,
            ogm2.PARENT_NAME        as PCD_CCG_NAME,
            ogm3.PARENT_ONS_CODE    as PCD_LAD_CODE, 
            ogm3.PARENT_NAME        as PCD_LAD_NAME,
            ogm4.PARENT_ONS_CODE    as PCD_REGION_CODE, 
            ogm4.PARENT_NAME        as PCD_REGION_NAME,
        --INDICES OF MULTIPLE DEPRIVATION
            iomd.INDEX_OF_MULT_DEPRIV_SCORE,
            iomd.INDEX_OF_MULT_DEPRIV_RANK,
            iomd.INDEX_OF_MULT_DEPRIV_DECILE,
            iomd.INCOME_SCORE,
            iomd.INCOME_RANK,
            iomd.INCOME_DECILE,
            iomd.HEALTH_DEPRIVATION_SCORE,
            iomd.HEALTH_DEPRIVATION_RANK,
            iomd.HEALTH_DEPRIVATION_DECILE,
            iomd.INCOME_DEPRIV_CHILDREN_SCORE,
            iomd.INCOME_DEPRIV_CHILDREN_RANK,
            iomd.INCOME_DEPRIV_CHILDREN_DECILE,
            iomd.INCOME_DEPRIV_ELDERLY_SCORE,
            iomd.INCOME_DEPRIV_ELDERLY_RANK,
            iomd.INCOME_DEPRIV_ELDERLY_DECILE,
        --CENSUS OUTPUT AREAS
            pl.CENSUS_OUTPUT_AREA,
            pl.CENSUS_OUTPUT_AREA_CODE             as SUBGROUP_CODE,
            pl.CENSUS_OUTPUT_AREA_SUBGROUP         as SUBGROUP_NAME,
            substr(pl.CENSUS_OUTPUT_AREA_CODE,1,2) as GROUP_CODE,
            pl.CENSUS_OUTPUT_AREA_GROUP            as GROUP_NAME,
            substr(pl.CENSUS_OUTPUT_AREA_CODE,1,1) as SUPERGROUP_CODE,
            pl.CENSUS_OUTPUT_AREA_SUPERGROUP       as SUPERGROUP_NAME        

from        (
            --find the latest postcode data for each postcode
            select
                        regexp_replace(upper(POSTCODE),'[^A-Z0-9]','') as POSTCODE,
                        CENSUS_LOWER as LSOA_CODE,
                        
                        YEAR_MONTH,
                        rank() over (partition by POSTCODE order by YEAR_MONTH desc) as rnk

            from        DIM.ONS_POSTCODE_DATA_DIM@dwcpb
            )                                                   opdd
inner join      STBUC.SCD2_INDEX_OF_MULTIPLE_DEPRIVATIONS@dwcpb iomd    on  opdd.LSOA_CODE          =   iomd.LSOA_CODE
left join       DALL_REF.POSTCODE_LOOKUP                        pl      on  opdd.POSTCODE           =   pl.PCD_NO_SPACES
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogm1    on  opdd.LSOA_CODE          =   ogm1.CHILD_ONS_CODE
                                                                        and ogm1.RELATIONSHIP       =   'LSOA_WARD2020'
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogm2    on  opdd.LSOA_CODE          =   ogm2.CHILD_ONS_CODE
                                                                        and ogm2.RELATIONSHIP       =   'LSOA_CCG2021'
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogm3    on  ogm1.PARENT_ONS_CODE    =   ogm3.CHILD_ONS_CODE
                                                                        and ogm3.RELATIONSHIP       =   'WARD2020_LAD2020'
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogm4    on  ogm3.PARENT_ONS_CODE    =   ogm4.CHILD_ONS_CODE
                                                                        and ogm4.RELATIONSHIP       =   'LAD2020_REG2020'

where       1=1
    and     opdd.rnk = 1
)
--select * from pcd;
--select count(*) from pcd;
-----SECTION END: POSTCODE CLASSIFICATION-------------------------------------------------------------------------------------------------------------

,


-----SECTION START: PATIENT CLASSIFICATION------------------------------------------------------------------------------------------------------------
--return the applicant data (only latest data is available)
--create a composite indicator based on the forename(1st 3 chars), surname,  dob and postcode (1st 2 chars)
--link to a postcode lookup to supplement data
--exclude asylum seekers and any invalid postcodes
patd as
(
select		d.CASEREF,
            d.APPLICANTID,
            d.CLIENTGROUP,
            case d.CLIENTGROUP
                when 0 then 'Student'
                when 1 then 'Benefits_Other'
                when 2 then 'Earner'
                when 3 then 'Pensioner'
                when 4 then 'Benefits_Other'
                when 5 then 'Unknown'
                when 6 then 'Asylum Seeker (not NAS)'
                when 7 then 'Asylum Seeker'
                else 'ERROR'
            end as CLIENTGROUP_DESC_FORMAT,
            case d.CLIENTGROUP
                when 0 then 'Student'
                when 1 then 'Benefits'
                when 2 then 'Earner'
                when 3 then 'Pensioner'
                when 4 then 'Other'
                when 5 then 'Unknown'
                when 6 then 'Asylum Seeker (not NAS)'
                when 7 then 'Asylum Seeker'
                else 'ERROR'
            end as CLIENTGROUP_DESC,
            substr(regexp_replace(upper(d.FORENAME),'[^A-Z]',''),1,3) as C1,
            regexp_replace(upper(d.SURNAME),'[^A-Z]','') as C2,
            to_char(d.DOB,'YYYYMMDD') as C3,
            substr(regexp_replace(upper(d.POSTCODE),'[^A-Z0-9]',''),1,2) as C4,            
		--composite patient ID
			case
				--if any of the required fields are missing no identifier is possible
				when d.FORENAME is null      		then 'na'
				when d.SURNAME is null         		then 'na'
				when d.DOB is null   				then 'na'
				when d.POSTCODE is null  			then 'na'
                --if the substr used to build the composite part is blank no identifier is possible
                when substr(regexp_replace(upper(d.FORENAME),'[^A-Z]',''),1,3) = ''         then 'na'
                when substr(regexp_replace(upper(d.FORENAME),'[^A-Z]',''),1,3) is null      then 'na'
                when regexp_replace(upper(d.SURNAME),'[^A-Z]','') = ''                      then 'na'
                when regexp_replace(upper(d.SURNAME),'[^A-Z]','') is null                   then 'na'
                when to_char(d.DOB,'YYYYMMDD') = ''                                         then 'na'
                when to_char(d.DOB,'YYYYMMDD') is null                                      then 'na'
                when substr(regexp_replace(upper(d.POSTCODE),'[^A-Z0-9]',''),1,2) = ''      then 'na'
                when substr(regexp_replace(upper(d.POSTCODE),'[^A-Z0-9]',''),1,2) is null   then 'na'
				--otherwise build a unique identifier from the available information			
				else	(
						--use the first three characters of the forename(s)
						substr(regexp_replace(upper(d.FORENAME),'[^A-Z]',''),1,3)||
						--use the full surname
						regexp_replace(upper(d.SURNAME),'[^A-Z]','')||
						--format the DOB as YYYYMMDD        
						to_char(d.DOB,'YYYYMMDD')||
						--first two characters of postcode
						substr(regexp_replace(upper(d.POSTCODE),'[^A-Z0-9]',''),1,2)
						)
			end as COMPOSITE_ID,
            case  
                when d.TITLE='Mr'       then 'Male'
                when d.TITLE='Fath'     then 'Male'
                when d.TITLE='Lord'     then 'Male'
                when d.TITLE='Sir'      then 'Male'
                when d.TITLE='Count'    then 'Male'
                when d.TITLE='Bro.'     then 'Male'
                when d.TITLE='Mrs'      then 'Female'
                when d.TITLE='Miss'     then 'Female'
                when d.TITLE='Sist'     then 'Female'
                when d.TITLE='Lady'     then 'Female'
                when d.TITLE='Ms'       then 'Female'
                                        else 'Unknown'
            end as GENDER,
            d.DOB,
            case
                when d.DOB is null                                          then -1
                when dc.REGISTERED is null                                  then -1
                when floor(months_between(dc.REGISTERED,d.DOB)/12) is null  then -1
                when floor(months_between(dc.REGISTERED,d.DOB)/12) < 15     then -1
                when floor(months_between(dc.REGISTERED,d.DOB)/12) > 100    then -1
                else floor(months_between(dc.REGISTERED,d.DOB)/12)
            end as AGE_AT_REGISTRATION,
            d.POSTCODE,
            pcd.PCD_LSOA_CODE,
            pcd.PCD_WARD_CODE,
            pcd.PCD_WARD_NAME,
			pcd.PCD_CCG_CODE,
            pcd.PCD_CCG_ID,
            pcd.PCD_CCG_NAME,
            pcd.PCD_LAD_CODE, 
            pcd.PCD_LAD_NAME,
            pcd.PCD_REGION_CODE, 
            pcd.PCD_REGION_NAME,
            pcd.SUBGROUP_CODE,
            pcd.SUBGROUP_NAME,
            pcd.GROUP_CODE,
            pcd.GROUP_NAME,
            pcd.SUPERGROUP_CODE,
            pcd.SUPERGROUP_NAME,
            pcd.INDEX_OF_MULT_DEPRIV_SCORE,
            pcd.INDEX_OF_MULT_DEPRIV_RANK,
            pcd.INDEX_OF_MULT_DEPRIV_DECILE,
            pcd.INCOME_SCORE,
            pcd.INCOME_RANK,
            pcd.INCOME_DECILE,
            pcd.HEALTH_DEPRIVATION_SCORE,
            pcd.HEALTH_DEPRIVATION_RANK,
            pcd.HEALTH_DEPRIVATION_DECILE,
            pcd.INCOME_DEPRIV_CHILDREN_SCORE,
            pcd.INCOME_DEPRIV_CHILDREN_RANK,
            pcd.INCOME_DEPRIV_CHILDREN_DECILE,
            pcd.INCOME_DEPRIV_ELDERLY_SCORE,
            pcd.INCOME_DEPRIV_ELDERLY_RANK,
            pcd.INCOME_DEPRIV_ELDERLY_DECILE
            
from		LIS.DATAAPPLICANT	d
--join to the postcode dataset to supplement data (where possible)
left join	                    pcd		on	regexp_replace(upper(d.POSTCODE),'[^A-Z0-9]','')	=	pcd.POSTCODE
--join to the DATACASE table to allow an age at registration to be calculated
left join   LIS.DATACASE        dc      on  d.CASEREF   =   dc.CASEREF

where		1=1
)
--select * from patd;
-----SECTION END: PATIENT CLASSIFICATION--------------------------------------------------------------------------------------------------------------

,

-----SECTION START: CASE STATUS-----------------------------------------------------------------------------------------------------------------------
--define a unique status for each case reference as each case can have multiple status flags
--a case can be determined as "certificate produced" if it reaches a status of PRINT/REPRI
--multiple status flags could be incorporated to identify is certain points have been reached in the application process
latest_status as
--find the latest status event for each CASEREF, using RECNO to find the latest entry
(
select      CASEREF,
            STATUS      as LATEST_STATUS,
            STATUS_DESC as LATEST_STATUS_DESC

from        (
            select      status.CASEREF,
                        status.RECNO,
                        status.ENTRYDATE,
                        status.STATUS,
                        lkp.STATUS  as STATUS_DESC,
                        rank() over (partition by status.CASEREF order by RECNO desc) as rnk
                        
            from        LIS.DATASTATUS      status
            left join   LIS.LOOKUPSTATUS    lkp     on  status.STATUS   =  lkp.STATUSID
            )

where       1=1
    and     rnk = 1
)
,
case_status as
(
--summarise data for each CASEREF showing the outcome and latest status
select      CASEREF,
        --case outcome
            case 
                when CERT_PRINTED = 'Y'                         then 'CERTIFICATE_ISSUED'
                when CASE_WITHDRAWN = 'Y'                       then 'APPLICATION_WITHDRAWN'
                when CERT_PRINTED = 'N' and PRE_2021_REG = 'N'  then 'APPLICATION_ONGOING'
                when CERT_PRINTED = 'N' and PRE_2021_REG = 'Y'  then 'APPLICATION_ABANDONED'
                                                                else 'ERROR'
            end as CASE_OUTCOME,
        --latest status: hard code to CERTIFICATE_PRINTED for the print cases to prefer further events being reported
            case when CERT_PRINTED = 'Y' then 'CERT_PRINTED'                    else LATEST_STATUS      end as LATEST_STATUS,
            case when CERT_PRINTED = 'Y' then 'Certificate issued to applicant' else LATEST_STATUS_DESC end as LATEST_STATUS_DESC,
            CERT_ISSUE_DATE

from        (
            --identify status data for each CASEREF, including data that can be used for further classification
            --anything prior to 2021 should be resolved by now based on the data being extracted in May2021
            select      status.CASEREF,
                        case when dcase.REGISTERED < to_date(20210101,'YYYYMMDD') then 'Y' else 'N' end   as PRE_2021_REG,  
                    --latest status
                        ls.LATEST_STATUS,
                        ls.LATEST_STATUS_DESC,
                    --withdrawn claims
                        --this is for formally withdrawn claims, many more claims simply remain open as no further contact is received from applicant
                        max (case status.STATUS
                                when 'NFAII'    then 'Y'
                                when 'NFAW'     then 'Y'
                                                else 'N'    end
                            ) as CASE_WITHDRAWN,
                    --certificate issued
                        --there are other status values that include print but these are the two that signify the certificate was issued
                        max (case status.STATUS
                                when 'PRINT'    then 'Y'
                                when 'REPRI'    then 'Y'
                                                else 'N'    end
                            ) as CERT_PRINTED,
                    --certificate issued
                        --there are other status values that include print but these are the two that signify the certificate was issued
                        max (case status.STATUS
                                when 'PRINT'    then ENTRYDATE
                                when 'REPRI'    then ENTRYDATE
                                                else null    end
                            ) as CERT_ISSUE_DATE
                        
            from        LIS.DATASTATUS      status
            inner join  LIS.DATACASE        dcase   on  status.CASEREF  =   dcase.CASEREF
            inner join  latest_status       ls      on  status.CASEREF  =   ls.CASEREF
            
            group by    status.CASEREF,
                        dcase.REGISTERED,
                        case when dcase.REGISTERED < to_date(20210101,'YYYYMMDD') then 'Y' else 'N' end,
                        ls.LATEST_STATUS,
                        ls.LATEST_STATUS_DESC
            )
)
--select * from case_status;
--select count(*) from case_status;
-----SECTION END: CASE STATUS-------------------------------------------------------------------------------------------------------------------------

,

-----SECTION START: CERTIFICATE DATA------------------------------------------------------------------------------------------------------------------
--get the valid from and to dates from the certificate data
--also return the rectype to show type of certificate
cert as
(
select		ASSESSMENTRECNO,
            RECTYPE,
            VALIDFROM,
            VALIDTO

from		LIS.DATACERTIFICATE
)
--select * from cert;
-----SECTION END: CERTIFICATE DATA--------------------------------------------------------------------------------------------------------------------

,

-----SECTION START: HC3 VALUE-------------------------------------------------------------------------------------------------------------------------
hc3_value as
(
--find distinct entries in the CUEXTRACT table as the table will contain mutliple entries for:
--  different people listed on the form
--  changes to the assistance value for HC3 claims
select      CASEREF,
            ASSRECNO,
            HC3DENTALAMOUNT,
            --rank the entries for each case reference to identify the latest entry 
            rank() over (partition by CASEREF order by ASSRECNO desc) as RNK
            
from        (
            select distinct
                        cue.CASEREF,
                        cue.ASSRECNO,
                    --convert entries to a valid number
                        nvl(replace(cue.HC3DENTALAMOUNT,' ',''),0) as HC3DENTALAMOUNT
                                            
            from        LIS.CUEXTRACT cue
            
            where       1=1
                and     cue.CERTIFICATETYPE = 'HC3'
            )
)
--select * from hc3_value;
-----SECTION END: HC3 VALUE---------------------------------------------------------------------------------------------------------------------------

,

-----SECTION START: NUMBER OF INDIVIDUALS COVERED-----------------------------------------------------------------------------------------------------
--use the DATADEPENDANTS table to identify the number of additional people covered by the LIS entitlement
dep_count as
(
select      CASEREF,
            count(distinct(DEPRECNO))   as DEPENDANT_COUNT
from        LIS.DATADEPENDANTS
group by    CASEREF
)
--select * from dep_count;
-----SECTION END: NUMBER OF INDIVIDUALS COVERED-------------------------------------------------------------------------------------------------------

,

-----SECTION START: LATEST CERTIFICATE DATA-----------------------------------------------------------------------------------------------------------
--for each CASEREF identify the latest certificate data within the DATACERTIFICATE table
--the DATASTATUS table will need to be used to link the tables
--this may resolve issues where the LASTASSESSMENTREC is not finding a certificate due to new records being created
latest_cert as
(
select      CASEREF,
            trim(RECTYPE) as RECTYPE,
            VALIDFROM,
            VALIDTO
from        (
            select      ds.CASEREF,
                        ds.ASSESSMENTRECNO,
                        dc.RECTYPE,
                        dc.VALIDFROM,
                        dc.VALIDTO,
                        rank() over (partition by ds.CASEREF order by ds.ASSESSMENTRECNO desc) as rnk
            from        LIS.DATASTATUS      ds
            inner join  LIS.DATACERTIFICATE dc  on  ds.ASSESSMENTRECNO  =   dc.ASSESSMENTRECNO
            group by    ds.CASEREF,
                        ds.ASSESSMENTRECNO,
                        dc.RECTYPE,
                        dc.VALIDFROM,
                        dc.VALIDTO
            )
where       1=1
    and     rnk = 1
)
--select * from latest_cert;
-----SECTION END: LATEST CERTIFICATE TYPE-------------------------------------------------------------------------------------------------------------

,

-----SECTION START: IDENTIFY DIGITAL APPLICATIONS-----------------------------------------------------------------------------------------------------
--Digital applications are not classified directly in the database
--however assessors are expected/required to add a reference to the digital application as a note.  
--This is typically stored as a single 8 character string in its own note record.
--Therefore as a proxy for digitial applications identify any CASEREF with a note containing a single unique 8 character alphanumeric string
digi_app as
(
select      CASEREF
from        (
            select      CASEREF,
                        NOTES,
                        sum(1) over (partition by NOTES) as notecount
            from        LIS.DATANOTE
            where       1=1
                and     LENGTH(NOTES) = 8
                and     REGEXP_LIKE(NOTES,'^[[:alnum:]]{8}$')
            )
where       1=1
    and     notecount = 1
group by    CASEREF
)
--select * from digi_app;
-----SECTION END: IDENTIFY DIGITAL APPLICATIONS-------------------------------------------------------------------------------------------------------

,

-----SECTION START: TIME DIMENSIONS CLASSIFICATIONS---------------------------------------------------------------------------------------------------
--create a basic lookup to find the FINANCIAL_YEAR and ACADEMIC_YEAR for any given month
--financial and academic years must be of the format yyyy/yy (e.g. 2019/20)
ymd as
(
select      YEAR_MONTH,
            case 
                when substr(YEAR_MONTH,5,2) in ('01','02','03') 
                then to_char(to_number(substr(YEAR_MONTH,1,4))-1)||'/'||substr(YEAR_MONTH,3,2)
                else substr(YEAR_MONTH,1,4)||'/'||to_char(to_number(substr(YEAR_MONTH,3,2))+1)
            end as FINANCIAL_YEAR,
            case 
                when substr(YEAR_MONTH,5,2) in ('09','10','11','12') 
                then substr(YEAR_MONTH,1,4)||'/'||to_char(to_number(substr(YEAR_MONTH,3,2))+1)
                else to_char(to_number(substr(YEAR_MONTH,1,4))-1)||'/'||substr(YEAR_MONTH,3,2)
            end as ACADEMIC_YEAR 
from        DIM.YEAR_MONTH_DIM@dwcpb
)
--select * from ymd;
-----SECTION END: TIME DIMENSIONS CLASSIFICATIONS-----------------------------------------------------------------------------------------------------

,

-----SECTION START: BASE CERTIFICATE DATA-------------------------------------------------------------------------------------------------------------
--combine the applicant, certificate and status data via the main case reference table
--ensure asylum seeker applications are excluded
--identify the month of application from the registered date and link this to get the financial quarter
--identify the patients age based on registered date and DOB
cert_base as
(
select  --case data
            d.CASEREF,
            d.REGISTERED,
            to_number(to_char(d.REGISTERED,'YYYYMM'))   as APPLICATION_MONTH,
            ymd.FINANCIAL_YEAR,
            ymd.ACADEMIC_YEAR,
            d.CLAIMTYPE,
            d.CERTTYPE,
            d.LASTASSESSMENTREC,
        --digital application flag, only applicable from Nov-18 onwards
            case 
                when da.CASEREF is null                                                         then 'PAPER'
                when da.CASEREF is not null and d.REGISTERED <  to_date(20181101,'YYYYMMDD')    then 'PAPER'
                when da.CASEREF is not null and d.REGISTERED >= to_date(20181101,'YYYYMMDD')    then 'DIGITAL'
                                                                                                else 'ERROR'
            end as APPLICATION_TYPE,                                                    
        --status
            cs.CASE_OUTCOME,
            cs.LATEST_STATUS,
            cs.LATEST_STATUS_DESC,
            cs.CERT_ISSUE_DATE,
        --certificate data
            --RECTYPE is the certificate type used by exemption checker (PECS/DECS)
            case
                when cert.RECTYPE in ('HC2','HC2A') and cs.CASE_OUTCOME = 'CERTIFICATE_ISSUED'                          then 'Successful'
                when cert.RECTYPE in ('HC3') and nvl(hc3v.HC3DENTALAMOUNT,0) < 384 and cs.CASE_OUTCOME = 'CERTIFICATE_ISSUED'  then 'Successful'
                when cs.CASE_OUTCOME = 'APPLICATION_ONGOING'                                                            then 'Ongoing'
                when cs.CASE_OUTCOME in ('APPLICATION_WITHDRAWN','APPLICATION_ABANDONED')                               then 'Withdrawn/Abandoned'
                when cert.RECTYPE in ('HBD08','HBDTC','HC4','HBD03')                                                    then 'Unneccesary'
                when cert.RECTYPE in ('HC3') and hc3v.HC3DENTALAMOUNT >= 384 and cs.CASE_OUTCOME = 'CERTIFICATE_ISSUED' then 'Unneccesary'
                                                                                                                        else 'ERROR'
            end as OUTCOME_LEVEL1,
            case
                when cert.RECTYPE in ('HC2','HC2A') and cs.CASE_OUTCOME = 'CERTIFICATE_ISSUED'                          then 'Full benefit'
                when cert.RECTYPE in ('HC3') and nvl(hc3v.HC3DENTALAMOUNT,0) < 384 and cs.CASE_OUTCOME = 'CERTIFICATE_ISSUED'  then 'Partial benefit'
                when cs.CASE_OUTCOME = 'APPLICATION_ONGOING'                                                            then 'Ongoing'
                when cs.CASE_OUTCOME in ('APPLICATION_WITHDRAWN','APPLICATION_ABANDONED')                               then 'Withdrawn/Abandoned'
                when cert.RECTYPE in ('HBD08','HBDTC','HBD03')                                                          then 'Already receiving benefits'
                when cert.RECTYPE in ('HC4')                                                                            then 'Over capital limit'
                when cert.RECTYPE in ('HC3') and hc3v.HC3DENTALAMOUNT >= 384 and cs.CASE_OUTCOME = 'CERTIFICATE_ISSUED' then 'Over income limit'
                                                                                                                        else 'ERROR'
            end as OUTCOME_LEVEL2,
            case 
                when cert.RECTYPE in ('HC3') and hc3v.HC3DENTALAMOUNT >= 384    then 'LIS11'
                                                                                else cert.RECTYPE
            end as RECTYPE,
            cert.RECTYPE as RECTYPE_ORIGINAL,
            case
                when cert.RECTYPE in ('HC2','HC2A')                             then 'HC2-Full'
                when cert.RECTYPE in ('HC3') and hc3v.HC3DENTALAMOUNT >= 384    then 'No_BSA_Support'
                when cert.RECTYPE in ('HC3')                                    then 'HC3-Partial'
                when cert.RECTYPE in ('HBD08','HBDTC','HC4','HBD03')            then 'No_BSA_Support'
                when cert.RECTYPE in ('UNKNW')                                  then 'UNKNOWN'
                                                                                else 'No_Outcome'
            end as CERT_GROUP,
			cert.VALIDFROM,
			cert.VALIDTO,
            round(months_between(VALIDTO,VALIDFROM),0) as CERT_LENGTH_MONTHS,
        --HC3 Value - only show for HC3 certificates
            case 
                when cert.RECTYPE in ('HC3') 
                then hc3v.HC3DENTALAMOUNT
                else null
            end as HC3DENTALAMOUNT,
            case 
                when cert.RECTYPE in ('HC3') and hc3v.HC3DENTALAMOUNT is not null 
                then hc3v.HC3DENTALAMOUNT/3
                else null
            end as HC3AMOUNT,
        

        --HC3 providing help or not
         -- Dental Treatment
          case
             when ymd.FINANCIAL_YEAR = '2015/16' and hc3v.HC3DENTALAMOUNT <= 18.80 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2016/17' and hc3v.HC3DENTALAMOUNT <= 19.70 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2017/18' and hc3v.HC3DENTALAMOUNT <= 20.60 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2018/19' and hc3v.HC3DENTALAMOUNT <= 21.60 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2019/20' and hc3v.HC3DENTALAMOUNT <= 22.70 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2020/21' and hc3v.HC3DENTALAMOUNT <= 23.80 then 'Some help'
             else 'No help'
          end as HELP_WITH_BAND_1,
          case
             when ymd.FINANCIAL_YEAR = '2015/16' and hc3v.HC3DENTALAMOUNT <= 51.30 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2016/17' and hc3v.HC3DENTALAMOUNT <= 53.90 then 'Some help' 
             when ymd.FINANCIAL_YEAR = '2017/18' and hc3v.HC3DENTALAMOUNT <= 56.30 then 'Some help' 
             when ymd.FINANCIAL_YEAR = '2018/19' and hc3v.HC3DENTALAMOUNT <= 59.10 then 'Some help' 
             when ymd.FINANCIAL_YEAR = '2019/20' and hc3v.HC3DENTALAMOUNT <= 62.10 then 'Some help' 
             when ymd.FINANCIAL_YEAR = '2020/21' and hc3v.HC3DENTALAMOUNT <= 65.20 then 'Some help' 
             else 'No help'
          end as HELP_WITH_BAND_2,
          case
             when ymd.FINANCIAL_YEAR = '2015/16' and hc3v.HC3DENTALAMOUNT <= 222.50 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2016/17' and hc3v.HC3DENTALAMOUNT <= 233.70 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2017/18' and hc3v.HC3DENTALAMOUNT <= 244.30 then 'Some help' 
             when ymd.FINANCIAL_YEAR = '2018/19' and hc3v.HC3DENTALAMOUNT <= 256.50 then 'Some help'
             when ymd.FINANCIAL_YEAR = '2019/20' and hc3v.HC3DENTALAMOUNT <= 269.30 then 'Some help' 
             when ymd.FINANCIAL_YEAR = '2020/21' and hc3v.HC3DENTALAMOUNT <= 282.80 then 'Some help' 
             else 'No help'
          end as HELP_WITH_BAND_3,           
        
         -- NHS Sight Test @ 21.31
          case
             when hc3v.HC3DENTALAMOUNT/3 <= 21.31 then 'Some help'
             else 'No help'
          end as HELP_WITH_SIGHT_TEST,  
          
        --No. of individuals covered: 1 + any dependants
            nvl(dc.DEPENDANT_COUNT,0) + 1   as INDIVIDUALS_COVERED,
        --applicant data
            patd.APPLICANTID,
            patd.COMPOSITE_ID,
			patd.CLIENTGROUP,
            patd.CLIENTGROUP_DESC,
            patd.CLIENTGROUP_DESC_FORMAT,
            patd.GENDER,
            patd.DOB,
            patd.AGE_AT_REGISTRATION,
            age.BAND_5YEARS,
            age.BAND_10YEARS,
			patd.POSTCODE,
            patd.PCD_LSOA_CODE,
            patd.PCD_WARD_CODE,
            patd.PCD_WARD_NAME,
            oirasba_wrd.IMD_AVERAGE_SCORE   as PCD_WARD_IMD_AVERAGE_SCORE,
            oirasba_wrd.IMD_RANK            as PCD_WARD_IMD_RANK,
			patd.PCD_CCG_CODE,
            patd.PCD_CCG_ID,
            patd.PCD_CCG_NAME,
            oirasba_ccg.IMD_AVERAGE_SCORE   as PCD_CCG_IMD_AVERAGE_SCORE,
            oirasba_ccg.IMD_RANK            as PCD_CCG_IMD_RANK,
            patd.PCD_LAD_CODE, 
            patd.PCD_LAD_NAME,
            oirasba_lad.IMD_AVERAGE_SCORE   as PCD_LAD_IMD_AVERAGE_SCORE,
            oirasba_lad.IMD_RANK            as PCD_LAD_IMD_RANK,
            patd.PCD_REGION_CODE, 
            patd.PCD_REGION_NAME,
            patd.SUBGROUP_CODE,
            patd.SUBGROUP_NAME,
            patd.GROUP_CODE,
            patd.GROUP_NAME,
            patd.SUPERGROUP_CODE,
            patd.SUPERGROUP_NAME,
            patd.INDEX_OF_MULT_DEPRIV_SCORE,
            patd.INDEX_OF_MULT_DEPRIV_RANK,
            patd.INDEX_OF_MULT_DEPRIV_DECILE,
            patd.INCOME_SCORE,
            patd.INCOME_RANK,
            patd.INCOME_DECILE,
            patd.HEALTH_DEPRIVATION_SCORE,
            patd.HEALTH_DEPRIVATION_RANK,
            patd.HEALTH_DEPRIVATION_DECILE,
            patd.INCOME_DEPRIV_CHILDREN_SCORE,
            patd.INCOME_DEPRIV_CHILDREN_RANK,
            patd.INCOME_DEPRIV_CHILDREN_DECILE,
            patd.INCOME_DEPRIV_ELDERLY_SCORE,
            patd.INCOME_DEPRIV_ELDERLY_RANK,
            patd.INCOME_DEPRIV_ELDERLY_DECILE  
                    
from        LIS.DATACASE                            d
inner join	case_status			                    cs		    on	d.CASEREF			        =	cs.CASEREF
left join                                           patd        on  d.CASEREF                   =   patd.CASEREF
left join   hc3_value                               hc3v        on  d.CASEREF                   =   hc3v.CASEREF
                                                                and hc3v.RNK                    =   1                   --only take the most recent value
left join   dep_count                               dc          on  d.CASEREF                   =   dc.CASEREF
left join   latest_cert                             cert        on  d.CASEREF                   =   cert.CASEREF
left join   DALL_REF.LKP_AGEBANDS                   age         on  patd.AGE_AT_REGISTRATION    =   age.AGE
--link to find the weighted IMD scores for the relevant geographies
left join   DALL_REF.ONS_IMD_RANK_AVG_SCORE_BY_AREA oirasba_wrd on  patd.PCD_WARD_CODE          =   oirasba_wrd.ONS_CODE
                                                                and oirasba_wrd.CATEGORY        =   'WARD_2020'
left join   DALL_REF.ONS_IMD_RANK_AVG_SCORE_BY_AREA oirasba_lad on  patd.PCD_LAD_CODE           =   oirasba_lad.ONS_CODE
                                                                and oirasba_lad.CATEGORY        =   'LAD_2020'
left join   DALL_REF.ONS_IMD_RANK_AVG_SCORE_BY_AREA oirasba_ccg on  patd.PCD_CCG_CODE           =   oirasba_ccg.ONS_CODE
                                                                and oirasba_ccg.CATEGORY        =   'CCG_2021'
left join                                           ymd         on  to_number(to_char(d.REGISTERED,'YYYYMM'))   =   ymd.YEAR_MONTH
left join   digi_app                                da          on  d.CASEREF                   =   da.CASEREF

where       1=1
    and		patd.CLIENTGROUP not in (7) 	--exclude asylum seekers
    and     d.REGISTERED >= to_date('20150401','YYYYMMDD')  --only 201504 onwards
    and     d.REGISTERED <= to_date('20210331', 'YYYYMMDD') --up to end of 202103 
    and     patd.PCD_REGION_CODE is not null
)
--select * from cert_base;
-----SECTION END: BASE CERTIFICATE DATA---------------------------------------------------------------------------------------------------------------

,

-----SECTION START: HANDLE POTENTIAL DUPLICATES-------------------------------------------------------------------------------------------------------
--a small number of cases exist where a COMPOSITE_ID has multiple CASEREFs that share the same REGISTERED date
--we only want to take one of these into the analysis
--to select which record we can prioritise based on the OUTCOME_LEVEL2 value:
--  "Full Benefit" > "Partial Benefit" > "Already receiving benefits" > "Over income limit" > "Over capital limit" > "Withdrawn/ Abandoned" > "Ongoing"
--  where two or more of the potential duplicates share the same outcome results will first be split based on the availablility of a valid GENDER and CLIENTGROUP
--  finally if all these are equal then the REGISTERED date/time and finally highest CASEREF will be used

potential_duplicates as
--find the records with two or more records sharing a REGISTERED date and COMPOSITE_ID
(
select      to_date(REGISTERED) as REGISTERED_DATE, 
            COMPOSITE_ID, 
            count(*) as RECORD_COUNT
from        cert_base
where       1=1
    and     COMPOSITE_ID != 'na'
group by    to_date(REGISTERED), 
            COMPOSITE_ID
    having  count(*) >=2
)
--select * from potential_duplicates;
,
score_duplicates as
--apply scoring logic that can be used to rank the records based on order of preference for analysis
(
select      cb.COMPOSITE_ID,
            cb.CASEREF,
            cb.REGISTERED,
            pd.REGISTERED_DATE,
            case    OUTCOME_LEVEL2
                when    'Full benefit'                  then 1      --of most interest
                when    'Partial benefit'               then 2
                when    'Already receiving benefits'    then 3
                when    'Over income limit'             then 4
                when    'Over capital limit'            then 5
                when    'Withdrawn/Abandoned'           then 6
                when    'Ongoing'                       then 7
                                                        else 999
            end as RANK_OUTCOME,
            case    CLIENTGROUP_DESC
                when    'Earner'                    then 1  --of most interest
                when    'Asylum Seeker (not NAS)'   then 1      
                when    'Student'                   then 1
                when    'Pensioner'                 then 1
                when    'Benefits'                  then 1
                when    'Other'                     then 2
                when    'Unknown'                   then 3
                                                    else 999
            end as RANK_CLIENTGROUP,
            case    GENDER
                when    'Male'      then 1  --of most interest
                when    'Female'    then 1
                when    'Unknown'   then 2
                                    else 999
            end as RANK_GENDER            
from        cert_base               cb
inner join  potential_duplicates    pd  on  cb.COMPOSITE_ID             = pd.COMPOSITE_ID
                                            and to_date(cb.REGISTERED)  = pd.REGISTERED_DATE
)
--select * from score_duplicates;
,
rank_duplicates as
--apply rankings based on the scoring to highlight which is the best record to keep for analysis (RNK = 1)
--only return records with a RNK > 1 as these will be the ones to exclude from the analysis
(
select      *
from        (
            select      COMPOSITE_ID,
                        REGISTERED_DATE,
                        REGISTERED,
                        CASEREF,
                        rank() over (partition by COMPOSITE_ID, REGISTERED_DATE order by RANK_OUTCOME asc, RANK_CLIENTGROUP asc, RANK_GENDER asc, REGISTERED desc, CASEREF desc) as RNK
            from        score_duplicates
            )
where       1=1
    and     RNK != 1
)
--select * from rank_duplicates;
-----SECTION END: HANDLE POTENTIAL DUPLICATES---------------------------------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--filter the main "cert_base" dataset to exclude any of the records in the "rank_duplicates" dataset
select      cb.*
from        cert_base       cb
left join   rank_duplicates rd  on  cb.CASEREF = rd.CASEREF
where       1=1
    and     rd.CASEREF is null
;


----------SCRIPT END----------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------