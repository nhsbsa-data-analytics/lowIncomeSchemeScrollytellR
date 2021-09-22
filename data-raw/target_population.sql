select      op.POPULATION_YEAR,                   
            ogm2.PARENT_ONS_CODE            as LAD_CODE,
            ogm2.PARENT_NAME                as LAD_NAME,       
            ogm3.PARENT_ONS_CODE            as RGN_CODE,
            ogm3.PARENT_NAME                as RGN_NAME,
            sum(op.TOTAL_POPULATION)        as ONS_POPULATION

from        DALL_REF.ONS_POPULATION                     op

inner join  DALL_REF.ONS_GEOGRAPHY_MAPPING              ogm1    on  op.GEOGRAPHY_ONS_CODE   =   ogm1.CHILD_ONS_CODE
                                                                and ogm1.RELATIONSHIP       =   'LSOA_WARD2020'
inner join  DALL_REF.ONS_GEOGRAPHY_MAPPING              ogm2    on  ogm1.PARENT_ONS_CODE    =   ogm2.CHILD_ONS_CODE
                                                                and ogm2.RELATIONSHIP       =   'WARD2020_LAD2020' 
                                                                
inner join  DALL_REF.ONS_GEOGRAPHY_MAPPING              ogm3    on  ogm2.PARENT_ONS_CODE    =   ogm3.CHILD_ONS_CODE
                                                                and ogm3.RELATIONSHIP       =   'LAD2020_REG2020'                                                          
                                                                
                                                                
where       1=1
    and     op.GEOGRAPHY_TYPE = 'LSOA'
    and     op.AGE >= 16

group by    op.POPULATION_YEAR,
            ogm2.PARENT_ONS_CODE,
            ogm2.PARENT_NAME,
            ogm3.PARENT_ONS_CODE,
            ogm3.PARENT_NAME
            ;
            
