create or replace package PKG_AZMM is

  --  
  -- Created : 23.08.2010 13:20:57
  --  
  FUNCTION FNC_CreatePeriodPeakAmount (InRefDate  IN DATE)  RETURN VARCHAR2;
  FUNCTION FNC_CreateAZMM_XXXX (InRefDate    DATE) RETURN VARCHAR2;
  FUNCTION FNC_CreateAZMM_XXXX_Out (InRefDate    DATE) RETURN VARCHAR2;
  PROCEDURE Prc_CreateAzmmPeriodXXXXPaid (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
  PROCEDURE Prc_CreateAzmmPeriodXXXXOut (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
  PROCEDURE Prc_CreateAzmmPeriodXXXXOutFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
  PROCEDURE Prc_CreateAzmmPeriodXXXXPadFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
  PROCEDURE PRC_AZZM (InPeriod    DATE);
  PROCEDURE Prc_CreateAzmmPrdXXXXPadCntFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
  PROCEDURE Prc_CreateAzmmPrdXXXXOutCntFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );

   FUNCTION FNC_CreateAZMM_HAYAT (InRefDate    DATE) RETURN VARCHAR2;
   FUNCTION FNC_CreateAZMM_HAYAT_Out (InRefDate    DATE) RETURN VARCHAR2 ;
   PROCEDURE Prc_CreateAzmmPeriodHYTPadFin ( InRefDate IN DATE, InBranch IN T_AZMM_HAYAT.Branch%TYPE );
   PROCEDURE Prc_CreateAzmmPeriodHYTOutFin ( InRefDate IN DATE, InBranch IN T_AZMM_HAYAT.Branch%TYPE );
   PROCEDURE Prc_CreateAzmmPrdHYTPadCntFin ( InRefDate IN DATE , InBranch IN T_AZMM_HAYAT.Branch%TYPE);
   PROCEDURE Prc_CreateAzmmPrdHYTOutCntFin ( InRefDate IN DATE, InBranch IN T_AZMM_HAYAT.Branch%TYPE );
   PROCEDURE Prc_CreateAzmmXXXXPadREINS (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
   PROCEDURE Prc_CreateAzmmXXXXOutREINS (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
   PROCEDURE Prc_CreateAzmmEUROKAOutFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
   PROCEDURE Prc_CreateAzmmEUROKAPadFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
   PROCEDURE Prc_CreateAzmmEUROKAPadCntFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );
   PROCEDURE Prc_CreateAzmmEUROKAOutCntFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE );


end PKG_AZMM;
/
create or replace package body PKG_AZMM is
 /*
AZMM bu sekilde cal?st?r?lmal?
---------------------------------------------
Declare
      vSql   varchar(4000) := 'create or replace view vDummy_topclaim_ln as 
          select * from (( select cd.claimid,ln(sum(
          case when
              (  (select count(*) from t_caseclaimperiod where tazminat = to_char(c.claimid ) and period = ''01/12/2010''  )=0 or c.statusid = 6 ) THEN
                       cd.paidamount
            else
                 (select sum(davatutari ) from t_caseclaimperiod where tazminat = to_char(c.claimid )   and period =''01/12/2010'' ) \*dava tutar�*\ /
                 (select  count(distinct  tazminat) from t_caseclaimperiod where davasistemno in  
                           (select davasistemno from t_caseclaimperiod  where tazminat = to_char(c.claimid )   and period =''01/12/2010'' )  ) \*tazminat adedi*\
            end 
           
           
           )) as lnpaidamount  
             from  t_claim c,t_claimdetail cd    where c.claimid=cd.claimid and c.eventdate>=''01/01/2005'' 
            and c.eventdate<=''31/12/2010''
                         group by cd.claimid having sum(cd.paidamount)>0  ) 
                      union all 
          (select azths.dosyano claimid, ln(sum(azths.tutar ) ) lnpaidamount 
                 from  t_azmm_ths azths  
               group by azths.dosyano ))
                order by lnpaidamount desc  ';
begin
  EXECUTE IMMEDIATE 'CREATE TABLE XXXXRESERVE.T_AZMMBUP'|| to_char(InPeriod,'MM_RRRR') ||' as (select * from t_azmm) ';
  pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'XXXXREserve Backup al?nd?');

  execute immediate vSql;
  pkg_azmm.PRC_AZZM('01/07/2010');
end;
*/

FUNCTION FNC_CreatePeriodPeakAmount (InRefDate  IN DATE)  RETURN VARCHAR2 IS
    vPeakValue   T_AZMM_PEAK.PEAKVALUE%TYPE;
    vPeakValue2   T_AZMM_PEAK.PEAKVALUE%TYPE;
    VIEW_SQLSTMT VARCHAR2(400);
    vSig   NUMBER := 3;
    vPeakSumType dbms_stat_funcs.summaryType;

    cursor cExClaims IS
       select c.claimid, sum(cd.paidamount) paidamount , sysdate  createdate
       from t_claim c, t_claimdetail cd
       where c.claimid = cd.claimid
           and c.statusid not in (2,4)
           and cd.createdate  < ( InRefDate + 1)
       group by c.claimid
       having sum(cd.paidamount) >vPeakValue2 and sum(cd.paidamount) >1 ;
BEGIN

 /*  < -- view yaratma hakk? yok. Su anda bu viewi d?sar?da  olusturuyorum..

    VIEW_SQLSTMT:=' create or replace view vDummy_topclaim_ln as select cd.claimid,ln(sum(cd.paidamount)) as lnpaidamount';
    VIEW_SQLSTMT:=VIEW_SQLSTMT ||' from  t_claim c,t_claimdetail cd ';
    VIEW_SQLSTMT:=VIEW_SQLSTMT ||' where c.claimid=cd.claimid and c.eventdate>='||''''||(InRefDate - 5*365.6)||'''';
    VIEW_SQLSTMT:=VIEW_SQLSTMT ||'  AND c.eventdate<='||''''||InRefDate||'''';
    VIEW_SQLSTMT:=VIEW_SQLSTMT ||' group by cd.claimid having sum(cd.paidamount)>0 order by sum(cd.paidamount) desc';
    -- dbms_output.put_line(VIEW_SQLSTMT);
    EXECUTE IMMEDIATE VIEW_SQLSTMT;
    > -- view yaratma hakk? yok. Su anda bu viewi d?sar?dan cal?st?r?yorum.     */
   -- cla?m deta?l tablosundan Q1 ve Q3'un katr?l de?erler? buluyor. ?ckvalue ?le p?ck hasar al?n?yor ve cursor'a parametre olarak gec?l?yor.
        dbms_stat_funcs.summary('XXXX', 'vDummy_topclaim_ln',  'lnpaidamount',  vSig,  vPeakSumType);
      vPeakValue:=(vPeakSumType.quantile_75 + ((vPeakSumType.quantile_75-vPeakSumType.quantile_25)*3));

       SELECT EXP(vPeakValue) INTO vPeakValue2  FROM DUAL;

        Insert into  T_AZMM_PEAK (PERIOD,PEAKVALUE,CREATEDATE)
         VALUES (TRUNC(InRefDate,'MM'), vPeakValue2, sysdate);

       --insert claim numbers exceeding peak value to t_azmm_peak_claims with the info period
       FOR vRecCl In cExClaims LOOP
            INSERT INTO T_AZMM_PEAK_CLAIMS (PERIOD,CLAIMID,PAIDAMOUNT,CREATEDATE,BRANCH)
              VALUES (TRUNC(InRefDate,'MM'),vRecCl.claimid,vRecCl.paidamount,sysdate,'XXXX');
       END LOOP;


       RETURN ('');
  EXCEPTION
     WHEN OTHERS THEN
      RETURN(XXXX.PKG_ERROR.FNC_GETERRMSG(SQLERRM));
END;

 FUNCTION FNC_GetTermClaimStatu(InClaimId XXXX.t_Claim.CLAIMID%TYPE, InRefDate IN DATE) RETURN NUMBER IS
          vStatu NUMBER;
          vPaidStatu NUMBER;
          vPaidDate date;
          vCount NUMBER;
          vClaImTypeID XXXX.t_ClaimGroup.CLAIMGROUPTYPE%TYPE;
BEGIN
     BEGIN
              SELECT CLAIMGROUPTYPE INTO vClaImTypeID FROM XXXX.t_ClaimGroup WHERE ClaImId = InClaImId;
     EXCEPTION WHEN OTHERS THEN
      vClaImTypeID:=1;
     END;
     SELECT Statusid, PaidDate INTO vPaidStatu, vPaidDate FROM XXXX.t_Claim WHERE ClaimId= InClaimId;

     SELECT COUNT(*) INTO vCount FROM XXXX.t_ClaimStatusLog WHERE ClaimId = InClaimId AND StatusId =6 ;


     SELECT CASE WHEN Clog2.Statusid IN (5,6) THEN TO_CHAR(Clog2.Statusid)
             WHEN Clog2.Statusid =1 AND vClaImTypeID = 1 THEN TO_CHAR(Clog2.Statusid)
             ELSE  Clog2.Statusid||CLOG2.REASONID END
      INTO vStatu
      FROM T_CLAIMSTATUSLOG CLOG2
      WHERE CLOG2.CREATEDATE = (SELECT  MAX(CLOG.CREATEDATE) FROM T_CLAIMSTATUSLOG CLOG
                                                              WHERE CLOG.CLAIMID = InClaimId
                                                              AND CLOG.CREATEDATE < (InRefDate + 1))
          AND CLOG2.CLAIMID = InClaimId
          AND ROWNUM = 1 ;

      IF vPaidStatu = 6 AND vCount = 0 AND vPaidDate < (InRefDate+1) THEN
          vStatu := 6;
      END IF;
      RETURN vStatu;
EXCEPTION WHEN OTHERS THEN
  RETURN -1;
END;

----------------------------------------------------------------------------------------------------------------------------------
 FUNCTION FNC_CreateAZMM_XXXX (InRefDate    DATE) RETURN VARCHAR2 IS
          -- InRefDate  : Ay?n son gunu girilecek. 31.01.2009 gibi
          -- Bu tarihte girilen sistemdeki tum tazminatlar once tekrar de?erlendiriliyor.
          -- T_AZMM tablosu truncate edilmeli ve son de?erleme ile yeninde doldurulumal?
    vTermStatusId               T_AZMM.TERMSTATUSID%TYPE;
   vTermStatusDate           T_AZMM.TERMSTATUSDATE%TYPE;
   vTermPaidDate              T_AZMM.PAIDDATE%TYPE;
   vNewpaidDate               T_AZMM.NEWPAIDDATE%TYPE;
   vConsclusion                  T_AZMM.CONCLUSION%TYPE;
   vFirstOutStandingDate    T_AZMM.FIRSTOUTSTANDINGDATE%TYPE;
   vCount                           NUMBER;
   vPeakValue                    T_AZMM_PEAK.PEAKVALUE%TYPE;
   vCountB                       Number;
   CURSOR cClaim IS

       SELECT  C.CLAIMID, C.EVENTDATE, C.STATUSID, C.STATUSDATE, C.WHERETOPAYID,
                      C.PAIDDATE,
                      SYSDATE CREATEDATE,
                      'XXXX' BRANCH, InRefDate RefDAte
                FROM T_CLAIM C
                WHERE C.EVENTDATE >= '01/06/2004'
                  AND C.EVENTDATE < (InRefDate+1)
                  AND C.Createdate < (InRefDate +1)
                  AND C.StatusId not in (2,4)
                  AND c.claimid not in (select claimid from t_azmm_peak_claims where branch = 'XXXX' and period = trunc(InRefDate,'MM') )
                  --debug
                 --AND c.claimid = 4952031
                  --debug
                  ;

   Cursor cClaimDetail is 
   
     SELECT  C.CLAIMID,  SYSDATE CREATEDATE,
                      'XXXX' BRANCH, InRefDate RefDAte,cd.payableamount,cd.paidamount,cd.invoiceamount,cd.detaillineid,cd.invoicedate,cd.invoicenumber,
                      cd.benefitid,cd.provisionamount,cd.reshare1,cd.reshare2
                FROM T_CLAIM C, T_ClaimDetail CD
                WHERE C.EVENTDATE >= '01/06/2004'
                  AND C.EVENTDATE < (InRefDate+1)
                  AND C.Createdate < (InRefDate +1)
                  AND C.StatusId not in (2,4)
                  AND c.claimid not in (select claimid from t_azmm_peak_claims where branch = 'XXXX' and period = trunc(InRefDate,'MM') )
                  And c.Claimid= Cd.claimid
                  --debug
                 --AND c.claimid = 4952031
                  --debug
                  ;

 BEGIN
   vCount  := 0;
   vCountB :=0;


   FOR vRec in cClaim LOOP
       vCount := vCount + 1;
       vTermStatusId    :=  FNC_GetTermClaimStatu(vRec.CLAIMID, InRefDate);
       vTermStatusDate  :=  FNC_GetClaimStatuDate(vRec.CLAIMID, InRefDate);
       vTermPaidDate    := null;
       vConsclusion     := '?';
       vFirstOutStandingDate  := null;

       SELECT MIN (PERIOD)
       INTO vFirstOutStandingDate
       FROM t_outstandingclaimsreserve
       WHERE CLAIMID = vRec.ClaimId;

        IF (  (  vRec.WhereToPayId = 1 and vTermStatusId IN (1,134,339,5,6) )
             OR
              (  vRec.WhereToPayId <>  1  and vRec.StatusId = 6)
            ) THEN
            --Odendi
              vConsclusion := 'O';
              IF (vRec.StatusId <> 6 ) THEN
                vTermPaidDate :=    XXXX.FNC_GetClaimPaidDate(vRec.CLAIMID, vTermStatusId,  vTermStatusDate,InRefDate);
              ELSE
                 vTermPaidDate  := vRec.paiddate;
              END IF;
         ELSE
           --
            vConsclusion := 'M';

         END IF;

        vNewpaidDate := XXXX.Fnc_Getnewpaiddate(vRec.claimid, InRefDate) ;

        INSERT INTO T_AZMM (CLAIMID,EVENTDATE,STATUSID,STATUSDATE,WHERETOPAYID,
                             TERMSTATUSID  ,TERMSTATUSDATE,PAIDDATE,NEWPAIDDATE,
                             CREATEDATE,BRANCH,REFDATE,CONCLUSION,FIRSTOUTSTANDINGDATE )
                     VALUES (vRec.ClaimId,vRec.eventdate,vRec.StatusId, vRec.statusdate,vRec.wheretopayid,
                       vTermStatusId, vTermStatusDate, vTermPaidDate, vNewPaiddate,
                       sysdate, 'XXXX',InRefdate, vConsclusion,vFirstOutStandingDate ) ;

        IF vCount > 100000 THEN
          vCount := 0;
          commit;
       END IF;

   END LOOP;
   
   
      for vrecdetail in cClaimDetail Loop
         vCountB := vCountB + 1;
         INSERT INTO  T_AZMM_DETAIL (claimid ,branch ,refdate ,createdate, Detaillineid ,invoicenumber ,invoicedate ,invoiceamount,payableamount,paidamount,benefitid,reshare1,reshare2,provisionamount)
                              Values(vrecDetail.claimid,vrecDetail.Branch,vrecDetail.refdate,vrecDetail.createdate,vrecDetail.DetailLineId,vRecDetail.invoicenumber,
                                     vrecDetail.invoicedate,vrecDetail.invoiceamount,vrecdetail.payableamount,vrecdetail.paidamount,vrecDetail.BenefitId,vrecDetail.reshare1,vrecDetail.reshare2,vrecDetail.provisionamount);
       
           IF vCountB > 100000 THEN
              vCountB := 0;
             commit;
           END IF;                                     
      END LOOP;

   
   
   COMMIT;
   RETURN ('');
  /* EXCEPTION
     WHEN OTHERS THEN
      RETURN(XXXX.PKG_ERROR.FNC_GETERRMSG(SQLERRM));*/
 END;
---------------------------------------------------------------------------------------------------------
 FUNCTION FNC_CreateAZMM_XXXX_Out (InRefDate    DATE) RETURN VARCHAR2 IS
          -- InRefDate  : Ay?n son gunu girilecek. 31.01.2009 gibi
          -- Bu tarihte girilen sistemdeki tum tazminatlar?n ustteki prosedur ile de?erlendirilen odeme tarihlerine gore muallak
          -- oldu?u tarihler icn ayr? sat?rlar olusturuluyor. .
          -- T_AZMM_OUTSTANDING  tablosu truncate edilmeli ve son de?erleme ile yeninde doldurulumal?
   vTermStatusId    T_AZMM.TERMSTATUSID%TYPE;
   vTermStatusDate  T_AZMM.TERMSTATUSDATE%TYPE;
   vTermPaidDate    T_AZMM.PAIDDATE%TYPE;
   vNewpaidDate     T_AZMM.NEWPAIDDATE%TYPE;
   vConsclusion     T_AZMM.CONCLUSION%TYPE;
   vFirstOutStandingDate T_AZMM.FIRSTOUTSTANDINGDATE%TYPE;
   vCount           NUMBER;
   vOutPaidAmount     T_OUTSTANDINGCLAIMSRESERVE.PAYABLEAMOUNT%TYPE;
   vOutReinsAmount     T_AZMM_OUTSTANDING.REINSSHARE%TYPE;
   vPeakValue        T_AZMM_PEAK.PEAKVALUE%TYPE;
   vPeakLastValue   T_AZMM_PEAK.PEAKVALUE%TYPE;
   vpolstartdate          T_POLICY.STARTDATE%TYPE;
   CURSOR cClaim IS

                   select   c.claimid, c.createdate, az.newpaiddate, periodt.period ,
                                     0 PAIDAMOUNT
                 from t_claim c,  t_azmm az , ( select distinct period from t_policychange ) periodt
                 where c.claimid = az.claimid and
                 --debug
                  --  az.claimid = 4951971 and
                  periodt.period between  trunc(c.createdate,'MM') and (least(trunc(nvl(az.newpaiddate,sysdate) ,'MM'), trunc(InRefdate)  )  -1);

   CURSOR cClaimRedDavalik IS
        select c.claimid, ccp.period createdate , ccp.period newpaiddate, ccp.period, ccp.davatutari 
         from XXXX.t_caseclaimperiod ccp, XXXX.t_claim c
        where ccp.tazminat = to_char(c.claimid)
        and c.statusid in (2,4);

  CURSOR cOut IS
     select * from t_azmm_outstanding;

   cursor cReas is
      select azo.claimid, azo.period, nvl(sum(nvl(reins.debit,0)),0)  reinsAmt 
      from t_azmm_outstanding azo, t_reinsurance reins
      where azo.claimid = reins.claimid
      and reins.startdate <= azo.period
      group by azo.claimid, azo.period;
 BEGIN
   vCount := 0;
   SELECT azp.peakvalue INTO vPeakValue
   FROM T_AZMM_PEAK azp
   WHERE PERIOD = TRUNC(InRefDate,'MM')
   AND ROWNUM = 1;

   FOR vRec in cClaim LOOP
        vCount := vCount +1;
        INSERT INTO T_AZMM_OUTSTANDING(CLAIMID,CREATEDATE,PAIDDATE,PERIOD,BRANCH,PAIDAMOUNT )
                     VALUES (vRec.ClaimId,vRec.createdate,vRec.newpaiddate, vRec.period, 'XXXX',0) ;

        IF vCount > 10000 THEN
          vCount := 0;
          commit;
       END IF;
   END LOOP;
   
   FOR vRecDavalik in cClaimRedDavalik LOOP
        vCount := vCount +1;
        INSERT INTO T_AZMM_OUTSTANDING(CLAIMID,CREATEDATE,PAIDDATE,PERIOD,BRANCH,PAIDAMOUNT )
                     VALUES (vRecDavalik.ClaimId,vRecDavalik.createdate,vRecDavalik.newpaiddate, vRecDavalik.period, 'XXXX',vRecDavalik.Davatutari) ;
   END LOOP;
   commit;

   --update paid amounts
   FOR vRecOut IN cOUT LOOP
        BEGIN
            if vRecOut.Paidamount <> 0 Then -- iptal ret daval�klardan gelen tutar, tekrar hesaplamaya gerek yok.
               vOutPaidAmount := vRecOut.Paidamount;
            else
                       select  nvl(sum( nvl(decode(nvl(ccp.davatutari,0) ,0,our.payableamount, ccp.davatutari) ,0)),0)  
                       into vOutPaidAmount
                       from t_outstandingclaimsreserve our, t_caseclaimperiod ccp
                       where our.claimid = vRecOut.claimid
                          and our.period = vRecOut.period
                          and our.executeorder  = 2
                          and our.statusid not in (2,4 ) 
                            and our.period = ccp.period (+)
                          and to_char(our.claimid )= ccp.tazminat(+);
              end if;

        EXCEPTION WHEN others THEN
            vOutPaidAmount := 0;
        END;



         if nvl(vOutPaidAmount ,0) <> 0  then

              if   vOutPaidAmount > vPeakValue then
                  select sum(cdIn.payableamount) into vOutPaidAmount
                  from t_claimdetail cdIn
                  where cdIn.claimid = vRecOut.claimid
                      and trunc(cdIn.createdate,'MM')  <=   InRefDate;

              end if;

               update  T_AZMM_OUTSTANDING set PAIDAMOUNT = vOutPaidAmount  where claimid = vRecOut.claimid and period = vRecOut.period;
              -- muallak tutarlar? odeninceye kadarki donemler icinde peak valuedam
              -- fazla de?ere ulasabilirler. E?er ilgili donemdepeak valueu ast?ysa
              -- muallak merdivenine dahil etme.
             vCount := vCount +1;
              IF vCount > 10000 THEN
                    vCount := 0;
              END IF;
         end if;
   END LOOP;

   COMMIT;

  for vREc in cReas  loop
      update t_azmm_outstanding set reinsshare = vRec.reinsAmt
       where claimid = vRec.claimid and period = vRec.period;
  end loop;

   COMMIT;
   RETURN ('');
  /* EXCEPTION
     WHEN OTHERS THEN
      RETURN(XXXX.PKG_ERROR.FNC_GETERRMSG(SQLERRM));*/
 END;

---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmPeriodXXXXPaid (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMM formatl? olarak tablo olusturur
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is
    select TO_CHAR(c.eventdate,'YYYY_MM') as OLAY_YIL_AY ,
          TO_CHAR(az.newpaiddate,'YYYY_MM') as BORDRO_YIL_AY,
          round( sum( case when
                       (  (select count(*) from t_caseclaimperiod where tazminat = to_char(c.claimid ) and period =trunc(InRefDate,'MM')  )=0 or c.statusid = 6 ) THEN
                                   cd.paidamount
                                else
                                       (select sum(davatutari ) from t_caseclaimperiod where tazminat = to_char(c.claimid )   and period =trunc(InRefDate,'MM') ) /
                                       (select  count(distinct  tazminat) from t_caseclaimperiod where davasistemno in  
                                                 (select davasistemno from t_caseclaimperiod  where tazminat = to_char(c.claimid )   and period =trunc(InRefDate,'MM'))  ) 
                                  end ))   as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(c.eventdate,'YYYY_MM') ORDER BY TO_CHAR(az.newPAIDdate,'YYYY_MM')) AS ROWNO
    from
      t_claim c,t_claimdetail cd , t_azmm az
    where c.claimid=cd.claimid
     AND c.claimid = az.claimid
     --odenenler icin paiddate alan? dolu
     AND az.newpaiddate is not null
     AND branch = 'XXXX'
     --AND c.eventdate>=a1/*hprescdat>=a1 */
    -- AND c.eventdate<=a2/*hprescdat<=a2 */
     --and hgnbr=71
           GROUP BY TO_CHAR(c.eventdate,'YYYY_MM'),TO_CHAR(az.newpaiddate,'YYYY_MM')
           ORDER BY TO_CHAR(c.eventdate,'YYYY_MM'),TO_CHAR(az.newpaiddate,'YYYY_MM');

    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPERIOD';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_OLAY_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR(ADD_MONTHS(ADD_MONTHS(InRefDate,1),-1 * array_a1),'YYYY_MM') ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(r.BORDRO_YIL_AY,'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(r.BORDRO_YIL_AY,'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      --dbms_output.put_line (INSERT_FINAL_VALUE);
                      --dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.bordro_yil_ay ||'-'||sqlerrm);
             END;
       end loop;



End;
--------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmPeriodXXXXOut (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMM formatl? olarak tablo olusturur . Olay tarihi baz?nda verdi?i icin verilen listeyi kolon baz?nda
-- toplad???nda o ay?n muakka??na ulas?rs?n.
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is
     select TO_CHAR(az.createdate, 'YYYY_MM') as OLAY_YIL_AY ,
          TO_CHAR(az.period,'YYYY_MM') as BORDRO_YIL_AY ,
          round( sum(az.paidamount))   as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(az.createdate,'YYYY_MM') ORDER BY TO_CHAR(az.period,'YYYY_MM')) AS ROWNO
    from
              t_azmm_outstanding  az
    where   branch = 'XXXX'

      --debug
     -- AND az.createdate > '01/01/2010'
      --AND az.claimid = 4231267
     --debug
           GROUP BY TO_CHAR(az.createdate,'YYYY_MM'),TO_CHAR(az.period,'YYYY_MM')
           ORDER BY TO_CHAR(az.createdate,'YYYY_MM'),TO_CHAR(az.period,'YYYY_MM');

    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPERIODOUT';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_YARAT_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR(ADD_MONTHS(ADD_MONTHS(InRefDate,1),-1 * array_a1),'YYYY_MM') ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_AY),'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN

                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_AY),'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      --dbms_output.put_line (INSERT_FINAL_VALUE);
                      --dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.bordro_yil_ay ||'-'||sqlerrm);
             END;
       end loop;

      IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN

               INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
               INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
               EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
               EXEcUTE IMMEDIATE 'COMMIT';
        END IF;


End;


---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmPeriodXXXXOutFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMMnin i formatl? olarak tablo olusturur .
--Kolonlarda yarat?l?s tarihi ile odeme tarihi aras?ndaki ay say?s? yer al?yor.
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

                select TO_CHAR(az.createdate, 'YYYY_MM') as OLAY_YIL_AY ,
         months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )  as BORDRO_YIL_DELTA ,
          sum(az.paidamount)     as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(az.createdate,'YYYY_MM') ORDER BY months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ) AS ROWNO
          from    t_azmm_outstanding  az
          where branch = 'XXXX'


               --debug
               -- AND az.createdate > '01/01/2010'
               --AND az.claimid = 4231267
                --debug
          GROUP BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )
          ORDER BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPERIODOUTFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_YARAT_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR( array_a1) ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                          INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                          INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                  --    dbms_output.put_line (INSERT_FINAL_VALUE);
                 --     dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                   --   dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
     --insert last createdate
       IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;

End;


   ---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmPeriodXXXXPadFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMM formatl? olarak tablo olusturur
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

     select TO_CHAR(c.eventdate,'YYYY_MM') as OLAY_YIL_AY ,
               case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                     0
                   else
                      months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                end   as BORDRO_YIL_DELTA ,
                 ( sum( case when
                       (  (select count(*) from t_caseclaimperiod where tazminat = to_char(c.claimid ) and period =trunc(InRefDate,'MM')  )=0 or c.statusid = 6 ) THEN
                                   cd.paidamount
                                else
                                       (select sum(davatutari ) from t_caseclaimperiod where tazminat = to_char(c.claimid )   and period =trunc(InRefDate,'MM') ) /
                                       (select  count(distinct  tazminat) from t_caseclaimperiod where davasistemno in  
                                                 (select davasistemno from t_caseclaimperiod  where tazminat = to_char(c.claimid )   and period =trunc(InRefDate,'MM'))  ) 
                                  end ))   as TOPLAMIC,
                ROW_NUMBER() OVER (PARTITION BY TO_CHAR(c.eventdate,'YYYY_MM') ORDER BY  case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                                                                                                                         0
                                                                                                                                                                        else
                                                                                                                                                                                                         months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                                                                                                                                         end )    AS ROWNO
         from
                t_claim c,t_claimdetail cd , t_azmm az
         where c.claimid=cd.claimid
           AND c.claimid = az.claimid
            --odenenler icin paiddate alan? dolu
            AND az.newpaiddate is not null
            AND branch = 'XXXX'
            --debug
            --AND trunc(az.eventdate ,'MM') = '01/07/2010'
           GROUP BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end
           ORDER BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                 0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end   ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPERIODFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_OLAY_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'||  TO_CHAR( array_a1)  ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA) ,'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      --dbms_output.put_line (INSERT_FINAL_VALUE);
                      --dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
       
        IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;

End;
---------------------------------------------------------------------------

PROCEDURE PRC_AZZM (InPeriod    DATE) IS
   vtemp  varchar2(1000);
  --vSql   varchar(4000) := 'create or replace view vDummy_topclaim_ln as select cd.claimid,ln(sum(cd.paidamount)) as lnpaidamount from  t_claim c,t_claimdetail cd  where c.claimid=cd.claimid and c.eventdate>=''29/07/2005''  AND c.eventdate<=''31/07/2010'' group by cd.claimid having sum(cd.paidamount)>0 order by sum(cd.paidamount) desc';
begin

   -- SELECT * FROM T_AZMM
   -- SELECT * FROM T_AZMM_OUTSTANDING
  -- execute immediate vSql;
   vtemp := pkg_azmm.FNC_CreatePeriodPeakAmount(last_day(InPeriod) );
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'PeakAmount Hesapla Tamamland?:'|| vTemp);

/*   EXECUTE IMMEDIATE 'CREATE TABLE XXXXRESERVE.T_AZMMBUP'|| to_char(InPeriod,'MM_RRRR') ||' as (select * from t_azmm) ';
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'XXXXREserve Backup al?nd?');
*/
  EXECUTE IMMEDIATE 'Truncate table T_AZMM';
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'T_AZMM tablosu truncate edildi');
   
    EXECUTE IMMEDIATE 'Truncate table T_AZMM_DETAIL';
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'T_AZMM_DETAIL tablosu truncate edildi'); 

   vtemp := pkg_azmm.FNC_CreateAZMM_XXXX(last_day(InPeriod));
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'Yeni T_AZMM kay?tlar? yarat?ld?');

   if vTemp is null then
      EXECUTE IMMEDIATE 'Truncate table t_azmm_outstanding';
      vtemp := pkg_azmm.FNC_CreateAZMM_XXXX_Out(last_day(InPeriod) );
   end if;
    pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'T_AZMMOutStanding tamamland? ');


   --select * from T_AZMMPERIOD2010_07
   pkg_azmm.Prc_CreateAzmmPeriodXXXXPaid('01/01/1985','01/01/2050',last_day(InPeriod));
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'Prc_CreateAzmmPeriodXXXXPaid  Tamamland?:');

  -- select * from T_AZMMPERIODFIN2010_07
   pkg_azmm.Prc_CreateAzmmPeriodXXXXPadFin('01/01/1985','01/01/2050',last_day(InPeriod));
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'Prc_CreateAzmmPeriodXXXXPadFin  Tamamland?:');

  -- select * from T_AZMMPERIODOUT2010_07
   pkg_azmm.Prc_CreateAzmmPeriodXXXXOut('01/01/1985','01/01/2050',last_day(InPeriod));
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'Prc_CreateAzmmPeriodXXXXPaid  Tamamland?:');

   --select * from T_AZMMPERIODOUTFIN2010_07
   pkg_azmm.Prc_CreateAzmmPeriodXXXXOutFin('01/01/1985','01/01/2050',last_day(InPeriod));
   pkg_error.PRC_Log('azmm',sysdate,InPeriod || 'Prc_CreateAzmmPeriodXXXXOutFin8  Tamamland?:');

   --select * from T_AZMMPERIODCNTFIN2010_07;
    pkg_azmm. Prc_CreateAzmmPrdXXXXPadCntFin('01/01/1985','01/01/2050',last_day(InPeriod));
    pkg_error.PRC_Log('azmm',sysdate,InPeriod || ' Prc_CreateAzmmPrdXXXXPadCntFin  Tamamland?:');

   -- select * from T_AZMMPERIODOUTCNTFIN2010_07;
    pkg_azmm. Prc_CreateAzmmPrdXXXXOutCntFin('01/01/1985','01/01/2050',last_day(InPeriod));
    pkg_error.PRC_Log('azmm',sysdate,InPeriod || ' Prc_CreateAzmmPrdXXXXOutCntFin Tamamland?:');

    --select * from T_AZMMPAIDREINS2010_07
    pkg_azmm.Prc_CreateAzmmXXXXPadREINS ('01/01/1985','01/01/2050',last_day(InPeriod) );
    pkg_error.PRC_Log('azmm',sysdate,InPeriod || ' Prc_CreateAzmmXXXXPadREINS Tamamland?:');

    -- select * from T_AZMMOUTREINS12010_07
    pkg_azmm.Prc_CreateAzmmXXXXOutREINS ('01/01/1985','01/01/2050',last_day(InPeriod) );
    pkg_error.PRC_Log('azmm',sysdate,InPeriod || ' Prc_CreateAzmmXXXXOutREINS Tamamland?:');

   --------------------------------------------------------------------------------------
    --hayat
   EXECUTE IMMEDIATE 'CREATE TABLE XXXXRESERVE.T_AZMMBUPHAYAT'|| to_char(InPeriod,'MM_RRRR') || ' as (select * from t_azmm_HAYAT) ';
   EXECUTE IMMEDIATE 'Truncate table T_AZMM_HAYAT';
   vtemp := pkg_azmm.FNC_CreateAZMM_HAYAT(last_day(InPeriod));

   if vTemp is null then
      EXECUTE IMMEDIATE 'Truncate table t_azmm_hayat_outstanding';
      vtemp := pkg_azmm.FNC_CreateAZMM_HAYAT_Out(InPeriod);
   end if;


--SELECT * FROM T_AZMMPERIODFIN_THS2010_07
--SELECT * FROM T_AZMMPERIODFIN_KZA2010_07
   pkg_azmm.Prc_CreateAzmmPeriodHYTPadFin(InPeriod,'THS');
   pkg_azmm.Prc_CreateAzmmPeriodHYTPadFin(InPeriod,'KZA');

--SELECT * FROM T_AZMMPRDOUTFINTHS2010_07
--SELECT * FROM T_AZMMPRDOUTFINKZA2010_07
   pkg_azmm.Prc_CreateAzmmPeriodHYTOutFin(InPeriod,'THS');
   pkg_azmm.Prc_CreateAzmmPeriodHYTOutFin(InPeriod,'KZA');

--SELECT * FROM T_AZMMPRDCNTFINTHS2010_07
--SELECT * FROM T_AZMMPRDCNTFINKZA2010_07
  pkg_azmm.Prc_CreateAzmmPrdHYTPadCntFin(InPeriod,'THS');
  pkg_azmm.Prc_CreateAzmmPrdHYTPadCntFin(InPeriod,'KZA');

--SELECT * FROM T_AZMMPRDCNTOUTFIN_THS2010_07
--SELECT * FROM T_AZMMPRDCNTOUTFIN_KZA2010_07
   pkg_azmm.Prc_CreateAzmmPrdHYTOutCntFin(InPeriod,'THS');
   pkg_azmm.Prc_CreateAzmmPrdHYTOutCntFin(InPeriod,'KZA');


END;


PROCEDURE Prc_CreateAzmmPrdXXXXPadCntFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMM formatl? olarak tablo olusturur
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

     select TO_CHAR(c.eventdate,'YYYY_MM') as OLAY_YIL_AY ,
               case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                     0
                   else
                      months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                end   as BORDRO_YIL_DELTA ,
                count( distinct (c.claimid ) )   as TOPLAMIC,
                ROW_NUMBER() OVER (PARTITION BY TO_CHAR(c.eventdate,'YYYY_MM') ORDER BY  case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                                                                                                                         0
                                                                                                                                                                        else
                                                                                                                                                                                                         months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                                                                                                                                         end )    AS ROWNO
         from
                t_claim c,t_claimdetail cd , t_azmm az
         where c.claimid=cd.claimid
           AND c.claimid = az.claimid
            --odenenler icin paiddate alan? dolu
            AND az.newpaiddate is not null
            AND branch = 'XXXX'
            --debug
            --AND trunc(az.eventdate ,'MM') = '01/07/2010'
           GROUP BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end
           ORDER BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                 0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end   ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPERIODCNTFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_OLAY_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'||  TO_CHAR( array_a1)  ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA) ,'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      --dbms_output.put_line (INSERT_FINAL_VALUE);
                      --dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
        IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;
     EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmPrdXXXXPadCntFin '|| sqlerrm );
End;

-----------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmPrdXXXXOutCntFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMMnin i formatl? olarak tablo olusturur .
--Kolonlarda yarat?l?s tarihi ile odeme tarihi aras?ndaki ay say?s? yer al?yor.
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

                select TO_CHAR(az.createdate, 'YYYY_MM') as OLAY_YIL_AY ,
         months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )  as BORDRO_YIL_DELTA ,
         count( distinct (az.paidamount))     as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(az.createdate,'YYYY_MM') ORDER BY months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ) AS ROWNO
          from    t_azmm_outstanding  az
          where branch = 'XXXX'


               --debug
               -- AND az.createdate > '01/01/2010'
               --AND az.claimid = 4231267
                --debug
          GROUP BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )
          ORDER BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPERIODOUTCNTFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_YARAT_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR( array_a1) ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                          INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                          INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                  --    dbms_output.put_line (INSERT_FINAL_VALUE);
                 --     dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                   --   dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
     --insert last createdate
       IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;
      EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmPrdXXXXOutCntFin '|| sqlerrm );
End;

--------------------------------------------------------------------------------------------------
 FUNCTION FNC_CreateAZMM_HAYAT (InRefDate    DATE) RETURN VARCHAR2 IS
          -- InRefDate  : Ay?n son gunu girilecek. 31.01.2009 gibi
          -- Bu tarihte girilen sistemdeki tum tazminatlar once tekrar de?erlendiriliyor.
          -- T_AZMM tablosu truncate edilmeli ve son de?erleme ile yeninde doldurulumal?
   vTermStatusId               T_AZMM.TERMSTATUSID%TYPE;
   vTermStatusDate           T_AZMM.TERMSTATUSDATE%TYPE;
   vTermPaidDate              T_AZMM.PAIDDATE%TYPE;
   vNewpaidDate               T_AZMM.NEWPAIDDATE%TYPE;
   vConsclusion                  T_AZMM.CONCLUSION%TYPE;
   vFirstOutStandingDate    T_AZMM.FIRSTOUTSTANDINGDATE%TYPE;
   vCount                           NUMBER;
   vPeakValue                    T_AZMM_PEAK.PEAKVALUE%TYPE;
   CURSOR cClaimH IS
      --HAYAT
                     select distinct p.polid,
                            htm.dosyano,
                            htm.sonerneden,
                            hi.hastar,
                            hi.ihbartar,
                            htm.odetar,
                             sum(decode((select count(*) from t_caseclaimperiod where tazminat = (htm.sonerneden || '-'|| htm.dosyano||'-'||htm.polid ) and period  = ADD_MONTHS((InRefDate+1),-1) ), 0,htm.dovtutar,(select sum(davatutari) from t_caseclaimperiod where tazminat = (htm.sonerneden || '-'|| htm.dosyano||'-'||htm.polid ) and period  = ADD_MONTHS((InRefDate+1),-1))))  tazminat,
                            round(sum(decode(hi.ihbstatus, 40,(nvl((htm.dovtutar*akalib.kurof(htm.dovkod, 'ES', htm.odetar)),0)) ,decode(p.branskod,'K',(nvl((htm.dovtutar*akalib.kurof(htm.dovkod, 'DA', InRefDate)),0)),'H',(nvl((htm.dovtutar*akalib.kurof(htm.dovkod, 'ES', InRefDate)),0)),null))),2) tazminatTL,
                            decode(hi.ihbstatus, 40, 'Odenmis', 'Muallak') Durum,
                            decode (p.urunkod,'TH','THS','KZA') Branch
              from police         p,
                   zeyl           z,
                   sigortali      s,
                   urun           u,
                   kaynaklar      k,
                   person         pe,
                   person         per,
                   person         per1,
                   htazodemaster  htm,
                   htazdetay      hd,
                   hihbar         hi,
                   hsonerdeger    hsd,
                   htazmaster     hm,
                   hsonerredneden hn
             where 1 = 1
               and z.polid = p.polid
               and z.zeylno = p.sonzeylno
               and s.polid = z.polid
               and s.zeylno = z.zeylno
               and u.kod = p.urunkod
               and k.kaynakkodu = decode(z.elemanno, '0', z.acenteno, z.elemanno)
               and pe.pid = s.pid
               and per.pid = k.pid
               and per1.pid = z.sigetid
               and htm.polid = p.polid
               and hd.dosyano = htm.dosyano
               and hd.sonerneden = htm.sonerneden
               and hd.polid = htm.polid
               and ((hd.taztantar <= InRefDate AND
                   htm.sonerneden not in ('TV', 'VG'))
                   OR (p.polbittar <= InRefDate AND htm.sonerneden in ('TV', 'VG')))
               and hd.tazdovtutar <> 0
               and hi.dosyano = hd.dosyano
               and hi.sonerneden = hd.sonerneden
               and hsd.degerkod = hd.degerkod
                 and hsd.degerkod = htm.degerkod
               and hm.polid = htm.polid
               and hm.sonerneden = htm.sonerneden
               and hm.dosyano = htm.dosyano
               and hd.redcek = hn.redkod(+)
               and p.branskod = 'K'
               and htm.degerkod not in (62)
               and (hd.redtar > InRefDate or hd.redtar is null)
               and not (htm.sonerneden in ('II', 'I', 'TI') and exists
                     (select 1
                           from zeyl
                          where polid = hm.polid
                            and zeyltip = '22'
                            and zeylno > hm.zeylno
                            and tantar <= InRefDate))
               --and p.polid in (35179,343798,489808,482924,443951,533071,458070,458152,458239,532355)
               and ( (hi.ihbstatus = 40)  and (htm.odetar is not null)
                    or
                     (hi.ihbstatus <> 40) and (htm.odetar is null)
                   )
             group by p.polid,
                      htm.dosyano,
                      htm.sonerneden,
                      hi.hastar,
                      hi.ihbartar,
                      htm.odetar,
                      decode(hi.ihbstatus, 40, 'Odenmis', 'Muallak'),
                      decode (p.urunkod,'TH','THS','KZA');



 BEGIN
   vCount := 0;


   FOR vRec in cClaimH LOOP
        vCount := vCount + 1;
        INSERT INTO T_AZMM_HAYAT (DOSYANO,HASTAR,DURUM,
                             ODETAR,IHBARTAR,
                             CREATEDATE,BRANCH,REFDATE,FIRSTOUTSTANDINGDATE ,PAIDAMOUNT)
                     VALUES (vRec.dosyano,vRec.hastar,vRec.durum, vRec.odetar,vRec.ihbartar,
                       sysdate, vRec.branch,InRefdate, vFirstOutStandingDate,vRec.tazminat ) ;

        IF vCount > 100000 THEN
          vCount := 0;
          commit;
       END IF;

   END LOOP;
   COMMIT;
   RETURN ('');
    EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'FNC_CreateAZMM_HAYAT '|| sqlerrm );

 END;


 ----------------------------------------------------------------------------------------------------------------------

 FUNCTION FNC_CreateAZMM_HAYAT_Out (InRefDate    DATE) RETURN VARCHAR2 IS
          -- InRefDate  : Ay?n son gunu girilecek. 31.01.2009 gibi
          -- Bu tarihte girilen sistemdeki tum tazminatlar?n ustteki prosedur ile de?erlendirilen odeme tarihlerine gore muallak
          -- oldu?u tarihler icn ayr? sat?rlar olusturuluyor. .
          -- T_AZMM_OUTSTANDING  tablosu truncate edilmeli ve son de?erleme ile yeninde doldurulumal?
   vTermStatusId    T_AZMM.TERMSTATUSID%TYPE;
   vTermStatusDate  T_AZMM.TERMSTATUSDATE%TYPE;
   vTermPaidDate    T_AZMM.PAIDDATE%TYPE;
   vNewpaidDate     T_AZMM.NEWPAIDDATE%TYPE;
   vConsclusion     T_AZMM.CONCLUSION%TYPE;
   vFirstOutStandingDate T_AZMM.FIRSTOUTSTANDINGDATE%TYPE;
   vCount           NUMBER;
   vOutPaidAmount     T_OUTSTANDINGCLAIMSRESERVE.PAYABLEAMOUNT%TYPE;

   CURSOR cClaimHO IS

                   select   azh.dosyano, azh.hastar,azh.ihbartar, azh.odetar, periodt.period ,azh.branch, durum,
                                     azh. PAIDAMOUNT
                 from   t_azmm_hayat azh , ( select distinct period from t_policychange ) periodt
                 where
                 --debug
                 --   az.claimid = 4231267 and
                  periodt.period between  trunc(azh.ihbartar,'MM') and (trunc(nvl(azh.odetar,sysdate+500),'MM') -1);

 BEGIN
   vCount := 0;

   FOR vRec in cClaimHO LOOP
        vCount := vCount +1;
        INSERT INTO T_AZMM_HAYAT_OUTSTANDING(DOSYANO,HASTAR,DURUM,ODETAR,
                                                                                             CREATEDATE,BRANCH,PERIOD,PAIDAMOUNT,IHBARTAR)
                     VALUES (vRec.dosyano, vRec.hastar, vRec.durum, vRec.odetar,
                                                                                             sysdate,vRec.branch,vRec.period, vRec.paidamount,vRec.ihbartar) ;

        IF vCount > 100000 THEN
          vCount := 0;
          commit;
       END IF;
   END LOOP;

   COMMIT;
   RETURN ('');
      EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'FNC_CreateAZMM_HAYAT_Out '|| sqlerrm );

 END;


   ---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmPeriodHYTPadFin ( InRefDate IN DATE, InBranch IN T_AZMM_HAYAT.Branch%TYPE )
--AZMM formatl? olarak tablo olusturur
IS

  cursor OLAY_CRSR is

     select TO_CHAR(azh.hastar,'YYYY_MM') as OLAY_YIL_AY ,
               case when ( months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )) <0 then
                     0
                   else
                      months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )
                end   as BORDRO_YIL_DELTA ,
                round( sum(azh.paidamount))   as TOPLAMIC,
                ROW_NUMBER() OVER (PARTITION BY TO_CHAR(azh.hastar,'YYYY_MM') ORDER BY  case when ( months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )) <0 then
                                                                                                                                                                                                         0
                                                                                                                                                                        else
                                                                                                                                                                                                         months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )
                                                                                                                                                                                                         end )    AS ROWNO
         from
                t_azmm_hayat azh
         where
            --odenenler icin paiddate alan? dolu
            azh.odetar is not null
            and azh.branch = InBranch
            --debug
            --AND trunc(az.eventdate ,'MM') = '01/07/2010'
           GROUP BY TO_CHAR(azh.hastar,'YYYY_MM'), case when ( months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )) <0 then
                                                                                                0
                                                                                           else
                                                                                              months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )
                                                                                           end
           ORDER BY TO_CHAR(azh.hastar,'YYYY_MM'), case when ( months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )) <0 then
                                                                                                 0
                                                                                           else
                                                                                              months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )
                                                                                           end   ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPERIODFIN_';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || InBranch||TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_OLAY_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'||  TO_CHAR( array_a1)  ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR  loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA) ,'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      --dbms_output.put_line (INSERT_FINAL_VALUE);
                      --dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
      EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmPeriodHYTPadFin '|| sqlerrm || '-'|| INSERT_FINAL_VALUE);

End;
---------------------------------------------------------------------------------------------------------------------

PROCEDURE Prc_CreateAzmmPeriodHYTOutFin ( InRefDate IN DATE,InBranch IN T_AZMM_HAYAT.Branch%TYPE )
--AZMMnin i formatl? olarak tablo olusturur .
--Kolonlarda yarat?l?s tarihi ile odeme tarihi aras?ndaki ay say?s? yer al?yor.
IS

  cursor OLAY_CRSR  is

                select TO_CHAR(azh.ihbartar, 'YYYY_MM') as OLAY_YIL_AY ,
         months_between(trunc(azh.period,'MM') ,trunc( azh.ihbartar,'MM') )  as BORDRO_YIL_DELTA ,
         round( sum(azh.paidamount))     as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(azh.ihbartar,'YYYY_MM') ORDER BY months_between(trunc(azh.period,'MM') ,trunc( azh.ihbartar,'MM') ) ) AS ROWNO
          from    t_azmm_hayat_outstanding  azh
          where branch = InBranch
               --debug
               -- AND az.createdate > '01/01/2010'
               --AND az.claimid = 4231267
                --debug
          GROUP BY TO_CHAR(azh.ihbartar,'YYYY_MM'),months_between(trunc(azh.period,'MM') ,trunc( azh.ihbartar,'MM') )
          ORDER BY TO_CHAR(azh.ihbartar,'YYYY_MM'),months_between(trunc(azh.period,'MM') ,trunc( azh.ihbartar,'MM') ) ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPRDOUTFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || InBranch||TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_YARAT_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR( array_a1) ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                          INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                          INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                  --    dbms_output.put_line (INSERT_FINAL_VALUE);
                 --     dbms_output.put_line('');
                --      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
               --       INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                    dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
     --insert last createdate
       IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;
      EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmPeriodHYTOutFin:: '|| sqlerrm || '-'|| INSERT_FINAL_VALUE);

End;


  ---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmPrdHYTPadCntFin ( InRefDate IN DATE ,InBranch IN T_AZMM_HAYAT.Branch%TYPE)
--AZMM formatl? olarak tablo olusturur
IS

  cursor OLAY_CRSR is

     select TO_CHAR(azh.hastar,'YYYY_MM') as OLAY_YIL_AY ,
               case when ( months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )) <0 then
                     0
                   else
                      months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )
                end   as BORDRO_YIL_DELTA ,
                count( distinct(azh.dosyano))   as TOPLAMIC,
                ROW_NUMBER() OVER (PARTITION BY TO_CHAR(azh.hastar,'YYYY_MM') ORDER BY  case when ( months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )) <0 then
                                                                                                                                                                                                         0
                                                                                                                                                                        else
                                                                                                                                                                                                         months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )
                                                                                                                                                                                                         end )    AS ROWNO
         from
                t_azmm_hayat azh
         where
            --odenenler icin paiddate alan? dolu
            azh.odetar is not null
            and branch = InBranch
            --debug
            --AND trunc(az.eventdate ,'MM') = '01/07/2010'
           GROUP BY TO_CHAR(azh.hastar,'YYYY_MM'), case when ( months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )) <0 then
                                                                                                0
                                                                                           else
                                                                                              months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )
                                                                                           end
           ORDER BY TO_CHAR(azh.hastar,'YYYY_MM'), case when ( months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )) <0 then
                                                                                                 0
                                                                                           else
                                                                                              months_between(trunc(azh.odetar,'MM') ,trunc (azh.hastar,'MM') )
                                                                                           end   ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPRDCNTFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || InBranch||TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_OLAY_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'||  TO_CHAR( array_a1)  ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR  loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA) ,'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      --dbms_output.put_line (INSERT_FINAL_VALUE);
                      --dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
       EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmPrdHYTPadCntFin:: '|| sqlerrm || '-'|| INSERT_FINAL_VALUE);

End;
---------------------------------------------------------------------------------------------------------------------

PROCEDURE Prc_CreateAzmmPrdHYTOutCntFin ( InRefDate IN DATE,InBranch IN T_AZMM_HAYAT.Branch%TYPE )
--AZMMnin i formatl? olarak tablo olusturur .
--Kolonlarda yarat?l?s tarihi ile odeme tarihi aras?ndaki ay say?s? yer al?yor.
IS

  cursor OLAY_CRSR  is

                select TO_CHAR(azh.ihbartar, 'YYYY_MM') as OLAY_YIL_AY ,
         months_between(trunc(azh.period,'MM') ,trunc( azh.ihbartar,'MM') )  as BORDRO_YIL_DELTA ,
         count( distinct(azh.dosyano))     as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(azh.ihbartar,'YYYY_MM') ORDER BY months_between(trunc(azh.period,'MM') ,trunc( azh.ihbartar,'MM') ) ) AS ROWNO
          from    t_azmm_hayat_outstanding  azh
          where branch = InBranch
               --debug
               -- AND az.createdate > '01/01/2010'
               --AND az.claimid = 4231267
                --debug
          GROUP BY TO_CHAR(azh.ihbartar,'YYYY_MM'),months_between(trunc(azh.period,'MM') ,trunc( azh.ihbartar,'MM') )
          ORDER BY TO_CHAR(azh.ihbartar,'YYYY_MM'),months_between(trunc(azh.period,'MM') ,trunc( azh.ihbartar,'MM') ) ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPRDCNTOUTFIN_';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || Substr(InBranch,1,3) || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_YARAT_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR( array_a1) ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                          INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                          INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                  --    dbms_output.put_line (INSERT_FINAL_VALUE);
                 --     dbms_output.put_line('');
                --      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
               --       INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                    dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
     --insert last createdate
       IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;
         EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmPrdHYTOutCntFin:: '|| sqlerrm || '-'|| INSERT_FINAL_VALUE);

End;

-----------------------------------------------------------------------------------------
   ---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmXXXXPadREINS (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMM formatl? olarak tablo olusturur
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

     select TO_CHAR(c.eventdate,'YYYY_MM') as OLAY_YIL_AY ,
               case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                     0
                   else
                      months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                end   as BORDRO_YIL_DELTA ,
                 round( sum(case when (pol.startdate <'31.12.2007' ) then
                        pkg_accountingreports.FNC_GETREINSURANCESCOR2007(cd.claimid, cd.benefitid, cd.detaillineid) *0.425
                else
                   nvl(cd.reshare1,0) + nvl(cd.reshare2,0)
                end ))  as TOPLAMIC,
                ROW_NUMBER() OVER (PARTITION BY TO_CHAR(c.eventdate,'YYYY_MM') ORDER BY  case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                                                                                                                         0
                                                                                                                                                                        else
                                                                                                                                                                                                         months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                                                                                                                                         end )    AS ROWNO
         from
                t_claim c,t_claimdetail cd , t_azmm az, t_policy pol
         where c.claimid=cd.claimid
           AND c.policyid = pol.policyid
           AND c.claimid = az.claimid
            --odenenler icin paiddate alan? dolu
            AND az.newpaiddate is not null
            AND branch = 'XXXX'
            --debug
            --AND trunc(az.eventdate ,'MM') = '01/07/2010'
           GROUP BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end
           ORDER BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                 0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end   ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMPAIDREINS';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_OLAY_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'||  TO_CHAR( array_a1)  ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA) ,'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      dbms_output.put_line (INSERT_FINAL_VALUE);
                      dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
        IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;
       
        EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmXXXXPadREINS:: '|| sqlerrm || '-'|| INSERT_FINAL_VALUE);

End;
----------------------------------------------------------------------------------------------------

PROCEDURE Prc_CreateAzmmXXXXOutREINS (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMMnin i formatl? olarak tablo olusturur .
--Kolonlarda yarat?l?s tarihi ile odeme tarihi aras?ndaki ay say?s? yer al?yor.
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

                select TO_CHAR(az.createdate, 'YYYY_MM') as OLAY_YIL_AY ,
         months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )  as BORDRO_YIL_DELTA ,
         round( sum(az.reinsshare))     as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(az.createdate,'YYYY_MM') ORDER BY months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ) AS ROWNO
          from    t_azmm_outstanding  az
          where branch = 'XXXX'


               --debug
               -- AND az.createdate > '01/01/2010'
               --AND az.claimid = 4231267
                --debug
          GROUP BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )
          ORDER BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMOUTREINS1';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_YARAT_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR( array_a1) ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                          INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                          INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                  --    dbms_output.put_line (INSERT_FINAL_VALUE);
                 --     dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                   --   dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
     --insert last createdate

       IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;

        EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmXXXXOutREINS:: '|| sqlerrm || '-'|| INSERT_FINAL_VALUE);

End;
--------  EUROKA
---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmEUROKAOutFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMMnin i formatl? olarak tablo olusturur .
--Kolonlarda yarat?l?s tarihi ile odeme tarihi aras?ndaki ay say?s? yer al?yor.
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

                select TO_CHAR(az.createdate, 'YYYY_MM') as OLAY_YIL_AY ,
         months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )  as BORDRO_YIL_DELTA ,
          sum(az.paidamount)     as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(az.createdate,'YYYY_MM') ORDER BY months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ) AS ROWNO
          from    t_azmm_outstanding  az, t_claim c
          where branch = 'XXXX'
             AND c.claimid = az.claimid
             AND c.policyid in ( 289484,289483)


               --debug
               -- AND az.createdate > '01/01/2010'
               --AND az.claimid = 4231267
                --debug
          GROUP BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )
          ORDER BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMEUROKAOUTFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_YARAT_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR( array_a1) ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                          INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                          INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                  --    dbms_output.put_line (INSERT_FINAL_VALUE);
                 --     dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                   --   dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
     --insert last createdate
       IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;

End;


  ---------------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmEUROKAPadFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMM formatl? olarak tablo olusturur
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

     select TO_CHAR(c.eventdate,'YYYY_MM') as OLAY_YIL_AY ,
               case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                     0
                   else
                      months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                end   as BORDRO_YIL_DELTA ,
                 sum(cd.paidamount)   as TOPLAMIC,
                ROW_NUMBER() OVER (PARTITION BY TO_CHAR(c.eventdate,'YYYY_MM') ORDER BY  case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                                                                                                                         0
                                                                                                                                                                        else
                                                                                                                                                                                                         months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                                                                                                                                         end )    AS ROWNO
         from
                t_claim c,t_claimdetail cd , t_azmm az
         where c.claimid=cd.claimid
           AND c.claimid = az.claimid
            --odenenler icin paiddate alan? dolu
            AND az.newpaiddate is not null
            AND branch = 'XXXX'
            AND c.policyid in (289483,289484)
            --debug
            --AND trunc(az.eventdate ,'MM') = '01/07/2010'
           GROUP BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end
           ORDER BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                 0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end   ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMEUROKAFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_OLAY_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'||  TO_CHAR( array_a1)  ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA) ,'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      --dbms_output.put_line (INSERT_FINAL_VALUE);
                      --dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;

End;



PROCEDURE Prc_CreateAzmmEUROKAPadCntFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMM formatl? olarak tablo olusturur
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

     select TO_CHAR(c.eventdate,'YYYY_MM') as OLAY_YIL_AY ,
               case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                     0
                   else
                      months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                end   as BORDRO_YIL_DELTA ,
                count( distinct (c.claimid ) )   as TOPLAMIC,
                ROW_NUMBER() OVER (PARTITION BY TO_CHAR(c.eventdate,'YYYY_MM') ORDER BY  case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                                                                                                                         0
                                                                                                                                                                        else
                                                                                                                                                                                                         months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                                                                                                                                         end )    AS ROWNO
         from
                t_claim c,t_claimdetail cd , t_azmm az
         where c.claimid=cd.claimid
           AND c.claimid = az.claimid
           AND c.policyid in (289483,289484)
            --odenenler icin paiddate alan? dolu
            AND az.newpaiddate is not null
            AND branch = 'XXXX'
            --debug
            --AND trunc(az.eventdate ,'MM') = '01/07/2010'
           GROUP BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end
           ORDER BY TO_CHAR(c.eventdate,'YYYY_MM'), case when ( months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )) <0 then
                                                                                                 0
                                                                                           else
                                                                                              months_between(trunc(az.newpaiddate,'MM') ,trunc (c.eventdate,'MM') )
                                                                                           end   ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMEUROKACNTFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_OLAY_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'||  TO_CHAR( array_a1)  ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA) ,'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                         INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                         INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                      --dbms_output.put_line (INSERT_FINAL_VALUE);
                      --dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_OLAY_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                      dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
     EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmPrdXXXXPadCntFin '|| sqlerrm );
End;


-----------------------------------------------------------------------------------------------------
PROCEDURE Prc_CreateAzmmEUROKAOutCntFin (start_date IN DATE, End_date IN DATE, InRefDate IN DATE )
--AZMMnin i formatl? olarak tablo olusturur .
--Kolonlarda yarat?l?s tarihi ile odeme tarihi aras?ndaki ay say?s? yer al?yor.
IS

  cursor OLAY_CRSR(a1 date,a2 date)  is

                select TO_CHAR(az.createdate, 'YYYY_MM') as OLAY_YIL_AY ,
         months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )  as BORDRO_YIL_DELTA ,
         count( distinct (az.paidamount))     as TOPLAMIC,
          ROW_NUMBER() OVER (PARTITION BY TO_CHAR(az.createdate,'YYYY_MM') ORDER BY months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ) AS ROWNO
          from    t_azmm_outstanding  az, t_claim c
          where branch = 'XXXX'
           and   az.claimid = c.claimid
           and c.policyid in (289483,289484)

               --debug
               -- AND az.createdate > '01/01/2010'
               --AND az.claimid = 4231267
                --debug
          GROUP BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') )
          ORDER BY TO_CHAR(az.createdate,'YYYY_MM'),months_between(trunc(az.period,'MM') ,trunc( az.createdate,'MM') ) ;


    V_AYLAR PLS_INTEGER;
    SqlStmt Varchar2(32767);
    INSERT_SQLSTMT VARCHAR2(4000);
    INSERT_VALUE_STMT VARCHAR2(4000);
    COL_NAME VARCHAR2(10);
    INSERT_FINAL_VALUE VARCHAR2(4000);
    V_ERROR_DESC VARCHAR2(100);
    V_TABLENAME VARCHAR2(50) := 'T_AZMMEUROKAOUTCNTFIN';
    TABLE_EXISTS NUMBER(1);
BEGIN
    v_TableName := v_TableName || TO_CHAR(InRefDate,'YYYY_MM');
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM USER_TABLES WHERE TABLE_NAME=:1' INTO TABLE_EXISTS USING V_TABLENAME;

    IF TABLE_EXISTS = 1 THEN
          EXECUTE IMMEDIATE 'DROP TABLE ' || V_TABLENAME;
    END IF;

   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_NUMERIC_CHARACTERS='||''''||'.,'||'''';


    SqlStmt:=' CREATE  TABLE '|| V_TABLENAME ||' (V_YARAT_TARIH VARCHAR2(8),CL_NULL NUMBER(15,2) ';
    SELECT MONTHS_BETWEEN(InRefDate ,InRefDate - 15*365.6 ) INTO v_aylar FROM DUAL;

         FOR array_a1 in 0..v_aylar loop
           sqlstmt:= sqlstmt || ',' || CHR(10)|| 'CL_'|| TO_CHAR( array_a1) ||' NUMBER(15,2)';
   END LOOP;

     SqlStmt:=sqlstmt || ')' ;
     -- || CHR(10) ||' On commit preserve rows';
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,1,2000)) ;
    --pkg_error.PRC_Log('AZMM',sysdate,substr(SqlStmt,2001,2000)) ;
    EXECUTE IMMEDIATE sqlstmt;

    For r in OLAY_CRSR(start_date,end_date) loop
            BEGIN
                   --debug
                   --IF (r.OLAY_YIL_AY = '2009_03' ) THEN
                   --        null;
                   --END IF;
                   IF (r.ROWNO > 1 )  THEN
                            COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                            INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                            INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                    END IF;
                    IF (r.ROWNO = 1)  THEN
                         IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
                         END IF;

                          INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

                         IF INSERT_VALUE_STMT <> ' ' THEN
                             EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
                             EXEcUTE IMMEDIATE 'COMMIT';
                         END IF;
                         INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                         INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                         COL_NAME:='CL_'||NVL(to_char(r.BORDRO_YIL_DELTA),'NULL');
                          INSERT_SQLSTMT:=INSERT_SQLSTMT||',' || COL_NAME ;
                          INSERT_VALUE_STMT:=INSERT_VALUE_STMT||','||r.TOPLAMIC;
                     END IF;
               --  pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,1,2000)) ;
                -- pkg_error.PRC_Log('AZMM',sysdate,substr(INSERT_FINAL_VALUE,2001,2000)) ;


            EXCEPTION WHEN others THEN
                  --    dbms_output.put_line (INSERT_FINAL_VALUE);
                 --     dbms_output.put_line('');
                      INSERT_SQLSTMT:='INSERT INTO '|| V_TABLENAME||'(V_YARAT_TARIH';
                      INSERT_VALUE_STMT:='VALUES ('||''''||r.OLAY_YIL_AY||'''';
                   --   dbms_output.put_line (r.olay_yil_ay ||'-'|| r.BORDRO_YIL_DELTA ||'-'||sqlerrm);
             END;
       end loop;
     --insert last createdate
       IF (INSERT_SQLSTMT <>' ' AND INSERT_VALUE_STMT <> ' ')  THEN
                             INSERT_SQLSTMT:=INSERT_SQLSTMT || ')';
                             INSERT_VALUE_STMT:=INSERT_VALUE_STMT || ')';
       END IF;

       INSERT_FINAL_VALUE:=INSERT_SQLSTMT||CHR(10)||INSERT_VALUE_STMT;

       IF INSERT_VALUE_STMT <> ' ' THEN
            EXECUTE IMMEDIATE INSERT_FINAL_VALUE ;
            EXEcUTE IMMEDIATE 'COMMIT';
       END IF;
      EXCEPTION WHEN others THEN
                   pkg_error.PRC_Log ('AZMM',sysdate, 'Prc_CreateAzmmPrdXXXXOutCntFin '|| sqlerrm );
End;
end PKG_AZMM;
/
