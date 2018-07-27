//ANDREWJA JOB  IBM,SP,CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEP1    EXEC ASMAC,
//       PARM.C='OBJECT,NODECK,XREF(SHORT),PC(ON,GEN,MC,MS)'
//SYSIN    DD   *
         PRINT NOGEN
*
*------------------------------------------------*
*
         PRINT OFF
&REG     SETA  0
.LOOP    ANOP                              GENERATE REGS.
R&REG    EQU   &REG
&REG     SETA  &REG+1
         AIF   (&REG LE 15).LOOP
         PRINT ON
*
*------------------------------------------------*
*
CICSRESP CSECT
         USING *,R15              setup addressibility
         STM   R14,R12,12(R13)    use r13 as base as well as
         LR    R2,R13             reg-save area
         B     CMNTTAIL           skip over the remarks
*
CMNTHEAD EQU   *
         PRINT GEN                print out remarks
         DC    CL8'&SYSDATE'      compiling date
         DC    C' '
         DC    CL5'&SYSTIME'      compiling time
         DC    C'ANDREW JAN'      author
         CNOP  2,4                ensure half word boundary
         PRINT NOGEN              disable macro expansion
CMNTTAIL EQU   *

         BALR  R12,0
         BAL   R13,76(R12)
         DROP  R15                 avoid compiling warning

SAVREG   DS    18F
         USING SAVREG,R13
         ST    R2,4(R13)
         ST    R13,8(R2)
*
D        USING IHADCB,INPUT        addressibility for input DCB
*
*
*---MAINSTREAM------------------------------------*
*
*
        BAL    R6,OPEN_FILES        open files
        BAL    R6,GET_STORAGE       allocate an area
*
        B      LOOP_BLKS            process
*
FINISH  EQU    *
*
         TM    F_SPLIT,L'F_SPLIT  no a multi-seg rec ?
         BZ    FINISH_1           no, go on
*
         L     R4,MRGADR          process the complete multi-seg rec
         STH   R9,0(R4)           replace the len. with the total len.
         MVI   2(R4),X'00'        pretend to be a non-split rec
         BAL   R6,PROCESS_REC
*
FINISH_1  EQU    *
        BAL    R6,CLOSE_FILES       close files
        B      RETURN               return to system
*
*-------------------------------------------------------*
*
OPEN_FILES EQU  *
         OPEN  (INPUT,INPUT,OUPUT,OUTPUT)
         BR    R6
*
*-------------------------------------------------------*
*
GET_STORAGE EQU  *
         LH    R11,D.DCBBLKSI     get input file's blksize
         GETMAIN EC,LV=(R11),A=BLKADR,BNDRY=PAGE
*
         GETMAIN EC,LV=(R11),A=MRGADR,BNDRY=PAGE
*
         CSRCESRV SERVICE=QUERY   get work area size required
         LTR   R4,R1              any work area required?
         BNZ   WAREQD             yes, get work area
         MVI   CSRWAPTR,X'80'     ind. requiring no work area
         B     NOWAREQD           go on
WAREQD   EQU  *
         GETMAIN R,LV=(R4),SP=0,LOC=(31,64)
         ST    R1,CSRWAPTR        save work area address
NOWAREQD EQU  *
         GETMAIN R,LV=32768,SP=0,LOC=(31,64)
         ST    R1,CSRXAPLA        save expanded record area
         BR    R6
*
*-------------------------------------------------------*
*
LOOP_BLKS   EQU   *
         L     R4,BLKADR
         READ  DECB,SF,INPUT,(R4) read a block
         CHECK DECB               check
         LH    R2,0(,R4)          block size
*        CVD   R2,WORKD           convert to packed decimal
*        MVC   OUTBUFF,BLANKS     clear buffer
*        UNPK  OUTBUFF(5),WORKD+5(3)  zone decimal
*        OI    OUTBUFF+4,X'F0'    make it readable
*        PUT   OUPUT,OUTBUFF      print
*
         LA    R4,4(,R4)          skip
         SH    R2,=H'4'           deduct the blk size remained
LOOP_RECS   EQU   *
         LH    R3,0(,R4)          rec len.
*        MVC   OUTBUFF,BLANKS     clear buffer
*        CVD   R2,WORKD           convert to packed decimal
*        UNPK  OUTBUFF(5),WORKD+5(3)  zone decimal
*        OI    OUTBUFF+4,X'F0'    make it readable
*        CVD   R3,WORKD           convert to packed decimal
*        UNPK  OUTBUFF+6(5),WORKD+5(3)  zone decimal
*        OI    OUTBUFF+6+4,X'F0'  make it readable
*        MVC   OUTBUFF+12(3),=C'###' eye catcher
*        PUT   OUPUT,OUTBUFF      print
*
         CLI   2(R4),X'00'        not a split rec ?
         BE    GO_DO_REC          no, branch
*
         CLI   2(R4),X'01'        the 1st segment of a split rec?
         BNE   SEG_02             no,  branch
*
SEG_01   EQU   *
*
         TM    F_SPLIT,L'F_SPLIT  last rec a multi-seg rec ?
         BZ    SEG_011            no, branch
*
         ST    R4,R4_SAV          temp. save the original looping base
         L     R4,MRGADR          process the complete multi-seg rec
         STH   R9,0(R4)           replace the len. with the total len.
         MVI   2(R4),X'00'        pretend to be a non-split rec
         BAL   R6,PROCESS_REC
         NI    F_SPLIT,X'FF'-L'F_SPLIT  reset  the flag
         L     R4,R4_SAV          restore back the orig.looping base
*
SEG_011  EQU   *
         LR    R9,R3              save the length of the 1st seg.
         L     R10,MRGADR         move-to address
         OI    F_SPLIT,L'F_SPLIT  set up the flag
         LR    R14,R4             copy the current address
         LR    R11,R3             move-to length
         LR    R15,R3             move-fm length
         B     SEG_03             branch
SEG_02   EQU   *
         AR    R9,R3              accumulate the total length
         SH    R9,=H'4'           deduct the first 4 bytes
         LA    R14,4(,R4)         skip the first 4 bytes
         LR    R11,R3             move-to length
         SH    R11,=H'4'          skip the first 4 bytes
         LR    R15,R11            move-fm length
SEG_03   EQU   *
         MVCL  R10,R14            construct complete rec from segs
         B     LOOP_NEXT          go process next
*
GO_DO_REC      EQU   *
         TM    F_SPLIT,L'F_SPLIT  no a multi-seg rec ?
         BZ    GO_DO_REC_1        process the one-segment single rec
*
         ST    R4,R4_SAV          temp. save the original looping base
         L     R4,MRGADR          process the complete multi-seg rec
         STH   R9,0(R4)           replace the len. with the total len.
         MVI   2(R4),X'00'        pretend to be a non-split rec
         BAL   R6,PROCESS_REC
         NI    F_SPLIT,X'FF'-L'F_SPLIT  reset  the flag
         L     R4,R4_SAV          restore back the orig.looping base
*
GO_DO_REC_1    EQU   *
         BAL   R6,PROCESS_REC     call the subroutine
*
LOOP_NEXT      EQU   *
         CR    R2,R3              remaining blk size <= rec line ?
         BNH   LOOP_RECS_END      leave the loop thru recs
         SR    R2,R3              bytes left
         AR    R4,R3              next rec
         B     LOOP_RECS          loops
LOOP_RECS_END  EQU   *
*
         B     LOOP_BLKS          loop
*
*--------------------------------------------------------*
*
PROCESS_REC    EQU   *
*
          STM   R14,R12,REC_SAV  save the regs
          LR    R10,R4           record content
          USING DFHSMFDS,R10     addressibility
          CLI   SMFRTY,X'6E'     110?
          BNE   PROCESS_REC_END  no, go back
          CLC   SMFSTY,=Y(SMFMNSTY)  monitoring data ?
          BNE   PROCESS_REC_END  no, go back
          CLC   SMFMNCL,=X'0003' performance data ?
          BNE   PROCESS_REC_END  no, go back
*
          CLC   SMFMNCRL,=XL2'0' not a compressed rec?
          BE    NOCOMPRS         no, bypass the expansion
*
          L     R4,CSRXAPLA      expanded rec area
          MVC   0(SMFMNIDA-DFHSMFDS,R4),0(R10)  copy smf header
*
          MVC   CSRXAPLL,=F'32768' the size of expanded area
          L     R1,CSRWAPTR      csr work area
          LR    R2,R10           current record
          A     R2,SMFASS        ptr to compressed data
          LH    R3,SMFASL        length of the compressed data
          A     R4,SMFASS        offset to data after decomp.
          L     R5,CSRXAPLL      expanded area length
          CSRCESRV SERVICE=EXPAND expand compressed data section
          ST    R15,RETCD        save the return code
          L     R14,CSRXAPLL     expand area length
          SR    R14,R5           expand area length
          LTR   R15,R15          successfully?
          BZ    EXPAND_OK        yes
          MVC   OUTBUFF,BLANKS   clear buffer
          MVC   OUTBUFF(18),=C'Expand Error Code:'
          PUT   OUTBUFF+19(4),RETCD print the return code
          ABEND 118,DUMP         abend with 118
*
EXPAND_OK EQU   *
*
          L     R10,CSRXAPLA     expanded rec area
*
NOCOMPRS  EQU   *
          LH    R3,SMFMNDRN      total number of performance data
          LH    R8,SMFMNDRL      length of each cics data record
          L     R9,SMFMNDRA      offset to the first CICS data record
          AR    R9,R10           address of the cics performance data
*
OUTLOOP   EQU   *
          USING MNR_ID_DATA,R9   addressibility
*transaction id
          MVC   OTRAN,MNR_ID_TRANID  copy the tran. id
*terminal id
          LA    R2,L'MNR_ID_TERMID  length of term id
          LA    R5,MNR_ID_TERMID start addr of term id for input
          LA    R4,OTERM         start addr of term id for output
OUTLOOP_00 EQU   *
          CLI   0(R5),X'00'      null?
          BE    OUTLOOP_01       yes, branch
          MVC   0(1,R4),0(R5)    copy one byte
          B     OUTLOOP_011      branch
OUTLOOP_01 EQU   *
          MVI   0(R4),C' '       replaced by space
OUTLOOP_011 EQU   *
          LA    R5,1(,R5)        check next byte
          LA    R4,1(,R4)        check next byte
          BCT   R2,OUTLOOP_00    loop thru all bytes
*user id
          MVC   OUSER,MNR_ID_USERID copy userid
*task number
          UNPK  W_7,MNR_ID_TASKNO get the task number
          OI    W_7+6,X'F0'       make it readable
          MVC   OTASK,W_7+2       copy to print
*start time
          L     R14,MNR_ID_START   higher bytes for reg pair
          L     R15,MNR_ID_START+4 lower bytes for reg pair
          LA    R1,SMFMNDTO        local to GMT diff.
          BAL   R6,DBL_ADD         add the diff.
          STM   R14,R15,WORKD      save the total
          STCKCONV STCKVAL=WORKD,CONVVAL=W_16,DATETYPE=YYYYMMDD
          UNPK  W_9,W_16+8(5)       FORMAT THE YYYYMMDD
          MVC   ODATEI,W_9+2        MOVE THE FORMATTED YYYYMMDD
          UNPK  W_13,W_16(7)        FORMAT THE YYYYMMDD
          MVC   OHHI,W_13           MOVE THE FORMATTED HH
          MVC   OMMI,W_13+2         MOVE THE FORMATTED MM
          MVC   OSSI,W_13+4         MOVE THE FORMATTED SS
*stop time
          L     R14,MNR_ID_STOP    higher bytes for reg pair
          L     R15,MNR_ID_STOP+4  lower bytes for reg pair
          LA    R1,SMFMNDTO        local to GMT diff.
          BAL   R6,DBL_ADD         add the diff.
          STM   R14,R15,WORKD      save the total
          STCKCONV STCKVAL=WORKD,CONVVAL=W_16,DATETYPE=YYYYMMDD
          UNPK  W_9,W_16+8(5)       FORMAT THE YYYYMMDD
          MVC   ODATEO,W_9+2        MOVE THE FORMATTED YYYYMMDD
          UNPK  W_13,W_16(7)        FORMAT THE YYYYMMDD
          MVC   OHHO,W_13           MOVE THE FORMATTED HH
          MVC   OMMO,W_13+2         MOVE THE FORMATTED MM
          MVC   OSSO,W_13+4         MOVE THE FORMATTED SS
*
          L     R5,MNR_ID_STOP+4   get the diff.
          S     R5,MNR_ID_START+4  get the diff.
          CVD   R5,WORKD           how many ms
          DP    WORKD,=P'4096'     how many ms
          UNPK  W_9,WORKD(5)       format the result
*
          LA    R2,2               check the leftmost 2 digits
          LA    R4,W_9             zero?
          LA    R5,ORESP           zero?
RESP_00   EQU   *
          CLI   0(R4),C'0'         zero?
          BE    RESP_01            no branch
          MVC   0(2,R5),0(R4)      left to the point
          BCTR  R2,0               force to leave the loop
RESP_01   EQU   *
          MVI   0(R5),C' '         replaced by space
          LA    R4,1(,R4)
          LA    R5,1(,R5)
          BCT   R2,RESP_00
          MVC   0(1,R5),0(R4)      always keeps this digit even zero
*
          MVI   ORESP+3,C'.'       the point
          MVC   ORESP+4(6),W_9+3   digits right to the point
          OI    ORESP+9,X'F0'    make the last digt readable
*
          PUT   OUPUT,OUTBUFF    print 80 bytes as a line
*
          AR    R9,R8            next data record
          BCT   R3,OUTLOOP       loop thru all data records
*
PROCESS_REC_END EQU   *
          LM    R14,R12,REC_SAV  save the regs
          BR    R6               go back
*
*--------------------------------------------------------*
*
CLOSE_FILES EQU  *
         CLOSE (INPUT,,OUPUT)     close files
         BR    R6
*
*--------------------------------------------------------*
*
RETURN   EQU   *
         L     R13,4(R13)
         RETURN (14,12),RC=0      back to caller
*
*--------------------------------------------------------*
*
*
DBL_SUBTRACT EQU   *
         SL    R15,4(,R1)          subtract low order words
         BC    2+1,DBLSUB1         branch if carry
         SL    R14,=A(1)           subtract carry fm high order word
DBLSUB1  EQU   *
         SL    R14,0(,R1)          subtract high order words
         BR    R6
*
*-----------------------------------------*
DBL_ADD      EQU   *
         AL    R15,4(,R1)          add low order words
         BC    8+4,DBLADD1         branch if no carry
         AL    R14,=A(1)           add carry into high order word
DBLADD1  EQU   *
         AL    R14,0(,R1)          add high order words
         BR    R6
*
*--------------------------------------------------------*
*
         LTORG

*
*--------------------------------------------------------*
*
WORKD    DS   D
W_13     DS   0CL13
W_9      DS   0CL9
W_7      DS   0CL7
*
REC_SAV  DS   18F
BLKADR   DS   A                  pointer to the block read
MRGADR   DS   A                  area for merging a split record
CSRWAPTR DS   A                  pointer to the work area
CSRXAPLA DS   A                  pointer to expaned rec. area
CSRXAPLL DS   A                  expand area length
R4_SAV   DS   F                  r4 saver
BLKLEFT  DS   H                  bytes left for a block
RECLEFT  DS   H                  bytes left for a record
RETCD    DS   F                  return code for error checking
FLAG     DC   X'00'              initial value
         ORG  FLAG
F_SPLIT  DS   0XL(B'10000000')   ind. a multi-seg rec
*
*--------------------------------------------------------*
*
INPUT  DCB DSORG=PS,DDNAME=INPUT,MACRF=R,EODAD=FINISH
OUPUT  DCB DSORG=PS,DDNAME=OUPUT,MACRF=PM
*
W_16     DS    0CL16          fullword boundary
P_HHMMSS DS    CL3            packed hhmmss
P_TH     DS    CL3            packed thijuu0000
         DS    CL2            reserved
P_Y4MMDD DS    CL4            packed yyyy
         DS    CL4            reserved
         ORG   P_Y4MMDD
P_Y4DDD  DS    CL4
         ORG   P_Y4MMDD
P_Y2K    DS    CL1
P_YYDDD  DS    CL3
*
BLANKS   DS    0CL80              blanks
         DC    80C' '
*
OUTBUFF  DS    CL80
         ORG   OUTBUFF
OTRAN    DS    CL4        transaction id
         DC    C','
OTERM    DS    CL4        terminal id
         DC    C','
OUSER    DS    CL8        user id
         DC    C','
OTASK    DS    CL5        type
         DC    C','
ODATEI   DS    CL6        start time
         DC    C'.'
OHHMMSSI DS    CL14       start time
         ORG   OHHMMSSI
OHHI     DS    CL2
         DC    C':'
OMMI     DS    CL2
         DC    C':'
OSSI     DS    CL8
         DC    C','
ODATEO   DS    CL6        start time
         DC    C'.'
OHHMMSSO DS    CL14       start time
         ORG   OHHMMSSO
OHHO     DS    CL2
         DC    C':'
OMMO     DS    CL2
         DC    C':'
OSSO     DS    CL8
ODI      DC    C','
ORESP    DS    CL10       response time
         DC    C','
*
       DCBD DSORG=PS      gen. PS-DCB dsect
*
       COPY DFHSMFDS                 smf header and product section
*
       COPY DFHMNRDS                 for using MNR_ID_DATA dsect
*
       END
/*
//*
//C.SYSLIB DD  DISP=SHR,DSN=ANDREWJ.DFHMAC
//         DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.MODGEN
//C.SYSLIN DD  DISP=SHR,DSN=ANDREWJ.SOURCE.JCL(CICSRESP)
//*
