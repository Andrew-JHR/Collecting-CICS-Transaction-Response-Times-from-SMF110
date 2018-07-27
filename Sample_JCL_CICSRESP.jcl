//ANDREWJA  JOB  IBM,SP,CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID            
//*                                                                  
//STEP01    EXEC PGM=IFASMFDP                                        
//INDD1     DD   DISP=SHR,DSN=SYS1.SMFBKUP........                   
//OUTDD1    DD   DISP=(NEW,PASS),DSN=&&TEMP,SPACE=(CYL,(50,50)),     
//    UNIT=SYSDA                                                     
//SYSPRINT  DD   SYSOUT=*                                            
//SYSIN     DD   *                                                   
 INDD(INDD1,OPTIONS(DUMP)) OUTDD(OUTDD1,TYPE(110(1)))                
 DATA(16305,16305)                                                   
 START(1330)                                                         
 END(1430)                                                           
/*                                                                   
//*                                                                  
//STEP02    EXEC PGM=LOADER,REGION=0M                                
//SYSPRINT  DD   SYSOUT=*                                            
//INPUT     DD   DSN=&&TEMP,DISP=(OLD,DELETE)      
//OUPUT     DD   SYSOUT=*,LRECL=80                                      
//SYSLIN    DD   *                                                      
 ESD                            CICSRESP       4  
 TXT              }    00 10/31/16 18.00ANDREW JAN {   <                
 TXT            &   &K   -}@ -}  0J j N   }   N< m  k    -J4 -M  0M ( }h
 TXT   y               Y   6 ^Nw  }           &^      }           &^    
 TXT   \         \}Q  }B       0                    }Wk N& 0}     i     
 TXT              }4  & N&    J           & N  6  N ( J                 
 TXT   &            &    0   70     J  \     7\           . MQ    n     
 TXT   h        J[n     Jsj N   J & N*  N< m  k    -J4m"N   N* l  N<o N 
 TXT   {         U   3 0J  l. MQ \    .^MQ     0JSj N   J & N*  N< m  k 
 TXT   8           -J4m"N   N* -J4   }J0     0J  0J   N  un>    MdN   M 
 TXT              MdN   M   MdN   M   Kw  N K     K N M}  N&  !       ! 
 TXT               &N  \K   K&     @ 0                &0N  \N  V    KsK|
 TXT            O@O K O@M\  O   N    7            i         N        &  
 TXT   Q         <  K O@       &    Oan &   KQK   &  0K k    &&       KF
 TXT            K Of  3 N   o0N K O N  \   0       -M   M8      L k    \
 TXT            M8K   \  0L                             k    \                  
 TXT              \  \O K \   3dN O K OnN 3FN O K O N K O N K OsN  \   0
 TXT                   -M   M8      L k    \M8K   \  0LM                
 TXT   0                    k    \            \  \O K \   3dN O K O N 3F
 TXT            N O K O N K O N K O N  &  $&  +&M8  M8M23dN M8      N  &
 TXT   -        OAn0    MK &     k &      &&   M K &   k.ODK OEN o0O   
 TXT   q        N{  O@   7     q  K q N  6  ( Mq       Y   6    q }  0  
 TXT   }          0    M \MM\   6;0   {MH;\MM;\   6                  
 TXT   8                        Expand Error Code:  %                   
 TXT                                                                    
 TXT                                               d    INPUT           
 TXT   H                                                                
 TXT                            OUPUT      &                            
 TXT                                                                    
 TXT                                                                    
 TXT                                                                    
 TXT   y        ,                                                       
 TXT            ,                                                       
 TXT            ,                                                       
 TXT            ,                                                       
 TXT   C        .                                                       
 TXT   F        :                                                       
 TXT   I        :                                                       
 TXT   K        ,                                                       
 TXT   R        .                                                       
 TXT            :                                                       
 TXT            :                                                       
 TXT   Y        ,                                                       
 TXT   3        ,                                                       
 RLD                   z       {   M                                    
 END                            1569623400 010616305                