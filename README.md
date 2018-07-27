# Collecting-CICS-Transaction-Response-Times-from-SMF110
This tool provides a simple method to gauge a CICS (Customer Information Control System) system’s performance in terms of the response times of online transactions. 

With this simple tool, you don’t have to count on OMEGA
Monitor for CICS to generate CICS online transaction response times for
you. 

This tool runs on a z/OS, normally in TSO, invoked by a JCL (Job
Control Language) to read an SMF file or files, where CICS transaction related
information records are kept. More specifically, SMF type 110 records are
processed by this tool to produce CICS transaction response times.

To have your CICS generated these needed SMF records, the CICS Monitoring Control Table should be properly configured first.
Simply put, specifying “DFHTASK” to the Monitoring Control Table: “DFHMCTxx” will let CICS produce SMF type 110
records for you.
