# DM_FILLER

scripts and programs to fill BD files

first step run Fill_DM_xxxx :

-> Fill_DM_xxxx a script to define what are your inputs for BGC-Argo DM 
        1 scans your directory RT (uncorrected data) 
        2 push data into a directory WORK (temporary data)
        3 create a lance_DM_xxxx script

second step run lance_DM_xxxx :

-> lance_DM_xxxx script is a script to fill your DM files
        1 use WRITE_DM_xxxx program 
        2 push data into a directory BP (corrected data)
        
        
There are different flavors of these programs, as inputs and applied corrections can be different 

DOXY_adj.R
DOXY_to_PPOX.R 
CHLA_adj_XX.R

are tools directly linked to some specific calculations for the different BGC PARAMETERS 
