#!/bin/bash

#------------------------------------------------------------------------------------#
#                                                                                    #
#                 SCRIPT for loop for MHWs definition by each year                   #
#                                                                                    #
#                                                                BY   :  KM.Noh      #
#                                                                DATE : 2019.05.02   #
#                                                                                    #
#------------------------------------------------------------------------------------#

# Specific cases are designed with specific lat, lon range
export N_dim1=4   # The number of dimension 1
export N_dim2=2   # The number of dimension 2

# Loop for each cases ncl file
for case1 in $(seq 1 $N_dim1); do 
    for case2 in $(seq 1 $N_dim2); do 
        export file_name="MHWs_Category_main_total.loop.f90"
        export line_change1="s/CASE1/"$case1"/"
        export line_change2="s/CASE2/"$case2"/"
        
        # Change the case and make temporal file
        sed -e $line_change1 -e $line_change2 $file_name > MHWs_Category_main_total.f90

        #Excute excutable file and remove file
        make
        EXE_CATEGORY
        make clean
    done 
done
