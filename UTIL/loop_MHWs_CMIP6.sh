#!/bin/bash

#------------------------------------------------------------------------------------#
#                                                                                    #
#                 SCRIPT for loop for MHWs definition by each year                   #
#                                                                                    #
#                                                                BY   :  KM.Noh      #
#                                                                DATE : 2019.05.02   #
#                                                                                    #
#------------------------------------------------------------------------------------#

# Specific cases are designed with specific regions
export list=$(cat ./DATA/model_list.dat)

# Loop for each cases fortran file
for model_list in $list; do 
    export file_name="MHW_main.CMIP6_SST.f90"
    export line_change1="s/model_name/"$model_list"/"

    # Change the case and make temporal file
    sed -e $line_change1 $file_name > MHW_main.f90

    #Excute excutable file and remove file
    make
    EXE_MHWs
    make clean
 done

