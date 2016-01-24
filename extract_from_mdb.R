# Get the 2014 WSDA crop data as MDB and extract TRS codes and Alfalfa Seed data.
# This code was tested on Windows Server 2008 with R 3.2.2.
#
# This is to demonstrate an alternative way to get the data instead of using 
# the web API and JSON.
#
# Copyright 2016 Brian High (https://github.com/brianhigh)
# License: GNU GPL v3 http://www.gnu.org/licenses/gpl.txt

#install.packages("RODBC")
library(RODBC)

# The zip link was found on this page as "Download WSDA GIS Data [ZIP 22.4 MB]":
#     http://agr.wa.gov/pestfert/natresources/aglanduse.aspx
url <- "http://agr.wa.gov/PestFert/NatResources/docs/2014WSDACropDistribution.zip"
zipfile <- "2014WSDACropDistribution.zip"
download.file(url, zipfile)
unzip(zipfile)

cropdb <- odbcConnectAccess("2014WSDACropDistribution.mdb")

# Find the CropTypeID for "Alfalfa Seed"
q <- "SELECT CropTypeID, CropType 
      FROM CropType 
      WHERE CropType = 'Alfalfa Seed';"
sqlQuery(cropdb, q)

# Output:
#   CropTypeID     CropType
# 1       1501 Alfalfa Seed

# Get TRS codes and crop acres for "Alfalfa Seed" using the CropType ID
q <- "SELECT TRS,ExactAcres 
      FROM CropData 
      WHERE CropType = 1501;"
AlfalfaSeed <- sqlQuery(cropdb, q)

# You could alternatively get the data in one query by using a JOIN...
q <- "SELECT ExactAcres AS acres, TRS AS trscode
      FROM CropData 
      INNER JOIN CropType 
          ON CropData.CropType = CropType.CropTypeID 
      WHERE CropType.CropType = 'Alfalfa Seed';"
AlfalfaSeed <- sqlQuery(cropdb, q)

# Write "Alfalfa Seed" data to file
write.csv(AlfalfaSeed, "alfalfa_from_mdb.csv", row.names=FALSE, quote=FALSE)

# If you compare this file with crop.csv made with trsgeocode.R, you will see 
# that they are identical. I.e., use diff or diff -w to compare them.

# Get all TRS codes from database and write to file
trscodes <- sqlQuery(cropdb, "SELECT DISTINCT(TRS) AS TRSCODE FROM CropData;")
write.csv(trscodes, "CropData_TRSCODES.csv", row.names=FALSE, quote=FALSE)