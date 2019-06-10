#!/bin/bash

# get file installation key from file
fileInstallKey=$(</media/matlab-license/fileInstallKey.txt)

# install matlab
./media/mathworks/install -mode silent -agreeToLicense yes -fileInstallationKey ${fileInstallKey} -c /media/matlab-license/license.lic

# after installing resume server as normal
/bin/bash /usr/bin/shiny-server.sh
