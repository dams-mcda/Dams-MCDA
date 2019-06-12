#!/bin/bash

# get file installation key from file
fileInstallKey=$(</media/matlab-license/fileInstallKey.txt)

# install matlab
./media/mathworks/install -mode silent -agreeToLicense yes -fileInstallationKey ${fileInstallKey} -c /media/matlab-license/license.lic

# copy license file over to install directory
cp /media/matlab-license/license.lic /usr/local/MATLAB/R2019a/licenses/license.lic

# run matlab
#./usr/local/MATLAB/R2019a/bin/matlab -nodesktop -nosplash -nodisplay &

# fix matlab glnxa64 binaries
cd /usr/local/MATLAB/R2019a/bin/glnxa64 && mv libexpat.so.1 libexpat.so.1.NOFIND

# after installing resume server as normal
/bin/bash /usr/bin/shiny-server.sh
