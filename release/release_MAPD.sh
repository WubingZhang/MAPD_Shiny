# I. Basic idea: 
# On //Apache2 server// side, listen to public port 80 for different URL requests
# When there are requests for URLs mapd.cistrome.org, use the corresponding conf files to ProxyPass / redirect requests to port 3838
# Apache2 conf file for mapd.cistrome.org is located in /etc/apache2/sites-available/MAPD.conf
# Include the above conf files in the /etc/apache2/sites-available/000-default.conf
# On //Shiny server// side, listen to port 3838, and define application locations for MAPD
# reload conf files for both Apache2 server and Shiny server
# ================================================================== #

# II. Operation steps for MAPD:
# ================================================================== #
# II.0 Copy source code of the app to /project
$ cp /liulab/jchen/Wubing_Project/MAPD /project/

# II.1. Add the apache config
$ cp MAPD.conf /etc/apache2/sites-available/MAPD.conf
$ echo 'Include sites-available/MAPD.conf' >> /etc/apache2/sites-available/000-default.conf

# II.2. Add config of shiny server
$ cat shiny-server.conf >> /etc/shiny-server/shiny-server.conf

# II.3. Reload Apache2 and Shiny server configurations
$ /etc/init.d/apache2 reload
$ systemctl restart shiny-server
