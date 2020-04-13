@echo off

set dirname=%~dp0
set MaxTableAngle=1.0
echo Starting LSDTable ...

java -cp "%dirname%ImageUtil_2.12-0.0.1-jar-with-dependencies.jar" -Dlog4j.configurationFile=%dirname%log4j2.xml edu.umro.LSDTable.LSDTable %*

pause
