@ECHO OFF

SET dir=%~dp0
SET R="C:\Program Files\R\R-4.0.3\bin\Rscript.exe"

%R% %dir%setWorkingDirectory.R %dir%
IF NOT EXIST data (
	ECHO ###There is no directory with the name data.
	ECHO ###The program will be cancled without the directory!
	CHOICE /C YN /M "###Do you want to create one?"
	IF ERRORLEVEL ==2 GOTO END
	IF ERRORLEVEL ==1 GOTO YES
) ELSE (
	GOTO START
)

:YES

MD data
ECHO ###Directory %dir%data created!

:START


CD %dir%data
curl -o reference.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/reference.csv
curl -o time-series-19-covid-combined.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv
CD ..

%R% %dir%prepareData.R
%R% %dir%startShinyServer.R

:END

ECHO ###Goodbye

PAUSE