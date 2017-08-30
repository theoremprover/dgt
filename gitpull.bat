SET WORKDIR=%~dp0
cd %WORKDIR%

where plink.exe > tmpFile
set /p GIT_SSH= < tmpFile
del tmpFile

git pull origin master
if errorlevel 1 goto :err
exit 0

:err
pause
exit
