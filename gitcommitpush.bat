SET WORKDIR=%~dp0
cd %WORKDIR%

where plink.exe > tmpFile
set /p GIT_SSH= < tmpFile
del tmpFile

git commit -a -m "No Message"
if errorlevel 2 goto :err
git push origin master
if errorlevel 1 goto :err
exit 0

:err
pause
exit
