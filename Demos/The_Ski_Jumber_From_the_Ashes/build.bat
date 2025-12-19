@echo off

if "%1"=="-h" (
  echo Usage: %~nx0 [docker-update ^| -c "command" ^| make-target]
  echo.
  echo   docker-update     Pull the latest Docker image
  echo   -c "command"      Execute a custom command in the Docker image
  echo   -h                Show this help message
  echo   [make-target]     Run make with given target ^(e.g. all, clean, etc.^)
  goto :done
)

docker version > nul 2>&1
if %errorlevel% equ 0 (
  echo Docker is running, using docker image as build environment...
  echo To update the docker image use %~nx0 docker-update
) else (
  echo Docker service is not running, start the Docker Desktop App.
  goto :done
)
if "%1"=="docker-update" (
  docker pull vintagecomputingcarinthia/c64build
  goto :done
) 

if "%1"=="-c" (
  echo Running custom command: %2 %3 %4 %5 %6 %7 %8 %9
  docker run -v %cd%:/host -w /host --rm -it vintagecomputingcarinthia/c64build sh -c "%2 %3 %4 %5 %6 %7 %8 %9"
  goto :done
)

for %%I in ("%CD%") do set CURRENT_FOLDER=%%~nxI
for %%I in ("%CD%\..") do set PARENT_FOLDER=%%~nxI

pushd ..\..
docker run -v %cd%:/host -w /host --rm vintagecomputingcarinthia/c64build make -C %PARENT_FOLDER%/%CURRENT_FOLDER% %1 T7DINCLUDE=../../library
popd

:done
IF /I %0 EQU "%~dpnx0" PAUSE