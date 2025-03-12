@echo off
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

rem Build the application
echo Building the application...
docker run -u 1000:uucp --rm -it -v %cd%\..\library:/t7d -v %cd%:/host -w /host vintagecomputingcarinthia/c64build make LIBDIR=/t7d %*

echo Application build complete.

:done
IF /I %0 EQU "%~dpnx0" PAUSE
