@echo off
pushd %~dp0

set day=day%1

cd ./src
runghc ./%day%.hs < ../input/%day%.txt || ( popd && exit /b )
echo.

popd