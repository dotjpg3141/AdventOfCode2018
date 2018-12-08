@echo off
pushd %~dp0

set day=day%1

cd ./src

echo ------ %day% a ------
runghc ./%day%a.hs < ../input/%day%.txt || ( popd && exit /b )
echo.

echo ------ %day% b ------
runghc ./%day%b.hs < ../input/%day%.txt || ( popd && exit /b )
echo.

popd