@echo off
pushd %~dp0

set day=day%1

echo ------ %day% a ------
runghc ./src/%day%a.hs < ./input/%day%.txt || ( popd && exit /b )
echo.

echo ------ %day% b ------
runghc ./src/%day%b.hs < ./input/%day%.txt || ( popd && exit /b )
echo.

popd