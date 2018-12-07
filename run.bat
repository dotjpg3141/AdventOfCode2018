@echo off
pushd %~dp0

set day=day%1

echo ------ %day% a ------
ghc ./src/%day%a.hs || exit /b
cd src
%day%a.exe < ../input/%day%.txt || exit /b
echo.
cd ..

echo ------ %day% b ------
ghc ./src/%day%b.hs || exit /b
cd src
%day%b.exe < ../input/%day%.txt || exit /b
echo.
cd ..

popd