@echo off

REM Solution By: https://blog.csdn.net/earbao/article/details/51728887

REM Get remainning unshifted command line args and save them into the set CMD_LINE_ARGS
set CMD_LINE_ARGS=
:setArgs
if ""%1""=="""" goto doneSetArgs
set CMD_LINE_ARGS=%CMD_LINE_ARGS% %1
shift
goto setArgs
:doneSetArgs

cd /d %~dp0

java^
    -Declipse.application=org.eclipse.jdt.ls.core.id1^
    -Dosgi.bundles.defaultStartLevel=4^
    -Declipse.product=org.eclipse.jdt.ls.core.product^
    -Dosgi.checkConfiguration=true^
    -Dosgi.sharedConfiguration.area=../config_win^
    -Dosgi.sharedConfiguration.area.readOnly=true^
    -Dosgi.configuration.cascaded=true^
    -noverify^
    -Xms1G^
    --add-modules=ALL-SYSTEM^
    --add-opens java.base/java.util=ALL-UNNAMED^
    --add-opens java.base/java.lang=ALL-UNNAMED^
    -jar ../plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar^
    %CMD_LINE_ARGS%
