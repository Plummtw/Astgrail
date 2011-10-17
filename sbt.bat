set SCRIPT_DIR=%~dp0
java -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:+CMSPermGenSweepingEnabled -XX:MaxPermSize=256m -Xms512M -Xmx1024M -Xss2M -jar "%SCRIPT_DIR%\sbt-launcher.jar" %*