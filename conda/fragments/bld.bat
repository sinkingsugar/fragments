mkdir %PREFIX%\dist\fragments
xcopy . %PREFIX%\dist\fragments /s /e
copy bin\.fragments-post-link.sh %PREFIX%\bin\
