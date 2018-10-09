mkdir %PREFIX%\dist\fragments
xcopy . %PREFIX%\dist\fragments /s /e
copy conda\fragments\.fragments-post-link.bat %PREFIX%\Scripts\
