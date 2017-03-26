#
# Create Shortcut 'orgmode' in folder %appdata%\Microsoft\Windows\Start Menu\Programme:
# C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -ExecutionPolicy unrestricted -command "& 'C:\org\custom\orgmode.ps1'"
#

$env:LANG="de_DE.UTF-8"
$env:ORG="C:\org"
$env:HOME="C:\org"
cd $env:HOME

C:\emacs\emacs-25.1\bin\runemacs.exe --debug-init -fn "-outline-Lucida Console-normal-r-normal-normal-18-97-96-96-c-*-iso8859-1" -l C:/org-index/run-tests.el C:/org-index/org-index.el C:/org-index/oidx.el
