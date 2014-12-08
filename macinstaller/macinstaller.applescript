display dialog Â
	"This will copy the files necessary to use Mandoku to your computer and will configure it properly. 
Please drag the Emacs.app on this volume to the Applications folder." buttons {"Cancel", "Next"} default button 2
display dialog Â
	"Please copy the Private Token from GitLab (gl.kanripo.org/profile/account) and paste it below, then press 'Next' to continue" buttons {"Cancel", "Next"} default button 2 default answer "Gitlab Private Token" with icon 2
set token to text returned of result
display dialog Â
	"Please enter your user name as shown in the Gitlab profile page." buttons {"Cancel", "Next"} default button 2 default answer "Gitlab Username" with icon 2
set username to text returned of result

display dialog Â
	"Please enter a name for the Mandoku data folder (this will be created in your home directory)" buttons {"Cancel", "Next"} default button 2 default answer "krp" with icon 2
set mdfolder to text returned of result
set krp to POSIX path of (path to home folder as text) & mdfolder
set UnixPath to POSIX path of ((path to me as text) & "::") --get path to parent folder
set the_script to UnixPath & "macinstaller/ffm.sh '" & token & "' '" & username & "' '" & krp & "'"
-- display dialog Â
-- 	the_script buttons {"Cancel", "OK"}
display dialog Â
	"The installer will now open a Terminal window and perform the rest of the installation.  Please follow the instructions there." buttons {"Cancel", "Next"} default button 2
tell application "Terminal"
	activate
	do script the_script
end tell
-- do shell script the_script
-- display dialog "The installer completed successfully." buttons {"OK"}
