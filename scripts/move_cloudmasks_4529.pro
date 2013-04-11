;IDL batchfile to call Justin's routine to intelligently move cloud masks. 

cloudmaskpath  = '+/projectnb/trenders/code/Landtrendr2012'
test= strpos(!path, cloudmaskpath)
if test eq -1 then !path = cloudmaskpath+":"+!path

image_directory = '/projectnb/trenders/scenes/045029/images/'
jmask_directory = '/projectnb/trenders/scenes/jcloudmasks/045029/'
backup_directory = '/projectnb/trenders/scenes/045029/fmasks_backup/'
move_cloudmasks, image_directory, jmask_directory, backup_ledaps_dir=backup_dir


