;IDL batchfile to call Justin's routine to intelligently move cloud masks. 

cloudmaskpath  = '+/projectnb/trenders/code/Landtrendr2012'
test= strpos(!path, cloudmaskpath)
if test eq -1 then !path = cloudmaskpath+":"+!path

image_directory = '/projectnb/trenders/scenes/046029/images/'
jmask_directory = '/projectnb/trenders/scenes/046029/jcloudmasks/'
backup_directory = 'projectnb/trenders/scenes/046029/fmasks_backup/'
move_cloudmasks, image_directory, jmask_directory, backup_ledaps_dir=backup_dir


