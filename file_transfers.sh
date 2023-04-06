onedrive:/Postdoc_Backup/MMRR/WAVS/Wave/ --include "*temp.gz"
rclone copy -vv -P onedrive:/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/ /Users/kprovost/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/
rclone copy -vv /Users/kprovost/Documents/Postdoc_Working/ onedrive:/Postdoc_Backup/
rclone copy -vv /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/BLB/ onedrive:/Postdoc_Backup/Sounds_and_Annotations/Zonotrichia.leucophrys/Wave/ --include "Zono*leuco*"
rclone copy -vv /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/XC/ onedrive:/Postdoc_Backup/Sounds_and_Annotations/Zonotrichia.leucophrys/Wave/ --include "Zono*leuco*"
rclone copy -vv /Users/kprovost/Documents/STAIRWAYPLOT/ dropbox:/STAIRWAYPLOT/
rclone copy -vv /vz-nas1-active/ProcessedGenomicReads/EVERY_PLATE/ANGSD/QOPTS/ remote:/Dissertation/CHAPTER2_GENOMES/ANALYSIS/QOPTS/
rclone copy -vv onedrive:/Postdoc_Backup/MMRR/Environment/Copernicus_Landcover/ /Users/kprovost/Documents/Postdoc_Working/MMRR/ --include "*gz"
rclone move -vv -P --tpslimit 1 --transfers=1 /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/Peucaea/ onedrive:/Postdoc_Backup/MMRR/WAVS/Wave/0STATSDONE/Peucaea/ --include "*gz"
rclone move -vv -P --transfers=2 /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/ \
rclone move -vv -P --transfers=2 /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/0STATSDONE/ onedrive:/Postdoc_Backup/MMRR/WAVS/Wave/0STATSDONE/ --include "*xml.gz"
rclone move -vv -P onedrive:/Postdoc_Backup/0EnvTemp/ /Users/kprovost/Documents/Postdoc_Working/ --include "*gb*geo*" ## run this, also there are more file types
rclone move -vv -P onedrive:/Postdoc_Backup/0EnvTemp/ /Users/kprovost/Documents/Postdoc_Working/ --include "*PROBAV_LC100*" ## run this
rclone move -vv -P onedrive:/Postdoc_Backup/MMRR/ /Users/kprovost/Documents/Postdoc_Working/ --include "*NASA*" ## run
rclone move -vv -P onedrive:/Postdoc_Backup/MMRR/0EnvTemp/ /Users/kprovost/Documents/Postdoc_Working/ --include "*CROPPED*" ## run
rclone move -vv -P onedrive:/Postdoc_Backup/MMRR/0EnvTemp/ /Users/kprovost/Documents/Postdoc_Working/ --include "*gb*geo*" ## run
rclone move -vv -P onedrive:/Postdoc_Backup/MMRR/0EnvTemp/ /Users/kprovost/Documents/Postdoc_Working/ --include "*PROBAV_LC100*" ## run
rclone move -vv -P onedrive:/Postdoc_Backup/MMRR/0EnvTemp/ /Users/kprovost/Documents/Postdoc_Working/ --include "*SOIL*" ## run
rclone move -vv -P onedrive:/Postdoc_Backup/MMRR/glccgbe20_tif/ /Users/kprovost/Documents/Postdoc_Working/ --include "*gb*geo*" ## run
rclone move -vv -P onedrive:/Postdoc_Backup/MMRR/WAVS/ /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/ --include "*Zonotrichia*Table.1*" --exclude ".resample.48000.wav_*" --tpslimit 1 --transfers=1
rclone move -vv "/Users/kprovost/Documents/CORNELL_DRIVE_BACKUP/" "columbiadrive:/Cornell Folders/"
rclone move -vv /Users/kprovost/Documents/Postdoc_Working/SoundShape/ onedrive:/Postdoc_Backup/SoundShape/ --include ".resample.48000.wav.gz"
rclone move -vv /Users/kprovost/Dropbox\ \(AMNH\)/STATS/ dropbox:/STATS/ --include "T*gz"
rclone move -vv dropbox:/Backup_Huxley_2022/ ~/Documents/STAIRWAYPLOT/ --include "*.ml*"
rsync -avzP --remove-source-files kprovost@huxley-master.pcc.amnh.org:/vz-nas1-active/ProcessedGenomicReads/EVERY_PLATE/ANGSD/OLDS/*saf.gz ~/Dropbox\ \(AMNH\)/OLDS/
rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/WAVS/Zonotrichia.leucophrys.nuttalli* /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/
rsync -avzP kprovost@huxley-master.pcc.amnh.org:/vz-nas1-active/ProcessedGenomicReads/EVERY_PLATE/ANGSD/MLSTATS/*/*ml* ~/Documents/STAIRWAYPLOT/
rsync -avzP kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/*toml /Users/kprovost/Documents/Postdoc_Working/
