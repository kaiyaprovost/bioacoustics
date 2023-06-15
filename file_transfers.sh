#onedrive:/Postdoc_Backup/MMRR/WAVS/Wave/ --include "*temp.gz"
#rclone copy -vv -P onedrive:/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/ /Users/kprovost/Documents/Postdoc_Working/Genetics/Birds-phylogatr-results_7dec2020/Aves/Passeriformes/Passerellidae/
#rclone copy -vv /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/BLB/ onedrive:/Postdoc_Backup/Sounds_and_Annotations/Zonotrichia.leucophrys/Wave/ --include "Zono*leuco*"
#rclone copy -vv /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/XC/ onedrive:/Postdoc_Backup/Sounds_and_Annotations/Zonotrichia.leucophrys/Wave/ --include "Zono*leuco*"
#rclone copy -vv /Users/kprovost/Documents/STAIRWAYPLOT/ dropbox:/STAIRWAYPLOT/
#rclone copy -vv /vz-nas1-active/ProcessedGenomicReads/EVERY_PLATE/ANGSD/QOPTS/ remote:/Dissertation/CHAPTER2_GENOMES/ANALYSIS/QOPTS/
#rclone copy -vv onedrive:/Postdoc_Backup/MMRR/Environment/Copernicus_Landcover/ /Users/kprovost/Documents/Postdoc_Working/MMRR/ --include "*gz"
#rclone move -vv "/Users/kprovost/Documents/CORNELL_DRIVE_BACKUP/" "columbiadrive:/Cornell Folders/"
#rclone move -vv /Users/kprovost/Dropbox\ \(AMNH\)/STATS/ dropbox:/STATS/ --include "T*gz"
#rclone move -vv dropbox:/Backup_Huxley_2022/ ~/Documents/STAIRWAYPLOT/ --include "*.ml*"
#rsync -avzP --remove-source-files kprovost@huxley-master.pcc.amnh.org:/vz-nas1-active/ProcessedGenomicReads/EVERY_PLATE/ANGSD/OLDS/*saf.gz ~/Dropbox\ \(AMNH\)/OLDS/
#rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/WAVS/Zonotrichia.leucophrys.nuttalli* /Users/kprovost/Documents/Postdoc_Working/MMRR/WAVS/Wave/
#rsync -avzP kprovost@huxley-master.pcc.amnh.org:/vz-nas1-active/ProcessedGenomicReads/EVERY_PLATE/ANGSD/MLSTATS/*/*ml* ~/Documents/STAIRWAYPLOT/
#rsync -avzP kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/*toml /Users/kprovost/Documents/Postdoc_Working/
rclone copy -vv -P /Users/kprovost/Documents/GitHub/ onedrive:/GitHub/
rclone copy -vv /Users/kprovost/Documents/Postdoc_Working/ onedrive:/Postdoc_Backup/

rclone move -vv -P -c /Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/ onedrive:/Postdoc_Backup/Sounds_and_Annotations/Aves/Passeriformes/Oscines/Passerellidae/Zonotrichia/ --user-agent "ISV|rclone.org|rclone/v1.58.1" --transfers=1 --checkers=4 --order-by name --include "*selections.txt*"

rclone move -vv -P -c /Users/kprovost/Documents/Postdoc_Working/JY_project/SoundShape/ onedrive:/Postdoc_Backup/SoundShape/ --user-agent "ISV|rclone.org|rclone/v1.58.1" --transfers=1 --checkers=4 --order-by name;







rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/*pugetensis.NA.25.toml /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/
rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/*pugetensis.NA.29.toml /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/
#rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/*pugetensis.NA.31.toml /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/
rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/*nuttalli.NA.40.toml /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/
gzip -fv /users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/NUTTALLI/NOTANNOTATED/38/Wave/*wav
gzip -fv /users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/28/Wave/*wav
gzip -fv /users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/29/Wave/*wav
gzip -fv /users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/31/Wave/*wav
gzip -fv /users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/37/Wave/*wav
gzip -fv /users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/38/Wave/*wav
gzip -fv /users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/39/Wave/*wav
rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/NUTTALLI/NOTANNOTATED/*/Wave/*gz /Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/LEUCOPHRYS/NUTTALLI/NOTANNOTATED/
rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/*/Wave/*gz /Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/LEUCOPHRYS/NUTTALLI/NOTANNOTATED/


rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/NUTTALLI/NOTANNOTATED/36/Wave/ /Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/LEUCOPHRYS/NUTTALLI/NOTANNOTATED/
rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/NUTTALLI/NOTANNOTATED/40/Wave/ /Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/LEUCOPHRYS/NUTTALLI/NOTANNOTATED/
rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/39/Wave/ /Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/

rsync -avzP /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/*csv kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/
rsync -avzP /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/*toml kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/
rsync -avzP /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/n41_Wave_prep_230613_140848.csv kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/
rsync -avzP /Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/LEUCOPHRYS/PUGETENSIS/DONE/*wav kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/Wave/
rsync -avzP /Users/kprovost/Documents/Postdoc_Working/JY_project/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/ kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/PUGETENSIS/NOTANNOTATED/
rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/*toml /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/
rsync -avzP kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/n41_Wave_prep_230613_140848.csv /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/
rsync -avzP kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/Wave/*wav_* /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/Wave/

rsync -avzP --remove-source-files kprovost@sftp.osc.edu:/users/PYS1065/kprovost/bioacoustics/p*csv /Users/kprovost/Documents/Postdoc_Working/JY_project/2023_Zonotrichia_Model/


bInGa!2#120412

/users/PYS1065/kprovost/bioacoustics/Zonotrichia/LEUCOPHRYS/NUTTALLI

n33 -- running pt 1
p24
p25

