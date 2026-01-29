
##### Transferring off of SD Cards #####

rsync -avzP --remove-source-files /Volumes/KProvost*13*/ /Volumes/T7SKPField/Caro_Provost_Transit_Noise/February_2025_Deployment/SMLI/Meadow/KProvost_13/

## T7SKBackup



##### 


## /Volumes/T7SKPField/Caro_Provost_Transit_Noise/February_2025_Deployment/
## /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/February_2025_Deployment/

## moving to the T7SKPField drive
rsync -avzP /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/ /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/

## moving to the T7SKBackup drive
rsync -avzP --remove-source-files /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/Hempstead/ /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/February_2025_Deployment/Hempstead/






rsync -avzP --remove-source-files /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/ /Volumes/T7SKPField/Caro_Provost_Transit_Noise/

rsync -avzP /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/ /Volumes/T7SKPField/Caro_Provost_Transit_Noise/


rsync -avzP /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/

rsync -avzP /Volumes/T7SKPField/Caro_Provost_Transit_Noise/February_2025_Deployment/GCBS/InnerTrail/ /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/GCBS/InnerTrail/ 

## Nov adel to nas
rsync -rlDvzP --remove-source-files /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/

rsync -rlDvzP --remove-source-files /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/Clark/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Clark/

## nov gcbc 010 to nas
rsync -avzP --exclude="*txt" --include="*gz" --exclude="*WAV" --remove-source-files /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/010m_trainline/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/010m_trainline/

## nov gcbc 019 to nas
rsync -avzP --exclude="*txt" --include="*gz" --exclude="*WAV" --remove-source-files /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/019m_LaurelLaneWest/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/019m_LaurelLaneWest/

## nov gcbc 075 to nas
rsync -avzP --exclude="*txt" --include="*gz" --exclude="*WAV" --remove-source-files /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/075m_LaurelLaneEast/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/075m_LaurelLaneEast/



rsync -avzP --exclude="*txt" --include="*gz" --exclude="*WAV" --remove-source-files /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/November_2024_Deployment/Adelphi/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/Adelphi/

rsync -avzP --exclude="*txt" --exclude="*WAV" --include="*gz" --remove-source-files --ignore-existing /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/SMLI/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/SMLI/


rsync -avzP --exclude="*" --include="*txt" /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/

rsync -avzP -e "ssh -p 22" /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/

rsync -avzP /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/ kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/ 

rsync -avzP --ignore-existing kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/121m_LaurelLaneSouth/ /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/121m_LaurelLaneSouth/

rsync -avzP kprovost@10.86.3.32::Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/ /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/ 

scp -r -O /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/ kprovost@10.86.3.32:/volume1/Folder1/Caro_Provost_Transit_Noise/

scp -r -O kprovost@10.86.3.32:/volume1/Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/Adelphi/ /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/November_2024_Deployment/ 

scp -r -O kprovost@10.86.3.32:/volume1/Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/GCBC/010m_trainline/ /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/GCBC/ 


ssh kprovost@10.86.3.32
## volume 1


cp -vrn /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/Field/Caro_29/2025061*/ /volumeUSB2/usbshare/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/Field/Caro_29/


cp -vr /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/GCBS/InnerTrail/KProvost_17/2025060*/ /volumeUSB1/usbshare/Caro_Provost_Transit_Noise/February_2025_Deployment/GCBS/InnerTrail/KProvost_17/





rsync -rlDvzP --remove-source-files /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/Caro\ 22/ /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/Caro_22/

rsync -rlDvzP --remove-source-files /volumeUSB1/usbshare/Caro_Provost_Transit_Noise/ /volume1/Folder1/Caro_Provost_Transit_Noise/

rsync -rlDvzP --remove-source-files /volumeUSB2/usbshare/Caro_Provost_Transit_Noise/ /volume1/Folder1/Caro_Provost_Transit_Noise/

rsync -rlDvzP --remove-source-files /volumeUSB2/usbshare/Caro_Provost_Transit_Noise/February_2025_Deployment/Adelphi/SouthAve/ /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Adelphi/SouthAve/

rsync -rlDvzP --remove-source-files /volumeUSB2/usbshare/Caro_Provost_Transit_Noise/February_2025_Deployment/ /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/


sync /Feb/Adel/KP1 to /Feb/Adel/SCB/KP1
sync /Feb/Adel/KP2 to /Feb/Adel/SCB/KP2
sync /Feb/Adel/KP3 to /Feb/Adel/Gener/KP2
sync /Feb/Adel/KP4 to /Feb/Adel/Gener/KP2



rsync -rlDvzP --remove-source-files /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Caro_Provost_Transit_Noise/February_2025_Deployment/ /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/


rsync -rlDvP --remove-source-files --no-compress /volumeUSB1/usbshare/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/ /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/

rsync -rlDvzP --remove-source-files /volumeUSB2/usbshare/Caro_Provost_Transit_Noise/ /volume1/Folder1/Caro_Provost_Transit_Noise/


rsync -rlDvzP --remove-source-files /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Adelphi/KProvost_01/ /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Adelphi/SCB/KProvost_01/

rsync -rlDvzP --remove-source-files /volume1/Folder1/Caro_Provost_Transit_Noise/November_2024_Deployment/Adelphi/SCB/KProvost_01/ /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/Adelphi/SouthAve/KProvost_01/




## move only the txt files off, copy do not remove source files

## hangs
cd /volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/
for i in */*/*/*.txt*; do echo $i; cp -v "/volume1/Folder1/Caro_Provost_Transit_Noise/February_2025_Deployment/${i}" "/volumeUSB1/usbshare/Caro_Provost_Transit_Noise/February_2025_Deployment/${i}"; echo; done;

cd /volume1/Folder1/Caro_Provost_Transit_Noise/
for i in */*/*/*/*/*/; do echo "$i"; mkdir "/volumeUSB1/usbshare/Caro_Provost_Transit_Noise/${i}"; done;



rsync -rlDvzP --remove-source-files /volumeUSB2/usbshare/Caro_Provost_Transit_Noise/ /volume1/Folder1/Caro_Provost_Transit_Noise/; rsync -rlDvzP --remove-source-files /volumeUSB1/usbshare/Caro_Provost_Transit_Noise/ /volume1/Folder1/Caro_Provost_Transit_Noise/; 


-rlptgoD

rsync -rlDvzP /volume1/Folder1/Caro_Provost_Transit_Noise/


## also rsync via rclone

rclone copy --dry-run -P --order-by "size,descending" --include *.gz ~/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/ onedrive:Research/Caro_Provost_Transit_Noise/February_2025_Deployment/ --ignore-existing

rclone lsd dropbox:Backup_Huxley_2022/Huxley_Backup_20August2020/VZ-NAS2/RawGenomicReads-Archive/AMN_245113-Archive/filtered/desert\ birds\ filtered/

rclone copy --dry-run -P --order-by "size,descending" dropbox:Backup_Huxley_2022/Huxley_Backup_20August2020/VZ-NAS2/RawGenomicReads-Archive/AMN_245113-Archive/filtered/desert\ birds\ filtered/bilineata/ ~/Documents/Research/SRA/

rclone copy --dry-run -P --order-by "size,descending" onedrive:Research/Caro_Provost_Transit_Noise/February_2025_Deployment/ ~/Documents/Research/Clark/

## short read archive stuff

