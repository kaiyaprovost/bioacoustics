source /Users/kprovost/Documents/GitHub/birdnet-wrapper/activate-bird-venv

python3.11 /Users/kprovost/Documents/GitHub/birdnet-wrapper/main.py -i /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/TrashTree -o /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/TrashTree -s /Users/kprovost/Documents/GitHub/birdnet-wrapper/examples/species_list_noise.txt -n; for i in /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/February_2025_Deployment/Francis/TrashTree/*/*/*txt; do gzip -fv ${i%.BirdNET.selection.table.txt}.WAV; gzip -fv $i; done;

for i in /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/February_2025_Deployment/*/*/*/*/*txt; do gzip -fv ${i%.BirdNET.selection.table.txt}.WAV; gzip -fv $i; done;

cd /volume1/Folder1/Caro_Provost_Transit_Noise/
for i in ./February_2025_Deployment/Adelphi/SCB/*/*/*txt; do gzip -fv ${i%.BirdNET.selection.table.txt}.WAV; gzip -fv $i; done;

python3.11 /Users/kprovost/Documents/GitHub/birdnet-wrapper/main.py -h

python3.11 /Users/kprovost/Documents/GitHub/birdnet-wrapper/main.py -i /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/ -o /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/ -s /Users/kprovost/Documents/GitHub/birdnet-wrapper/examples/species_list_noise.txt -n

python3.11 /Users/kprovost/Documents/GitHub/birdnet-wrapper/main.py -i /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/Clark/ -o /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/Clark/ -s /Users/kprovost/Documents/GitHub/birdnet-wrapper/examples/species_list_noise.txt -n

python3.11 /Users/kprovost/Documents/GitHub/birdnet-wrapper/main.py -i /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/GCBS/ -o /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/GCBS/ -s /Users/kprovost/Documents/GitHub/birdnet-wrapper/examples/species_list_noise.txt -n

python3.11 /Users/kprovost/Documents/GitHub/birdnet-wrapper/main.py -i /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/Hempstead/ -o /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/Hempstead/ -s /Users/kprovost/Documents/GitHub/birdnet-wrapper/examples/species_list_noise.txt -n

python3.11 /Users/kprovost/Documents/GitHub/birdnet-wrapper/main.py -i /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/SMLI/ -o /Users/kprovost/Documents/Research/Caro_Provost_Transit_Noise/February_2025_Deployment/SMLI/ -s /Users/kprovost/Documents/GitHub/birdnet-wrapper/examples/species_list_noise.txt -n

python3.11 /Users/kprovost/Documents/GitHub/birdnet-wrapper/main.py -i /Volumes/T7SKPField/Caro_Provost_Transit_Noise/February_2025_Deployment/Clark/ -o /Volumes/T7SKPField/Caro_Provost_Transit_Noise/February_2025_Deployment/Clark/ -s /Users/kprovost/Documents/GitHub/birdnet-wrapper/examples/species_list_noise.txt -n

for i in /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/February_2025_Deployment/SMLI/*/*/*/*txt; do gzip -fv ${i%.BirdNET.selection.table.txt}.WAV; gzip -fv $i; done;


cd /Volumes/T7SKBackup/Caro_Provost_Transit_Noise/February_2025_Deployment/
for loc in */; do
for subloc in $loc/*/; do
for rec in $subloc/*/; do
for date in $rec/*/; do 
echo $date
ls -l  $date/*txt.gz | wc -l
cat $date/*txt.gz >> $subloc/folder_birdnet_21August2025_zip.txt.gz
#ls -l  $date/*txt | wc -l
#cat $date/*txt >> $subloc/folder_birdnet_21August2025.txt
done
done
done
done
