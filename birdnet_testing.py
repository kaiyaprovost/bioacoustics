from birdnetlib import Recording
from birdnetlib.analyzer import Analyzer
from datetime import datetime

# Load and initialize the BirdNET-Analyzer models.
analyzer = Analyzer()

recording = Recording(
    analyzer,
    "/Users/kprovost/Documents/Research/Calidris/Calidris-pusilla-276968.mp3",
    lat=71.3414,
    lon=-156.6118,
    date=datetime(year=2015, month=6, day=14), # use date or week_48
    min_conf=0.25,
)
recording.analyze()
print(recording.detections)