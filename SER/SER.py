#pip install teager-py
import numpy as np
from teager_py import Teager
#import librosa

#import os
#from matplotlib import pyplot as plt
#import IPython.display as ipd
#import pandas as pd
import pydub
#from pydub import AudioSegment
#import ffprobe
#import ffmpeg
#import numpy as np

#%matplotlib inline

filepath = "/project/graziul/data/Zone1/2018_10_15/201810151118-881734-27730.mp3"

import pydub

sound = pydub.AudioSegment.from_mp3(filepath)

