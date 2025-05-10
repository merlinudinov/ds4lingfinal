import os
import parselmouth
from praatio import tgio
import pandas as pd
import numpy as np

# === EDIT THESE PATHS ===
textgrid_folder = "/Users/merlin/Desktop/run/wholetxt/"
wav_folder = "/Users/merlin/Desktop/run/wholewav/"
output_file = "/Users/merlin/Desktop/run/formantresults.csv"

# ========================

rows = []

for fname in os.listdir(textgrid_folder):
    if not fname.endswith(".TextGrid"):
        continue

    tg_path = os.path.join(textgrid_folder, fname)
    wav_path = os.path.join(wav_folder, fname.replace(".TextGrid", ".wav"))
    if not os.path.exists(wav_path):
        continue

    # Load TextGrid and WAV
    tg = tgio.openTextgrid(tg_path)
    print("Processing:", fname)
    tier = tg.tierDict['2']  # use the tier named '2'
    snd = parselmouth.Sound(wav_path)
    formants = snd.to_formant_burg(time_step=0.002, max_number_of_formants=5, maximum_formant=5500, window_length=0.01)

    for start, end, label in tier.entryList:
        label = label.strip()
        if label == "" or label == "#":
            continue

        times = np.linspace(start, end, 10)

        for point_idx, t in enumerate(times, start=1):
            f1 = formants.get_value_at_time(1, t)
            f2 = formants.get_value_at_time(2, t)
            f3 = formants.get_value_at_time(3, t)
            rows.append({
                "file": fname,
                "label": label,
                "point_index": point_idx,
                "time": t,
                "f1": f1,
                "f2": f2,
                "f3": f3
            })

# Save to CSV
output_df = pd.DataFrame(rows)
output_df.to_csv(output_file, index=False)
print("Done! Results saved to", output_file)
