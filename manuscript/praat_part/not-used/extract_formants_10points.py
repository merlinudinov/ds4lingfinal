import os
import parselmouth
from praatio import tgio
import pandas as pd

# === EDIT THESE ===
textgrid_folder = "/Users/merlin/Desktop/run/s1txt/"
wav_folder = "/Users/merlin/Desktop/run/s1wav/"
output_file = "/Users/merlin/Desktop/run/s1results.csv"

rows = []

for fname in os.listdir(textgrid_folder):
    if not fname.endswith(".TextGrid"):
        continue
    tg_path = os.path.join(textgrid_folder, fname)
    wav_path = os.path.join(wav_folder, fname.replace(".TextGrid", ".wav"))
    if not os.path.exists(wav_path):
        continue

    tg = tgio.openTextgrid(tg_path)
    print("Tier names found in file:", list(tg.tierDict.keys()))
    tier = tg.tierDict['2']
    snd = parselmouth.Sound(wav_path)
    formants = snd.to_formant_burg(time_step=0.002, max_number_of_formants=5, maximum_formant=5500, window_length=0.01)

    for start, end, label in tier.entryList:
        if label.strip() == "#" or label.strip() == "":
            continue
        midpoint = (start + end) / 2
        f1 = formants.get_value_at_time(1, midpoint)
        f2 = formants.get_value_at_time(2, midpoint)
        f3 = formants.get_value_at_time(3, midpoint)
        rows.append({
            "filename": fname.replace(".TextGrid", ""),
            "vowel": label,
            "start": start,
            "end": end,
            "duration": end - start,
            "F1": f1,
            "F2": f2,
            "F3": f3,
        })

# Save to CSV
df = pd.DataFrame(rows)
df.to_csv(output_file, index=False)
print(f"✅ Done! Output saved to {output_file}")