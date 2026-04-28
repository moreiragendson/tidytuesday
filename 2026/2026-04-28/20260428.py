# Import libraries --------------------------------------------------------

import re
import pathlib
import requests
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import matplotlib.patches as mpatches

# Colors & fonts ----------------------------------------------------------

txt_color  = "#062635"
bg_color   = "#ffffff"
grid_color = "#3a6070"

custom_pal = {
    "Duty-free":      "#1696d2",
    "% of price":     "#0a4c6a",
    "Fixed fee/unit": "#55b748",
    "% + fixed fee":  "#ec008b",
}
CAT_ORDER = ["Duty-free", "% of price", "Fixed fee/unit", "% + fixed fee"]

CHAPTER_NAMES = {
    1:"Live animals",   2:"Meat",            3:"Fish",
    4:"Dairy & eggs",   5:"Other animal",    6:"Plants",
    7:"Vegetables",     8:"Fruits & nuts",   9:"Coffee & spices",
    10:"Cereals",       11:"Milling",        12:"Oil seeds",
    13:"Gums",          14:"Plaiting",       15:"Fats & oils",
    16:"Meat preps",    17:"Sugar",          18:"Cocoa",
    19:"Cereal preps",  20:"Fruit preps",    21:"Misc food",
    22:"Beverages",     23:"Animal feed",    24:"Tobacco",
}

_FONTS_DIR = pathlib.Path(__file__).parent / "fonts"
_FONTS_DIR.mkdir(exist_ok=True)

def _load_google_font(family, weight=400):
    tag = f"{family.replace(' ', '_')}_{weight}"
    fp = _FONTS_DIR / f"{tag}.ttf"
    if not fp.exists():
        css = requests.get(
            f"https://fonts.googleapis.com/css?family={family.replace(' ', '+')}:{weight}",
            headers={"User-Agent": "Mozilla/4.0"}, timeout=10,
        ).text
        m = re.search(r"url\((https://fonts\.gstatic\.com[^)]+)\)", css)
        if m:
            fp.write_bytes(requests.get(m.group(1), timeout=10).content)
    if fp.exists():
        fm.fontManager.addfont(str(fp))

for _family in ["Fira Sans", "Source Sans 3"]:
    _load_google_font(_family)
_load_google_font("Fira Sans", weight=700)

title_font = "Fira Sans"
body_font  = "Source Sans 3 ExtraLight"

# Load & wrangle ----------------------------------------------------------

BASE = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/"
tariff = pd.read_csv(BASE + "tariff_agricultural.csv")

df = tariff.copy()
df["chapter"] = df["hts8"].astype(str).str.zfill(8).str[:2].astype(int)
df = df[df["agreement"] == "mfn"]

latest = (
    df.sort_values("begin_effective_date", ascending=False)
    .groupby("hts8")
    .first()
    .reset_index()
)
ANIMAL_CHAPTERS = list(range(1, 6))   # 01 Live animals → 05 Other animal
latest = latest[latest["chapter"].isin(ANIMAL_CHAPTERS)]

latest["ad_val_rate"]   = latest["ad_val_rate"].fillna(0).clip(lower=0)
latest["specific_rate"] = latest["specific_rate"].fillna(0).clip(lower=0)
latest = latest[
    (latest["ad_val_rate"] < 9000) &
    (latest["specific_rate"] < 9000)
]

def _categorize(row):
    av, sp = row["ad_val_rate"], row["specific_rate"]
    if av > 0 and sp > 0:  return "% + fixed fee"
    if av > 0:             return "% of price"
    if sp > 0:             return "Fixed fee/unit"
    return "Duty-free"

latest["category"] = latest.apply(_categorize, axis=1)

counts = (
    latest.groupby(["chapter", "category"])
    .size()
    .reset_index(name="n")
)
full_idx = pd.MultiIndex.from_product([ANIMAL_CHAPTERS, CAT_ORDER],
                                       names=["chapter", "category"])
counts = (
    counts.set_index(["chapter", "category"])
    .reindex(full_idx, fill_value=0)
    .reset_index()
)
ch_totals = counts.groupby("chapter")["n"].sum().rename("total")
counts    = counts.join(ch_totals, on="chapter")
counts["pct"] = counts["n"] / counts["total"].replace(0, np.nan) * 100

# Build chart -------------------------------------------------------------

chapters   = ANIMAL_CHAPTERS
total_codes = int(ch_totals.sum())

# Row heights proportional to number of HTS codes (bottom = ch 24, top = ch 1)
chapters_rev = list(reversed(chapters))
heights  = [int(ch_totals.get(ch, 0)) for ch in chapters_rev]
y_starts = np.concatenate([[0], np.cumsum(heights[:-1])])

FIXED_GAP = total_codes * 0.0012   # same absolute gap for every row

fig, ax = plt.subplots(figsize=(4, 3), facecolor=bg_color)
ax.set_facecolor(bg_color)

for row_i, ch in enumerate(chapters_rev):
    h     = heights[row_i]
    if h == 0:
        continue
    y0     = y_starts[row_i]
    h_vis  = max(h - FIXED_GAP, h * 0.1)   # never collapse to zero
    y0_vis = y0 + (h - h_vis) / 2

    ch_data = counts[counts["chapter"] == ch].set_index("category")
    left    = 0.0

    for cat in CAT_ORDER:
        pct = float(ch_data.loc[cat, "pct"]) if cat in ch_data.index else 0.0
        if pct == 0:
            left += pct
            continue

        ax.barh(y0_vis, pct, height=h_vis, left=left,
                color=custom_pal[cat], edgecolor=bg_color,
                linewidth=0.3, align="edge", zorder=2)

        if pct > 8:
            ax.text(left + pct / 2, y0_vis + h_vis / 2,
                    f"{pct:.0f}%", ha="center", va="center",
                    fontsize=3, color="#ffffff", fontfamily=body_font,
                    fontweight="bold")
        left += pct

    # Chapter label on left
    ax.text(-1.5, y0 + h / 2,
            f"{CHAPTER_NAMES[ch]}",
            ha="right", va="center",
            fontsize=3.5, color=txt_color, fontfamily=body_font,
            clip_on=False)

    # Code count on right
    ax.text(-1.5, y0 - 20 + h / 2,
            f"{h:,}",
            ha="right", va="center",
            fontsize=3, color=txt_color, fontfamily=body_font, alpha=0.6,
            clip_on=False)
# Axes
ax.set_xlim(0, 100)
ax.set_ylim(-total_codes * 0.01, total_codes * 1.02)
ax.set_xticks([])
ax.tick_params(axis="y", left=False, labelleft=False)

for sp in ax.spines.values():
    sp.set_visible(False)
ax.xaxis.grid(True, color=grid_color, linewidth=0.4, alpha=0.5)
ax.set_axisbelow(True)

# Legend
legend_patches = [mpatches.Patch(color=custom_pal[c], label=c) for c in CAT_ORDER]
fig.legend(
    handles=legend_patches, frameon=False,
    labelcolor=txt_color, loc="upper center",
    bbox_to_anchor=(0.5, 0.860), ncol=4,
    prop={"family": body_font, "size": 4},
    handlelength=1, handleheight=0.8, handletextpad=0.4, columnspacing=0.8,
)

fig.suptitle(
    "US Tariffs on Animal Products: Fish Mostly Enters Free",
    fontfamily=title_font, fontsize=5.5, fontweight=700, color=txt_color,
    x=0.5, y=0.998, ha="center",
)
fig.text(
    0.5, 0.89,
    "MFN is the default US tariff applied to all WTO member countries "
    "without a preferential trade agreement\n— the baseline most of the world pays. "
    "Bar height represents each category’s share of the total,\nwhile bar width shows the proportion within each category by tariff type.",
    fontsize=5, color=txt_color, ha="center", fontfamily=body_font,
    wrap=True,
)
fig.text(
    0.5, 0.012,
    "TidyTuesday 2026 Week 17 — US Agricultural Tariffs  ·  Data: USITC Tariff Database  ·  Viz & Design: Gendson Moreira",
    fontsize=4, color=txt_color, ha="center", fontfamily=body_font,
)
fig.subplots_adjust(left=0.22, right=0.93, top=0.81, bottom=0.06)

out = pathlib.Path(__file__).parent / "20260428.png"
fig.savefig(out, dpi=300, bbox_inches="tight", facecolor=bg_color)
print(f"Saved: {out}")
