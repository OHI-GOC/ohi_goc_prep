"""Download Gulf windows of VIIRS annual median-masked radiance.

The global file is a single GeoTIFF of about 15 arc-seconds. Earth Engine
splits the annual product across two collections, both band median_masked, in
nanowatts per square centimetre per steradian:

- NOAA/VIIRS/DNB/ANNUAL_V21 for 2013-2021
- NOAA/VIIRS/DNB/ANNUAL_V22 for 2022 onward

This script asks only for the Gulf rectangle and writes one GeoTIFF per year.
The northern and southern halves are requested separately so each request
stays under Earth Engine's download-size limit, then merged here.

Requires a one-time `earthengine authenticate` on this machine, and a Google
Cloud project registered with Earth Engine (pass --project or EE_PROJECT).
"""

import argparse
import io
import os
import sys
import zipfile
from pathlib import Path

import ee
import requests


def initialize(project):
    project = project or os.environ.get("EE_PROJECT") or None
    if project:
        ee.Initialize(project=project)
    else:
        ee.Initialize()


def download_bytes(url):
    response = requests.get(url, timeout=600)
    response.raise_for_status()
    content = response.content
    if content[:2] == b"PK":
        with zipfile.ZipFile(io.BytesIO(content)) as archive:
            names = [name for name in archive.namelist() if name.lower().endswith(".tif")]
            if not names:
                raise RuntimeError("Earth Engine zip did not contain a GeoTIFF.")
            return archive.read(names[0])
    if content[:2] in (b"II", b"MM"):
        return content
    raise RuntimeError(
        "Earth Engine did not return a GeoTIFF. First bytes: "
        + content[:180].decode("utf-8", errors="replace")
    )


def download_half(image, year, west, south, east, north, part_path):
    geom = ee.Geometry.Rectangle(
        [west, south, east, north], proj="EPSG:4326", geodesic=False
    )
    url = image.getDownloadURL(
        {
            "name": f"vnl_v22_{year}",
            "bands": ["median_masked"],
            "region": geom,
            "crs": "EPSG:4326",
            # Metres. 15 arc-seconds is about 464 m. The notebook checks the
            # GeoTIFF resolution after download, because a geographic CRS can
            # be misread as degrees.
            "scale": 463.83,
            "format": "GEO_TIFF",
        }
    )
    part_path.write_bytes(download_bytes(url))
    print(f"wrote {part_path}", flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-dir", required=True)
    parser.add_argument("--years", required=True, help="Comma-separated years")
    parser.add_argument("--west", type=float, required=True)
    parser.add_argument("--south", type=float, required=True)
    parser.add_argument("--east", type=float, required=True)
    parser.add_argument("--north", type=float, required=True)
    parser.add_argument("--project", default="")
    args = parser.parse_args()

    try:
        initialize(args.project)
    except Exception as error:
        print(
            "Earth Engine is not ready. Run `earthengine authenticate` once, "
            "and set EE_PROJECT to the Google Cloud project registered with "
            "Earth Engine.\n"
            f"{error}",
            file=sys.stderr,
        )
        return 1

    out_dir = os.path.abspath(args.out_dir)
    os.makedirs(out_dir, exist_ok=True)
    # v2.2 on Earth Engine begins in 2022. Earlier years of the same annual
    # product are in the v2.1 collection.
    collections = {
        "v21": ee.ImageCollection("NOAA/VIIRS/DNB/ANNUAL_V21").select("median_masked"),
        "v22": ee.ImageCollection("NOAA/VIIRS/DNB/ANNUAL_V22").select("median_masked"),
    }

    for year in [int(piece) for piece in args.years.split(",") if piece]:
        source = "v22" if year >= 2022 else "v21"
        images = collections[source].filter(ee.Filter.calendarRange(year, year, "year"))
        count = images.size().getInfo()
        if count < 1:
            print(f"No VIIRS annual image for {year} in {source}.", file=sys.stderr)
            return 1
        image = images.mosaic().rename("median_masked")
        mid = (args.south + args.north) / 2.0
        halves = [
            (args.south, mid + 0.15, "s"),
            (mid - 0.15, args.north, "n"),
        ]
        for south, north, tag in halves:
            part = os.path.join(out_dir, f"vnl_v22_median_masked_{year}_{tag}.tif")
            download_half(
                image,
                year,
                args.west,
                south,
                args.east,
                north,
                Path(part),
            )
        print(f"downloaded {year}", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
