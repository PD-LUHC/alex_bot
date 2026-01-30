# Troubleshooting

**`Error: Not compatible with STRSXP: [type=list]` when downloading boundaries**  
We build a full query URL string for `sf::st_read()` (instead of passing `query` as a list) to avoid this. Ensure you have the latest package code.

**`sf` install issues**  
On macOS/Linux, install system libraries for GDAL/GEOS/PROJ before installing `sf`. See the `sf` README for platform-specific notes.
