# FAQ

**Q: The silhouette shows the answer.**  
A: By default `plot_selected()` and `plot_with_surroundings()` do **not** add a title. Set `show_title = TRUE` only when you want to reveal.

**Q: Buffers look odd.**  
A: Buffers are computed in EPSG:27700 (meters) and then reprojected to WGS84 for plotting.

**Q: MSOA support?**  
A: Yes. `area_type = "msoa"` uses the EW 2021 layer; nation toggles are limited to `E` and `W`.
