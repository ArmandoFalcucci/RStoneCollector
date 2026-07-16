# Branding assets

Drop a file here to replace the default hammer icon in the app header.

## Logo

Place ONE of the following files in this folder:

- `logo.svg` — vector format, scales perfectly, best for any size. Recommended.
- `logo.png` — raster format, transparent background recommended.
- `logo.jpg` — raster, opaque background.

Resolution: anything up to ~40x40 pixels of effective display area. Larger files work but will be scaled down. The header reserves about 32 pixels of vertical space.

SVG is preferred because it stays sharp at any DPI and adapts to both light and dark mode if you use `currentColor` for fills.

If you provide both `logo.svg` and `logo.png`, the SVG wins.

If no logo file is found, the default hammer icon is shown.

## Sizing tips

- For PNG/JPG: export at 64x64 with the actual logo content centered in a 48x48 area. The browser will scale to fit.
- For SVG: set `viewBox="0 0 40 40"` or similar. Use `fill="currentColor"` if you want it to inherit the header text color.

## Reload

Changes to files in this folder are picked up the next time you launch the app (`shiny::runApp()`).
