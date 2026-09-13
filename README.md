# BatchPlotModelToPDF

AutoLISP untuk AutoCAD yang membuat satu PDF multi-sheet dari beberapa frame di Model Space.

## Fitur

- Seleksi border/frame terluar setiap lembar.
- Urutan otomatis kiri ke kanan.
- Plotter otomatis `DWG To PDF.pc3`.
- Default `ISO full bleed A4` dan Portrait.
- Landscape membalik ukuran efektif menjadi 297 × 210 mm.
- Plot area Window, Fit to paper, Center plot.
- `monochrome.ctb`, plot styles, lineweights, Legacy wireframe.
- PDF multi-sheet melalui Publish.

## Penggunaan

1. Simpan DWG terlebih dahulu dan buka tab Model.
2. Ketik `APPLOAD`, lalu muat `BatchPlotModelToPDF.lsp`.
3. Pastikan command line menampilkan versi yang termuat.
4. Jalankan `BPP` (atau `BATCHPLOTPDF`).
5. Pilih polyline tertutup atau block border untuk setiap lembar, lalu tekan Enter.
6. Tekan Enter pada pilihan A4 dan Portrait untuk memakai default.
7. Tentukan nama file PDF.

DWG disimpan otomatis sebelum Publish agar named page setup dapat dibaca oleh AutoCAD. Page setup sementara dihapus setelah proses selesai.

## Persyaratan

- AutoCAD for Windows versi modern dengan dukungan Visual LISP/ActiveX.
- Drawing menggunakan CTB dan memiliki `monochrome.ctb`.
- Plotter `DWG To PDF.pc3` tersedia.
- DWG sudah pernah disimpan.

## Perintah

- `BPP` — shortcut.
- `BATCHPLOTPDF` — nama command lengkap.
