;;; ------------------------------------------------------------------------
;;; BatchPlotModelToPDF.lsp
;;; Version: 2.9
;;; Command: BATCHPLOTPDF
;;;
;;; Creates one multi-sheet PDF from window plots in Model Space.  A temporary
;;; named Model-space page setup is created for each selected frame; no
;;; Paper-space viewport is required.
;;; Select one outer paper border/frame for each sheet.  The frames are
;;; sorted from left to right (then top to bottom when their X is equal).
;;;
;;; Requirements: AutoCAD for Windows (full AutoCAD, not AutoCAD LT), a
;;; saved DWG, a CTB drawing, DWG To PDF.pc3 (or another PDF PC3), and
;;; monochrome.ctb. Runtime plotting must be verified in AutoCAD.
;;; ------------------------------------------------------------------------

(vl-load-com)

;;; AutoCAD ActiveX plot enum values used here.
(setq BP:acWindow 4
      BP:acScaleToFit 0
      BP:ac0degrees 0
      BP:ac90degrees 1
      BP:acMillimeters 1)

(defun BP:variant-list (value)
  (cond
    ((= (type value) 'VARIANT) (BP:variant-list (vlax-variant-value value)))
    ((= (type value) 'SAFEARRAY) (vlax-safearray->list value))
    ((listp value) value)
    (T nil)))

(defun BP:contains-ci (text fragment)
  (and text fragment
       (vl-string-search (strcase fragment) (strcase text))))

(defun BP:find-ci (items wanted / item found)
  (foreach item items
    (if (= (strcase item) (strcase wanted))
      (setq found item)))
  found)

(defun BP:first-containing (items fragment / item found)
  (foreach item items
    (if (and (not found) (BP:contains-ci item fragment))
      (setq found item)))
  found)

(defun BP:unique-name (collection stem / number candidate)
  (setq number 1
        candidate stem)
  (while (not (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-Item (list collection candidate))))
    (setq number (1+ number)
          candidate (strcat stem (itoa number))))
  candidate)

(defun BP:choose-plotter (layout / devices default option number answer)
  (setq devices (BP:variant-list (vla-GetPlotDeviceNames layout))
        default (cond ((BP:find-ci devices "DWG To PDF.pc3"))
                      ((BP:first-containing devices "AutoCAD PDF"))
                      ((BP:first-containing devices "PDF"))
                      (T (vla-get-ConfigName layout))))
  (if (not (member default devices))
    (setq default (car devices)))
  (if (not devices)
    nil
    (progn
      (princ "\nPlotter yang tersedia:")
      (setq number 1)
      (foreach option devices
        (princ (strcat "\n  " (itoa number) ". " option))
        (setq number (1+ number)))
      (princ "\n  0. Ketik nama plotter/PC3 sendiri")
      (setq answer nil)
      (while (not answer)
        (setq number
          (getint
            (strcat "\nNomor plotter <"
                    (itoa (1+ (vl-position default devices))) ">: ")))
        (cond
          ((null number) (setq answer default))
          ((= number 0)
            (setq option (getstring T "\nNama plotter/PC3: "))
            (if (/= option "") (setq answer option)))
          ((and (> number 0) (<= number (length devices)))
            (setq answer (nth (1- number) devices)))
          (T (princ "\nNomor tidak valid. Coba lagi."))))
      answer)))

(defun BP:default-pdf-plotter (layout / devices)
  ;; Use the standard Autodesk PDF driver automatically, without prompting.
  (setq devices (BP:variant-list (vla-GetPlotDeviceNames layout)))
  (BP:find-ci devices "DWG To PDF.pc3"))

(defun BP:media-for-size (media size / full any item)
  (foreach item media
    (cond
      ((and (not full) (BP:contains-ci item (strcat "FULL_BLEED_" size)))
       (setq full item))
      ((and (not any) (BP:contains-ci item size))
       (setq any item))))
  ;; AutoLISP OR returns T/nil, not the matching string.
  (if full full any))

(defun BP:choose-media-from-list (media / i item choice)
  (princ "\nUkuran kertas yang tersedia pada plotter ini:")
  (setq i 1)
  (foreach item media
    (princ (strcat "\n  " (itoa i) ". " item))
    (setq i (1+ i)))
  (setq choice nil)
  (while (not choice)
    (setq i (getint "\nNomor ukuran kertas: "))
    (if (and i (> i 0) (<= i (length media)))
      (setq choice (nth (1- i) media))
      (princ "\nNomor tidak valid. Coba lagi.")))
  choice)

(defun BP:choose-media (media / answer chosen)
  (initget "A4 A3 A2 A1 A0 Daftar")
  (setq answer (getkword "\nUkuran kertas [A4/A3/A2/A1/A0/Daftar] <A4>: "))
  (if (null answer) (setq answer "A4"))
  (if (= answer "Daftar")
    (BP:choose-media-from-list media)
    (progn
      (setq chosen (BP:media-for-size media answer))
      (if chosen
        chosen
        (progn
          (princ (strcat "\nUkuran " answer " tidak ditemukan pada plotter ini."))
          (BP:choose-media-from-list media))))))

(defun BP:media-dimensions (media / left xpos right width height)
  (setq left (vl-string-search "_(" media)
        xpos (vl-string-search "_x_" media)
        right (vl-string-search "_MM" media))
  (if (and left xpos right (> xpos left) (> right xpos))
    (progn
      ;; vl-string-search is zero based; substr is one based.
      (setq width (atof (substr media (+ left 3) (- xpos (+ left 2))))
            height (atof (substr media (+ xpos 4) (- right (+ xpos 3)))))
      (list width height))
    nil))

;;; Returns T when the media's native width is greater than its height.
(defun BP:media-landscape-p (media / dimensions)
  (setq dimensions (BP:media-dimensions media))
  (if dimensions
    (> (car dimensions) (cadr dimensions))
    ;; Most PDF PC3 ISO media names that do not expose dimensions are landscape.
    T))

(defun BP:effective-paper-text (media orientation / dimensions short-side long-side)
  (setq dimensions (BP:media-dimensions media))
  (if dimensions
    (progn
      (setq short-side (min (car dimensions) (cadr dimensions))
            long-side (max (car dimensions) (cadr dimensions)))
      (strcat (rtos (if (= orientation "Portrait") short-side long-side) 2 2)
              " x "
              (rtos (if (= orientation "Portrait") long-side short-side) 2 2)
              " mm"))
    orientation))

(defun BP:plot-rotation (media orientation / native-landscape)
  (setq native-landscape (BP:media-landscape-p media))
  ;; The canonical media may be stored as either W x H or H x W. Rotate it
  ;; when needed so the effective dimensions always follow the user choice:
  ;; Portrait = short x long; Landscape = long x short.
  (if (= orientation "Portrait")
    (if native-landscape BP:ac90degrees BP:ac0degrees)
    (if native-landscape BP:ac0degrees BP:ac90degrees)))

(defun BP:set-legacy-wireframe (plot-config / ename data)
  ;; PLOTSETTINGS DXF group 76: 0=As displayed, 1=Legacy wireframe,
  ;; 2=Legacy hidden, 3=Rendered. ActiveX does not expose this setting for
  ;; a Model-space PlotConfiguration, so update its database record directly.
  (setq ename (vlax-vla-object->ename plot-config)
        data (entget ename))
  (if (assoc 76 data)
    (entmod (subst (cons 76 1) (assoc 76 data) data))
    (entmod (append data (list (cons 76 1))))))

(defun BP:choose-orientation (/ answer)
  ;; Keep both choices inside one bracket pair.  This is the standard
  ;; AutoCAD keyword format and makes both items appear in Dynamic Input.
  (initget "Portrait Landscape")
  (setq answer
    (getkword "\nOrientasi [Portrait/Landscape] <Portrait>: "))
  (if answer answer "Portrait"))

(defun BP:bounding-box (ename / object low high)
  (setq object (vlax-ename->vla-object ename))
  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-GetBoundingBox (list object 'low 'high)))
    nil
    (list (BP:variant-list low) (BP:variant-list high))))

(defun BP:valid-box-p (box / low high)
  (and box
       (setq low (car box) high (cadr box))
       (> (- (car high) (car low)) 1e-8)
       (> (- (cadr high) (cadr low)) 1e-8)))

(defun BP:frame-candidate-p (ename / data type flags)
  ;; RECTANGLE is normally an LWPOLYLINE.  INSERT is also allowed so a
  ;; title-border stored as a block reference can be selected as one sheet.
  (setq data (entget ename)
        type (cdr (assoc 0 data))
        flags (cdr (assoc 70 data)))
  (cond
    ((= type "INSERT") T)
    ((member type '("LWPOLYLINE" "POLYLINE"))
      (= 1 (logand (if flags flags 0) 1)))
    (T nil)))

;;; Each record is: (min-point max-point original-selection-index)
(defun BP:record-before-p (a b / ax bx ay by tolerance)
  (setq ax (car (car a))
        bx (car (car b))
        ay (cadr (car a))
        by (cadr (car b))
        tolerance 1e-7)
  (if (< (abs (- ax bx)) tolerance)
    (> ay by)                            ; same column: top sheet first
    (< ax bx)))                          ; primary order: left to right

(defun BP:collect-frames (selection / index ename box records)
  (setq index 0)
  (repeat (sslength selection)
    (setq ename (ssname selection index)
          box (BP:bounding-box ename))
    (cond
      ((not (BP:frame-candidate-p ename))
        (princ (strcat "\nObjek ke-" (itoa (1+ index))
                       " dilewati: pilih polyline tertutup atau block border.")))
      ((BP:valid-box-p box)
        (setq records (cons (append box (list index)) records)))
      (T
        (princ (strcat "\nFrame ke-" (itoa (1+ index))
                       " dilewati: bounding box tidak valid."))))
    (setq index (1+ index)))
  (vl-sort records 'BP:record-before-p))

(defun BP:point2d (point / array)
  ;; SetWindowToPlot requires a two-double SAFEARRAY.
  (setq array (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-fill array (list (car point) (cadr point)))
  (vlax-make-variant array))

(defun BP:configure-layout (layout plotter media orientation frame / available)
  (setq BP:stage "mengatur plotter DWG To PDF.pc3")
  (vla-put-ConfigName layout plotter)
  (setq BP:stage "memuat informasi plotter")
  (vla-RefreshPlotDeviceInfo layout)
  (setq BP:stage "memvalidasi nama ukuran kertas")
  (setq available (BP:variant-list (vla-GetCanonicalMediaNames layout)))
  (if (not (and (= (type media) 'STR) (member media available)))
    (progn
      (princ "\nNama media tidak valid untuk plotter aktif: ")
      (prin1 media)
      (exit)))
  (setq BP:stage (strcat "mengatur ukuran kertas: " media))
  (vla-put-CanonicalMediaName layout media)
  (setq BP:stage "mengatur satuan kertas")
  (vla-put-PaperUnits layout BP:acMillimeters)
  (setq BP:stage "mengatur rotasi Portrait/Landscape")
  (vla-put-PlotRotation layout (BP:plot-rotation media orientation))
  ;; AutoCAD requires SetWindowToPlot BEFORE PlotType is set to acWindow.
  ;; Bounding-box points contain XYZ, so pass explicit XY arrays here.
  (setq BP:stage "mengatur koordinat Window Plot")
  (vla-SetWindowToPlot layout
                        (BP:point2d
                          (list (car (car frame)) (cadr (car frame))))
                        (BP:point2d
                          (list (car (cadr frame)) (cadr (cadr frame)))))
  (setq BP:stage "mengaktifkan Plot Area Window")
  (vla-put-PlotType layout BP:acWindow)
  (setq BP:stage "mengaktifkan Scale to Fit")
  (vla-put-UseStandardScale layout :vlax-true)
  (vla-put-StandardScale layout BP:acScaleToFit)
  (setq BP:stage "mengaktifkan Center Plot")
  (vla-put-CenterPlot layout :vlax-true)
  (setq BP:stage "menonaktifkan Hidden Plot")
  (vla-put-PlotHidden layout :vlax-false)
  (setq BP:stage "mengaktifkan plot style")
  (vla-put-PlotWithPlotStyles layout :vlax-true)
  (setq BP:stage "memasang monochrome.ctb")
  (vla-put-StyleSheet layout "monochrome.ctb")
  (setq BP:stage "mengaktifkan lineweight")
  (vla-put-PlotWithLineweights layout :vlax-true)
  (setq BP:stage "menonaktifkan Scale lineweights")
  (vla-put-ScaleLineweights layout :vlax-false)
  (setq BP:stage "mengatur Shade plot ke Legacy wireframe")
  (BP:set-legacy-wireframe layout))

(defun BP:write-dsd (filename drawing layouts pdf sheet-layout-name / file sheet-number layout name outdir)
  (setq outdir (vl-filename-directory pdf)
        file (open filename "w"))
  (foreach line
    (list "[DWF6Version]" "Ver=1"
          "[DWF6MinorVersion]" "MinorVer=1")
    (write-line line file))
  (setq sheet-number 1)
  (foreach layout layouts
    (setq name (vla-get-Name layout))
    (foreach line
      (list (strcat "[DWF6Sheet:" (vl-filename-base drawing) "-" name "]")
            (strcat "DWG=" drawing)
            (strcat "Layout=" sheet-layout-name)
            (strcat "Setup=" name "|" drawing)
            (strcat "OriginalSheetPath=" drawing)
            "Has Plot Port=0"
            "Has3DDWF=0")
      (write-line line file))
    (setq sheet-number (1+ sheet-number)))
  (foreach line
    (list "[Target]"
          "Type=6"
          (strcat "DWF=" pdf)
          (strcat "OUT=" outdir)
          "PWD="
          "[AutoCAD Block Data]"
          "IncludeBlockInfo=0"
          "BlockTmplFilePath="
          "[SheetSet Properties]"
          "IsSheetSet=FALSE"
          "IsHomogeneous=FALSE"
          "SheetSet Name="
          "NoOfCopies=1"
          "PlotStampOn=FALSE"
          "ViewFile=FALSE"
          "JobID=0"
          "SelectionSetName="
          "AcadProfile="
          "CategoryName="
          (strcat "LogFilePath=" (vl-filename-directory filename)
                  "\\" (vl-filename-base filename) ".log")
          "IncludeLayer=FALSE"
          "LineMerge=FALSE"
          "CurrentPrecision="
          "PromptForDwfName=FALSE"
          "PwdProtectPublishedDWF=FALSE"
          "PromptForPwd=FALSE"
          "RepublishingMarkups=FALSE"
          "PublishSheetSetMetadata=FALSE"
          "PublishSheetMetadata=FALSE"
          "3DDWFOptions=0 0"
          "[PdfOptions]"
          "IncludeHyperlinks=TRUE"
          "CreateBookmarks=TRUE"
          "CaptureFontsInDrawing=TRUE"
          "ConvertTextToGeometry=FALSE")
    (write-line line file))
  (close file)
  filename)

(defun BP:delete-layouts (document layouts old-layout / layout)
  (if old-layout
    (vl-catch-all-apply 'vla-put-ActiveLayout (list document old-layout)))
  (foreach layout layouts
    (vl-catch-all-apply 'vla-Delete (list layout))))

(defun BP:restore-vars (saved-vars / pair)
  (foreach pair saved-vars
    (if (and (car pair) (cdr pair))
      (setvar (car pair) (cdr pair)))))

(defun c:BATCHPLOTPDF
       (/ *error* acad document old-layout saved-vars selection frames
          plotter probe probe-name media media-list styles orientation output dsd
          layouts counter layout-name layout model-backup model-backup-name publish-saved
          model-layout source-layout source-layout-name source-model-type initial-tilemode)
  (vl-load-com)
  (setq acad (vlax-get-acad-object)
        document (vla-get-ActiveDocument acad)
        old-layout (vla-get-ActiveLayout document)
        model-layout (vla-get-Layout (vla-get-ModelSpace document))
        initial-tilemode (getvar "TILEMODE")
        source-layout (if (= initial-tilemode 1) model-layout old-layout)
        source-layout-name (if (= initial-tilemode 1)
                             "Model"
                             (vla-get-Name old-layout))
        source-model-type (= initial-tilemode 1)
        saved-vars (list (cons "CMDECHO" (getvar "CMDECHO"))
                         (cons "FILEDIA" (getvar "FILEDIA"))
                         (cons "BACKGROUNDPLOT" (getvar "BACKGROUNDPLOT"))
                         (cons "PLOTTRANSPARENCYOVERRIDE"
                               (getvar "PLOTTRANSPARENCYOVERRIDE")))
        layouts nil
        dsd nil)
  (defun *error* (message)
    (if BP:stage
      (princ (strcat "\nTahap yang gagal: " BP:stage)))
    (if (and message
             (not (wcmatch (strcase message) "*CANCEL*,*EXIT*,*QUIT*")))
      (princ (strcat "\nBatch plot dihentikan: " message)))
    (if probe
      (vl-catch-all-apply 'vla-Delete (list probe)))
    ;; Restore the user's original Model page setup if a failure occurred
    ;; while the Model layout was being used as the configuration template.
    (if model-backup
      (progn
        (vl-catch-all-apply 'vla-CopyFrom (list source-layout model-backup))
        (vl-catch-all-apply 'vla-Delete (list model-backup))))
    (if (= initial-tilemode 0) (setvar "TILEMODE" 0))
    (BP:delete-layouts document layouts old-layout)
    (if publish-saved
      (vl-catch-all-apply 'vla-Save (list document)))
    (if (and dsd (findfile dsd)) (vl-file-delete dsd))
    (BP:restore-vars saved-vars)
    (vl-catch-all-apply 'vla-EndUndoMark (list document))
    (princ))
  (cond
    ((= (getvar "DWGTITLED") 0)
      (alert "Simpan drawing terlebih dahulu. PUBLISH membutuhkan file DWG yang sudah tersimpan."))
    ((= (getvar "PSTYLEMODE") 0)
      (alert "Drawing ini memakai STB. Skrip ini membutuhkan CTB, khususnya monochrome.ctb."))
    (T
      (vla-StartUndoMark document)
      (setq BP:stage nil)
      (princ "\nPilih satu border/frame TERLUAR pada ruang aktif untuk setiap lembar, lalu Enter: ")
      (setq selection (ssget))
      (if (null selection)
        (princ "\nTidak ada frame yang dipilih.")
        (progn
          (setq frames (BP:collect-frames selection))
          (if (null frames)
            (alert "Tidak ada frame dengan bounding box yang valid.")
            (progn
              (setq plotter (BP:default-pdf-plotter source-layout))
              (if (null plotter)
                (alert "Plotter DWG To PDF.pc3 tidak ditemukan. Pastikan PDF PC3 bawaan AutoCAD tersedia.")
                (progn
                  ;; A temporary plot configuration obtains the exact media and
                  ;; plot-style lists of the chosen PC3 without touching an existing layout.
                  (setq probe-name (BP:unique-name (vla-get-PlotConfigurations document)
                                                   "BP_PLOT_PROBE_")
                        ;; Read media using a Model-space configuration.
                        probe (vla-Add (vla-get-PlotConfigurations document)
                                       probe-name
                                       (if source-model-type :vlax-true :vlax-false)))
                  (vla-CopyFrom probe source-layout)
                  (vla-put-ConfigName probe plotter)
                  (vla-RefreshPlotDeviceInfo probe)
                  (setq media-list (BP:variant-list (vla-GetCanonicalMediaNames probe))
                        styles (BP:variant-list (vla-GetPlotStyleTableNames probe)))
                  (vla-Delete probe)
                  (setq probe nil)
                  (cond
                    ((null media-list)
                      (alert "Plotter ini tidak menyediakan daftar ukuran kertas."))
                    ((not (BP:find-ci styles "monochrome.ctb"))
                      (alert "monochrome.ctb tidak tersedia untuk plotter/drawing ini."))
                    (T
                      (setq media (BP:choose-media media-list))
                      (setq orientation (BP:choose-orientation))
                      (princ
                        (strcat "\nUkuran kertas efektif: "
                                (BP:effective-paper-text media orientation)
                                " (" orientation ")"))
                      (setq output
                        (getfiled "Simpan PDF gabungan"
                                  (strcat (getvar "DWGPREFIX")
                                          (vl-filename-base (getvar "DWGNAME"))
                                          "_batch.pdf")
                                  "pdf" 1))
                      (if (null output)
                        (princ "\nPemilihan file PDF dibatalkan.")
                        (progn
                          (if (/= (strcase (vl-filename-extension output)) ".PDF")
                            (setq output (strcat output ".pdf")))
                          (if (findfile output)
                            (progn
                              (initget "Yes No")
                              (if (/= (getkword
                                       "\nPDF sudah ada. Timpa? [Yes/No] <No>: ") "Yes")
                                (setq output nil))))
                          (if output
                            (progn
                              ;; User requested automatic DWG saving without confirmation.
                              (princ "\nDWG akan disimpan otomatis untuk Publish.")
                              ;; Back up Model, configure it, and copy settings
                              ;; into each named page setup for publishing.
                              (setq BP:stage "mencadangkan page setup Model")
                              (setq model-backup-name
                                (BP:unique-name (vla-get-PlotConfigurations document)
                                                "BP_MODEL_BACKUP_")
                                    model-backup
                                (vla-Add (vla-get-PlotConfigurations document)
                                         model-backup-name
                                         (if source-model-type :vlax-true :vlax-false)))
                              (vla-CopyFrom model-backup source-layout)
                              (setq counter 1)
                              (foreach frame frames
                                ;; Configure the actual Model layout first; this is
                                ;; the same target used by AutoCAD's Plot dialog.
                                (BP:configure-layout source-layout plotter media orientation frame)
                                (setq BP:stage "membuat temporary Model-space page setup")
                                (setq layout-name
                                  (BP:unique-name (vla-get-PlotConfigurations document)
                                                  (strcat "BP_PDF_SETUP_" (itoa counter) "_"))
                                      layout (vla-Add (vla-get-PlotConfigurations document)
                                                      layout-name
                                                      (if source-model-type :vlax-true :vlax-false)))
                                (setq layouts (append layouts (list layout))
                                      counter (1+ counter))
                                (setq BP:stage "menyalin setup Model ke sheet sementara")
                                (vla-CopyFrom layout source-layout)
                                ;; CopyFrom may replace the plot-settings name.
                                ;; DSD must refer to the unique name assigned here.
                                (vla-put-Name layout layout-name)
                                )
                              ;; Restore the user's original Model plot settings
                              ;; before publishing the copied named page setups.
                              (setq BP:stage "memulihkan page setup Model asli")
                              (vla-CopyFrom source-layout model-backup)
                              (vla-Delete model-backup)
                              (setq model-backup nil)
                              (vla-Regen document 1)
                              (setq BP:stage "menyimpan page setup agar dapat dibaca Publish")
                              (vla-Save document)
                              (setq publish-saved T)
                              (setq dsd
                                (strcat (vl-filename-directory output) "\\"
                                        (vl-filename-base output) "_BPPLOT_TEMP.dsd"))
                              (BP:write-dsd dsd
                                            (strcat (getvar "DWGPREFIX") (getvar "DWGNAME"))
                                            layouts output source-layout-name)
                              (setvar "CMDECHO" 0)
                              (setvar "FILEDIA" 0)
                              (setvar "BACKGROUNDPLOT" 0)
                              ;; Match the unchecked Plot transparency option.
                              ;; Restore the user's original value after publishing.
                              (setvar "PLOTTRANSPARENCYOVERRIDE" 0)
                              (princ (strcat "\nMenerbitkan " (itoa (length layouts))
                                             " sheet ke: " output))
                              ;; Each DSD entry plots Model with its matching named
                              ;; page setup. Type=6 creates one multi-sheet PDF.
                              (setq BP:stage "menjalankan Publish multi-sheet PDF")
                              (vl-cmdf "_.-PUBLISH" dsd)
                              (if (= initial-tilemode 0) (setvar "TILEMODE" 0))
                              (BP:delete-layouts document layouts old-layout)
                              (setq layouts nil)
                              (setq BP:stage "menyimpan DWG setelah membersihkan page setup sementara")
                              (vla-Save document)
                              (setq publish-saved nil)
                              (if (findfile dsd) (vl-file-delete dsd))
                              (setq dsd nil)
                              (setq BP:stage nil)
                              (BP:restore-vars saved-vars)
                              (if (findfile output)
                                (alert (strcat "Selesai. PDF multi-sheet dibuat:\n" output))
                                (alert "PUBLISH selesai, tetapi file PDF belum ditemukan. Periksa Publish log/Command Line."))))))))))))))))
  (vl-catch-all-apply 'vla-EndUndoMark (list document))
  (princ))

;;; Shortcut command.  Reload the LSP after editing to make it available.
(defun c:BPP ()
  (c:BATCHPLOTPDF))

(princ "\nBatchPlotModelToPDF v2.9 loaded. Uses the active Model or Layout space; automatic DWG save. Default: DWG To PDF.pc3, ISO full bleed A4, Portrait. Commands: BATCHPLOTPDF atau BPP")
(princ)
