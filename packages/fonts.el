;;/------------------------------------\;;
;;|          SANS-SERIF FONT           |;;
;;\------------------------------------/;;
;; sans serif for variable-pitch and mixed-pitch	     

;;/------------------------------------\;;
;;|             SERIF FONT             |;;
;;\------------------------------------/;;
(ox/create-fontset "fontset-serif")


;;/------------------------------------\;;
;;|              EMOJI FONT            |;;
;;\------------------------------------/;;
(ox/set-font "fontset-default"
	     'emoji
	     "Noto Color Emoji"
	     "Noto Emoji")

;; emacs on windows does not support colored emojis
(when (eq system-type 'windows-nt)
  (ox/set-font "fontset-default"
	       'emoji
	       "Noto Emoji"
	       "Segoe UI Emoji"))

;;/------------------------------------\;;
;;|            FALLBACK FONT           |;;
;;\------------------------------------/;;
;; to support most characters across all scripts
;; this also speeds up emacs on windows significantly when it comes to rendering
;; characters in these scripts

;; South East Asia: ជំរាបសួរ, ສະບາຍດີ, မင်္ဂလာပါ, สวัสดีครับ
(ox/set-font "fontset-default"
	     'khmer
	     "Noto Sans Khmer"
	     "Leelawadee UI")
(ox/set-font "fontset-default"
	     'lao
	     "Noto Sans Lao"
	     "Leelawadee UI")
(ox/set-font "fontset-default"
	     'burmese
	     "Noto Sans Myanmar"
	     "Myanmar Text")
(ox/set-font "fontset-default"
	     'thai
	     "Noto Sans Thai"
	     "Leelawadee UI")

;; Africa: ሠላም
(ox/set-font "fontset-default"
	     'ethiopic
	     "Ebrima")

;; South Asia: નમસ્તે, नमस्ते, ನಮಸ್ಕಾರ, നമസ്കാരം, ଶୁଣିବେ,
;;              ආයුබෝවන්, வணக்கம், నమస్కారం, བཀྲ་ཤིས་བདེ་ལེགས༎
(ox/set-font "fontset-default"
	     'gujarati
	     "Noto Sans Gujarati"
	     "Nirmala UI")
(ox/set-font "fontset-default"
	     'devanagari
	     "Noto Sans Devanagari"
	     "Nirmala UI")
(ox/set-font "fontset-default"
	     'kannada
	     "Noto Sans Kannada"
	     "Nirmala UI")
(ox/set-font "fontset-default"
	     'malayalam
	     "Noto Sans Malayalam"
	     "Nirmala UI")
(ox/set-font "fontset-default"
	     'oriya
	     "Noto Sans Oriya"
	     "Nirmala UI")
(ox/set-font "fontset-default"
	     'sinhala
	     "Noto Sans Sinhala"
	     "Nirmala UI")
(ox/set-font "fontset-default"
	     'tamil
	     "Noto Sans Tamil"
	     "Nirmala UI")
(ox/set-font "fontset-default"
	     'telugu
	     "Noto Sans Telugu"
	     "Nirmala UI")
(ox/set-font "fontset-default"
	     'tibetan
	     "Noto Sans Tibetan"
	     "Microsoft Himalaya")

;;;;;; SANS-SERIF ;;;;;;
;;(new-fontset "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-sans" nil)
;;(ox/set-default-font "fontset-sans"
;;		     "Arial"
;;		     "Noto Sans"
;;		     "sans-serif")

;;;;;; EMOJI FONT ;;;;;;
;;(if (eq system-type 'windows-nt)
;;    ;; colored emojis are not supported on windows
;;    (ox/set-font t 'emoji "Noto Emoji" "emoji")
;;  (ox/set-font t 'emoji "Noto Color Emoji" "Noto Emoji" "emoji"))
