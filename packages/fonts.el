(defun ox/create-fontset (name)
  "create an empty fontset named NAME"
  (create-fontset-from-fontset-spec
   (font-xlfd-name
    (font-spec :registry name))))

(defun ox/set-default-font (&rest fonts)
  "set the default/main emacs font with fallbacks"
  (unless (null fonts)
    (if (find-font (font-spec :name (car fonts)))
	(set-face-attribute 'default nil
			    :font (car fonts)
			    :height 90)
      (apply #'ox/set-default-font (cdr fonts)))))

(defun ox/set-font (fontset script &rest fonts)
  "set a font for a specific SCRIPT in FONTSET with fallbacks.
   if non of the fonts are available look for an appropriate font on the system"
  (set-fontset-font fontset script (car fonts))
  (dolist (font (cdr fonts))
    (set-fontset-font fontset script font nil 'append))
  (set-fontset-font fontset script (font-spec :script script) nil 'append))

;;/------------------------------------\;;
;;|           MONOSPACE FONT           |;;
;;\------------------------------------/;;
;; monospace as the main emacs font
;; Latin
(ox/set-default-font "ioZevka Code"
		     "ioZevka Mono"
		     "JetBrains Mono"
		     "Noto Sans Mono"
		     "Courier New"
		     "monospace")

;; Arabic/Farsi: چطوری, السّلام عليكم
(ox/set-font "fontset-default"
	     'arabic
	     (font-spec :name "Vazir Code Extra Height WOL" :size 13)
	     (font-spec :name "Vazir Code Extra Height" :size 13)
	     (font-spec :name "Vazir Code WOL" :size 13)
	     (font-spec :name "Vazir Code" :size 13))

;; Hebrew: חפש סתם אהב
(ox/set-font "fontset-default"
	     'hebrew
	     "FreeMono"
	     "Courier New")

;; CJK: 你好, 早晨, こんにちは, 안녕하세요
(ox/set-font "fontset-default"
	     'han
	     "Sarasa Mono SC"
	     "Source Han Mono SC"
	     "Noto Sans CJK SC Regular"
	     "MS Gothic")
(ox/set-font "fontset-default"
	     'kana
	     "Sarasa Mono J"
	     "Noto Sans CJK JP Regular"
	     "MS Gothic")
(ox/set-font "fontset-default"
	     'hangul
	     "Sarasa Mono K"
	     "Source Han Mono K"
	     "Noto Sans CJK KR Regular"
	     "Malgun Gothic")

;;/------------------------------------\;;
;;|          SANS-SERIF FONT           |;;
;;\------------------------------------/;;
;; sans serif for variable-pitch and mixed-pitch
(ox/create-fontset "fontset-sans")

;; Latin
(ox/set-font "fontset-sans"
	     'latin
	     "ioZevka Quasi"
	     "Noto Sans"
	     "Arimo"
	     "Liberation Sans"
	     "FreeSans"
	     "DejaVu Sans"
	     "Segoe UI")
	     

;; Arabic/Farsi: چطوری, السّلام عليكم
(ox/set-font "fontset-sans"
	     'arabic
	     "Vazirmatn UI NL"
	     "Vazirmatn UI"
	     "Vazirmatn NL"
	     "Vazirmatn"
	     "Noto Sans Arabic"
	     "Noto Naskh Arabic"
	     "IRANSansWeb")

;; Hebrew:  חפש סתם אהב
(ox/set-font "fontset-sans"
	     'hebrew
	     "Noto Sans Hebrew"
	     "Courier New")

(set-face-attribute 'variable-pitch nil
		    :font "fontset-sans"
		    :fontset "fontset-sans")

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
