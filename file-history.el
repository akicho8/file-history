;;; file-history.el --- 最近アクセスしたファイルを素早く開く

;; Copyright (C) 2006-2011  Free Software Foundation, Inc.

;; Author: akicho8 <akicho8@gmail.com>
;; Keywords: file, bookmark

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 使い方:
;;
;;   (require 'file-history)
;;   (global-set-key (kbd "C-t") 'file-history)
;;

;;; Code:

(defvar file-history-buffer-name "*ファイル履歴情報*"
  "履歴バッファ名")

(defvar file-history-display-rows 50
  "表示行数")

(defvar file-history-content-limit 80
  "ファイル内容を表示する文字数")

(defvar file-history-content-display t
  "ファイル内容を横に表示するか?")

(defvar file-history-exclude-regexp "\\.\\(elc\\|jpg\\|gif\\|bmp\\|png\\)$"
  "正規表現にマッチするファイルはリストに表示させない")

;; 以下は内部用
(defvar file-history-before-buffer nil
  "file-historyを起動する前のバッファ")
(defvar file-history-buffers nil
  "ファイルリスト")
(defvar file-history-current-dir nil
  "前回いたディレクトリ")

(defvar file-history-mode-hook nil
  "フック")

(defun file-history-get-buffer-list ()
  "ファイルを開いているバッファのリストを取得する"
  (let (ary (i 0))
    (catch 'break
      (dolist (buf (buffer-list))
        (if (>= i file-history-display-rows)
            (throw 'break nil))
        (when (file-history-buffer-p buf)
          (add-to-list 'ary buf)
          (setq i (1+ i)))))
    (reverse ary)))

(defun file-history-buffer-p (buffer)
  "リストに出すバッファか?"
  (let ((case-fold-search t))
    (and (buffer-file-name buffer)
         (not (string-match file-history-exclude-regexp (buffer-file-name buffer))))))

(defun file-history ()
  "起動コマンド"
  (interactive)
  (if (eq major-mode 'file-history-mode)
      (progn
        (file-history-back)
        )
    (setq file-history-current-dir default-directory)
    (setq file-history-before-buffer (current-buffer)) ;起動元のバッファを覚えておく
    (switch-to-buffer (get-buffer-create file-history-buffer-name))
    (cd file-history-current-dir)
    (setq file-history-buffers (file-history-get-buffer-list))
    (file-history-draw)
    (file-history-index-set 0)
    (file-history-mode)
    )
  )

(defun file-history-direct-back ()
  "前のバッファに戻るときに使う。
特別頻繁に実行するので global-set-key に直接登録できる関数を用意した。
以下のように設定しておくと便利
;; (global-set-key [?\\C-,] 'file-history-direct-back)"
  (interactive)
  (setq file-history-before-buffer (current-buffer))
  ;; 取得バッファ数はひとつ前を知りたいだけなので2つあればよい。
  (let ((file-history-display-rows 2))
    (setq file-history-buffers (file-history-get-buffer-list))
    (file-history-back)))

(defun file-history-draw ()
  (interactive)
  (setq buffer-read-only nil)
  (goto-char (point-min))
  (erase-buffer)
  (insert "No Filename                                 Path\n")
  (insert "== ======================================== ===========================================================================\n")
  ;; 検索
  (let ((i 1) dir infobuf)
    (catch 'break
      (dolist (buf file-history-buffers)
        (if (> i file-history-display-rows)
            (throw 'break nil))
        ;; 表示
        ;; (insert (format (concat "%2d %-30s %-" (number-to-string file-history-content-limit) "s\n")
        (insert (format (concat "%2d %-40s %s\n")
                        i
                        (file-name-nondirectory (buffer-file-name buf))
                        (file-name-directory (buffer-file-name buf))
                        ))
        (setq i (+ i 1)))))
  (insert "== ======================================== ===========================================================================\n")
  (setq buffer-read-only t))

(defun file-history-get-current-buffer ()
  "現在の行のバッファを取得"
  (let ((line
         (save-excursion
           (move-to-column 0)
           (count-lines (point-min) (point))))
        buffer)
    ;; (message "0から始まる現在の行=%d" line)
    (setq buffer (nth (file-history-index) file-history-buffers))
    ;;     (unless buffer
    ;;       (error "バッファが見付かりません。")
    ;;       )
    buffer
    )
  )

(defun file-history-open-current ()
  "現在の行のバッファに移動する"
  (interactive)
  (file-history-delete-other-windows)
  (switch-to-buffer (file-history-get-current-buffer)))

(defun file-history-delete ()
  "現在の行のバッファを削除する"
  (interactive)
  (kill-buffer (file-history-get-current-buffer))
  (file-history-update))

(defun file-history-update ()
  "更新"
  (interactive)
  (setq file-history-buffers (file-history-get-buffer-list))
  (let ((save-index (file-history-index)))
    (setq file-history-buffers (file-history-get-buffer-list))
    (file-history-draw)
    (file-history-index-set save-index))
  (message "update"))

(defun file-history-eshell ()
  "eshell起動"
  (interactive)
  (let ((buffer (file-history-get-current-buffer)))
    (eshell)
    (goto-char (point-max))
    (eshell-bol)
    (unless (eobp)
      (kill-line))
    (insert (concat "pushd " (file-name-directory (buffer-file-name buffer))))
    (eshell-send-input)))

(defun file-history-dired ()
  "dired起動"
  (interactive)
  (let ((buffer (file-history-get-current-buffer)))
    (dired-at-point (file-name-directory (buffer-file-name buffer)))))

(defun file-history-quit ()
  "画面を閉じる"
  (interactive)
  (file-history-delete-other-windows)
  (kill-buffer file-history-buffer-name))

(defun file-history-back ()
  "前のバッファに戻る
基本的に2番目のファイルにスイッチする。
でもそれまでに開いていたバッファが管理外の場合は1番目のファイルに切替える。"
  (interactive)
  (file-history-delete-other-windows)
  (switch-to-buffer
   (nth
    (if (file-history-buffer-p file-history-before-buffer) 1 0)
    file-history-buffers)))

(defun file-history-index ()
  "0から始まるバッファ番号"
  (interactive)
  (save-excursion
    (move-to-column 0)
    (if (re-search-forward "[0-9]+" nil t)
        (1- (string-to-int (buffer-substring (match-beginning 0) (match-end 0))))
      ;; (error "カーソルが変なところにあります。")
      (1- (length file-history-buffers))
      ))
  )

(defun file-history-forward-line (arg)
  "カーソルの移動。範囲外だと移動しない。"
  (interactive "p")
  (let ((index (+ (file-history-index) arg)))
    (if (file-history-index-p index)
        (file-history-index-set index))))

(defun file-history-index-p (index)
  "指定したインデックスは範囲内か?"
  (and (<= 0 index)
       (< index (length file-history-buffers))))

(defun file-history-index-set (index)
  "指定したインデックスにセットする。範囲補正あり"
  (interactive "p")
  (setq index (max index 0))
  (setq index (min index (1- (length file-history-buffers))))
  (goto-line (+ 3 index))
  (move-to-column 3))

(defun file-history-next-line ()
  "カーソルを下に移動"
  (interactive)
  (file-history-forward-line 1))

(defun file-history-previous-line ()
  "カーソルを上に移動"
  (interactive)
  (file-history-forward-line -1))

(defun file-history-delete-other-windows ())

(defun file-history-mode ()
  "\\{file-history-mode-map}"
  (interactive)
  (setq major-mode 'file-history-mode)
  (setq mode-name "ファイル選択")
  (setq file-history-mode-map (make-sparse-keymap))
  (define-key file-history-mode-map "p" 'file-history-previous-line)
  (define-key file-history-mode-map "n" 'file-history-next-line)
  (define-key file-history-mode-map "\C-m" 'file-history-open-current)
  (define-key file-history-mode-map "\C-t" 'file-history-back)
  (define-key file-history-mode-map "q" 'file-history-quit)
  (define-key file-history-mode-map "d" 'file-history-delete)
  (define-key file-history-mode-map "g" 'file-history-update)
  (define-key file-history-mode-map "e" 'file-history-eshell)
  (define-key file-history-mode-map "h" 'file-history-eshell)
  (define-key file-history-mode-map "." 'file-history-dired)
  (use-local-map file-history-mode-map)
  (run-hooks 'file-history-mode-hook)
  )

;; (global-set-key "\C-t" 'file-history)

(provide 'file-history)
;;; file-history.el ends here
