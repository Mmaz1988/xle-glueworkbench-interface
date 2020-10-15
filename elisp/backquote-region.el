;;    Copyright (C) 2019–2020 Mary Dalrymple
;;
;;    This file is part of XLE+Glue (https://github.com/Mmaz1988/xle-glueworkbench-interface).
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun backquote-region ()
  "Characters that must be quoted in XLE are prefixed with a backquote."
  (interactive)
  (let ((xle-quoted-chars '(?\< ?\> ?\( ?\) ?\{ ?\} ?\[ ?\] ?* ?+
  ?- ?& ?| ?\\ ?/ ?~ ?^ ?$ ?` ?# ?' ?, ?: ?\? ?= ?! ?% ?@
  ?. ?\; ?_ )))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(cond
	 ((member (following-char) xle-quoted-chars)
	  (insert "`")
	  (forward-char 1))
	 (t
	  (forward-char 1)))))))


(defun backquote-space-region ()
  "Characters that must be quoted in XLE are prefixed with a backquote and surrounded by whitespace.  This is necessary (at least for some characters in some positions) if the resulting sequence of characters is used in a call to the XLE CONCAT template."
  (interactive)
  (let ((xle-quoted-chars '(?\< ?\> ?\( ?\) ?\{ ?\} ?\[ ?\] ?* ?+
  ?- ?& ?| ?\\ ?/ ?~ ?^ ?$ ?` ?# ?' ?, ?: ?\? ?= ?! ?% ?@
  ?. ?\; ?_ )))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(cond
	 ((member (following-char) xle-quoted-chars)
	  (insert " `")
	  (forward-char 1)
	  (insert " "))
	 (t
	  (forward-char 1)))))))

 
