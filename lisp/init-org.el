(require 'org)

(defvar org-agenda-dir ""
  "gtd org files location")
(setq
   org-agenda-dir "~/.emacs.d/org-notes")

;; define the refile targets
(setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
(setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
(setq org-agenda-files (list org-agenda-dir))

(setq org-src-fontify-natively t)

;; the %i would copy the selected text into the template
;;http://www.howardism.org/Technical/Emacs/journaling-org.html
;;add multi-file journal
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
	 "* TODO [#B] %?\n  %i\n %U"
	 :empty-lines 1)
	("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
	 "* %?\n  %i\n %U"
	 :empty-lines 1)
	("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
	 "* TODO [#B] %?\n  %i\n %U"
	 :empty-lines 1)
	("s" "Code Snippet" entry
	 (file org-agenda-file-code-snippet)
	 "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
	("w" "work" entry (file+headline org-agenda-file-gtd "Work")
	 "* TODO [#A] %?\n  %i\n %U"
	 :empty-lines 1)
	("x" "Web Collections" entry
	 (file+headline org-agenda-file-note "Web")
	 "* %U %:annotation\n\n%:initial\n\n%?")
	("p" "Protocol" entry (file+headline org-agenda-file-note "Inbox")
	 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("L" "Protocol Link" entry (file+headline org-agenda-file-note "Inbox")
	 "* %? [[%:link][%:description]] \nCaptured On: %U")
	("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
	 "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
	 :empty-lines 1)
	("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
	 "* TODO [#C] %?\n  %i\n %a \n %U"
	 :empty-lines 1)
	("j" "Journal Entry"
	 entry (file+datetree org-agenda-file-journal)
	 "* %?"
	 :empty-lines 1)))

(setq org-agenda-custom-commands
            '(
              ("w" . "task arrangement")
              ("wa" "important and urgent" tags-todo "+PRIORITY=\"A\"")
              ("wb" "important but not urgent" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "not important and not urgent" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "project schedule")
              ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"work\"")
              ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
              ("W" "Weekly Review"
               ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))))

"show notifications when pomodoro end"
(add-hook 'org-pomodoro-finished-hook '(lambda () (sound-wav-play (expand-file-name "~/.emacs.d/game_win.wav"))))
(add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (sound-wav-play (expand-file-name "~/.emacs.d/game_win.wav"))))
(add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (sound-wav-play (expand-file-name "~/.emacs.d/game_win.wav"))))

(defun lisatiy/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'lisatiy/org-summary-todo)

(provide 'init-org)
  
