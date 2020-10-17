; length
(length "one two three")
(length "")
(length)

; char
(char "one two three" 0)
(char "one two three" 12)
(char "one two three" -1)
(char "")
(char)

; streq
(string= "one" "t")
(string= nil "one")
(string= "one" nil)
(string= "one" "one")

; string-append
(concatenate 'string "one" " and")
(concatenate 'string "" "and")
(concatenate 'string "one" "")
(concatenate 'string "" "")
(concatenate 'string nil "")
(concatenate 'string "" nil)
(concatenate 'string "")
(concatenate 'string nil)
(concatenate 'string)