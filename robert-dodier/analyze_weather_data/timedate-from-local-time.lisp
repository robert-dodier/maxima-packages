(unless (find-package "LOCAL-TIME")
  (ql:quickload "local-time"))

(defmfun $parse_timedate (s)
  (let ((timestamp (local-time:parse-timestring s)))
    (local-time:timestamp-to-universal timestamp)))

(defmfun $timedate (u)
  (let ((timestamp (local-time:universal-to-timestamp u)))
    (local-time:format-timestring nil timestamp)))

(defmfun $decode_time (u)
  (let ((timestamp (local-time:universal-to-timestamp u)))
    (cons '(mlist) (multiple-value-list (local-time:decode-timestamp timestamp)))))


