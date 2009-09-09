<html>
  <head>
    <title>Registration</title>
  </head>
  <body>
    
<?spark
(define (show-form)
  (let ((out (open-output-string)))
    (fprintf out "<form action=\"./regis.sml\">")
    (fprintf out "First Name: <input type=\"text\" name=\"fname\" />")
    (get-output-string out)))

(define (handle-form)
  "<b>First Name=$fname</b>")

(if (= (string-length "$fname") 0)
    (show-form)
    (handle-form))
?>

  </body>
</html>
