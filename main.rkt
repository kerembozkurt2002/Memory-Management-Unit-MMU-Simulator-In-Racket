;kerem bozkurt
;2020400177
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))



(define (binary_to_decimal binary)
  (define (helper binary index)
    (if (null? binary)
        0
        (+ (if (char=? (car binary) #\1)
               (expt 2 index)
               0)
           (helper (cdr binary) (- index 1)))))
  (helper (string->list binary) (- (string-length binary) 1))
  )




(define (relocator args limit base)
  (define (helper addr)
    (let ((number (binary_to_decimal addr)))
      (if (> number limit)
          -1
          (+ number base))))
  (map helper args))




(define (log2 n)
  (if (= n 1)
      0
      (+ 1 (log2 (/ n 2)))))

(define (divide_address_space num page_size)
  (let* ((backbits (+ 10 (log2 page_size))) 
         (adress_length (string-length num))
         (page_length (- adress_length backbits))
         (page_number (substring num 0 page_length))
         (page_offset (substring num page_length adress_length)))
    (list page_number page_offset)))




(define (page args page_table page_size)
  (define (translate addr)
    (let* ((divided (divide_address_space addr page_size))
           (page-number (car divided))
           (page_back (cadr divided))
           (frame_number (list-ref page_table (binary_to_decimal page-number))))
      (string-append frame_number page_back)))
  (map translate args))



(define (find_sin value num)
  (define radian_value (degrees->radians value) )
  (define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))

  (define (term n)
    (let ((exp (expt radian_value (+ (* 2 n) 1)))
          (fact (factorial (+ (* 2 n) 1))))
      (/ exp fact)))

  (define (taylor-series n)
    (define (helper i sum)
      (if (= i 0)
          sum
          (helper (- i 1) (+ sum (* (if (even? i) 1 -1) (term (- i 1)))))))
    (helper n 0))

  (let ((result (taylor-series num)))
    (if (> radian_value pi)
        result
        (- result))))



(define (sum_to_ten number)
  (define decimal_string (number->string number))
  (define float_part (second (string-split decimal_string ".")))
  
  (define (sum_rec lst i sum)
    (if (or (null? lst) (>= i 10))
        sum
        (sum_rec (cdr lst) (+ i 1) (+ sum (string->number (string (car lst)))))))

  (sum_rec (string->list float_part) 0 0))


(define (find_decimal_point str)
  (define (helper index)
    (if (>= index (string-length str))
        -1 
        (if (char=? (string-ref str index) #\.)
            index
            (helper (+ index 1)))))
  (helper 0))
(define (myhash_decimal arg size)
  (define decimal_string (number->string arg))
  (define decimal_point (find_decimal_point decimal_string))
  
  (let ((start (+ decimal_point 1))
            (end (+ decimal_point 11))) 
        (define sum_string (substring decimal_string start end))
        (define sum (string->number sum_string))
        (modulo sum size)))





(define (myhash arg size)
  (define decimal_value (binary_to_decimal arg))
  (define number_for_sin (+ (modulo decimal_value 5 ) 1))
  (define after_sin(find_sin decimal_value number_for_sin ))
  (define after_sum_to_ten (sum_to_ten after_sin ))
  (define result (modulo after_sum_to_ten size ))
  (+ result 0)
  )


(define (hashed_page arg table_size page_table page_size)
  (let* ((divided (divide_address_space arg page_size))
         (page-number (car divided))
         (page-offset (cadr divided))
         (hash-value (myhash page-number table_size))
         (page-list (list-ref page_table hash-value)))
    (define (find-frame page-number page-list)
          (if (string=? (car (car page-list)) page-number)
              (cadr (car page-list))
              (find-frame page-number (cdr page-list))))
    (let ((frame-number (find-frame page-number page-list)))
      (string-append frame-number page-offset))))



(define (split_addresses args size)
  (define (helper str)
    (if (string=? str "")
        '()
        (cons (list (substring str 0 size))
              (helper (substring str size (string-length str))))))
  (helper args))


(define (map_addresses args table_size page_table page_size address_space_size)
  (let* ((splitted (apply append (split_addresses args address_space_size)))
         (hashed (map (lambda (addr) (hashed_page addr table_size page_table page_size)) splitted)))
    hashed))







