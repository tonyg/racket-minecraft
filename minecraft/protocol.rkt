#lang racket/base

;; https://github.com/brooksc/mcpipy/blob/master/mcpi/mcpi_protocol_spec.txt
;; http://www.wiki.vg/Minecraft_Pi_Protocol

(require racket/match)
(require racket/tcp)
(require racket/string)
(require racket/format)

(require "type-id.rkt")
(require "vec.rkt")

(provide (struct-out minecraft-connection)

	 current-minecraft-connection

	 minecraft-connect
	 minecraft-disconnect

	 minecraft-send-line
	 minecraft-read-line
	 minecraft-send-command
	 minecraft-read-numbers
	 minecraft-read-number
	 minecraft-read-entities

	 minecraft-get-block
	 minecraft-get-block/data
	 minecraft-get-blocks/data
	 minecraft-set-block
	 minecraft-set-blocks
	 minecraft-get-height
	 minecraft-get-player-ids
	 minecraft-world-setting
	 minecraft-checkpoint-save
	 minecraft-checkpoint-restore
	 minecraft-post
	 minecraft-get-tile
	 minecraft-set-tile
	 minecraft-get-pos
	 minecraft-set-pos
	 minecraft-player-setting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct minecraft-connection (in out) #:transparent)

(define current-minecraft-connection (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c)
  (when (not (current-minecraft-connection))
    (minecraft-connect))
  (current-minecraft-connection))

(define (minecraft-connect [server "localhost"] [port 4711])
  (define-values (in out) (tcp-connect server port))
  (current-minecraft-connection (minecraft-connection in out)))

(define (minecraft-disconnect)
  (match-define (minecraft-connection in out) (c))
  (tcp-abandon-port in)
  (tcp-abandon-port out)
  (current-minecraft-connection #f))

(define (minecraft-send-line line [flush #t])
  (define out (minecraft-connection-out (c)))
  (display line out)
  (newline out)
  (when flush (flush-output out)))

(define (minecraft-read-line)
  (read-line (minecraft-connection-in (c))))

(define (minecraft-send-command cmd [flush #t])
  (define cmd-strs (map ~a cmd))
  (minecraft-send-line (string-append (car cmd-strs) "(" (string-join (cdr cmd-strs) ",") ")")
		       flush))

(define (minecraft-read-numbers)
  (map string->number (string-split (minecraft-read-line) ",")))

(define (minecraft-read-number)
  (car (minecraft-read-numbers)))

(define (minecraft-read-entities)
  (string-split (minecraft-read-line) "|"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (I n) (truncate (inexact->exact n)))

(define (minecraft-get-block p)
  (match-define (vector x y z) p)
  (minecraft-send-command (list '|world.getBlock| (I x) (I y) (I z)))
  (minecraft-read-number))

(define (minecraft-get-block/data p)
  (match-define (vector x y z) p)
  (minecraft-send-command (list '|world.getBlockWithData| (I x) (I y) (I z)))
  (match-define (list t d) (minecraft-read-numbers))
  (cons t d))

(define (minecraft-get-blocks/data p1 p2)
  (define c1 (v-min p1 p2))
  (define c2 (v+ (v-max p1 p2) (vector 1 1 1)))
  (for* ([x (in-range (vector-ref c1 0) (vector-ref c2 0))]
	 [y (in-range (vector-ref c1 1) (vector-ref c2 1))]
	 [z (in-range (vector-ref c1 2) (vector-ref c2 2))])
    (minecraft-send-command (list '|world.getBlockWithData| (I x) (I y) (I z)) #f))
  (flush-output (minecraft-connection-out (c)))
  (values (v- c2 c1)
	  (for*/hash ([x (in-range (vector-ref c1 0) (vector-ref c2 0))]
		      [y (in-range (vector-ref c1 1) (vector-ref c2 1))]
		      [z (in-range (vector-ref c1 2) (vector-ref c2 2))])
	    (values (v- (vector x y z) c1)
		    (match (minecraft-read-numbers) [(list t d) (cons t d)])))))

(define (minecraft-set-block p type-id)
  (match-define (vector x y z) p)
  (minecraft-send-command
   (if (pair? type-id)
       (list '|world.setBlock| (I x) (I y) (I z)
	     (type-id->number (car type-id))
	     (cdr type-id))
       (list '|world.setBlock| (I x) (I y) (I z)
	     (type-id->number type-id)))))

(define (minecraft-set-blocks p1 p2 type-id)
  (match-define (vector x1 y1 z1) p1)
  (match-define (vector x2 y2 z2) p2)
  (minecraft-send-command
   (if (pair? type-id)
       (list '|world.setBlocks| (I x1) (I y1) (I z1) (I x2) (I y2) (I z2)
	     (type-id->number (car type-id))
	     (cdr type-id))
       (list '|world.setBlocks| (I x1) (I y1) (I z1) (I x2) (I y2) (I z2)
	     (type-id->number type-id)))))

(define (minecraft-get-height x z)
  (minecraft-send-command (list '|world.getHeight| (I x) (I z)))
  (minecraft-read-number))

(define (minecraft-get-player-ids)
  (minecraft-send-command (list '|world.getPlayerIds|))
  (minecraft-read-entities))

(define (minecraft-world-setting key value)
  (minecraft-send-command (list '|world.setting|
				key
				(case value
				  ((#t) 1)
				  ((#f) 0)
				  (else value)))))

(define (minecraft-checkpoint-save)
  (minecraft-send-command (list '|world.checkpoint.save|)))

(define (minecraft-checkpoint-restore)
  (minecraft-send-command (list '|world.checkpoint.restore|)))

(define (minecraft-post message)
  (minecraft-send-command (list '|chat.post| message)))

(define (minecraft-get-tile)
  (minecraft-send-command (list '|player.getTile|))
  (match-define (list x y z) (minecraft-read-numbers))
  (vector x y z))

(define (minecraft-set-tile p)
  (match-define (vector x y z) p)
  (minecraft-send-command (list '|player.setTile| (I x) (I y) (I z))))

(define (minecraft-get-pos)
  (minecraft-send-command (list '|player.getPos|))
  (match-define (list x y z) (minecraft-read-numbers))
  (vector x y z))

(define (minecraft-set-pos p)
  (match-define (vector x y z) p)
  (minecraft-send-command (list '|player.setPos| x y z)))

(define (minecraft-player-setting key value)
  (minecraft-send-command (list '|player.setting|
				key
				(case value
				  ((#t) 1)
				  ((#f) 0)
				  (else value)))))

;; (require racket/trace) ;; %%%
;; (trace minecraft-set-blocks) ;; %%%
