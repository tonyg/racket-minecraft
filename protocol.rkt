#lang racket/base

;; https://github.com/brooksc/mcpipy/blob/master/mcpi/mcpi_protocol_spec.txt
;; http://www.wiki.vg/Minecraft_Pi_Protocol

(require racket/match)
(require racket/tcp)
(require racket/string)
(require racket/format)

(require "type-id.rkt")

(provide (struct-out minecraft-connection)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (minecraft-connect server [port 4711])
  (define-values (in out) (tcp-connect server port))
  (minecraft-connection in out))

(define (minecraft-disconnect c)
  (match-define (minecraft-connection in out) c)
  (tcp-abandon-port in)
  (tcp-abandon-port out))

(define (minecraft-send-line c line)
  (define out (minecraft-connection-out c))
  (display line out)
  (newline out)
  (flush-output out))

(define (minecraft-read-line c)
  (read-line (minecraft-connection-in c)))

(define (minecraft-send-command c cmd)
  (define cmd-strs (map ~a cmd))
  (minecraft-send-line c (string-append (car cmd-strs) "(" (string-join (cdr cmd-strs) ",") ")")))

(define (minecraft-read-numbers c)
  (map string->number (string-split (minecraft-read-line c) ",")))

(define (minecraft-read-number c)
  (car (minecraft-read-numbers c)))

(define (minecraft-read-entities c)
  (string-split (minecraft-read-line c) "|"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (I n) (truncate (inexact->exact n)))

(define (minecraft-get-block c p)
  (match-define (vector x y z) p)
  (minecraft-send-command c (list '|world.getBlock| (I x) (I y) (I z)))
  (minecraft-read-number c))

(define (minecraft-get-block/data c p)
  (match-define (vector x y z) p)
  (minecraft-send-command c (list '|world.getBlockWithData| (I x) (I y) (I z)))
  (match-define (list t d) (minecraft-read-numbers c))
  (values t d))

(define (minecraft-set-block c p type-id [block-data #f])
  (match-define (vector x y z) p)
  (minecraft-send-command c
			  (if block-data
			      (list '|world.setBlock| (I x) (I y) (I z)
				    (type-id->number type-id)
				    block-data)
			      (list '|world.setBlock| (I x) (I y) (I z)
				    (type-id->number type-id)))))

(define (minecraft-set-blocks c p1 p2 type-id [block-data #f])
  (match-define (vector x1 y1 z1) p1)
  (match-define (vector x2 y2 z2) p2)
  (minecraft-send-command c
			  (if block-data
			      (list '|world.setBlocks| (I x1) (I y1) (I z1) (I x2) (I y2) (I z2)
				    (type-id->number type-id)
				    block-data)
			      (list '|world.setBlocks| (I x1) (I y1) (I z1) (I x2) (I y2) (I z2)
				    (type-id->number type-id)))))

(define (minecraft-get-height c x z)
  (minecraft-send-command c (list '|world.getHeight| (I x) (I z)))
  (minecraft-read-number c))

(define (minecraft-get-player-ids c)
  (minecraft-send-command c (list '|world.getPlayerIds|))
  (minecraft-read-entities c))

(define (minecraft-world-setting c key value)
  (minecraft-send-command c (list '|world.setting|
				  key
				  (case value
				    ((#t) 1)
				    ((#f) 0)
				    (else value)))))

(define (minecraft-checkpoint-save c)
  (minecraft-send-command c (list '|world.checkpoint.save|)))

(define (minecraft-checkpoint-restore c)
  (minecraft-send-command c (list '|world.checkpoint.restore|)))

(define (minecraft-post c message)
  (minecraft-send-command c (list '|chat.post| message)))

(define (minecraft-get-tile c)
  (minecraft-send-command c (list '|player.getTile|))
  (match-define (list x y z) (minecraft-read-numbers c))
  (vector x y z))

(define (minecraft-set-tile c p)
  (match-define (vector x y z) p)
  (minecraft-send-command c (list '|player.setTile| (I x) (I y) (I z))))

(define (minecraft-get-pos c)
  (minecraft-send-command c (list '|player.getPos|))
  (match-define (list x y z) (minecraft-read-numbers c))
  (vector x y z))

(define (minecraft-set-pos c p)
  (match-define (vector x y z) p)
  (minecraft-send-command c (list '|player.setPos| x y z)))

(define (minecraft-player-setting c key value)
  (minecraft-send-command c (list '|player.setting|
				  key
				  (case value
				    ((#t) 1)
				    ((#f) 0)
				    (else value)))))
