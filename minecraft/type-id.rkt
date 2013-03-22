#lang racket/base

(provide (all-defined-out))

(define (type-id->number type-id)
  (hash-ref type-id-map type-id
	    (lambda ()
	      (cond
	       [(symbol? type-id) (error 'type-id->number "No such Minecraft type ID: ~a" type-id)]
	       [(or (< type-id 0)
		    (>= type-id 256))
		(error 'type-id->number "Type ID out of range: ~a" type-id)]
	       [else type-id]))))

(define (number->type-id n) (hash-ref type-number-map n (lambda () n)))

;; http://www.minecraftwiki.net/wiki/Data_values_(Pocket_Edition)
;; https://github.com/brooksc/mcpipy/blob/master/mcpi/mcpi_protocol_spec.txt
(define type-id-map (hash
		     'air 0
		     'stone 1
		     'grass 2
		     'dirt 3
		     'cobblestone 4
		     'wood-planks 5
		     'sapling 6
		     'bedrock 7
		     'water-flowing 8
		     'water 8
		     'water-stationary 9
		     'lava-flowing 10
		     'lava 10
		     'lava-stationary 11
		     'sand 12
		     'gravel 13
		     'gold-ore 14
		     'iron-ore 15
		     'coal-ore 16
		     'wood 17
		     'leaves 18
		     'glass 20
		     'lapis-lazuli-ore 21
		     'lapis-lazuli-block 22
		     'sandstone 24
		     'bed 26
		     'cobweb 30
		     'grass-tall 31
		     'wool 35
		     'flower-yellow 37
		     'flower-cyan 38
		     'mushroom-brown 39
		     'mushroom-red 40
		     'gold-block 41
		     'iron-block 42
		     'stone-slab-double 43
		     'stone-slab 44
		     'brick-block 45
		     'tnt 46
		     'bookshelf 47
		     'moss-stone 48
		     'obsidian 49
		     'torch 50
		     'fire 51
		     'stairs-wood 53
		     'chest 54
		     'diamond-ore 56
		     'diamond-block 57
		     'crafting-table 58
		     'farmland 60
		     'furnace-inactive 61
		     'furnace-active 62
		     'door-wood 64
		     'ladder 65
		     'stairs-cobblestone 67
		     'door-iron 71
		     'redstone-ore 73
		     'snow 78
		     'ice 79
		     'snow-block 80
		     'cactus 81
		     'clay 82
		     'sugar-cane 83
		     'fence 85
		     'glowstone-block 89
		     'bedrock-invisible 95
		     'stone-brick 98
		     'glass-pane 102
		     'melon 103
		     'fence-gate 107
		     'glowing-obsidian 246
		     'nether-reactor-core 247))

(define type-number-map
  (for/hash ([(k v) (in-hash type-id-map)])
    (values v k)))
