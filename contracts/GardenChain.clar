;; GardenChain: Urban Gardening and Plant Cultivation Reward System
;; Version: 1.0.0

;; Constants
(define-constant GARDEN_CAPACITY u3200000)
(define-constant BASE_GROWING_REWARD u35)
(define-constant HARVEST_BONUS u15)
(define-constant MAX_GARDENER_LEVEL u18)
(define-constant ERR_INVALID_GARDENING_ACTIVITY u1)
(define-constant ERR_NO_GARDEN_TOKENS u2)
(define-constant ERR_GARDEN_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_GROWING_SEASON u2304)
(define-constant COMPOSTING_MULTIPLIER u8)
(define-constant MIN_COMPOSTING_PERIOD u1152)
(define-constant EARLY_HARVEST_PENALTY u25)

;; Data Variables
(define-data-var total-garden-tokens-distributed uint u0)
(define-data-var total-gardening-activities uint u0)
(define-data-var garden-supervisor principal tx-sender)

;; Data Maps
(define-map gardener-activities principal uint)
(define-map gardener-garden-tokens principal uint)
(define-map planting-activity-start-time principal uint)
(define-map gardener-cultivation-level principal uint)
(define-map gardener-last-activity principal uint)
(define-map gardener-composting-materials principal uint)
(define-map gardener-composting-start-block principal uint)
(define-map plant-variety-grown principal uint)
(define-map gardener-harvest-count principal uint)
(define-map seasonal-yield-bonus principal uint)

;; Public Functions
(define-public (plant-seeds (crop-variety uint) (plot-size uint))
  (let
    (
      (gardener tx-sender)
    )
    (asserts! (and (> crop-variety u0) (> plot-size u0) (<= plot-size u20)) (err ERR_INVALID_GARDENING_ACTIVITY))
    (map-set planting-activity-start-time gardener burn-block-height)
    (map-set plant-variety-grown gardener crop-variety)
    (ok true)
  ))

(define-public (harvest-crops (crop-variety uint) (yield-quality uint))
  (let
    (
      (gardener tx-sender)
      (start-block (default-to u0 (map-get? planting-activity-start-time gardener)))
      (blocks-growing (- burn-block-height start-block))
      (last-activity-block (default-to u0 (map-get? gardener-last-activity gardener)))
      (cultivation-level (default-to u0 (map-get? gardener-cultivation-level gardener)))
      (capped-cultivation (if (<= cultivation-level MAX_GARDENER_LEVEL) cultivation-level MAX_GARDENER_LEVEL))
      (yield-bonus (/ (* yield-quality u8) u100))
      (seasonal-bonus (default-to u0 (map-get? seasonal-yield-bonus gardener)))
      (growing-reward (+ BASE_GROWING_REWARD (* capped-cultivation HARVEST_BONUS) yield-bonus seasonal-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-growing crop-variety) (<= yield-quality u100)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-activities gardener (+ (default-to u0 (map-get? gardener-activities gardener)) u1))
    (map-set gardener-garden-tokens gardener (+ (default-to u0 (map-get? gardener-garden-tokens gardener)) growing-reward))
    
    (if (< (- burn-block-height last-activity-block) BLOCKS_PER_GROWING_SEASON)
      (map-set gardener-cultivation-level gardener (+ cultivation-level u1))
      (map-set gardener-cultivation-level gardener u1)
    )
    
    (map-set gardener-harvest-count gardener (+ (default-to u0 (map-get? gardener-harvest-count gardener)) u1))
    
    ;; Reset seasonal bonus after use
    (map-set seasonal-yield-bonus gardener u0)
    
    (map-set gardener-last-activity gardener burn-block-height)
    (var-set total-gardening-activities (+ (var-get total-gardening-activities) u1))
    (var-set total-garden-tokens-distributed (+ (var-get total-garden-tokens-distributed) growing-reward))
    
    (asserts! (<= (var-get total-garden-tokens-distributed) GARDEN_CAPACITY) (err ERR_GARDEN_CAPACITY_EXCEEDED))
    (ok growing-reward)
  ))

(define-public (claim-garden-rewards)
  (let
    (
      (gardener tx-sender)
      (token-balance (default-to u0 (map-get? gardener-garden-tokens gardener)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_GARDEN_TOKENS))
    (map-set gardener-garden-tokens gardener u0)
    (ok token-balance)
  ))

;; Composting Features
(define-public (start-composting (organic-materials uint))
  (let
    (
      (gardener tx-sender)
    )
    (asserts! (> organic-materials u0) (err ERR_INVALID_GARDENING_ACTIVITY))
    (asserts! (>= (var-get total-garden-tokens-distributed) organic-materials) (err ERR_GARDEN_CAPACITY_EXCEEDED))
    
    (map-set gardener-composting-materials gardener organic-materials)
    (map-set gardener-composting-start-block gardener burn-block-height)
    (var-set total-garden-tokens-distributed (- (var-get total-garden-tokens-distributed) organic-materials))
    (ok organic-materials)
  ))

(define-public (harvest-compost)
  (let
    (
      (gardener tx-sender)
      (composting-amount (default-to u0 (map-get? gardener-composting-materials gardener)))
      (composting-start-block (default-to u0 (map-get? gardener-composting-start-block gardener)))
      (blocks-composting (- burn-block-height composting-start-block))
      (penalty (if (< blocks-composting MIN_COMPOSTING_PERIOD) (/ (* composting-amount EARLY_HARVEST_PENALTY) u100) u0))
      (composting-bonus (if (>= blocks-composting MIN_COMPOSTING_PERIOD) (/ (* composting-amount COMPOSTING_MULTIPLIER) u100) u0))
      (final-amount (+ (- composting-amount penalty) composting-bonus))
    )
    (asserts! (> composting-amount u0) (err ERR_NO_GARDEN_TOKENS))
    
    ;; Set seasonal bonus for next harvest
    (map-set seasonal-yield-bonus gardener (/ composting-bonus u2))
    
    (map-set gardener-composting-materials gardener u0)
    (map-set gardener-composting-start-block gardener u0)
    (var-set total-garden-tokens-distributed (+ (var-get total-garden-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (plant-companion-crops (primary-crop uint) (companion-crop uint))
  (let
    (
      (gardener tx-sender)
      (cultivation-level (default-to u0 (map-get? gardener-cultivation-level gardener)))
      (companion-bonus (+ BASE_GROWING_REWARD (* cultivation-level u5)))
    )
    (asserts! (and (> primary-crop u0) (> companion-crop u0) (>= cultivation-level u3)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-garden-tokens gardener (+ (default-to u0 (map-get? gardener-garden-tokens gardener)) companion-bonus))
    (var-set total-garden-tokens-distributed (+ (var-get total-garden-tokens-distributed) companion-bonus))
    
    (ok companion-bonus)
  ))

(define-public (create-seed-bank (seed-varieties uint) (preservation-method uint))
  (let
    (
      (gardener tx-sender)
      (harvest-count (default-to u0 (map-get? gardener-harvest-count gardener)))
      (seed-bank-bonus (+ (* seed-varieties u20) (* harvest-count u5)))
    )
    (asserts! (and (> seed-varieties u0) (>= harvest-count u10)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-garden-tokens gardener (+ (default-to u0 (map-get? gardener-garden-tokens gardener)) seed-bank-bonus))
    (var-set total-garden-tokens-distributed (+ (var-get total-garden-tokens-distributed) seed-bank-bonus))
    
    (ok seed-bank-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-gardening-activity-count (user principal))
  (default-to u0 (map-get? gardener-activities user)))

(define-read-only (get-garden-token-balance (user principal))
  (default-to u0 (map-get? gardener-garden-tokens user)))

(define-read-only (get-cultivation-level (user principal))
  (default-to u0 (map-get? gardener-cultivation-level user)))

(define-read-only (get-harvest-count (user principal))
  (default-to u0 (map-get? gardener-harvest-count user)))

(define-read-only (get-composting-materials (user principal))
  (default-to u0 (map-get? gardener-composting-materials user)))

(define-read-only (get-seasonal-bonus (user principal))
  (default-to u0 (map-get? seasonal-yield-bonus user)))

(define-read-only (get-garden-stats)
  {
    total-gardening-activities: (var-get total-gardening-activities),
    total-garden-tokens-distributed: (var-get total-garden-tokens-distributed),
    garden-capacity: GARDEN_CAPACITY
  })

(define-read-only (calculate-harvest-reward (cultivation-level uint) (yield-quality uint) (seasonal-bonus uint))
  (let
    (
      (capped-cultivation (if (<= cultivation-level MAX_GARDENER_LEVEL) cultivation-level MAX_GARDENER_LEVEL))
      (yield-bonus (/ (* yield-quality u8) u100))
    )
    (+ BASE_GROWING_REWARD (* capped-cultivation HARVEST_BONUS) yield-bonus seasonal-bonus)
  ))

;; Private Functions
(define-private (is-garden-supervisor)
  (is-eq tx-sender (var-get garden-supervisor)))

(define-private (validate-crop-parameters (crop-variety uint) (yield-quality uint))
  (and (> crop-variety u0) (<= yield-quality u100)))