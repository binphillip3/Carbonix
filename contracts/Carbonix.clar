;; title: Carbonix
;; version: 1.0.0
;; summary: Carbon Offset Marketplace with tokenized offsets validated by oracles
;; description: A decentralized marketplace for carbon offset credits with oracle validation

;; traits
(define-trait oracle-trait
  (
    (validate-offset (uint principal) (response bool uint))
  )
)

;; token definitions
(define-fungible-token carbon-credit)

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-validated (err u104))
(define-constant err-not-validated (err u105))
(define-constant err-expired (err u106))
(define-constant err-unauthorized (err u107))
(define-constant err-invalid-price (err u108))

;; data vars
(define-data-var next-offset-id uint u1)
(define-data-var oracle-address (optional principal) none)
(define-data-var platform-fee-rate uint u250)

;; data maps
(define-map carbon-offsets
  uint
  {
    creator: principal,
    amount: uint,
    price-per-credit: uint,
    description: (string-ascii 256),
    validation-status: bool,
    created-at: uint,
    expires-at: uint,
    total-sold: uint
  }
)

(define-map user-balances
  principal
  uint
)

(define-map marketplace-listings
  uint
  {
    seller: principal,
    offset-id: uint,
    amount-available: uint,
    price-per-credit: uint,
    listed-at: uint
  }
)

(define-map user-purchases
  {buyer: principal, offset-id: uint}
  uint
)

;; public functions
(define-public (set-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set oracle-address (some oracle))
    (ok true)
  )
)

(define-public (create-offset (amount uint) (price-per-credit uint) (description (string-ascii 256)) (expires-at uint))
  (let
    (
      (offset-id (var-get next-offset-id))
      (current-height stacks-block-height)
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> price-per-credit u0) err-invalid-price)
    (asserts! (> expires-at current-height) err-invalid-amount)
    
    (map-set carbon-offsets offset-id
      {
        creator: tx-sender,
        amount: amount,
        price-per-credit: price-per-credit,
        description: description,
        validation-status: false,
        created-at: current-height,
        expires-at: expires-at,
        total-sold: u0
      }
    )
    
    (var-set next-offset-id (+ offset-id u1))
    (ok offset-id)
  )
)

(define-public (validate-offset (offset-id uint))
  (let
    (
      (offset (unwrap! (map-get? carbon-offsets offset-id) err-not-found))
      (oracle (unwrap! (var-get oracle-address) err-not-found))
    )
    (asserts! (is-eq tx-sender oracle) err-unauthorized)
    (asserts! (not (get validation-status offset)) err-already-validated)
    (asserts! (< stacks-block-height (get expires-at offset)) err-expired)
    
    (map-set carbon-offsets offset-id
      (merge offset {validation-status: true})
    )
    
    (try! (ft-mint? carbon-credit (get amount offset) (get creator offset)))
    (ok true)
  )
)

(define-public (list-for-sale (offset-id uint) (amount uint) (price-per-credit uint))
  (let
    (
      (offset (unwrap! (map-get? carbon-offsets offset-id) err-not-found))
      (user-balance (ft-get-balance carbon-credit tx-sender))
      (listing-id (var-get next-offset-id))
    )
    (asserts! (get validation-status offset) err-not-validated)
    (asserts! (>= user-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> price-per-credit u0) err-invalid-price)
    
    (try! (ft-transfer? carbon-credit amount tx-sender (as-contract tx-sender)))
    
    (map-set marketplace-listings listing-id
      {
        seller: tx-sender,
        offset-id: offset-id,
        amount-available: amount,
        price-per-credit: price-per-credit,
        listed-at: stacks-block-height
      }
    )
    
    (var-set next-offset-id (+ listing-id u1))
    (ok listing-id)
  )
)

(define-public (purchase-credits (listing-id uint) (amount uint))
  (let
    (
      (listing (unwrap! (map-get? marketplace-listings listing-id) err-not-found))
      (total-cost (* amount (get price-per-credit listing)))
      (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
      (seller-amount (- total-cost platform-fee))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (get amount-available listing) amount) err-insufficient-balance)
    
    (try! (stx-transfer? total-cost tx-sender (get seller listing)))
    (try! (stx-transfer? platform-fee (get seller listing) contract-owner))
    
    (try! (as-contract (ft-transfer? carbon-credit amount tx-sender tx-sender)))
    
    (map-set marketplace-listings listing-id
      (merge listing {amount-available: (- (get amount-available listing) amount)})
    )
    
    (map-set user-purchases
      {buyer: tx-sender, offset-id: (get offset-id listing)}
      (+ amount (default-to u0 (map-get? user-purchases {buyer: tx-sender, offset-id: (get offset-id listing)})))
    )
    
    (ok true)
  )
)

(define-public (retire-credits (amount uint))
  (let
    (
      (user-balance (ft-get-balance carbon-credit tx-sender))
    )
    (asserts! (>= user-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    (try! (ft-burn? carbon-credit amount tx-sender))
    (ok true)
  )
)

(define-public (cancel-listing (listing-id uint))
  (let
    (
      (listing (unwrap! (map-get? marketplace-listings listing-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get seller listing)) err-unauthorized)
    
    (try! (as-contract (ft-transfer? carbon-credit (get amount-available listing) tx-sender (get seller listing))))
    (map-delete marketplace-listings listing-id)
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount)
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

;; read only functions
(define-read-only (get-offset-details (offset-id uint))
  (map-get? carbon-offsets offset-id)
)

(define-read-only (get-listing-details (listing-id uint))
  (map-get? marketplace-listings listing-id)
)

(define-read-only (get-user-credit-balance (user principal))
  (ft-get-balance carbon-credit user)
)

(define-read-only (get-user-purchases (buyer principal) (offset-id uint))
  (default-to u0 (map-get? user-purchases {buyer: buyer, offset-id: offset-id}))
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-oracle-address)
  (var-get oracle-address)
)

(define-read-only (get-next-offset-id)
  (var-get next-offset-id)
)

(define-read-only (get-contract-owner)
  contract-owner
)

;; private functions
(define-private (is-valid-offset (offset-id uint))
  (match (map-get? carbon-offsets offset-id)
    offset (and 
             (get validation-status offset)
             (< stacks-block-height (get expires-at offset)))
    false
  )
)