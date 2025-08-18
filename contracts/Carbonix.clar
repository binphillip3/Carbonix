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
(define-constant err-portfolio-not-found (err u109))
(define-constant err-portfolio-limit-exceeded (err u110))
(define-constant err-duplicate-offset (err u111))
(define-constant err-portfolio-empty (err u112))
(define-constant err-invalid-weights (err u113))
(define-constant err-subscription-not-found (err u114))
(define-constant err-subscription-inactive (err u115))
(define-constant err-insufficient-balance-subscription (err u116))
(define-constant err-invalid-frequency (err u117))
(define-constant err-subscription-expired (err u118))
(define-constant max-portfolio-size u20)
(define-constant max-subscription-targets u10)

;; data vars
(define-data-var next-offset-id uint u1)
(define-data-var oracle-address (optional principal) none)
(define-data-var platform-fee-rate uint u250)
(define-data-var next-portfolio-id uint u1)
(define-data-var next-subscription-id uint u1)

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

(define-map portfolios
  uint
  {
    owner: principal,
    name: (string-ascii 64),
    total-value: uint,
    created-at: uint,
    last-rebalanced: uint,
    is-active: bool
  }
)

(define-map portfolio-holdings
  {portfolio-id: uint, offset-id: uint}
  {
    amount: uint,
    target-weight: uint,
    purchase-price: uint,
    added-at: uint
  }
)

(define-map portfolio-performance
  uint
  {
    initial-investment: uint,
    current-value: uint,
    total-retired: uint,
    diversification-score: uint
  }
)

(define-map subscriptions
  uint
  {
    subscriber: principal,
    budget-per-period: uint,
    frequency-blocks: uint,
    next-execution: uint,
    auto-retire: bool,
    is-active: bool,
    created-at: uint,
    total-spent: uint,
    executions-count: uint
  }
)

(define-map subscription-targets
  {subscription-id: uint, target-index: uint}
  {
    offset-id: uint,
    allocation-percentage: uint
  }
)

(define-map subscription-history
  {subscription-id: uint, execution-id: uint}
  {
    executed-at: uint,
    amount-spent: uint,
    credits-purchased: uint,
    credits-retired: uint
  }
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

(define-public (create-portfolio (name (string-ascii 64)))
  (let
    (
      (portfolio-id (var-get next-portfolio-id))
      (current-height stacks-block-height)
    )
    (asserts! (> (len name) u0) err-invalid-amount)
    
    (map-set portfolios portfolio-id
      {
        owner: tx-sender,
        name: name,
        total-value: u0,
        created-at: current-height,
        last-rebalanced: current-height,
        is-active: true
      }
    )
    
    (map-set portfolio-performance portfolio-id
      {
        initial-investment: u0,
        current-value: u0,
        total-retired: u0,
        diversification-score: u0
      }
    )
    
    (var-set next-portfolio-id (+ portfolio-id u1))
    (ok portfolio-id)
  )
)

(define-public (add-to-portfolio (portfolio-id uint) (offset-id uint) (amount uint) (target-weight uint))
  (let
    (
      (portfolio (unwrap! (map-get? portfolios portfolio-id) err-portfolio-not-found))
      (offset (unwrap! (map-get? carbon-offsets offset-id) err-not-found))
      (existing-holding (map-get? portfolio-holdings {portfolio-id: portfolio-id, offset-id: offset-id}))
      (user-balance (ft-get-balance carbon-credit tx-sender))
      (current-holdings-count (get-portfolio-holdings-count portfolio-id))
    )
    (asserts! (is-eq tx-sender (get owner portfolio)) err-unauthorized)
    (asserts! (get is-active portfolio) err-portfolio-empty)
    (asserts! (get validation-status offset) err-not-validated)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= user-balance amount) err-insufficient-balance)
    (asserts! (and (> target-weight u0) (<= target-weight u10000)) err-invalid-weights)
    (asserts! (is-none existing-holding) err-duplicate-offset)
    (asserts! (< current-holdings-count max-portfolio-size) err-portfolio-limit-exceeded)
    
    (try! (ft-transfer? carbon-credit amount tx-sender (as-contract tx-sender)))
    
    (map-set portfolio-holdings 
      {portfolio-id: portfolio-id, offset-id: offset-id}
      {
        amount: amount,
        target-weight: target-weight,
        purchase-price: (get price-per-credit offset),
        added-at: stacks-block-height
      }
    )
    
    (map-set portfolios portfolio-id
      (merge portfolio {
        total-value: (+ (get total-value portfolio) (* amount (get price-per-credit offset))),
        last-rebalanced: stacks-block-height
      })
    )
    
    (ok true)
  )
)

(define-public (batch-retire-from-portfolio (portfolio-id uint) (retirement-list (list 10 {offset-id: uint, amount: uint})))
  (let
    (
      (portfolio (unwrap! (map-get? portfolios portfolio-id) err-portfolio-not-found))
    )
    (asserts! (is-eq tx-sender (get owner portfolio)) err-unauthorized)
    (asserts! (get is-active portfolio) err-portfolio-empty)
    (asserts! (> (len retirement-list) u0) err-invalid-amount)
    
    (try! (fold batch-retire-helper retirement-list (ok portfolio-id)))
    (ok true)
  )
)

(define-public (rebalance-portfolio (portfolio-id uint) (rebalancing-list (list 15 {offset-id: uint, new-target-weight: uint})))
  (let
    (
      (portfolio (unwrap! (map-get? portfolios portfolio-id) err-portfolio-not-found))
      (total-weight (fold sum-weights rebalancing-list u0))
    )
    (asserts! (is-eq tx-sender (get owner portfolio)) err-unauthorized)
    (asserts! (get is-active portfolio) err-portfolio-empty)
    (asserts! (is-eq total-weight u10000) err-invalid-weights)
    (asserts! (> (len rebalancing-list) u0) err-invalid-amount)
    
    (try! (fold rebalance-helper rebalancing-list (ok portfolio-id)))
    
    (map-set portfolios portfolio-id
      (merge portfolio {last-rebalanced: stacks-block-height})
    )
    
    (ok true)
  )
)

(define-public (remove-from-portfolio (portfolio-id uint) (offset-id uint))
  (let
    (
      (portfolio (unwrap! (map-get? portfolios portfolio-id) err-portfolio-not-found))
      (holding (unwrap! (map-get? portfolio-holdings {portfolio-id: portfolio-id, offset-id: offset-id}) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner portfolio)) err-unauthorized)
    (asserts! (get is-active portfolio) err-portfolio-empty)
    
    (try! (as-contract (ft-transfer? carbon-credit (get amount holding) tx-sender (get owner portfolio))))
    (map-delete portfolio-holdings {portfolio-id: portfolio-id, offset-id: offset-id})
    
    (let
      (
        (holding-value (* (get amount holding) (get purchase-price holding)))
        (new-total-value (if (>= (get total-value portfolio) holding-value)
                           (- (get total-value portfolio) holding-value)
                           u0))
      )
      (map-set portfolios portfolio-id
        (merge portfolio {total-value: new-total-value})
      )
    )
    
    (ok true)
  )
)

(define-public (deactivate-portfolio (portfolio-id uint))
  (let
    (
      (portfolio (unwrap! (map-get? portfolios portfolio-id) err-portfolio-not-found))
    )
    (asserts! (is-eq tx-sender (get owner portfolio)) err-unauthorized)
    (asserts! (get is-active portfolio) err-portfolio-empty)
    
    (map-set portfolios portfolio-id
      (merge portfolio {is-active: false})
    )
    
    (ok true)
  )
)

(define-public (create-subscription (budget-per-period uint) (frequency-blocks uint) (auto-retire bool) (target-allocations (list 10 {offset-id: uint, allocation-percentage: uint})))
  (let
    (
      (subscription-id (var-get next-subscription-id))
      (current-height stacks-block-height)
      (total-allocation (fold sum-allocations target-allocations u0))
    )
    (asserts! (> budget-per-period u0) err-invalid-amount)
    (asserts! (> frequency-blocks u0) err-invalid-frequency)
    (asserts! (is-eq total-allocation u10000) err-invalid-weights)
    (asserts! (> (len target-allocations) u0) err-invalid-amount)
    (asserts! (<= (len target-allocations) max-subscription-targets) err-portfolio-limit-exceeded)
    
    (try! (fold validate-subscription-targets target-allocations (ok u0)))
    
    (map-set subscriptions subscription-id
      {
        subscriber: tx-sender,
        budget-per-period: budget-per-period,
        frequency-blocks: frequency-blocks,
        next-execution: (+ current-height frequency-blocks),
        auto-retire: auto-retire,
        is-active: true,
        created-at: current-height,
        total-spent: u0,
        executions-count: u0
      }
    )
    
    (fold set-subscription-targets-indexed
      target-allocations
      {subscription-id: subscription-id, current-index: u0})
    
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

(define-public (execute-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
      (current-height stacks-block-height)
      (execution-id (get executions-count subscription))
    )
    (asserts! (get is-active subscription) err-subscription-inactive)
    (asserts! (>= current-height (get next-execution subscription)) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) (get budget-per-period subscription)) err-insufficient-balance-subscription)
    
    (let
      (
        (target-indices (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
        (purchase-result (fold execute-subscription-purchase 
          target-indices 
          {subscription-id: subscription-id, total-purchased: u0, total-spent: u0, total-retired: u0}))
      )
      (map-set subscriptions subscription-id
        (merge subscription {
          next-execution: (+ current-height (get frequency-blocks subscription)),
          total-spent: (+ (get total-spent subscription) (get total-spent purchase-result)),
          executions-count: (+ execution-id u1)
        })
      )
      
      (map-set subscription-history 
        {subscription-id: subscription-id, execution-id: execution-id}
        {
          executed-at: current-height,
          amount-spent: (get total-spent purchase-result),
          credits-purchased: (get total-purchased purchase-result),
          credits-retired: (get total-retired purchase-result)
        }
      )
      
      (ok true)
    )
  )
)

(define-public (pause-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (get is-active subscription) err-subscription-inactive)
    
    (map-set subscriptions subscription-id
      (merge subscription {is-active: false})
    )
    
    (ok true)
  )
)

(define-public (resume-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (not (get is-active subscription)) err-invalid-amount)
    
    (map-set subscriptions subscription-id
      (merge subscription {
        is-active: true,
        next-execution: (+ stacks-block-height (get frequency-blocks subscription))
      })
    )
    
    (ok true)
  )
)

(define-public (update-subscription-budget (subscription-id uint) (new-budget uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (> new-budget u0) err-invalid-amount)
    
    (map-set subscriptions subscription-id
      (merge subscription {budget-per-period: new-budget})
    )
    
    (ok true)
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions subscription-id) err-subscription-not-found))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    
    (map-set subscriptions subscription-id
      (merge subscription {is-active: false})
    )
    
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

(define-read-only (get-portfolio-details (portfolio-id uint))
  (map-get? portfolios portfolio-id)
)

(define-read-only (get-portfolio-holding (portfolio-id uint) (offset-id uint))
  (map-get? portfolio-holdings {portfolio-id: portfolio-id, offset-id: offset-id})
)

(define-read-only (get-portfolio-performance (portfolio-id uint))
  (map-get? portfolio-performance portfolio-id)
)

(define-read-only (get-next-portfolio-id)
  (var-get next-portfolio-id)
)

(define-read-only (get-portfolio-holdings-count (portfolio-id uint))
  (let
    (
      (offsets-list (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20))
      (result (fold count-holdings-helper 
        (map create-holding-key offsets-list) 
        {portfolio-id: portfolio-id, count: u0}))
    )
    (get count result)
  )
)

(define-read-only (calculate-portfolio-value (portfolio-id uint))
  (let
    (
      (offsets-list (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20))
      (result (fold calculate-value-helper 
        (map create-holding-key offsets-list) 
        {portfolio-id: portfolio-id, total-value: u0}))
    )
    (get total-value result)
  )
)

(define-read-only (get-subscription-details (subscription-id uint))
  (map-get? subscriptions subscription-id)
)

(define-read-only (get-subscription-target (subscription-id uint) (target-index uint))
  (map-get? subscription-targets {subscription-id: subscription-id, target-index: target-index})
)

(define-read-only (get-subscription-history (subscription-id uint) (execution-id uint))
  (map-get? subscription-history {subscription-id: subscription-id, execution-id: execution-id})
)

(define-read-only (get-next-subscription-id)
  (var-get next-subscription-id)
)

(define-read-only (is-subscription-ready (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription
    (and 
      (get is-active subscription)
      (>= stacks-block-height (get next-execution subscription)))
    false)
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

(define-private (batch-retire-helper (retirement-item {offset-id: uint, amount: uint}) (portfolio-id-result (response uint uint)))
  (match portfolio-id-result
    portfolio-id-val
    (let
      (
        (holding (map-get? portfolio-holdings {portfolio-id: portfolio-id-val, offset-id: (get offset-id retirement-item)}))
      )
      (match holding
        holding-data
        (begin
          (asserts! (>= (get amount holding-data) (get amount retirement-item)) err-insufficient-balance)
          (try! (as-contract (ft-burn? carbon-credit (get amount retirement-item) tx-sender)))
          (if (is-eq (get amount holding-data) (get amount retirement-item))
            (map-delete portfolio-holdings {portfolio-id: portfolio-id-val, offset-id: (get offset-id retirement-item)})
            (map-set portfolio-holdings {portfolio-id: portfolio-id-val, offset-id: (get offset-id retirement-item)}
              (merge holding-data {amount: (- (get amount holding-data) (get amount retirement-item))})))
          (ok portfolio-id-val))
        err-not-found))
    error-val
    (err error-val))
)

(define-private (sum-weights (weight-item {offset-id: uint, new-target-weight: uint}) (total uint))
  (+ total (get new-target-weight weight-item))
)

(define-private (rebalance-helper (rebalance-item {offset-id: uint, new-target-weight: uint}) (portfolio-id-result (response uint uint)))
  (match portfolio-id-result
    portfolio-id-val
    (let
      (
        (holding (map-get? portfolio-holdings {portfolio-id: portfolio-id-val, offset-id: (get offset-id rebalance-item)}))
      )
      (match holding
        holding-data
        (begin
          (map-set portfolio-holdings {portfolio-id: portfolio-id-val, offset-id: (get offset-id rebalance-item)}
            (merge holding-data {target-weight: (get new-target-weight rebalance-item)}))
          (ok portfolio-id-val))
        err-not-found))
    error-val
    (err error-val))
)

(define-private (create-holding-key (offset-id uint))
  offset-id
)

(define-private (count-holdings-helper (offset-id uint) (acc {portfolio-id: uint, count: uint}))
  (let
    (
      (holding (map-get? portfolio-holdings {portfolio-id: (get portfolio-id acc), offset-id: offset-id}))
    )
    (if (is-some holding)
      {portfolio-id: (get portfolio-id acc), count: (+ (get count acc) u1)}
      acc)
  )
)

(define-private (calculate-value-helper (offset-id uint) (acc {portfolio-id: uint, total-value: uint}))
  (let
    (
      (holding (map-get? portfolio-holdings {portfolio-id: (get portfolio-id acc), offset-id: offset-id}))
      (offset (map-get? carbon-offsets offset-id))
    )
    (match holding
      holding-data
      (match offset
        offset-data
        {
          portfolio-id: (get portfolio-id acc),
          total-value: (+ (get total-value acc) (* (get amount holding-data) (get price-per-credit offset-data)))
        }
        acc)
      acc)
  )
)

(define-private (sum-allocations (allocation {offset-id: uint, allocation-percentage: uint}) (total uint))
  (+ total (get allocation-percentage allocation))
)

(define-private (validate-subscription-targets (target {offset-id: uint, allocation-percentage: uint}) (result (response uint uint)))
  (match result
    success-val
    (let
      (
        (offset (map-get? carbon-offsets (get offset-id target)))
      )
      (match offset
        offset-data
        (if (get validation-status offset-data)
          (ok success-val)
          err-not-validated)
        err-not-found))
    error-val
    (err error-val))
)

(define-private (set-subscription-targets-indexed (target {offset-id: uint, allocation-percentage: uint}) (acc {subscription-id: uint, current-index: uint}))
  (begin
    (map-set subscription-targets 
      {subscription-id: (get subscription-id acc), target-index: (get current-index acc)}
      {
        offset-id: (get offset-id target),
        allocation-percentage: (get allocation-percentage target)
      })
    {subscription-id: (get subscription-id acc), current-index: (+ (get current-index acc) u1)})
)

(define-private (execute-subscription-purchase (target-index uint) (acc {subscription-id: uint, total-purchased: uint, total-spent: uint, total-retired: uint}))
  (let
    (
      (subscription-id (get subscription-id acc))
      (subscription (unwrap-panic (map-get? subscriptions subscription-id)))
      (target (map-get? subscription-targets {subscription-id: subscription-id, target-index: target-index}))
    )
    (match target
      target-data
      (let
        (
          (allocation-amount (/ (* (get budget-per-period subscription) (get allocation-percentage target-data)) u10000))
          (offset (unwrap-panic (map-get? carbon-offsets (get offset-id target-data))))
          (credits-to-buy (/ allocation-amount (get price-per-credit offset)))
        )
        (if (> credits-to-buy u0)
          (let
            (
              (actual-cost (* credits-to-buy (get price-per-credit offset)))
              (retired-amount (if (get auto-retire subscription) credits-to-buy u0))
            )
            (if (get auto-retire subscription)
              {
                subscription-id: subscription-id,
                total-purchased: (+ (get total-purchased acc) credits-to-buy),
                total-spent: (+ (get total-spent acc) actual-cost),
                total-retired: (+ (get total-retired acc) retired-amount)
              }
              {
                subscription-id: subscription-id,
                total-purchased: (+ (get total-purchased acc) credits-to-buy),
                total-spent: (+ (get total-spent acc) actual-cost),
                total-retired: (get total-retired acc)
              }))
          acc))
      acc)
  )
)


