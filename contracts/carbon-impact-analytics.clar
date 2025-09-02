;; Carbon Impact Analytics
;; Analytics and impact tracking for carbon offset activities

;; constants  
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-not-found (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-goal-exists (err u104))

;; data vars
(define-data-var analytics-enabled bool true)
(define-data-var global-credits-retired uint u0)
(define-data-var global-co2-offset uint u0)

;; data maps
(define-map user-impact-profile
  principal
  {
    total-credits-purchased: uint,
    total-credits-retired: uint,
    carbon-footprint-reduced: uint,
    sustainability-score: uint,
    first-activity: uint,
    last-activity: uint,
    goal-achievements: uint,
    streak-days: uint
  }
)

(define-map user-monthly-stats
  {user: principal, month: uint, year: uint}
  {
    credits-purchased: uint,
    credits-retired: uint,
    co2-reduced: uint,
    transactions-count: uint
  }
)

(define-map carbon-neutrality-goals
  principal
  {
    target-co2-reduction: uint,
    target-deadline: uint,
    current-progress: uint,
    is-achieved: bool,
    created-at: uint
  }
)

;; public functions
(define-public (record-carbon-purchase (user principal) (credits-amount uint))
  (let
    (
      (current-profile (default-to 
        {total-credits-purchased: u0, total-credits-retired: u0, carbon-footprint-reduced: u0, 
         sustainability-score: u0, first-activity: stacks-block-height, last-activity: stacks-block-height,
         goal-achievements: u0, streak-days: u0}
        (map-get? user-impact-profile user)))
      (current-month (/ stacks-block-height u4320))
      (current-year (/ stacks-block-height u52560))
    )
    (asserts! (var-get analytics-enabled) err-unauthorized)
    (asserts! (> credits-amount u0) err-invalid-amount)
    
    (map-set user-impact-profile user
      (merge current-profile {
        total-credits-purchased: (+ (get total-credits-purchased current-profile) credits-amount),
        last-activity: stacks-block-height,
        sustainability-score: (calculate-sustainability-score user credits-amount u0)
      })
    )
    
    (let
      (
        (monthly-stats (default-to {credits-purchased: u0, credits-retired: u0, co2-reduced: u0, transactions-count: u0}
                                   (map-get? user-monthly-stats {user: user, month: current-month, year: current-year})))
      )
      (map-set user-monthly-stats {user: user, month: current-month, year: current-year}
        (merge monthly-stats {
          credits-purchased: (+ (get credits-purchased monthly-stats) credits-amount),
          transactions-count: (+ (get transactions-count monthly-stats) u1)
        })
      )
    )
    
    (ok true)
  )
)

(define-public (record-carbon-retirement (user principal) (credits-amount uint))
  (let
    (
      (current-profile (unwrap! (map-get? user-impact-profile user) err-not-found))
      (co2-reduced (* credits-amount u1000))
      (current-month (/ stacks-block-height u4320))
      (current-year (/ stacks-block-height u52560))
    )
    (asserts! (var-get analytics-enabled) err-unauthorized)
    (asserts! (> credits-amount u0) err-invalid-amount)
    
    (map-set user-impact-profile user
      (merge current-profile {
        total-credits-retired: (+ (get total-credits-retired current-profile) credits-amount),
        carbon-footprint-reduced: (+ (get carbon-footprint-reduced current-profile) co2-reduced),
        last-activity: stacks-block-height,
        sustainability-score: (calculate-sustainability-score user u0 credits-amount)
      })
    )
    
    (let
      (
        (monthly-stats (default-to {credits-purchased: u0, credits-retired: u0, co2-reduced: u0, transactions-count: u0}
                                   (map-get? user-monthly-stats {user: user, month: current-month, year: current-year})))
      )
      (map-set user-monthly-stats {user: user, month: current-month, year: current-year}
        (merge monthly-stats {
          credits-retired: (+ (get credits-retired monthly-stats) credits-amount),
          co2-reduced: (+ (get co2-reduced monthly-stats) co2-reduced),
          transactions-count: (+ (get transactions-count monthly-stats) u1)
        })
      )
    )
    
    (var-set global-credits-retired (+ (var-get global-credits-retired) credits-amount))
    (var-set global-co2-offset (+ (var-get global-co2-offset) co2-reduced))
    
    (let
      ((goal-result (update-goal-progress user co2-reduced)))
      (ok true))
  )
)

(define-public (set-carbon-neutrality-goal (target-co2-reduction uint) (target-deadline uint))
  (let
    (
      (existing-goal (map-get? carbon-neutrality-goals tx-sender))
    )
    (asserts! (> target-co2-reduction u0) err-invalid-amount)
    (asserts! (> target-deadline stacks-block-height) err-invalid-amount)
    (asserts! (is-none existing-goal) err-goal-exists)
    
    (map-set carbon-neutrality-goals tx-sender
      {
        target-co2-reduction: target-co2-reduction,
        target-deadline: target-deadline,
        current-progress: u0,
        is-achieved: false,
        created-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (toggle-analytics (enabled bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set analytics-enabled enabled)
    (ok true)
  )
)

;; read-only functions
(define-read-only (get-user-impact-profile (user principal))
  (map-get? user-impact-profile user)
)

(define-read-only (get-user-monthly-stats (user principal) (month uint) (year uint))
  (map-get? user-monthly-stats {user: user, month: month, year: year})
)

(define-read-only (get-carbon-neutrality-goal (user principal))
  (map-get? carbon-neutrality-goals user)
)

(define-read-only (get-global-impact-stats)
  {
    total-credits-retired: (var-get global-credits-retired),
    total-co2-offset: (var-get global-co2-offset),
    analytics-enabled: (var-get analytics-enabled)
  }
)

;; private functions
(define-private (calculate-sustainability-score (user principal) (purchased uint) (retired uint))
  (let
    (
      (current-profile (default-to 
        {total-credits-purchased: u0, total-credits-retired: u0, carbon-footprint-reduced: u0, 
         sustainability-score: u0, first-activity: stacks-block-height, last-activity: stacks-block-height,
         goal-achievements: u0, streak-days: u0}
        (map-get? user-impact-profile user)))
      (total-purchased (+ (get total-credits-purchased current-profile) purchased))
      (total-retired (+ (get total-credits-retired current-profile) retired))
      (retirement-bonus (if (> retired u0) (* retired u150) u0))
      (activity-bonus (if (> (get streak-days current-profile) u7) u500 u0))
      (goal-bonus (* (get goal-achievements current-profile) u200))
    )
    (+ 
      (+ (* total-retired u100) (/ total-purchased u2))
      (+ retirement-bonus activity-bonus)
      goal-bonus)
  )
)

(define-private (update-goal-progress (user principal) (co2-reduced uint))
  (match (map-get? carbon-neutrality-goals user)
    goal
    (let
      (
        (new-progress (+ (get current-progress goal) co2-reduced))
        (is-achieved (>= new-progress (get target-co2-reduction goal)))
      )
      (map-set carbon-neutrality-goals user
        (merge goal {
          current-progress: new-progress,
          is-achieved: is-achieved
        })
      )
      
      (if is-achieved
        (let
          (
            (profile (unwrap-panic (map-get? user-impact-profile user)))
          )
          (map-set user-impact-profile user
            (merge profile {goal-achievements: (+ (get goal-achievements profile) u1)}))
          (ok true))
        (ok false)))
    (ok false))
)
