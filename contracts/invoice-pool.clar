(define-constant ERR_NOT_AUTHORIZED (err u400))
(define-constant ERR_POOL_NOT_FOUND (err u401))
(define-constant ERR_POOL_CLOSED (err u402))
(define-constant ERR_POOL_EXISTS (err u403))
(define-constant ERR_INVALID_AMOUNT (err u404))
(define-constant ERR_INSUFFICIENT_FUNDS (err u405))
(define-constant ERR_POOL_NOT_FILLED (err u406))
(define-constant ERR_POOL_SETTLED (err u407))
(define-constant ERR_NO_STAKE (err u408))

(define-map invoice-pools
  { invoice-id: uint }
  {
    target-amount: uint,
    current-amount: uint,
    min-stake: uint,
    is-open: bool,
    is-settled: bool,
    created-at: uint,
    settled-at: uint,
    total-participants: uint
  }
)

(define-map pool-stakes
  { invoice-id: uint, investor: principal }
  { stake-amount: uint, claimed: bool }
)

(define-map user-pool-balances
  { user: principal }
  { balance: uint }
)

(define-private (get-pool-balance (user principal))
  (default-to u0 (get balance (map-get? user-pool-balances { user: user })))
)

(define-private (set-pool-balance (user principal) (amount uint))
  (map-set user-pool-balances { user: user } { balance: amount })
)

(define-public (create-pool (invoice-id uint) (target-amount uint) (min-stake uint))
  (begin
    (asserts! (> target-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> min-stake u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? invoice-pools { invoice-id: invoice-id })) ERR_POOL_EXISTS)
    
    (map-set invoice-pools
      { invoice-id: invoice-id }
      {
        target-amount: target-amount,
        current-amount: u0,
        min-stake: min-stake,
        is-open: true,
        is-settled: false,
        created-at: stacks-block-height,
        settled-at: u0,
        total-participants: u0
      }
    )
    (ok invoice-id)
  )
)

(define-public (join-pool (invoice-id uint) (stake-amount uint))
  (let (
    (pool (unwrap! (map-get? invoice-pools { invoice-id: invoice-id }) ERR_POOL_NOT_FOUND))
    (current-stake (default-to { stake-amount: u0, claimed: false } 
                    (map-get? pool-stakes { invoice-id: invoice-id, investor: tx-sender })))
    (new-stake-amount (+ (get stake-amount current-stake) stake-amount))
    (new-pool-amount (+ (get current-amount pool) stake-amount))
    (user-balance (get-pool-balance tx-sender))
  )
    (asserts! (get is-open pool) ERR_POOL_CLOSED)
    (asserts! (not (get is-settled pool)) ERR_POOL_SETTLED)
    (asserts! (>= stake-amount (get min-stake pool)) ERR_INVALID_AMOUNT)
    (asserts! (<= new-pool-amount (get target-amount pool)) ERR_INVALID_AMOUNT)
    (asserts! (>= user-balance stake-amount) ERR_INSUFFICIENT_FUNDS)
    
    (set-pool-balance tx-sender (- user-balance stake-amount))
    
    (map-set pool-stakes
      { invoice-id: invoice-id, investor: tx-sender }
      { stake-amount: new-stake-amount, claimed: false }
    )
    
    (map-set invoice-pools
      { invoice-id: invoice-id }
      (merge pool {
        current-amount: new-pool-amount,
        total-participants: (if (is-eq (get stake-amount current-stake) u0) 
                              (+ (get total-participants pool) u1)
                              (get total-participants pool)),
        is-open: (< new-pool-amount (get target-amount pool))
      })
    )
    (ok new-stake-amount)
  )
)

(define-public (settle-pool (invoice-id uint) (payout-amount uint))
  (let ((pool (unwrap! (map-get? invoice-pools { invoice-id: invoice-id }) ERR_POOL_NOT_FOUND)))
    (asserts! (is-eq tx-sender contract-caller) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-open pool)) ERR_POOL_NOT_FILLED)
    (asserts! (not (get is-settled pool)) ERR_POOL_SETTLED)
    (asserts! (>= payout-amount u0) ERR_INVALID_AMOUNT)
    
    (map-set invoice-pools
      { invoice-id: invoice-id }
      (merge pool { is-settled: true, settled-at: stacks-block-height })
    )
    (ok true)
  )
)

(define-public (claim-payout (invoice-id uint) (total-payout uint))
  (let (
    (pool (unwrap! (map-get? invoice-pools { invoice-id: invoice-id }) ERR_POOL_NOT_FOUND))
    (stake (unwrap! (map-get? pool-stakes { invoice-id: invoice-id, investor: tx-sender }) ERR_NO_STAKE))
    (stake-amount (get stake-amount stake))
    (payout-share (/ (* total-payout stake-amount) (get current-amount pool)))
  )
    (asserts! (get is-settled pool) ERR_POOL_NOT_FILLED)
    (asserts! (not (get claimed stake)) ERR_POOL_SETTLED)
    (asserts! (> stake-amount u0) ERR_NO_STAKE)
    
    (set-pool-balance tx-sender (+ (get-pool-balance tx-sender) payout-share))
    
    (map-set pool-stakes
      { invoice-id: invoice-id, investor: tx-sender }
      (merge stake { claimed: true })
    )
    (ok payout-share)
  )
)

(define-read-only (get-pool (invoice-id uint))
  (map-get? invoice-pools { invoice-id: invoice-id })
)

(define-read-only (get-stake (invoice-id uint) (investor principal))
  (map-get? pool-stakes { invoice-id: invoice-id, investor: investor })
)

(define-read-only (get-user-pool-balance (user principal))
  (get-pool-balance user)
)

(define-read-only (calculate-ownership-percent (invoice-id uint) (investor principal))
  (match (map-get? invoice-pools { invoice-id: invoice-id })
    pool (match (map-get? pool-stakes { invoice-id: invoice-id, investor: investor })
      stake (some (/ (* (get stake-amount stake) u10000) (get current-amount pool)))
      none)
    none)
)
