(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_BUNDLE_NOT_FOUND (err u301))
(define-constant ERR_BUNDLE_ALREADY_EXISTS (err u302))
(define-constant ERR_INVALID_BUNDLE (err u303))
(define-constant ERR_BUNDLE_NOT_FOR_SALE (err u304))
(define-constant ERR_BUNDLE_SOLD (err u305))
(define-constant ERR_INSUFFICIENT_FUNDS (err u306))
(define-constant ERR_EMPTY_BUNDLE (err u307))

(define-data-var next-bundle-id uint u1)

(define-map invoice-bundles
  { bundle-id: uint }
  {
    creator: principal,
    invoice-ids: (list 20 uint),
    total-face-value: uint,
    bundle-price: uint,
    is-for-sale: bool,
    current-owner: principal,
    created-at: uint,
    is-sold: bool
  }
)

(define-map user-bundle-balances
  { user: principal }
  { balance: uint }
)

(define-private (get-bundle-balance (user principal))
  (default-to u0 (get balance (map-get? user-bundle-balances { user: user })))
)

(define-private (set-bundle-balance (user principal) (amount uint))
  (map-set user-bundle-balances { user: user } { balance: amount })
)

(define-public (create-bundle (invoice-ids (list 20 uint)) (bundle-price uint))
  (let ((bundle-id (var-get next-bundle-id)))
    (asserts! (> (len invoice-ids) u0) ERR_EMPTY_BUNDLE)
    (asserts! (> bundle-price u0) ERR_INVALID_BUNDLE)
    
    (map-set invoice-bundles
      { bundle-id: bundle-id }
      {
        creator: tx-sender,
        invoice-ids: invoice-ids,
        total-face-value: u0,
        bundle-price: bundle-price,
        is-for-sale: false,
        current-owner: tx-sender,
        created-at: stacks-block-height,
        is-sold: false
      }
    )
    
    (var-set next-bundle-id (+ bundle-id u1))
    (ok bundle-id)
  )
)

(define-public (list-bundle-for-sale (bundle-id uint))
  (let ((bundle (unwrap! (map-get? invoice-bundles { bundle-id: bundle-id }) ERR_BUNDLE_NOT_FOUND)))
    (asserts! (is-eq (get current-owner bundle) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-sold bundle)) ERR_BUNDLE_SOLD)
    
    (map-set invoice-bundles
      { bundle-id: bundle-id }
      (merge bundle { is-for-sale: true })
    )
    (ok true)
  )
)

(define-public (buy-bundle (bundle-id uint))
  (let (
    (bundle (unwrap! (map-get? invoice-bundles { bundle-id: bundle-id }) ERR_BUNDLE_NOT_FOUND))
    (price (get bundle-price bundle))
    (seller (get current-owner bundle))
  )
    (asserts! (get is-for-sale bundle) ERR_BUNDLE_NOT_FOR_SALE)
    (asserts! (not (is-eq tx-sender seller)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-sold bundle)) ERR_BUNDLE_SOLD)
    (asserts! (>= (get-bundle-balance tx-sender) price) ERR_INSUFFICIENT_FUNDS)
    
    (set-bundle-balance tx-sender (- (get-bundle-balance tx-sender) price))
    (set-bundle-balance seller (+ (get-bundle-balance seller) price))
    
    (map-set invoice-bundles
      { bundle-id: bundle-id }
      (merge bundle { current-owner: tx-sender, is-for-sale: false, is-sold: true })
    )
    (ok true)
  )
)

(define-read-only (get-bundle (bundle-id uint))
  (map-get? invoice-bundles { bundle-id: bundle-id })
)

(define-read-only (get-bundle-count)
  (var-get next-bundle-id)
)
