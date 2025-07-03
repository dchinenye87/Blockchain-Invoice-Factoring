(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVOICE_NOT_FOUND (err u101))
(define-constant ERR_INVOICE_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_INVOICE_NOT_FOR_SALE (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_INVOICE_EXPIRED (err u106))
(define-constant ERR_CANNOT_BUY_OWN_INVOICE (err u107))
(define-constant ERR_INVOICE_ALREADY_PAID (err u108))
(define-constant ERR_INVALID_SCORE (err u109))

(define-data-var next-invoice-id uint u1)
(define-data-var platform-fee-rate uint u250)

(define-map invoices
  { invoice-id: uint }
  {
    issuer: principal,
    debtor: principal,
    amount: uint,
    due-date: uint,
    created-at: uint,
    is-for-sale: bool,
    sale-price: uint,
    current-owner: principal,
    is-paid: bool,
    description: (string-ascii 256)
  }
)

(define-map invoice-offers
  { invoice-id: uint, buyer: principal }
  {
    offer-amount: uint,
    expires-at: uint
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-private (get-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

(define-private (set-balance (user principal) (amount uint))
  (map-set user-balances { user: user } { balance: amount })
)

(define-private (add-to-balance (user principal) (amount uint))
  (let ((current-balance (get-balance user)))
    (set-balance user (+ current-balance amount))
  )
)

(define-private (subtract-from-balance (user principal) (amount uint))
  (let ((current-balance (get-balance user)))
    (if (>= current-balance amount)
      (begin
        (set-balance user (- current-balance amount))
        (ok true)
      )
      ERR_INSUFFICIENT_FUNDS
    )
  )
)

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

(define-public (deposit (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (add-to-balance tx-sender amount)
    (ok amount)
  )
)

(define-public (withdraw (amount uint))
  (begin
    (try! (subtract-from-balance tx-sender amount))
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (ok amount)
  )
)

(define-public (create-invoice (debtor principal) (amount uint) (due-date uint) (description (string-ascii 256)))
  (let ((invoice-id (var-get next-invoice-id)))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> due-date stacks-block-height) ERR_INVOICE_EXPIRED)
    (asserts! (is-none (map-get? invoices { invoice-id: invoice-id })) ERR_INVOICE_ALREADY_EXISTS)
    
    (map-set invoices
      { invoice-id: invoice-id }
      {
        issuer: tx-sender,
        debtor: debtor,
        amount: amount,
        due-date: due-date,
        created-at: stacks-block-height,
        is-for-sale: false,
        sale-price: u0,
        current-owner: tx-sender,
        is-paid: false,
        description: description
      }
    )
    
    (var-set next-invoice-id (+ invoice-id u1))
    (ok invoice-id)
  )
)

(define-public (list-invoice-for-sale (invoice-id uint) (sale-price uint))
  (let ((invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (is-eq (get current-owner invoice) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (> sale-price u0) ERR_INVALID_AMOUNT)
    (asserts! (> (get due-date invoice) stacks-block-height) ERR_INVOICE_EXPIRED)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { is-for-sale: true, sale-price: sale-price })
    )
    (ok true)
  )
)

(define-public (remove-invoice-from-sale (invoice-id uint))
  (let ((invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (is-eq (get current-owner invoice) tx-sender) ERR_NOT_AUTHORIZED)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { is-for-sale: false, sale-price: u0 })
    )
    (ok true)
  )
)

(define-public (buy-invoice (invoice-id uint))
  (let (
    (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
    (sale-price (get sale-price invoice))
    (current-owner (get current-owner invoice))
    (platform-fee (calculate-platform-fee sale-price))
    (seller-amount (- sale-price platform-fee))
  )
    (asserts! (get is-for-sale invoice) ERR_INVOICE_NOT_FOR_SALE)
    (asserts! (not (is-eq tx-sender current-owner)) ERR_CANNOT_BUY_OWN_INVOICE)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (> (get due-date invoice) stacks-block-height) ERR_INVOICE_EXPIRED)
    
    (try! (subtract-from-balance tx-sender sale-price))
    (add-to-balance current-owner seller-amount)
    (add-to-balance CONTRACT_OWNER platform-fee)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice {
        current-owner: tx-sender,
        is-for-sale: false,
        sale-price: u0
      })
    )
    (ok true)
  )
)

(define-public (make-offer (invoice-id uint) (offer-amount uint) (expires-at uint))
  (let ((invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (> offer-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> expires-at stacks-block-height) ERR_INVOICE_EXPIRED)
    (asserts! (not (is-eq tx-sender (get current-owner invoice))) ERR_CANNOT_BUY_OWN_INVOICE)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    
    (map-set invoice-offers
      { invoice-id: invoice-id, buyer: tx-sender }
      { offer-amount: offer-amount, expires-at: expires-at }
    )
    (ok true)
  )
)

(define-public (accept-offer (invoice-id uint) (buyer principal))
  (let (
    (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
    (offer (unwrap! (map-get? invoice-offers { invoice-id: invoice-id, buyer: buyer }) ERR_INVOICE_NOT_FOUND))
    (offer-amount (get offer-amount offer))
    (platform-fee (calculate-platform-fee offer-amount))
    (seller-amount (- offer-amount platform-fee))
  )
    (asserts! (is-eq (get current-owner invoice) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (> (get expires-at offer) stacks-block-height) ERR_INVOICE_EXPIRED)
    
    (try! (subtract-from-balance buyer offer-amount))
    (add-to-balance tx-sender seller-amount)
    (add-to-balance CONTRACT_OWNER platform-fee)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice {
        current-owner: buyer,
        is-for-sale: false,
        sale-price: u0
      })
    )
    
    (map-delete invoice-offers { invoice-id: invoice-id, buyer: buyer })
    (ok true)
  )
)

(define-public (pay-invoice (invoice-id uint))
  (let ((invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get debtor invoice)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    
    (try! (subtract-from-balance tx-sender (get amount invoice)))
    (add-to-balance (get current-owner invoice) (get amount invoice))
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { is-paid: true, is-for-sale: false })
    )
    (ok true)
  )
)

(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices { invoice-id: invoice-id })
)

(define-read-only (get-user-balance (user principal))
  (get-balance user)
)

(define-read-only (get-offer (invoice-id uint) (buyer principal))
  (map-get? invoice-offers { invoice-id: invoice-id, buyer: buyer })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-next-invoice-id)
  (var-get next-invoice-id)
)


(define-map credit-scores
  { user: principal }
  {
    score: uint,
    total-invoices: uint,
    paid-on-time: uint,
    late-payments: uint,
    defaults: uint,
    last-updated: uint
  }
)

(define-map payment-history
  { user: principal, invoice-id: uint }
  {
    payment-status: uint,
    payment-block: uint
  }
)

(define-private (get-credit-score (user principal))
  (default-to 
    { score: u750, total-invoices: u0, paid-on-time: u0, late-payments: u0, defaults: u0, last-updated: u0 }
    (map-get? credit-scores { user: user })
  )
)

(define-private (calculate-new-score (current-score uint) (total-invoices uint) (paid-on-time uint) (late-payments uint) (defaults uint))
  (let (
    (payment-rate (if (> total-invoices u0) (/ (* paid-on-time u100) total-invoices) u0))
    (default-rate (if (> total-invoices u0) (/ (* defaults u100) total-invoices) u0))
  )
    (if (<= total-invoices u5)
      u750
      (+ u300 
         (/ (* payment-rate u4) u1)
         (if (is-eq defaults u0) u150 u0)
         (if (<= default-rate u5) u100 u0)
      )
    )
  )
)

(define-private (update-credit-score (user principal) (payment-type uint))
  (let (
    (current-data (get-credit-score user))
    (new-total (+ (get total-invoices current-data) u1))
    (new-paid (if (is-eq payment-type u1) (+ (get paid-on-time current-data) u1) (get paid-on-time current-data)))
    (new-late (if (is-eq payment-type u2) (+ (get late-payments current-data) u1) (get late-payments current-data)))
    (new-defaults (if (is-eq payment-type u3) (+ (get defaults current-data) u1) (get defaults current-data)))
    (new-score (calculate-new-score (get score current-data) new-total new-paid new-late new-defaults))
  )
    (map-set credit-scores
      { user: user }
      {
        score: new-score,
        total-invoices: new-total,
        paid-on-time: new-paid,
        late-payments: new-late,
        defaults: new-defaults,
        last-updated: stacks-block-height
      }
    )
  )
)

(define-public (record-payment (invoice-id uint) (debtor principal) (payment-type uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= payment-type u3) ERR_INVALID_SCORE)
    
    (map-set payment-history
      { user: debtor, invoice-id: invoice-id }
      { payment-status: payment-type, payment-block: stacks-block-height }
    )
    
    (update-credit-score debtor payment-type)
    (ok true)
  )
)

(define-public (mark-invoice-default (invoice-id uint))
  (let ((invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (< (get due-date invoice) stacks-block-height) ERR_INVOICE_NOT_FOUND)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    
    (try! (record-payment invoice-id (get debtor invoice) u3))
    (ok true)
  )
)

(define-read-only (get-user-credit-score (user principal))
  (get-credit-score user)
)

(define-read-only (get-credit-rating (user principal))
  (let ((score (get score (get-credit-score user))))
    (if (>= score u800) "AAA"
    (if (>= score u750) "AA"
    (if (>= score u700) "A"
    (if (>= score u650) "BBB"
    (if (>= score u600) "BB"
    (if (>= score u550) "B"
    "C"))))))
  )
  )