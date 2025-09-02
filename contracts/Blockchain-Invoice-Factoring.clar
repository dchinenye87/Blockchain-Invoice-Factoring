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

(define-constant ERR_DISPUTE_NOT_FOUND (err u110))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u111))
(define-constant ERR_DISPUTE_ALREADY_RESOLVED (err u112))
(define-constant ERR_INVALID_DISPUTE_STATUS (err u113))

(define-constant ERR_INVALID_RISK_PARAMS (err u200))
(define-constant ERR_RISK_ASSESSMENT_EXISTS (err u201))

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

(define-map invoice-disputes
  { invoice-id: uint }
  {
    complainant: principal,
    respondent: principal,
    reason: (string-ascii 128),
    status: uint,
    created-at: uint,
    resolved-at: uint,
    resolution: (string-ascii 256),
    evidence-hash: (buff 32)
  }
)

(define-public (raise-dispute (invoice-id uint) (reason (string-ascii 128)) (evidence-hash (buff 32)))
  (let ((invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (is-none (map-get? invoice-disputes { invoice-id: invoice-id })) ERR_DISPUTE_ALREADY_EXISTS)
    (asserts! (or (is-eq tx-sender (get current-owner invoice)) 
                  (is-eq tx-sender (get debtor invoice))) ERR_NOT_AUTHORIZED)
    
    (map-set invoice-disputes
      { invoice-id: invoice-id }
      {
        complainant: tx-sender,
        respondent: (if (is-eq tx-sender (get current-owner invoice)) (get debtor invoice) (get current-owner invoice)),
        reason: reason,
        status: u1,
        created-at: stacks-block-height,
        resolved-at: u0,
        resolution: "",
        evidence-hash: evidence-hash
      }
    )
    (ok true)
  )
)

(define-public (resolve-dispute (invoice-id uint) (resolution (string-ascii 256)) (refund-to-buyer bool))
  (let (
    (dispute (unwrap! (map-get? invoice-disputes { invoice-id: invoice-id }) ERR_DISPUTE_NOT_FOUND))
    (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute) u1) ERR_DISPUTE_ALREADY_RESOLVED)
    
    (if refund-to-buyer
      (begin
        (add-to-balance (get complainant dispute) (get amount invoice))
        (try! (subtract-from-balance (get respondent dispute) (get amount invoice)))
      )
      true
    )
    
    (map-set invoice-disputes
      { invoice-id: invoice-id }
      (merge dispute {
        status: u2,
        resolved-at: stacks-block-height,
        resolution: resolution
      })
    )
    (ok true)
  )
)

(define-read-only (get-dispute (invoice-id uint))
  (map-get? invoice-disputes { invoice-id: invoice-id })
)


(define-map invoice-risk-assessments
  { invoice-id: uint }
  {
    risk-score: uint,
    days-to-maturity: uint,
    debtor-credit-score: uint,
    invoice-amount-tier: uint,
    industry-risk-factor: uint,
    suggested-discount: uint,
    assessment-timestamp: uint
  }
)

(define-map industry-risk-factors
  { industry-code: uint }
  { risk-multiplier: uint }
)

(define-private (get-amount-tier (amount uint))
  (if (<= amount u100000) u1
  (if (<= amount u500000) u2
  (if (<= amount u1000000) u3
  (if (<= amount u5000000) u4
  u5))))
)

(define-private (calculate-days-to-maturity (due-date uint))
  (if (> due-date stacks-block-height)
    (- due-date stacks-block-height)
    u0)
)

(define-private (calculate-risk-score (debtor-score uint) (days uint) (amount-tier uint) (industry-factor uint))
  (let (
    (time-risk (if (< days u50) u300 (if (< days u100) u200 u100)))
    (credit-risk (- u1000 debtor-score))
    (amount-risk (* amount-tier u50))
    (industry-risk (* industry-factor u10))
  )
    (+ time-risk credit-risk amount-risk industry-risk)
  )
)

(define-private (calculate-suggested-discount (risk-score uint))
  (if (< risk-score u300) u5
  (if (< risk-score u500) u10
  (if (< risk-score u700) u15
  (if (< risk-score u900) u25
  u35))))
)

(define-public (set-industry-risk (industry-code uint) (risk-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= risk-multiplier u100) ERR_INVALID_RISK_PARAMS)
    (map-set industry-risk-factors { industry-code: industry-code } { risk-multiplier: risk-multiplier })
    (ok true)
  )
)

(define-public (assess-invoice-risk (invoice-id uint) (industry-code uint))
  (let (
    (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
    (debtor-credit (get-credit-score (get debtor invoice)))
    (days-to-maturity (calculate-days-to-maturity (get due-date invoice)))
    (amount-tier (get-amount-tier (get amount invoice)))
    (industry-factor (default-to u5 (get risk-multiplier (map-get? industry-risk-factors { industry-code: industry-code }))))
    (risk-score (calculate-risk-score (get score debtor-credit) days-to-maturity amount-tier industry-factor))
    (suggested-discount (calculate-suggested-discount risk-score))
  )
    (asserts! (is-eq tx-sender (get current-owner invoice)) ERR_NOT_AUTHORIZED)
    (asserts! (is-none (map-get? invoice-risk-assessments { invoice-id: invoice-id })) ERR_RISK_ASSESSMENT_EXISTS)
    
    (map-set invoice-risk-assessments
      { invoice-id: invoice-id }
      {
        risk-score: risk-score,
        days-to-maturity: days-to-maturity,
        debtor-credit-score: (get score debtor-credit),
        invoice-amount-tier: amount-tier,
        industry-risk-factor: industry-factor,
        suggested-discount: suggested-discount,
        assessment-timestamp: stacks-block-height
      }
    )
    (ok { risk-score: risk-score, suggested-discount: suggested-discount })
  )
)

(define-read-only (get-risk-assessment (invoice-id uint))
  (map-get? invoice-risk-assessments { invoice-id: invoice-id })
)

(define-read-only (get-pricing-recommendation (invoice-id uint))
  (match (map-get? invoice-risk-assessments { invoice-id: invoice-id })
    assessment (let (
      (invoice (unwrap-panic (map-get? invoices { invoice-id: invoice-id })))
      (discount-rate (get suggested-discount assessment))
      (face-value (get amount invoice))
      (suggested-price (- face-value (/ (* face-value discount-rate) u100)))
    )
      (some { 
        face-value: face-value,
        suggested-price: suggested-price,
        discount-rate: discount-rate,
        risk-score: (get risk-score assessment)
      })
    )
    none
  )
)

(define-read-only (get-industry-risk-factor (industry-code uint))
  (map-get? industry-risk-factors { industry-code: industry-code })
)