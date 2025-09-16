(define-trait nft-trait
  (
    (get-last-token-id () (response uint uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    (get-owner (uint) (response (optional principal) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

(define-non-fungible-token ai-model-license uint)

(define-data-var last-model-id uint u0)
(define-data-var last-license-id uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var platform-fee uint u250)
(define-data-var pricing-window uint u1440)

(define-map ai-models
  uint
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    owner: principal,
    price-per-inference: uint,
    total-inferences: uint,
    is-active: bool,
    metadata-uri: (string-ascii 256),
    audit-score: uint,
    base-price: uint,
    min-price: uint,
    max-price: uint,
    last-price-update: uint
  }
)

(define-map model-licenses
  uint
  {
    model-id: uint,
    licensee: principal,
    inferences-purchased: uint,
    inferences-used: uint,
    expires-at: uint,
    is-active: bool
  }
)

(define-map model-audits
  {model-id: uint, auditor: principal}
  {
    score: uint,
    audit-date: uint,
    comments: (string-ascii 256)
  }
)

(define-map user-balances
  principal
  uint
)

(define-map dao-votes
  {model-id: uint, voter: principal}
  {
    vote: bool,
    voting-power: uint
  }
)

(define-map governance-proposals
  uint
  {
    proposer: principal,
    description: (string-ascii 256),
    target-model: uint,
    votes-for: uint,
    votes-against: uint,
    expires-at: uint,
    executed: bool
  }
)

(define-map model-usage-stats
  {model-id: uint, window-start: uint}
  {
    usage-count: uint,
    window-end: uint
  }
)

(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-MODEL-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-FUNDS (err u402))
(define-constant ERR-INVALID-AMOUNT (err u400))
(define-constant ERR-LICENSE-EXPIRED (err u403))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-NOT-OWNER (err u405))

(define-private (get-next-model-id)
  (begin
    (var-set last-model-id (+ (var-get last-model-id) u1))
    (var-get last-model-id)
  )
)

(define-private (get-next-license-id)
  (begin
    (var-set last-license-id (+ (var-get last-license-id) u1))
    (var-get last-license-id)
  )
)

(define-public (register-model 
  (name (string-ascii 64))
  (description (string-ascii 256))
  (price-per-inference uint)
  (metadata-uri (string-ascii 256))
  (min-price uint)
  (max-price uint)
)
  (let
    (
      (model-id (get-next-model-id))
    )
    (asserts! (<= min-price price-per-inference) ERR-INVALID-AMOUNT)
    (asserts! (<= price-per-inference max-price) ERR-INVALID-AMOUNT)
    (map-set ai-models model-id
      {
        name: name,
        description: description,
        owner: tx-sender,
        price-per-inference: price-per-inference,
        total-inferences: u0,
        is-active: true,
        metadata-uri: metadata-uri,
        audit-score: u0,
        base-price: price-per-inference,
        min-price: min-price,
        max-price: max-price,
        last-price-update: stacks-block-height
      }
    )
    (ok model-id)
  )
)

(define-public (purchase-license 
  (model-id uint)
  (inferences-amount uint)
  (payment uint)
)
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
      (total-cost (* (get price-per-inference model) inferences-amount))
      (platform-cut (/ (* total-cost (var-get platform-fee)) u10000))
      (model-owner-cut (- total-cost platform-cut))
      (license-id (get-next-license-id))
      (user-balance (default-to u0 (map-get? user-balances tx-sender)))
    )
    (asserts! (get is-active model) ERR-MODEL-NOT-FOUND)
    (asserts! (>= payment total-cost) ERR-INSUFFICIENT-FUNDS)
    (asserts! (>= user-balance payment) ERR-INSUFFICIENT-FUNDS)
    
    (map-set user-balances tx-sender (- user-balance payment))
    (map-set user-balances (get owner model) 
      (+ (default-to u0 (map-get? user-balances (get owner model))) model-owner-cut))
    (map-set user-balances (var-get contract-owner)
      (+ (default-to u0 (map-get? user-balances (var-get contract-owner))) platform-cut))
    
    (try! (nft-mint? ai-model-license license-id tx-sender))
    
    (map-set model-licenses license-id
      {
        model-id: model-id,
        licensee: tx-sender,
        inferences-purchased: inferences-amount,
        inferences-used: u0,
        expires-at: (+ stacks-block-height u144000),
        is-active: true
      }
    )
    (ok license-id)
)
  )
(define-public (extend-license (license-id uint) (additional-inferences uint) (payment uint))
  (let
    (
      (license (unwrap! (map-get? model-licenses license-id) ERR-MODEL-NOT-FOUND))
      (model-id (get model-id license))
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
      (total-cost (* (get price-per-inference model) additional-inferences))
      (platform-cut (/ (* total-cost (var-get platform-fee)) u10000))
      (model-owner-cut (- total-cost platform-cut))
      (user-balance (default-to u0 (map-get? user-balances tx-sender)))
    )
    (asserts! (is-eq (get licensee license) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active license) ERR-LICENSE-EXPIRED)
    (asserts! (< stacks-block-height (get expires-at license)) ERR-LICENSE-EXPIRED)
    (asserts! (>= payment total-cost) ERR-INSUFFICIENT-FUNDS)
    (asserts! (>= user-balance payment) ERR-INSUFFICIENT-FUNDS)
    (map-set user-balances tx-sender (- user-balance payment))
    (map-set user-balances (get owner model) 
      (+ (default-to u0 (map-get? user-balances (get owner model))) model-owner-cut))
    (map-set user-balances (var-get contract-owner)
      (+ (default-to u0 (map-get? user-balances (var-get contract-owner))) platform-cut))
    (map-set model-licenses license-id
      (merge license {inferences-purchased: (+ (get inferences-purchased license) additional-inferences)})
    )
    (ok true)
  )
)

(define-public (use-inference (license-id uint))
  (let
    (
      (license (unwrap! (map-get? model-licenses license-id) ERR-MODEL-NOT-FOUND))
      (model-id (get model-id license))
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
    )
    (asserts! (is-eq (get licensee license) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active license) ERR-LICENSE-EXPIRED)
    (asserts! (< stacks-block-height (get expires-at license)) ERR-LICENSE-EXPIRED)
    (asserts! (< (get inferences-used license) (get inferences-purchased license)) ERR-INSUFFICIENT-FUNDS)
    
    (map-set model-licenses license-id
      (merge license {inferences-used: (+ (get inferences-used license) u1)})
    )
    
    (map-set ai-models model-id
      (merge model {total-inferences: (+ (get total-inferences model) u1)})
    )
    (unwrap-panic (update-usage-stats model-id))
    (ok true)
  )
)

(define-public (audit-model 
  (model-id uint)
  (score uint)
  (comments (string-ascii 256))
)
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
    )
    (asserts! (<= score u100) ERR-INVALID-AMOUNT)
    (asserts! (is-some (map-get? user-balances tx-sender)) ERR-NOT-AUTHORIZED)
    
    (map-set model-audits {model-id: model-id, auditor: tx-sender}
      {
        score: score,
        audit-date: stacks-block-height,
        comments: comments
      }
    )
    (ok true)
  )
)

(define-public (deposit-funds (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-balances tx-sender 
      (+ (default-to u0 (map-get? user-balances tx-sender)) amount))
    (ok amount)
  )
)

(define-public (withdraw-funds (amount uint))
  (let
    (
      (user-balance (default-to u0 (map-get? user-balances tx-sender)))
    )
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set user-balances tx-sender (- user-balance amount))
    (ok amount)
  )
)

(define-public (vote-on-model 
  (model-id uint)
  (vote bool)
  (voting-power uint)
)
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
      (user-balance (default-to u0 (map-get? user-balances tx-sender)))
    )
    (asserts! (>= user-balance voting-power) ERR-INSUFFICIENT-FUNDS)
    
    (map-set dao-votes {model-id: model-id, voter: tx-sender}
      {
        vote: vote,
        voting-power: voting-power
      }
    )
    (try! (update-dynamic-pricing model-id))
    (ok true)
  )
)

(define-public (deactivate-model (model-id uint))
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
    )
    (asserts! (is-eq (get owner model) tx-sender) ERR-NOT-OWNER)
    (map-set ai-models model-id (merge model {is-active: false}))
    (ok true)
  )
)

(define-read-only (get-model (model-id uint))
  (map-get? ai-models model-id)
)

(define-read-only (get-license (license-id uint))
  (map-get? model-licenses license-id)
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-model-audit (model-id uint) (auditor principal))
  (map-get? model-audits {model-id: model-id, auditor: auditor})
)

(define-read-only (get-last-token-id)
  (ok (var-get last-license-id))
)

(define-read-only (get-token-uri (token-id uint))
  (ok none)
)

(define-read-only (get-owner (token-id uint))
  (let
    (
      (license (map-get? model-licenses token-id))
    )
    (match license
      license-data (ok (some (get licensee license-data)))
      (ok none)
    )
  )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (let
    (
      (license (unwrap! (map-get? model-licenses token-id) ERR-MODEL-NOT-FOUND))
    )
    (asserts! (is-eq sender tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get licensee license) sender) ERR-NOT-AUTHORIZED)
    
    (try! (nft-transfer? ai-model-license token-id sender recipient))
    (map-set model-licenses token-id (merge license {licensee: recipient}))
    (ok true)
  )
)

(define-private (get-window-start (current-block uint))
  (let
    (
      (window-size (var-get pricing-window))
    )
    (- current-block (mod current-block window-size))
  )
)

(define-private (update-usage-stats (model-id uint))
  (let
    (
      (current-block stacks-block-height)
      (window-start (get-window-start current-block))
      (window-end (+ window-start (var-get pricing-window)))
      (current-stats (map-get? model-usage-stats {model-id: model-id, window-start: window-start}))
    )
    (match current-stats
      stats (map-set model-usage-stats {model-id: model-id, window-start: window-start}
        {
          usage-count: (+ (get usage-count stats) u1),
          window-end: window-end
        })
      (map-set model-usage-stats {model-id: model-id, window-start: window-start}
        {
          usage-count: u1,
          window-end: window-end
        })
    )
    (ok true)
  )
)

(define-private (calculate-demand-multiplier (usage-count uint))
  (if (<= usage-count u10)
    u80
    (if (<= usage-count u50)
      u90
      (if (<= usage-count u100)
        u100
        (if (<= usage-count u200)
          u110
          (if (<= usage-count u500)
            u120
            (if (<= usage-count u1000)
              u150
              u200
            )
          )
        )
      )
    )
  )
)

(define-private (update-dynamic-pricing (model-id uint))
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
      (current-block stacks-block-height)
      (window-start (get-window-start current-block))
      (usage-stats (map-get? model-usage-stats {model-id: model-id, window-start: window-start}))
      (usage-count (match usage-stats stats (get usage-count stats) u0))
      (demand-multiplier (calculate-demand-multiplier usage-count))
      (base-price (get base-price model))
      (new-price (/ (* base-price demand-multiplier) u100))
      (min-price (get min-price model))
      (max-price (get max-price model))
      (final-price (if (< new-price min-price) 
                      min-price 
                      (if (> new-price max-price) 
                          max-price 
                          new-price)))
    )
    (map-set ai-models model-id
      (merge model 
        {
          price-per-inference: final-price,
          last-price-update: current-block
        }
      )
    )
    (ok final-price)
  )
)

(define-public (manual-price-update (model-id uint))
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
    )
    (asserts! (is-eq (get owner model) tx-sender) ERR-NOT-OWNER)
    (update-dynamic-pricing model-id)
  )
)

(define-read-only (get-current-price (model-id uint))
  (match (map-get? ai-models model-id)
    model (ok (get price-per-inference model))
    ERR-MODEL-NOT-FOUND
  )
)

(define-read-only (get-usage-stats (model-id uint))
  (let
    (
      (current-block stacks-block-height)
      (window-start (get-window-start current-block))
    )
    (map-get? model-usage-stats {model-id: model-id, window-start: window-start})
  )
)

(define-read-only (get-pricing-info (model-id uint))
  (match (map-get? ai-models model-id)
    model (ok {
      current-price: (get price-per-inference model),
      base-price: (get base-price model),
      min-price: (get min-price model),
      max-price: (get max-price model),
      last-update: (get last-price-update model)
    })
    ERR-MODEL-NOT-FOUND
  )
)
