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

(define-map model-feedback
  {model-id: uint, user: principal}
  {
    rating: uint,
    comment: (string-ascii 256),
    submitted-at: uint
  }
)

(define-map user-reputation
  principal
  uint
)

(define-map user-stakes
  principal
  {
    staked-amount: uint,
    staked-at: uint
  }
)

(define-data-var total-staked uint u0)
(define-data-var total-fees-collected uint u0)
(define-data-var referral-reward-percentage uint u500)

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

    (distribute-staking-rewards platform-cut)

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
    (distribute-staking-rewards platform-cut)
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
      (current-rep (default-to u0 (map-get? user-reputation tx-sender)))
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
    (map-set user-reputation tx-sender (+ current-rep u2))
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

(define-public (stake-funds (amount uint))
  (let
    (
      (user-balance (default-to u0 (map-get? user-balances tx-sender)))
      (current-stake (default-to {staked-amount: u0, staked-at: u0} (map-get? user-stakes tx-sender)))
      (new-staked-amount (+ (get staked-amount current-stake) amount))
    )
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-FUNDS)
    (map-set user-balances tx-sender (- user-balance amount))
    (map-set user-stakes tx-sender
      {
        staked-amount: new-staked-amount,
        staked-at: stacks-block-height
      }
    )
    (var-set total-staked (+ (var-get total-staked) amount))
    (ok new-staked-amount)
  )
)

(define-public (unstake-funds (amount uint))
  (let
    (
      (current-stake (unwrap! (map-get? user-stakes tx-sender) ERR-INSUFFICIENT-FUNDS))
      (staked-amount (get staked-amount current-stake))
    )
    (asserts! (>= staked-amount amount) ERR-INSUFFICIENT-FUNDS)
    (map-set user-balances tx-sender (+ (default-to u0 (map-get? user-balances tx-sender)) amount))
    (if (is-eq (- staked-amount amount) u0)
      (map-delete user-stakes tx-sender)
      (map-set user-stakes tx-sender
        (merge current-stake {staked-amount: (- staked-amount amount)})
      )
    )
    (var-set total-staked (- (var-get total-staked) amount))
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

(define-private (distribute-staking-rewards (fee-amount uint))
  (let
    (
      (total-staked-amount (var-get total-staked))
    )
    (if (> total-staked-amount u0)
      (let
        (
          (reward-per-stake (/ fee-amount total-staked-amount))
        )
        (var-set total-fees-collected (+ (var-get total-fees-collected) fee-amount))
        true
      )
      true
    )
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

(define-public (submit-feedback
  (model-id uint)
  (rating uint)
  (comment (string-ascii 256))
)
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
      (current-rep (default-to u0 (map-get? user-reputation tx-sender)))
    )
    (asserts! (<= rating u5) ERR-INVALID-AMOUNT)
    (asserts! (is-some (map-get? user-balances tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set model-feedback {model-id: model-id, user: tx-sender}
      {
        rating: rating,
        comment: comment,
        submitted-at: stacks-block-height
      }
    )
    (map-set user-reputation tx-sender (+ current-rep u1))
    (ok true)
  )
)

(define-read-only (get-feedback (model-id uint) (user principal))
  (map-get? model-feedback {model-id: model-id, user: user})
)

(define-read-only (get-average-rating (model-id uint))
  (let
    (
      (feedbacks (list))
      (total-rating u0)
      (count u0)
    )
    (ok {
      average-rating: (if (> count u0) (/ total-rating count) u0),
      total-feedbacks: count
    })
  )
)

(define-read-only (get-staking-info (user principal))
  (map-get? user-stakes user)
)

(define-read-only (get-total-staked)
  (var-get total-staked)
)

(define-read-only (get-total-fees-collected)
  (var-get total-fees-collected)
)

(define-data-var last-subscription-id uint u0)

(define-map model-subscriptions
  uint
  {
    model-id: uint,
    subscriber: principal,
    start-block: uint,
    end-block: uint,
    fee-paid: uint
  }
)

(define-data-var last-version-id uint u0)

(define-map model-versions
  {model-id: uint, version-id: uint}
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    metadata-uri: (string-ascii 256),
    created-at: uint,
    is-active: bool
  }
)

(define-map license-versions
  uint
  uint
)

(define-map user-referrers
  principal
  principal
)

(define-public (set-referrer (referrer principal))
  (begin
    (asserts! (not (is-eq tx-sender referrer)) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? user-referrers tx-sender)) ERR-ALREADY-EXISTS)
    (map-set user-referrers tx-sender referrer)
    (ok true)
  )
)

(define-read-only (get-referrer (user principal))
  (map-get? user-referrers user)
)

(define-private (get-next-subscription-id)
  (begin
    (var-set last-subscription-id (+ (var-get last-subscription-id) u1))
    (var-get last-subscription-id)
  )
)

(define-public (subscribe-to-model (model-id uint) (duration-blocks uint) (payment uint))
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
      (subscription-fee (* (get price-per-inference model) u1000))
      (user-balance (default-to u0 (map-get? user-balances tx-sender)))
      (subscription-id (get-next-subscription-id))
      (start-block stacks-block-height)
      (end-block (+ start-block duration-blocks))
    )
    (asserts! (get is-active model) ERR-MODEL-NOT-FOUND)
    (asserts! (>= payment subscription-fee) ERR-INSUFFICIENT-FUNDS)
    (asserts! (>= user-balance payment) ERR-INSUFFICIENT-FUNDS)
    (map-set user-balances tx-sender (- user-balance payment))
    (map-set user-balances (get owner model) (+ (default-to u0 (map-get? user-balances (get owner model))) payment))
    (map-set model-subscriptions subscription-id
      {
        model-id: model-id,
        subscriber: tx-sender,
        start-block: start-block,
        end-block: end-block,
        fee-paid: payment
      }
    )
    (ok subscription-id)
  )
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? model-subscriptions subscription-id)
)

(define-public (use-inference-with-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? model-subscriptions subscription-id) ERR-MODEL-NOT-FOUND))
      (model-id (get model-id subscription))
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
    )
    (asserts! (is-eq (get subscriber subscription) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= stacks-block-height (get end-block subscription)) ERR-LICENSE-EXPIRED)
    (asserts! (get is-active model) ERR-MODEL-NOT-FOUND)
    (map-set ai-models model-id
      (merge model {total-inferences: (+ (get total-inferences model) u1)})
    )
    (unwrap-panic (update-usage-stats model-id))
    (ok true)
  )
)

(define-private (get-next-version-id)
  (begin
    (var-set last-version-id (+ (var-get last-version-id) u1))
    (var-get last-version-id)
  )
)

(define-public (register-model-version
  (model-id uint)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (metadata-uri (string-ascii 256))
)
  (let
    (
      (model (unwrap! (map-get? ai-models model-id) ERR-MODEL-NOT-FOUND))
      (version-id (get-next-version-id))
    )
    (asserts! (is-eq (get owner model) tx-sender) ERR-NOT-OWNER)
    (asserts! (get is-active model) ERR-MODEL-NOT-FOUND)
    (map-set model-versions {model-id: model-id, version-id: version-id}
      {
        name: name,
        description: description,
        metadata-uri: metadata-uri,
        created-at: stacks-block-height,
        is-active: true
      }
    )
    (ok version-id)
  )
)

(define-public (upgrade-license-to-version (license-id uint) (version-id uint))
  (let
    (
      (license (unwrap! (map-get? model-licenses license-id) ERR-MODEL-NOT-FOUND))
      (model-id (get model-id license))
      (version (unwrap! (map-get? model-versions {model-id: model-id, version-id: version-id}) ERR-MODEL-NOT-FOUND))
    )
    (asserts! (is-eq (get licensee license) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active license) ERR-LICENSE-EXPIRED)
    (asserts! (get is-active version) ERR-MODEL-NOT-FOUND)
    (map-set license-versions license-id version-id)
    (ok true)
  )
)

(define-read-only (get-model-version (model-id uint) (version-id uint))
  (map-get? model-versions {model-id: model-id, version-id: version-id})
)

(define-read-only (get-license-version (license-id uint))
  (map-get? license-versions license-id)
)
