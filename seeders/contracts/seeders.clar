;; ByteSeed - Predictive Flash Loan Arbitrage Protocol
;; A DeFi protocol for prediction-based arbitrage staking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-prediction-exists (err u102))
(define-constant err-prediction-not-found (err u103))
(define-constant err-prediction-expired (err u104))
(define-constant err-already-resolved (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-transfer-failed (err u107))

;; Prediction window in blocks
(define-constant prediction-window u10)
(define-constant min-stake-amount u1000000) ;; 1 STX in micro-STX

;; Data Variables
(define-data-var total-staked uint u0)
(define-data-var insurance-fund uint u0)
(define-data-var prediction-counter uint u0)
(define-data-var total-rewards-distributed uint u0)

;; Data Maps
(define-map predictions
  { prediction-id: uint }
  {
    predictor: principal,
    stake-amount: uint,
    predicted-profit: uint,
    dex-pair: (string-ascii 50),
    created-at: uint,
    resolved: bool,
    successful: bool,
    reward-amount: uint
  }
)

(define-map user-stakes
  { user: principal }
  {
    total-staked: uint,
    active-predictions: uint,
    successful-predictions: uint,
    total-rewards: uint
  }
)

(define-map arbitrage-opportunities
  { opportunity-id: uint }
  {
    dex-pair: (string-ascii 50),
    profit-amount: uint,
    execution-block: uint,
    executed: bool
  }
)

;; Read-only functions
(define-read-only (get-prediction (prediction-id uint))
  (map-get? predictions { prediction-id: prediction-id })
)

(define-read-only (get-user-stakes (user principal))
  (default-to
    { total-staked: u0, active-predictions: u0, successful-predictions: u0, total-rewards: u0 }
    (map-get? user-stakes { user: user })
  )
)

(define-read-only (get-insurance-fund)
  (ok (var-get insurance-fund))
)

(define-read-only (get-total-staked)
  (ok (var-get total-staked))
)

(define-read-only (get-protocol-stats)
  (ok {
    total-staked: (var-get total-staked),
    insurance-fund: (var-get insurance-fund),
    total-predictions: (var-get prediction-counter),
    total-rewards: (var-get total-rewards-distributed)
  })
)

(define-read-only (is-prediction-active (prediction-id uint))
  (match (get-prediction prediction-id)
    prediction
      (let ((current-block block-height))
        (and
          (not (get resolved prediction))
          (< current-block (+ (get created-at prediction) prediction-window))
        )
      )
    false
  )
)

;; Private functions
(define-private (update-user-stakes (user principal) (amount uint) (is-new bool))
  (let (
    (current-stakes (get-user-stakes user))
  )
    (map-set user-stakes
      { user: user }
      {
        total-staked: (if is-new 
          (+ (get total-staked current-stakes) amount)
          (get total-staked current-stakes)
        ),
        active-predictions: (+ (get active-predictions current-stakes) u1),
        successful-predictions: (get successful-predictions current-stakes),
        total-rewards: (get total-rewards current-stakes)
      }
    )
  )
)

;; Public functions
(define-public (create-prediction (predicted-profit uint) (dex-pair (string-ascii 50)) (stake-amount uint))
  (let (
    (prediction-id (+ (var-get prediction-counter) u1))
    (sender tx-sender)
  )
    (asserts! (>= stake-amount min-stake-amount) err-invalid-amount)
    
    ;; Transfer stake from user
    (try! (stx-transfer? stake-amount sender (as-contract tx-sender)))
    
    ;; Create prediction record
    (map-set predictions
      { prediction-id: prediction-id }
      {
        predictor: sender,
        stake-amount: stake-amount,
        predicted-profit: predicted-profit,
        dex-pair: dex-pair,
        created-at: block-height,
        resolved: false,
        successful: false,
        reward-amount: u0
      }
    )
    
    ;; Update counters
    (var-set prediction-counter prediction-id)
    (var-set total-staked (+ (var-get total-staked) stake-amount))
    (update-user-stakes sender stake-amount true)
    
    (ok prediction-id)
  )
)

(define-public (resolve-prediction (prediction-id uint) (actual-profit uint) (successful bool))
  (let (
    (prediction (unwrap! (get-prediction prediction-id) err-prediction-not-found))
    (predictor (get predictor prediction))
    (stake-amount (get stake-amount prediction))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get resolved prediction)) err-already-resolved)
    
    (if successful
      ;; Successful prediction - calculate and distribute rewards
      (let (
        (reward-multiplier u2) ;; 2x multiplier for successful predictions
        (reward-amount (* stake-amount reward-multiplier))
        (current-user-stakes (get-user-stakes predictor))
      )
        ;; Update prediction
        (map-set predictions
          { prediction-id: prediction-id }
          (merge prediction {
            resolved: true,
            successful: true,
            reward-amount: reward-amount
          })
        )
        
        ;; Return stake + rewards
        (try! (as-contract (stx-transfer? reward-amount tx-sender predictor)))
        
        ;; Update user stats
        (map-set user-stakes
          { user: predictor }
          {
            total-staked: (get total-staked current-user-stakes),
            active-predictions: (- (get active-predictions current-user-stakes) u1),
            successful-predictions: (+ (get successful-predictions current-user-stakes) u1),
            total-rewards: (+ (get total-rewards current-user-stakes) reward-amount)
          }
        )
        
        ;; Update protocol stats
        (var-set total-staked (- (var-get total-staked) stake-amount))
        (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) reward-amount))
        
        (ok { success: true, reward: reward-amount })
      )
      ;; Failed prediction - stake goes to insurance fund
      (let (
        (insurance-contribution (/ (* stake-amount u80) u100)) ;; 80% to insurance
        (current-user-stakes (get-user-stakes predictor))
      )
        ;; Update prediction
        (map-set predictions
          { prediction-id: prediction-id }
          (merge prediction {
            resolved: true,
            successful: false,
            reward-amount: u0
          })
        )
        
        ;; Add to insurance fund
        (var-set insurance-fund (+ (var-get insurance-fund) insurance-contribution))
        
        ;; Update user stats
        (map-set user-stakes
          { user: predictor }
          {
            total-staked: (get total-staked current-user-stakes),
            active-predictions: (- (get active-predictions current-user-stakes) u1),
            successful-predictions: (get successful-predictions current-user-stakes),
            total-rewards: (get total-rewards current-user-stakes)
          }
        )
        
        ;; Update protocol stats
        (var-set total-staked (- (var-get total-staked) stake-amount))
        
        (ok { success: false, reward: u0 })
      )
    )
  )
)

(define-public (withdraw-from-insurance (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get insurance-fund)) err-insufficient-balance)
    
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (var-set insurance-fund (- (var-get insurance-fund) amount))
    
    (ok amount)
  )
)

;; Initialize contract
(begin
  (var-set prediction-counter u0)
  (var-set total-staked u0)
  (var-set insurance-fund u0)
  (var-set total-rewards-distributed u0)
)