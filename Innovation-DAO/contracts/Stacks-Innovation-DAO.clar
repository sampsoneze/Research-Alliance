;; ScienceFund DAO - Decentralized Research Funding Platform
;; A comprehensive smart contract for democratizing scientific research funding through 
;; decentralized governance, milestone-based funding, and community-driven proposal evaluation

;; ERROR CONSTANTS - All validation and authorization errors
(define-constant ERR-OWNER-ONLY-ACCESS (err u100))
(define-constant ERR-RESOURCE-NOT-FOUND (err u101))
(define-constant ERR-UNAUTHORIZED-ACCESS (err u102))
(define-constant ERR-INVALID-FUNDING-AMOUNT (err u103))
(define-constant ERR-VOTING-PERIOD-EXPIRED (err u104))
(define-constant ERR-VOTING-STILL-ACTIVE (err u105))
(define-constant ERR-DUPLICATE-VOTE-ATTEMPT (err u106))
(define-constant ERR-INSUFFICIENT-TREASURY-FUNDS (err u107))
(define-constant ERR-PROPOSAL-NOT-IN-FUNDED-STATUS (err u108))
(define-constant ERR-MILESTONE-DOES-NOT-EXIST (err u109))
(define-constant ERR-MILESTONE-ALREADY-MARKED-COMPLETE (err u110))
(define-constant ERR-INVALID-PROPOSAL-PARAMETERS (err u111))
(define-constant ERR-MEMBER-ALREADY-EXISTS (err u112))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u113))
(define-constant ERR-INVALID-INPUT-DATA (err u114))

;; CONFIGURATION CONSTANTS
(define-constant contract-administrator tx-sender)
(define-constant minimum-stx-deposit-for-proposals u1000000) ;; 1 STX in microSTX
(define-constant default-proposal-voting-duration u1440) ;; ~10 days in blocks
(define-constant required-quorum-percentage u51) ;; 51% participation required
(define-constant reputation-bonus-for-voting u10)
(define-constant reputation-bonus-for-milestone-completion u50)
(define-constant reputation-bonus-for-researcher-verification u100)
(define-constant base-voting-power u1)
(define-constant reputation-to-voting-power-ratio u100)

;; STATE VARIABLES - Global contract state
(define-data-var current-proposal-counter uint u1)
(define-data-var total-dao-treasury-balance uint u0)
(define-data-var active-proposal-deposit-requirement uint minimum-stx-deposit-for-proposals)
(define-data-var current-voting-duration-blocks uint default-proposal-voting-duration)
(define-data-var minimum-quorum-participation-rate uint required-quorum-percentage)

;; DATA STRUCTURES - Core contract data maps
;; Research funding proposal registry
(define-map research-funding-proposals 
  uint 
  {
    proposal-submitter: principal,
    research-project-title: (string-ascii 100),
    detailed-research-description: (string-ascii 500),
    total-requested-funding: uint,
    scientific-research-category: (string-ascii 50),
    proposal-submission-block: uint,
    voting-deadline-block: uint,
    accumulated-yes-votes: uint,
    accumulated-no-votes: uint,
    unique-voters-count: uint,
    current-proposal-status: (string-ascii 20), ;; "active", "passed", "rejected", "funded", "completed"
    completed-milestones-count: uint,
    planned-total-milestones: uint
  }
)

;; Individual voting records for accountability
(define-map community-member-votes 
  {proposal-identifier: uint, voting-member: principal} 
  {member-vote-choice: bool, applied-voting-weight: uint}
)

;; DAO community membership registry
(define-map dao-community-members 
  principal 
  {
    accumulated-reputation-score: uint,
    lifetime-financial-contributions: uint,
    membership-registration-block: uint,
    verified-researcher-status: bool
  }
)

;; Project milestone tracking system
(define-map research-project-milestones
  {parent-proposal-id: uint, milestone-sequence-number: uint}
  {
    milestone-description-text: (string-ascii 200),
    allocated-milestone-funding: uint,
    milestone-completion-status: bool,
    completion-timestamp-block: uint
  }
)

;; Verified researcher profile database
(define-map verified-researcher-profiles
  principal
  {
    researcher-full-name: (string-ascii 50),
    affiliated-institution: (string-ascii 100),
    primary-research-domains: (string-ascii 200),
    published-papers-count: uint,
    institution-verification-status: bool
  }
)

;; INPUT VALIDATION FUNCTIONS - Security and data integrity checks
(define-private (validate-string-input (input (string-ascii 500)))
  (let ((input-length (len input)))
    (and 
      (> input-length u0)
      (< input-length u501)
      ;; Check for basic printable characters (ASCII 32-126)
      (not (is-eq input ""))
    )
  )
)

(define-private (validate-short-string (input (string-ascii 100)))
  (let ((input-length (len input)))
    (and 
      (> input-length u0)
      (< input-length u101)
      (not (is-eq input ""))
    )
  )
)

(define-private (validate-medium-string (input (string-ascii 200)))
  (let ((input-length (len input)))
    (and 
      (> input-length u0)
      (< input-length u201)
      (not (is-eq input ""))
    )
  )
)

(define-private (validate-name-string (input (string-ascii 50)))
  (let ((input-length (len input)))
    (and 
      (> input-length u0)
      (< input-length u51)
      (not (is-eq input ""))
    )
  )
)

(define-private (validate-uint-input (input uint))
  (and 
    (>= input u0)
    (<= input u340282366920938463463374607431768211455) ;; Max uint value
  )
)

(define-private (validate-milestone-number (milestone-num uint) (total-milestones uint))
  (and 
    (> milestone-num u0)
    (<= milestone-num total-milestones)
    (validate-uint-input milestone-num)
  )
)

(define-private (validate-principal-input (input principal))
  (not (is-eq input 'SP000000000000000000002Q6VF78)) ;; Not null principal
)

;; READ-ONLY QUERY FUNCTIONS - Data retrieval without state modification
(define-read-only (fetch-research-proposal-details (proposal-identifier uint))
  (map-get? research-funding-proposals proposal-identifier)
)

(define-read-only (fetch-community-member-profile (member-address principal))
  (map-get? dao-community-members member-address)
)

(define-read-only (fetch-member-vote-record (proposal-identifier uint) (voter-address principal))
  (map-get? community-member-votes {proposal-identifier: proposal-identifier, voting-member: voter-address})
)

(define-read-only (get-current-treasury-balance)
  (var-get total-dao-treasury-balance)
)

(define-read-only (get-active-voting-period-duration)
  (var-get current-voting-duration-blocks)
)

(define-read-only (get-required-quorum-threshold)
  (var-get minimum-quorum-participation-rate)
)

(define-read-only (calculate-member-voting-influence (member-address principal))
  (let ((member-profile (unwrap! (map-get? dao-community-members member-address) u0)))
    (+ base-voting-power (/ (get accumulated-reputation-score member-profile) reputation-to-voting-power-ratio))
  )
)

(define-read-only (fetch-project-milestone-details (proposal-identifier uint) (milestone-number uint))
  (map-get? research-project-milestones {parent-proposal-id: proposal-identifier, milestone-sequence-number: milestone-number})
)

(define-read-only (fetch-researcher-verification-profile (researcher-address principal))
  (map-get? verified-researcher-profiles researcher-address)
)

(define-read-only (evaluate-proposal-passage-status (proposal-identifier uint))
  (let ((proposal-data (unwrap! (map-get? research-funding-proposals proposal-identifier) false)))
    (let (
      (supporting-votes (get accumulated-yes-votes proposal-data))
      (opposing-votes (get accumulated-no-votes proposal-data))
      (total-cast-votes (+ supporting-votes opposing-votes))
      (required-participation (var-get minimum-quorum-participation-rate))
      (estimated-total-members (get-estimated-active-member-count))
    )
      (and 
        (>= total-cast-votes (/ (* estimated-total-members required-participation) u100))
        (> supporting-votes opposing-votes)
        (is-eq (get current-proposal-status proposal-data) "active")
        (<= (get voting-deadline-block proposal-data) stacks-block-height)
      )
    )
  )
)

(define-read-only (get-estimated-active-member-count)
  ;; Simplified member count estimation - production would maintain accurate counter
  u100
)

;; INTERNAL UTILITY FUNCTIONS - Private helper functions

(define-private (verify-dao-membership-status (user-address principal))
  (is-some (map-get? dao-community-members user-address))
)

(define-private (register-new-community-member (member-address principal))
  (map-set dao-community-members member-address {
    accumulated-reputation-score: u0,
    lifetime-financial-contributions: u0,
    membership-registration-block: stacks-block-height,
    verified-researcher-status: false
  })
)

(define-private (increase-member-reputation-score (member-address principal) (reputation-increment uint))
  (let ((existing-member-data (unwrap! (map-get? dao-community-members member-address) false)))
    (map-set dao-community-members member-address 
      (merge existing-member-data {accumulated-reputation-score: (+ (get accumulated-reputation-score existing-member-data) reputation-increment)})
    )
    true
  )
)

;; CORE MEMBERSHIP FUNCTIONS - DAO participation and researcher registration
;; Community membership registration
(define-public (register-for-dao-membership)
  (begin
    (asserts! (not (verify-dao-membership-status tx-sender)) ERR-MEMBER-ALREADY-EXISTS)
    (register-new-community-member tx-sender)
    (ok true)
  )
)

;; Scientific researcher profile creation
(define-public (establish-researcher-credentials (full-name (string-ascii 50)) 
                                                (institution-name (string-ascii 100)) 
                                                (research-specializations (string-ascii 200)))
  (let (
    (validated-name full-name)
    (validated-institution institution-name)
    (validated-specializations research-specializations)
  )
    (begin
      (asserts! (verify-dao-membership-status tx-sender) ERR-UNAUTHORIZED-ACCESS)
      ;; Validate all input strings
      (asserts! (validate-name-string validated-name) ERR-INVALID-INPUT-DATA)
      (asserts! (validate-short-string validated-institution) ERR-INVALID-INPUT-DATA)
      (asserts! (validate-medium-string validated-specializations) ERR-INVALID-INPUT-DATA)
      
      (map-set verified-researcher-profiles tx-sender {
        researcher-full-name: validated-name,
        affiliated-institution: validated-institution,
        primary-research-domains: validated-specializations,
        published-papers-count: u0,
        institution-verification-status: false
      })
      (let ((current-member-profile (unwrap! (map-get? dao-community-members tx-sender) ERR-RESOURCE-NOT-FOUND)))
        (map-set dao-community-members tx-sender 
          (merge current-member-profile {verified-researcher-status: true})
        )
      )
      (ok true)
    )
  )
)

;; PROPOSAL MANAGEMENT FUNCTIONS - Research funding proposal lifecycle
;; Submit new research funding proposal
(define-public (submit-research-funding-proposal (project-title (string-ascii 100))
                                                 (research-description (string-ascii 500))
                                                 (requested-funding-amount uint)
                                                 (research-category (string-ascii 50))
                                                 (planned-milestone-count uint))
  (let (
    (new-proposal-id (var-get current-proposal-counter))
    (required-deposit (var-get active-proposal-deposit-requirement))
    (validated-title project-title)
    (validated-description research-description)
    (validated-category research-category)
  )
    (begin
      (asserts! (verify-dao-membership-status tx-sender) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (> requested-funding-amount u0) ERR-INVALID-FUNDING-AMOUNT)
      (asserts! (> planned-milestone-count u0) ERR-INVALID-PROPOSAL-PARAMETERS)
      
      ;; Validate all input strings
      (asserts! (validate-short-string validated-title) ERR-INVALID-INPUT-DATA)
      (asserts! (validate-string-input validated-description) ERR-INVALID-INPUT-DATA)
      (asserts! (validate-name-string validated-category) ERR-INVALID-INPUT-DATA)
      
      ;; Secure proposal deposit from submitter
      (try! (stx-transfer? required-deposit tx-sender (as-contract tx-sender)))
      
      ;; Register new proposal in system
      (map-set research-funding-proposals new-proposal-id {
        proposal-submitter: tx-sender,
        research-project-title: validated-title,
        detailed-research-description: validated-description,
        total-requested-funding: requested-funding-amount,
        scientific-research-category: validated-category,
        proposal-submission-block: stacks-block-height,
        voting-deadline-block: (+ stacks-block-height (var-get current-voting-duration-blocks)),
        accumulated-yes-votes: u0,
        accumulated-no-votes: u0,
        unique-voters-count: u0,
        current-proposal-status: "active",
        completed-milestones-count: u0,
        planned-total-milestones: planned-milestone-count
      })
      
      ;; Advance proposal counter for next submission
      (var-set current-proposal-counter (+ new-proposal-id u1))
      
      (ok new-proposal-id)
    )
  )
)

;; Define specific project milestone
(define-public (establish-project-milestone (proposal-identifier uint) 
                                           (milestone-number uint) 
                                           (milestone-description (string-ascii 200)) 
                                           (milestone-funding-allocation uint))
  (let (
    (proposal-details (unwrap! (map-get? research-funding-proposals proposal-identifier) ERR-RESOURCE-NOT-FOUND))
    (validated-description milestone-description)
    (validated-milestone-num milestone-number)
  )
    (begin
      (asserts! (is-eq (get proposal-submitter proposal-details) tx-sender) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (is-eq (get current-proposal-status proposal-details) "active") ERR-VOTING-PERIOD-EXPIRED)
      (asserts! (> milestone-funding-allocation u0) ERR-INVALID-FUNDING-AMOUNT)
      
      ;; Validate inputs
      (asserts! (validate-medium-string validated-description) ERR-INVALID-INPUT-DATA)
      (asserts! (validate-milestone-number validated-milestone-num (get planned-total-milestones proposal-details)) ERR-INVALID-INPUT-DATA)
      
      (map-set research-project-milestones {parent-proposal-id: proposal-identifier, milestone-sequence-number: validated-milestone-num} {
        milestone-description-text: validated-description,
        allocated-milestone-funding: milestone-funding-allocation,
        milestone-completion-status: false,
        completion-timestamp-block: u0
      })
      
      (ok true)
    )
  )
)

;; VOTING SYSTEM FUNCTIONS - Democratic proposal evaluation
;; Cast vote on active research proposal
(define-public (cast-proposal-vote (proposal-identifier uint) (vote-in-support bool))
  (let (
    (proposal-details (unwrap! (map-get? research-funding-proposals proposal-identifier) ERR-RESOURCE-NOT-FOUND))
    (member-voting-power (calculate-member-voting-influence tx-sender))
  )
    (begin
      (asserts! (verify-dao-membership-status tx-sender) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (> (get voting-deadline-block proposal-details) stacks-block-height) ERR-VOTING-PERIOD-EXPIRED)
      (asserts! (is-none (map-get? community-member-votes {proposal-identifier: proposal-identifier, voting-member: tx-sender})) ERR-DUPLICATE-VOTE-ATTEMPT)
      
      ;; Record individual vote for transparency
      (map-set community-member-votes {proposal-identifier: proposal-identifier, voting-member: tx-sender} {
        member-vote-choice: vote-in-support,
        applied-voting-weight: member-voting-power
      })
      
      ;; Update aggregate proposal vote tallies
      (if vote-in-support
        (map-set research-funding-proposals proposal-identifier 
          (merge proposal-details {
            accumulated-yes-votes: (+ (get accumulated-yes-votes proposal-details) member-voting-power),
            unique-voters-count: (+ (get unique-voters-count proposal-details) u1)
          })
        )
        (map-set research-funding-proposals proposal-identifier 
          (merge proposal-details {
            accumulated-no-votes: (+ (get accumulated-no-votes proposal-details) member-voting-power),
            unique-voters-count: (+ (get unique-voters-count proposal-details) u1)
          })
        )
      )
      
      ;; Reward active participation with reputation
      (increase-member-reputation-score tx-sender reputation-bonus-for-voting)
      
      (ok true)
    )
  )
)

;; Finalize proposal after voting period completion
(define-public (finalize-proposal-voting (proposal-identifier uint))
  (let ((proposal-details (unwrap! (map-get? research-funding-proposals proposal-identifier) ERR-RESOURCE-NOT-FOUND)))
    (begin
      (asserts! (<= (get voting-deadline-block proposal-details) stacks-block-height) ERR-VOTING-STILL-ACTIVE)
      (asserts! (is-eq (get current-proposal-status proposal-details) "active") ERR-VOTING-PERIOD-EXPIRED)
      
      (if (evaluate-proposal-passage-status proposal-identifier)
        (begin
          ;; Mark proposal as community-approved
          (map-set research-funding-proposals proposal-identifier 
            (merge proposal-details {current-proposal-status: "passed"})
          )
          (ok "passed")
        )
        (begin
          ;; Mark proposal as rejected and refund security deposit
          (map-set research-funding-proposals proposal-identifier 
            (merge proposal-details {current-proposal-status: "rejected"})
          )
          (try! (as-contract (stx-transfer? (var-get active-proposal-deposit-requirement) 
                                           tx-sender 
                                           (get proposal-submitter proposal-details))))
          (ok "rejected")
        )
      )
    )
  )
)

;; FUNDING DISTRIBUTION FUNCTIONS - Treasury management and fund allocation
;; Distribute approved funding to researcher
(define-public (distribute-approved-research-funding (proposal-identifier uint))
  (let ((proposal-details (unwrap! (map-get? research-funding-proposals proposal-identifier) ERR-RESOURCE-NOT-FOUND)))
    (begin
      (asserts! (is-eq (get current-proposal-status proposal-details) "passed") ERR-UNAUTHORIZED-ACCESS)
      (asserts! (>= (var-get total-dao-treasury-balance) (get total-requested-funding proposal-details)) ERR-INSUFFICIENT-TREASURY-FUNDS)
      
      ;; Transfer approved funds to research proposer
      (try! (as-contract (stx-transfer? (get total-requested-funding proposal-details) 
                                       tx-sender 
                                       (get proposal-submitter proposal-details))))
      
      ;; Update DAO treasury balance
      (var-set total-dao-treasury-balance (- (var-get total-dao-treasury-balance) (get total-requested-funding proposal-details)))
      
      ;; Update proposal to funded status
      (map-set research-funding-proposals proposal-identifier 
        (merge proposal-details {current-proposal-status: "funded"})
      )
      
      (ok true)
    )
  )
)

;; Mark research milestone as completed
(define-public (mark-milestone-completion (proposal-identifier uint) (milestone-number uint))
  (let (
    (proposal-details (unwrap! (map-get? research-funding-proposals proposal-identifier) ERR-RESOURCE-NOT-FOUND))
    (validated-milestone-num milestone-number)
    (milestone-details (unwrap! (map-get? research-project-milestones 
                                        {parent-proposal-id: proposal-identifier, milestone-sequence-number: validated-milestone-num}) 
                               ERR-MILESTONE-DOES-NOT-EXIST))
  )
    (begin
      (asserts! (is-eq (get proposal-submitter proposal-details) tx-sender) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (is-eq (get current-proposal-status proposal-details) "funded") ERR-PROPOSAL-NOT-IN-FUNDED-STATUS)
      (asserts! (not (get milestone-completion-status milestone-details)) ERR-MILESTONE-ALREADY-MARKED-COMPLETE)
      
      ;; Validate milestone number
      (asserts! (validate-milestone-number validated-milestone-num (get planned-total-milestones proposal-details)) ERR-INVALID-INPUT-DATA)
      
      ;; Update milestone completion record
      (map-set research-project-milestones {parent-proposal-id: proposal-identifier, milestone-sequence-number: validated-milestone-num}
        (merge milestone-details {
          milestone-completion-status: true,
          completion-timestamp-block: stacks-block-height
        })
      )
      
      ;; Update proposal milestone progress
      (let ((updated-milestone-count (+ (get completed-milestones-count proposal-details) u1)))
        (map-set research-funding-proposals proposal-identifier 
          (merge proposal-details {completed-milestones-count: updated-milestone-count})
        )
        
        ;; Check for complete project conclusion
        (if (is-eq updated-milestone-count (get planned-total-milestones proposal-details))
          (map-set research-funding-proposals proposal-identifier 
            (merge proposal-details {
              current-proposal-status: "completed",
              completed-milestones-count: updated-milestone-count
            })
          )
          true
        )
      )
      
      ;; Reward significant reputation for milestone achievement
      (increase-member-reputation-score tx-sender reputation-bonus-for-milestone-completion)
      
      (ok true)
    )
  )
)

;; Contribute funds to DAO research treasury
(define-public (contribute-to-research-treasury (contribution-amount uint))
  (begin
    (asserts! (> contribution-amount u0) ERR-INVALID-FUNDING-AMOUNT)
    (try! (stx-transfer? contribution-amount tx-sender (as-contract tx-sender)))
    (var-set total-dao-treasury-balance (+ (var-get total-dao-treasury-balance) contribution-amount))
    
    ;; Update member contribution records and reputation
    (if (verify-dao-membership-status tx-sender)
      (let ((current-member-profile (unwrap! (map-get? dao-community-members tx-sender) ERR-RESOURCE-NOT-FOUND)))
        (begin
          (map-set dao-community-members tx-sender 
            (merge current-member-profile {
              lifetime-financial-contributions: (+ (get lifetime-financial-contributions current-member-profile) contribution-amount)
            })
          )
          (increase-member-reputation-score tx-sender (/ contribution-amount u10000)) ;; 1 reputation per 0.01 STX
        )
      )
      (register-new-community-member tx-sender)
    )
    
    (ok true)
  )
)

;; ADMINISTRATIVE GOVERNANCE FUNCTIONS - Contract configuration and management

;; Adjust proposal voting duration
(define-public (configure-voting-period-duration (updated-duration-blocks uint))
  (let ((validated-duration updated-duration-blocks))
    (begin
      (asserts! (is-eq tx-sender contract-administrator) ERR-OWNER-ONLY-ACCESS)
      ;; Validate duration (should be reasonable - between 1 block and 52560 blocks ~1 year)
      (asserts! (and (> validated-duration u0) (<= validated-duration u52560)) ERR-INVALID-INPUT-DATA)
      (var-set current-voting-duration-blocks validated-duration)
      (ok true)
    )
  )
)

;; Modify minimum proposal security deposit
(define-public (adjust-proposal-deposit-requirement (updated-deposit-amount uint))
  (let ((validated-deposit updated-deposit-amount))
    (begin
      (asserts! (is-eq tx-sender contract-administrator) ERR-OWNER-ONLY-ACCESS)
      ;; Validate deposit amount (should be reasonable - between 0.01 STX and 100 STX)
      (asserts! (and (>= validated-deposit u10000) (<= validated-deposit u100000000)) ERR-INVALID-INPUT-DATA)
      (var-set active-proposal-deposit-requirement validated-deposit)
      (ok true)
    )
  )
)

;; Update required participation quorum
(define-public (modify-quorum-participation-threshold (updated-threshold-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-administrator) ERR-OWNER-ONLY-ACCESS)
    (asserts! (<= updated-threshold-percentage u100) ERR-INVALID-FUNDING-AMOUNT)
    (var-set minimum-quorum-participation-rate updated-threshold-percentage)
    (ok true)
  )
)

;; Officially verify researcher institutional credentials
(define-public (grant-official-researcher-verification (researcher-address principal))
  (let ((validated-address researcher-address))
    (begin
      (asserts! (is-eq tx-sender contract-administrator) ERR-OWNER-ONLY-ACCESS)
      ;; Validate principal input
      (asserts! (validate-principal-input validated-address) ERR-INVALID-INPUT-DATA)
      (let ((researcher-profile (unwrap! (map-get? verified-researcher-profiles validated-address) ERR-RESOURCE-NOT-FOUND)))
        (map-set verified-researcher-profiles validated-address 
          (merge researcher-profile {institution-verification-status: true})
        )
        (increase-member-reputation-score validated-address reputation-bonus-for-researcher-verification)
        (ok true)
      )
    )
  )
)

;; Emergency treasury fund recovery (administrative override)
(define-public (execute-emergency-treasury-withdrawal (withdrawal-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-administrator) ERR-OWNER-ONLY-ACCESS)
    (asserts! (>= (var-get total-dao-treasury-balance) withdrawal-amount) ERR-INSUFFICIENT-TREASURY-FUNDS)
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender contract-administrator)))
    (var-set total-dao-treasury-balance (- (var-get total-dao-treasury-balance) withdrawal-amount))
    (ok true)
  )
)