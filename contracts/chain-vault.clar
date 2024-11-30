;; title: chain-vault
;; summary: Stacks Blockchain Banking Smart Contract
;; description: A comprehensive smart contract for a secure and user-friendly banking platform


;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-ACCOUNT-ALREADY-EXISTS (err u102))
(define-constant ERR-ACCOUNT-NOT-FOUND (err u103))
(define-constant ERR-TRANSACTION-FAILED (err u104))
(define-constant ERR-INVALID-DEPOSIT-AMOUNT (err u105))
(define-constant ERR-INVALID-ACCOUNT-TYPE (err u106))


;; User accounts map
;; Define a data map to store a user account information
(define-map user-accounts 
	principal
  	{
		balance: uint,
		is-verified: bool,
		account-type: (string-ascii 20),
		last-transaction-timestamp: uint
  	}
)

;; Transaction History map
(define-map transaction-history
	{ sender: principal, recipient: principal, timestamp: uint }
	{
		amount: uint,
		transaction-type: (string-ascii 20)
	}
)



;; Create a new bank account
(define-public (create-account (account-type (string-ascii 20)))
	(begin
		;; Validate account-type input
		(if (or (is-eq account-type "") (> (len account-type) (to-uint 20)))
			ERR-INVALID-ACCOUNT-TYPE
			(begin
				;; Ensure the account does not already exist
				(asserts! (is-none (map-get? user-accounts tx-sender)) ERR-ACCOUNT-ALREADY-EXISTS)

			;; Create the account	
				(map-set user-accounts
					tx-sender
					{
						balance: u0,
						is-verified: false,
						account-type: account-type,
						last-transaction-timestamp: stacks-block-height
					}
				)

				;; Return success	
				(ok true)
			)
		)
	)
)

;; Deposit funds into account
(define-public (deposit (amount uint))
	(begin
		;; Validate the input
		(if (<= amount u0)
			ERR-INVALID-DEPOSIT-AMOUNT
			(let (
				;; Fetch the current account
				(current-account 
					(unwrap! 
        				(map-get? user-accounts tx-sender) 
        				ERR-ACCOUNT-NOT-FOUND
      				)
    			)
				(new-balance 
      				(+ (get balance current-account) amount)
    			)
  			)
    			(map-set user-accounts 
      				tx-sender 
      				(merge current-account 
        				{ 
        					balance: new-balance,
          					last-transaction-timestamp: stacks-block-height
						}
      				)
				)
    
    			(map-set transaction-history 
					{
    	    			sender: tx-sender,
    	    			recipient: tx-sender,
    	    			timestamp: stacks-block-height
					}
    				{ 
    	    			amount: amount,
    	    			transaction-type: "deposit"
					}
				)
    
				;; Return the new balance as success
    			(ok new-balance)
			)
		)
	)
)

;; Withdraw funds from  account
(define-public (withdraw (amount uint))
	(let (
		(current-account
			(unwrap!
				(map-get? user-accounts tx-sender)
				ERR-ACCOUNT-NOT-FOUND
			)
		)
	)
		(asserts!
			(>= (get balance current-account) amount)
			ERR-INSUFFICIENT-FUNDS
		)

		(let (
			(new-balance
				(- (get balance current-account) amount)
			)
		)
			(map-set user-accounts
				tx-sender
				(merge current-account
					{
						balance: new-balance,
						last-transaction-timestamp: stacks-block-height
					}
				)
			)

			(map-set transaction-history
				{
					sender: tx-sender,
					recipient: tx-sender,
					timestamp: stacks-block-height
				}
				{
					amount: amount,
					transaction-type: "withdrawal"
				}
			)

			(ok new-balance)
		)
	)
)

;; Transfer funds between accounts
(define-public (transfer (recipient principal) (amount uint))
	(begin
		;; Validate recipient is not the sender
		(asserts! (not (is-eq tx-sender recipient)) ERR-UNAUTHORIZED)
		
		;; Validate amount is greater than zero
		(asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)

		(let (
			(sender-account
				(unwrap!
					(map-get? user-accounts tx-sender)
					ERR-ACCOUNT-NOT-FOUND
				)
			)
			(recipient-account
				(unwrap!
					(map-get? user-accounts recipient)
					ERR-ACCOUNT-NOT-FOUND
				)
			)
		)
			(asserts! (>= (get balance sender-account) amount)
			ERR-INSUFFICIENT-FUNDS
			)

			(let (
				(sender-new-balance
					(- (get balance sender-account) amount)
				)
				(recipient-new-balance
					(+ (get balance recipient-account) amount)
				)
			)
				(map-set user-accounts
					tx-sender
					(merge sender-account
						{
							balance: sender-new-balance,
							last-transaction-timestamp: stacks-block-height
						}
					)
				)

				(map-set user-accounts
					recipient
					(merge recipient-account
						{
							balance: recipient-new-balance,
							last-transaction-timestamp: stacks-block-height
						}
					)
				)

				(map-set transaction-history
					{
						sender: tx-sender,
						recipient: recipient,
						timestamp: stacks-block-height
					}
					{
						amount: amount,
						transaction-type: "transfer"
					}
				)

				(ok true)
			)
		)
	)
)

;; Get account balance
(define-read-only (get-balance (user principal))
	(match
		(map-get? user-accounts user)
		account (some (get balance account))
		none
	)
)

;; Admin function to verify user accounts
(define-public (verify-account (user principal))
	(begin
		;; Only contract owner can verify accounts
		(asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)

		(asserts! (not (is-eq CONTRACT-OWNER user)) ERR-INVALID-ACCOUNT-TYPE)

		(let (
			(current-account
				(unwrap!
					(map-get? user-accounts user)
					ERR-ACCOUNT-NOT-FOUND
				)
			)
		)
			(map-set user-accounts
				user
				(merge current-account
					{is-verified: true}
				)
			)

			(ok true)
		)
	)
)

;; Initialize the contract
(define-private (initialize)
	(begin
		(print "Chain-Vault Banking System Initialized")
		true
	)
)

;; Run initialization on contract deploy
(initialize)