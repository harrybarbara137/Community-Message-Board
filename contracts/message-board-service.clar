;; Community Message Board - Core Contract
;; A production-ready forum system with posting, voting, categories, and moderation

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POST-NOT-FOUND (err u101))
(define-constant ERR-INVALID-CATEGORY (err u102))
(define-constant ERR-POST-LOCKED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-CANNOT-VOTE-OWN-POST (err u105))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u106))
(define-constant ERR-SPAM-DETECTED (err u107))
(define-constant ERR-CONTENT-TOO-LONG (err u108))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-CONTENT-LENGTH u1000)
(define-constant MIN-REPUTATION-TO-POST u10)
(define-constant SPAM-THRESHOLD u5) ;; Max posts per block
(define-constant REPUTATION-VOTE-REWARD u2)
(define-constant REPUTATION-POST-REWARD u5)

;; Data Variables
(define-data-var next-post-id uint u1)
(define-data-var next-category-id uint u1)

;; Data Maps
(define-map posts
  { post-id: uint }
  {
    author: principal,
    title: (string-ascii 100),
    content: (string-utf8 1000),
    category-id: uint,
    upvotes: uint,
    downvotes: uint,
    created-at: uint,
    is-locked: bool,
    is-hidden: bool
  }
)

(define-map categories
  { category-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    creator: principal,
    created-at: uint,
    is-active: bool
  }
)

(define-map votes
  { post-id: uint, voter: principal }
  { vote-type: bool } ;; true for upvote, false for downvote
)

(define-map user-stats
  { user: principal }
  {
    reputation: uint,
    total-posts: uint,
    posts-this-block: uint,
    last-post-block: uint
  }
)

(define-map moderators
  { moderator: principal }
  { is-active: bool }
)

;; Public Functions

;; Create a new category
(define-public (create-category (name (string-ascii 50)) (description (string-ascii 200)))
  (let
    (
      (category-id (var-get next-category-id))
      (current-block stacks-block-height)
    )
    (asserts! (> (len name) u0) ERR-INVALID-CATEGORY)
    (map-set categories
      { category-id: category-id }
      {
        name: name,
        description: description,
        creator: tx-sender,
        created-at: current-block,
        is-active: true
      }
    )
    (var-set next-category-id (+ category-id u1))
    (ok category-id)
  )
)

;; Create a new post
(define-public (create-post
  (title (string-ascii 100))
  (content (string-utf8 1000))
  (category-id uint))
  (let
    (
      (post-id (var-get next-post-id))
      (current-block stacks-block-height)
      (user-stats-data (get-user-stats tx-sender))
      (last-block (get last-post-block user-stats-data))
      (posts-this-block (if (is-eq last-block current-block)
                          (get posts-this-block user-stats-data)
                          u0))
    )
    ;; Validation checks
    (asserts! (> (len title) u0) ERR-INVALID-CATEGORY)
    (asserts! (> (len content) u0) ERR-INVALID-CATEGORY)
    (asserts! (<= (len content) MAX-CONTENT-LENGTH) ERR-CONTENT-TOO-LONG)
    (asserts! (is-some (map-get? categories { category-id: category-id })) ERR-INVALID-CATEGORY)
    (asserts! (>= (get reputation user-stats-data) MIN-REPUTATION-TO-POST) ERR-INSUFFICIENT-REPUTATION)
    (asserts! (< posts-this-block SPAM-THRESHOLD) ERR-SPAM-DETECTED)

    ;; Create the post
    (map-set posts
      { post-id: post-id }
      {
        author: tx-sender,
        title: title,
        content: content,
        category-id: category-id,
        upvotes: u0,
        downvotes: u0,
        created-at: current-block,
        is-locked: false,
        is-hidden: false
      }
    )

    ;; Update user stats
    (map-set user-stats
      { user: tx-sender }
      (merge user-stats-data {
        total-posts: (+ (get total-posts user-stats-data) u1),
        posts-this-block: (+ posts-this-block u1),
        last-post-block: current-block,
        reputation: (+ (get reputation user-stats-data) REPUTATION-POST-REWARD)
      })
    )

    (var-set next-post-id (+ post-id u1))
    (ok post-id)
  )
)

;; Vote on a post
(define-public (vote-post (post-id uint) (is-upvote bool))
  (let
    (
      (post-data (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
      (existing-vote (map-get? votes { post-id: post-id, voter: tx-sender }))
      (post-author (get author post-data))
      (voter-stats (get-user-stats tx-sender))
      (author-stats (get-user-stats post-author))
    )
    ;; Validation checks
    (asserts! (not (is-eq tx-sender post-author)) ERR-CANNOT-VOTE-OWN-POST)
    (asserts! (not (get is-locked post-data)) ERR-POST-LOCKED)
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)

    ;; Record the vote
    (map-set votes
      { post-id: post-id, voter: tx-sender }
      { vote-type: is-upvote }
    )

    ;; Update post vote counts
    (map-set posts
      { post-id: post-id }
      (merge post-data {
        upvotes: (if is-upvote
                   (+ (get upvotes post-data) u1)
                   (get upvotes post-data)),
        downvotes: (if is-upvote
                    (get downvotes post-data)
                    (+ (get downvotes post-data) u1))
      })
    )

    ;; Update voter reputation (small reward for participation)
    (map-set user-stats
      { user: tx-sender }
      (merge voter-stats {
        reputation: (+ (get reputation voter-stats) u1)
      })
    )

    ;; Update author reputation based on vote type
    (map-set user-stats
      { user: post-author }
      (merge author-stats {
        reputation: (if is-upvote
                     (+ (get reputation author-stats) REPUTATION-VOTE-REWARD)
                     (if (> (get reputation author-stats) REPUTATION-VOTE-REWARD)
                       (- (get reputation author-stats) REPUTATION-VOTE-REWARD)
                       u0))
      })
    )

    (ok true)
  )
)

;; Add moderator (only contract owner)
(define-public (add-moderator (moderator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set moderators { moderator: moderator } { is-active: true })
    (ok true)
  )
)

;; Remove moderator (only contract owner)
(define-public (remove-moderator (moderator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set moderators { moderator: moderator } { is-active: false })
    (ok true)
  )
)

;; Lock/unlock post (moderators only)
(define-public (toggle-post-lock (post-id uint))
  (let
    (
      (post-data (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
      (is-mod (default-to false (get is-active (map-get? moderators { moderator: tx-sender }))))
    )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) is-mod) ERR-NOT-AUTHORIZED)
    (map-set posts
      { post-id: post-id }
      (merge post-data { is-locked: (not (get is-locked post-data)) })
    )
    (ok true)
  )
)

;; Hide/unhide post (moderators only)
(define-public (toggle-post-visibility (post-id uint))
  (let
    (
      (post-data (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
      (is-mod (default-to false (get is-active (map-get? moderators { moderator: tx-sender }))))
    )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) is-mod) ERR-NOT-AUTHORIZED)
    (map-set posts
      { post-id: post-id }
      (merge post-data { is-hidden: (not (get is-hidden post-data)) })
    )
    (ok true)
  )
)

;; Read-only functions

;; Get post by ID
(define-read-only (get-post (post-id uint))
  (map-get? posts { post-id: post-id })
)

;; Get category by ID
(define-read-only (get-category (category-id uint))
  (map-get? categories { category-id: category-id })
)

;; Get user vote on post
(define-read-only (get-user-vote (post-id uint) (user principal))
  (map-get? votes { post-id: post-id, voter: user })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
  (default-to
    { reputation: u50, total-posts: u0, posts-this-block: u0, last-post-block: u0 }
    (map-get? user-stats { user: user })
  )
)

;; Check if user is moderator
(define-read-only (is-moderator (user principal))
  (default-to false (get is-active (map-get? moderators { moderator: user })))
)

;; Get current post ID counter
(define-read-only (get-next-post-id)
  (var-get next-post-id)
)

;; Get current category ID counter
(define-read-only (get-next-category-id)
  (var-get next-category-id)
)

;; Calculate post score (upvotes - downvotes)
(define-read-only (get-post-score (post-id uint))
  (match (map-get? posts { post-id: post-id })
    post-data
      (let
        (
          (upvotes (get upvotes post-data))
          (downvotes (get downvotes post-data))
        )
        (ok (if (>= upvotes downvotes)
              (- upvotes downvotes)
              u0))
      )
    ERR-POST-NOT-FOUND
  )
)

;; Check if post is visible (not hidden and category is active)
(define-read-only (is-post-visible (post-id uint))
  (match (map-get? posts { post-id: post-id })
    post-data
      (let
        (
          (category-data (unwrap! (map-get? categories { category-id: (get category-id post-data) }) (ok false)))
        )
        (ok (and
          (not (get is-hidden post-data))
          (get is-active category-data)
        ))
      )
    (ok false)
  )
)
