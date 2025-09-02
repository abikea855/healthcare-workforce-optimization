;; Healthcare Training and Wellness Management Contract
;; Provides functionality for training coordination, wellness support, and career development

(define-constant ERR-UNAUTHORIZED u200)
(define-constant ERR-INVALID-EMPLOYEE u201)
(define-constant ERR-TRAINING-NOT-FOUND u202)
(define-constant ERR-WELLNESS-NOT-FOUND u203)
(define-constant ERR-INVALID-SCORE u204)
(define-constant ERR-PROGRAM-NOT-FOUND u205)
(define-constant ERR-INVALID-DATE u206)

;; Training program data structures
(define-map training-programs uint {
    program-id: uint,
    program-name: (string-ascii 50),
    description: (string-ascii 200),
    duration-hours: uint,
    required-skill-level: uint,
    certification-type: (string-ascii 30),
    max-participants: uint,
    current-participants: uint,
    instructor: principal,
    start-date: uint,
    end-date: uint,
    is-active: bool
})

;; Employee training enrollment
(define-map training-enrollments uint {
    enrollment-id: uint,
    employee: principal,
    program-id: uint,
    enrollment-date: uint,
    completion-date: (optional uint),
    progress-percentage: uint,
    final-score: (optional uint),
    status: (string-ascii 20),
    notes: (string-ascii 200)
})

;; Wellness assessment data
(define-map wellness-assessments uint {
    assessment-id: uint,
    employee: principal,
    stress-level: uint,
    work-life-balance: uint,
    job-satisfaction: uint,
    burnout-risk: uint,
    physical-wellness: uint,
    mental-wellness: uint,
    assessment-date: uint,
    assessor: principal,
    recommendations: (string-ascii 200)
})

;; Wellness programs and interventions
(define-map wellness-programs uint {
    program-id: uint,
    program-name: (string-ascii 50),
    program-type: (string-ascii 30),
    description: (string-ascii 200),
    target-participants: uint,
    duration-weeks: uint,
    wellness-focus: (string-ascii 30),
    success-metrics: (list 5 (string-ascii 30)),
    coordinator: principal,
    is-active: bool,
    created-at: uint
})

;; Career development tracking
(define-map career-development uint {
    development-id: uint,
    employee: principal,
    current-position: (string-ascii 30),
    target-position: (string-ascii 30),
    development-plan: (string-ascii 200),
    required-training: (list 5 uint),
    mentor: (optional principal),
    target-completion: uint,
    progress-percentage: uint,
    status: (string-ascii 20),
    created-at: uint
})

;; Certification tracking
(define-map employee-certifications principal {
    active-certifications: (list 10 (string-ascii 30)),
    expiring-certifications: (list 5 {cert-name: (string-ascii 30), expiry-date: uint}),
    required-renewals: (list 5 (string-ascii 30)),
    ce-credits-earned: uint,
    ce-credits-required: uint,
    last-updated: uint
})

;; Contract administration
(define-data-var contract-owner principal tx-sender)
(define-map authorized-wellness-coordinators principal bool)
(define-map authorized-training-coordinators principal bool)

;; ID counters
(define-data-var next-program-id uint u1)
(define-data-var next-enrollment-id uint u1)
(define-data-var next-assessment-id uint u1)
(define-data-var next-wellness-program-id uint u1)
(define-data-var next-development-id uint u1)

;; Initialize with owner permissions
(map-set authorized-wellness-coordinators tx-sender true)
(map-set authorized-training-coordinators tx-sender true)

;; Authorization functions
(define-private (is-owner (user principal))
    (is-eq user (var-get contract-owner))
)

(define-private (is-wellness-coordinator (user principal))
    (or (is-owner user) (default-to false (map-get? authorized-wellness-coordinators user)))
)

(define-private (is-training-coordinator (user principal))
    (or (is-owner user) (default-to false (map-get? authorized-training-coordinators user)))
)

(define-public (add-wellness-coordinator (coordinator principal))
    (begin
        (asserts! (is-owner tx-sender) (err ERR-UNAUTHORIZED))
        (map-set authorized-wellness-coordinators coordinator true)
        (ok true)
    )
)

(define-public (add-training-coordinator (coordinator principal))
    (begin
        (asserts! (is-owner tx-sender) (err ERR-UNAUTHORIZED))
        (map-set authorized-training-coordinators coordinator true)
        (ok true)
    )
)

;; Training program management
(define-public (create-training-program
    (program-name (string-ascii 50))
    (description (string-ascii 200))
    (duration-hours uint)
    (required-skill-level uint)
    (certification-type (string-ascii 30))
    (max-participants uint)
    (instructor principal)
    (start-date uint)
    (end-date uint)
)
    (let ((program-id (var-get next-program-id)))
        (asserts! (is-training-coordinator tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (> end-date start-date) (err ERR-INVALID-DATE))
        (asserts! (<= required-skill-level u10) (err ERR-INVALID-SCORE))
        
        (map-set training-programs program-id {
            program-id: program-id,
            program-name: program-name,
            description: description,
            duration-hours: duration-hours,
            required-skill-level: required-skill-level,
            certification-type: certification-type,
            max-participants: max-participants,
            current-participants: u0,
            instructor: instructor,
            start-date: start-date,
            end-date: end-date,
            is-active: true
        })
        
        (var-set next-program-id (+ program-id u1))
        (ok program-id)
    )
)

(define-public (enroll-in-training
    (employee principal)
    (program-id uint)
)
    (let ((enrollment-id (var-get next-enrollment-id)))
        (asserts! (is-training-coordinator tx-sender) (err ERR-UNAUTHORIZED))
        
        (match (map-get? training-programs program-id)
            program-data (begin
                (asserts! (get is-active program-data) (err ERR-PROGRAM-NOT-FOUND))
                (asserts! (< (get current-participants program-data) (get max-participants program-data)) (err ERR-PROGRAM-NOT-FOUND))
                
                (map-set training-enrollments enrollment-id {
                    enrollment-id: enrollment-id,
                    employee: employee,
                    program-id: program-id,
                    enrollment-date: block-height,
                    completion-date: none,
                    progress-percentage: u0,
                    final-score: none,
                    status: "enrolled",
                    notes: ""
                })
                
                ;; Update participant count
                (map-set training-programs program-id 
                    (merge program-data { current-participants: (+ (get current-participants program-data) u1) }))
                
                (var-set next-enrollment-id (+ enrollment-id u1))
                (ok enrollment-id)
            )
            (err ERR-TRAINING-NOT-FOUND)
        )
    )
)

(define-public (update-training-progress
    (enrollment-id uint)
    (progress-percentage uint)
    (notes (string-ascii 200))
)
    (begin
        (asserts! (is-training-coordinator tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (<= progress-percentage u100) (err ERR-INVALID-SCORE))
        
        (match (map-get? training-enrollments enrollment-id)
            enrollment-data (begin
                (map-set training-enrollments enrollment-id 
                    (merge enrollment-data { 
                        progress-percentage: progress-percentage,
                        notes: notes,
                        status: (if (is-eq progress-percentage u100) "completed" "in-progress")
                    }))
                (ok true)
            )
            (err ERR-TRAINING-NOT-FOUND)
        )
    )
)

;; Wellness assessment functions
(define-public (conduct-wellness-assessment
    (employee principal)
    (stress-level uint)
    (work-life-balance uint)
    (job-satisfaction uint)
    (burnout-risk uint)
    (physical-wellness uint)
    (mental-wellness uint)
    (recommendations (string-ascii 200))
)
    (let ((assessment-id (var-get next-assessment-id)))
        (asserts! (is-wellness-coordinator tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (and 
            (<= stress-level u10)
            (<= work-life-balance u10)
            (<= job-satisfaction u10)
            (<= burnout-risk u10)
            (<= physical-wellness u10)
            (<= mental-wellness u10)
        ) (err ERR-INVALID-SCORE))
        
        (map-set wellness-assessments assessment-id {
            assessment-id: assessment-id,
            employee: employee,
            stress-level: stress-level,
            work-life-balance: work-life-balance,
            job-satisfaction: job-satisfaction,
            burnout-risk: burnout-risk,
            physical-wellness: physical-wellness,
            mental-wellness: mental-wellness,
            assessment-date: block-height,
            assessor: tx-sender,
            recommendations: recommendations
        })
        
        (var-set next-assessment-id (+ assessment-id u1))
        (ok assessment-id)
    )
)

;; Wellness program management
(define-public (create-wellness-program
    (program-name (string-ascii 50))
    (program-type (string-ascii 30))
    (description (string-ascii 200))
    (target-participants uint)
    (duration-weeks uint)
    (wellness-focus (string-ascii 30))
    (success-metrics (list 5 (string-ascii 30)))
)
    (let ((program-id (var-get next-wellness-program-id)))
        (asserts! (is-wellness-coordinator tx-sender) (err ERR-UNAUTHORIZED))
        
        (map-set wellness-programs program-id {
            program-id: program-id,
            program-name: program-name,
            program-type: program-type,
            description: description,
            target-participants: target-participants,
            duration-weeks: duration-weeks,
            wellness-focus: wellness-focus,
            success-metrics: success-metrics,
            coordinator: tx-sender,
            is-active: true,
            created-at: block-height
        })
        
        (var-set next-wellness-program-id (+ program-id u1))
        (ok program-id)
    )
)

;; Career development functions
(define-public (create-development-plan
    (employee principal)
    (current-position (string-ascii 30))
    (target-position (string-ascii 30))
    (development-plan (string-ascii 200))
    (required-training (list 5 uint))
    (mentor (optional principal))
    (target-completion uint)
)
    (let ((development-id (var-get next-development-id)))
        (asserts! (is-training-coordinator tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (> target-completion block-height) (err ERR-INVALID-DATE))
        
        (map-set career-development development-id {
            development-id: development-id,
            employee: employee,
            current-position: current-position,
            target-position: target-position,
            development-plan: development-plan,
            required-training: required-training,
            mentor: mentor,
            target-completion: target-completion,
            progress-percentage: u0,
            status: "active",
            created-at: block-height
        })
        
        (var-set next-development-id (+ development-id u1))
        (ok development-id)
    )
)

(define-public (update-development-progress
    (development-id uint)
    (progress-percentage uint)
    (status (string-ascii 20))
)
    (begin
        (asserts! (is-training-coordinator tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (<= progress-percentage u100) (err ERR-INVALID-SCORE))
        
        (match (map-get? career-development development-id)
            dev-data (begin
                (map-set career-development development-id 
                    (merge dev-data { 
                        progress-percentage: progress-percentage,
                        status: status
                    }))
                (ok true)
            )
            (err ERR-PROGRAM-NOT-FOUND)
        )
    )
)

;; Certification management
(define-public (update-employee-certifications
    (employee principal)
    (active-certifications (list 10 (string-ascii 30)))
    (expiring-certifications (list 5 {cert-name: (string-ascii 30), expiry-date: uint}))
    (required-renewals (list 5 (string-ascii 30)))
    (ce-credits-earned uint)
    (ce-credits-required uint)
)
    (begin
        (asserts! (is-training-coordinator tx-sender) (err ERR-UNAUTHORIZED))
        
        (map-set employee-certifications employee {
            active-certifications: active-certifications,
            expiring-certifications: expiring-certifications,
            required-renewals: required-renewals,
            ce-credits-earned: ce-credits-earned,
            ce-credits-required: ce-credits-required,
            last-updated: block-height
        })
        (ok true)
    )
)

;; Analytics and reporting functions
(define-read-only (calculate-wellness-score (employee principal))
    (match (map-get? wellness-assessments (- (var-get next-assessment-id) u1))
        assessment-data (let (
            (stress (get stress-level assessment-data))
            (balance (get work-life-balance assessment-data))
            (satisfaction (get job-satisfaction assessment-data))
            (burnout (get burnout-risk assessment-data))
            (physical (get physical-wellness assessment-data))
            (mental (get mental-wellness assessment-data))
        )
            (some (/ (+ balance satisfaction physical mental (- u10 stress) (- u10 burnout)) u6))
        )
        none
    )
)

(define-read-only (get-training-completion-rate (program-id uint))
    ;; Simplified calculation - would typically aggregate all enrollments
    (some u75) ;; Placeholder
)

;; Read-only functions
(define-read-only (get-training-program (program-id uint))
    (map-get? training-programs program-id)
)

(define-read-only (get-training-enrollment (enrollment-id uint))
    (map-get? training-enrollments enrollment-id)
)

(define-read-only (get-wellness-assessment (assessment-id uint))
    (map-get? wellness-assessments assessment-id)
)

(define-read-only (get-wellness-program (program-id uint))
    (map-get? wellness-programs program-id)
)

(define-read-only (get-career-development (development-id uint))
    (map-get? career-development development-id)
)

(define-read-only (get-employee-certifications (employee principal))
    (map-get? employee-certifications employee)
)
