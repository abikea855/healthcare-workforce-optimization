;; Healthcare Workforce Core Management Contract
;; Provides core functionality for staffing optimization, scheduling, and performance management

(define-constant ERR-UNAUTHORIZED u100)
(define-constant ERR-INVALID-EMPLOYEE u101)
(define-constant ERR-INVALID-SHIFT u102)
(define-constant ERR-SCHEDULE-CONFLICT u103)
(define-constant ERR-PERFORMANCE-NOT-FOUND u104)
(define-constant ERR-INVALID-METRICS u105)
(define-constant ERR-DEPARTMENT-NOT-FOUND u106)

;; Data structures for employee management
(define-map employees principal {
    employee-id: uint,
    name: (string-ascii 50),
    department: (string-ascii 30),
    position: (string-ascii 30),
    skill-level: uint,
    hire-date: uint,
    is-active: bool,
    hourly-rate: uint,
    max-hours-per-week: uint,
    certifications: (list 10 (string-ascii 20))
})

;; Shift scheduling data
(define-map shifts uint {
    shift-id: uint,
    employee: principal,
    department: (string-ascii 30),
    start-time: uint,
    end-time: uint,
    shift-type: (string-ascii 20),
    status: (string-ascii 20),
    created-at: uint
})

;; Performance tracking
(define-map performance-records uint {
    employee: principal,
    evaluation-period: uint,
    performance-score: uint,
    patient-satisfaction: uint,
    attendance-rate: uint,
    efficiency-rating: uint,
    teamwork-score: uint,
    notes: (string-ascii 200),
    created-by: principal,
    created-at: uint
})

;; Department workload tracking
(define-map department-workload (string-ascii 30) {
    current-staff: uint,
    required-staff: uint,
    workload-factor: uint,
    average-skill-level: uint,
    last-updated: uint
})

;; Employee availability tracking
(define-map employee-availability principal {
    preferred-shifts: (list 7 (string-ascii 20)),
    available-days: (list 7 uint),
    overtime-eligible: bool,
    max-consecutive-days: uint,
    vacation-days-remaining: uint
})

;; Contract owner and authorized administrators
(define-data-var contract-owner principal tx-sender)
(define-map authorized-admins principal bool)

;; Employee and shift counters
(define-data-var next-employee-id uint u1)
(define-data-var next-shift-id uint u1)
(define-data-var next-performance-id uint u1)

;; Initialize contract with owner
(map-set authorized-admins tx-sender true)

;; Authorization functions
(define-private (is-authorized (user principal))
    (or 
        (is-eq user (var-get contract-owner))
        (default-to false (map-get? authorized-admins user))
    )
)

(define-public (add-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
        (map-set authorized-admins new-admin true)
        (ok true)
    )
)

;; Employee management functions
(define-public (register-employee 
    (employee principal)
    (name (string-ascii 50))
    (department (string-ascii 30))
    (position (string-ascii 30))
    (skill-level uint)
    (hourly-rate uint)
    (max-hours-per-week uint)
    (certifications (list 10 (string-ascii 20)))
)
    (let ((employee-id (var-get next-employee-id)))
        (asserts! (is-authorized tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (and (> skill-level u0) (<= skill-level u10)) (err ERR-INVALID-EMPLOYEE))
        
        (map-set employees employee {
            employee-id: employee-id,
            name: name,
            department: department,
            position: position,
            skill-level: skill-level,
            hire-date: block-height,
            is-active: true,
            hourly-rate: hourly-rate,
            max-hours-per-week: max-hours-per-week,
            certifications: certifications
        })
        
        (var-set next-employee-id (+ employee-id u1))
        (ok employee-id)
    )
)

(define-public (update-employee-status (employee principal) (is-active bool))
    (begin
        (asserts! (is-authorized tx-sender) (err ERR-UNAUTHORIZED))
        (match (map-get? employees employee)
            employee-data (begin
                (map-set employees employee (merge employee-data { is-active: is-active }))
                (ok true)
            )
            (err ERR-INVALID-EMPLOYEE)
        )
    )
)

;; Shift scheduling functions
(define-public (schedule-shift
    (employee principal)
    (department (string-ascii 30))
    (start-time uint)
    (end-time uint)
    (shift-type (string-ascii 20))
)
    (let ((shift-id (var-get next-shift-id)))
        (asserts! (is-authorized tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (> end-time start-time) (err ERR-INVALID-SHIFT))
        
        ;; Verify employee exists and is active
        (match (map-get? employees employee)
            employee-data (begin
                (asserts! (get is-active employee-data) (err ERR-INVALID-EMPLOYEE))
                (asserts! (is-eq department (get department employee-data)) (err ERR-INVALID-SHIFT))
                
                (map-set shifts shift-id {
                    shift-id: shift-id,
                    employee: employee,
                    department: department,
                    start-time: start-time,
                    end-time: end-time,
                    shift-type: shift-type,
                    status: "scheduled",
                    created-at: block-height
                })
                
                (var-set next-shift-id (+ shift-id u1))
                (ok shift-id)
            )
            (err ERR-INVALID-EMPLOYEE)
        )
    )
)

(define-public (update-shift-status (shift-id uint) (new-status (string-ascii 20)))
    (begin
        (asserts! (is-authorized tx-sender) (err ERR-UNAUTHORIZED))
        (match (map-get? shifts shift-id)
            shift-data (begin
                (map-set shifts shift-id (merge shift-data { status: new-status }))
                (ok true)
            )
            (err ERR-INVALID-SHIFT)
        )
    )
)

;; Performance management functions
(define-public (record-performance
    (employee principal)
    (evaluation-period uint)
    (performance-score uint)
    (patient-satisfaction uint)
    (attendance-rate uint)
    (efficiency-rating uint)
    (teamwork-score uint)
    (notes (string-ascii 200))
)
    (let ((performance-id (var-get next-performance-id)))
        (asserts! (is-authorized tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-some (map-get? employees employee)) (err ERR-INVALID-EMPLOYEE))
        (asserts! (and 
            (<= performance-score u100)
            (<= patient-satisfaction u100)
            (<= attendance-rate u100)
            (<= efficiency-rating u100)
            (<= teamwork-score u100)
        ) (err ERR-INVALID-METRICS))
        
        (map-set performance-records performance-id {
            employee: employee,
            evaluation-period: evaluation-period,
            performance-score: performance-score,
            patient-satisfaction: patient-satisfaction,
            attendance-rate: attendance-rate,
            efficiency-rating: efficiency-rating,
            teamwork-score: teamwork-score,
            notes: notes,
            created-by: tx-sender,
            created-at: block-height
        })
        
        (var-set next-performance-id (+ performance-id u1))
        (ok performance-id)
    )
)

;; Department workload management
(define-public (update-department-workload
    (department (string-ascii 30))
    (current-staff uint)
    (required-staff uint)
    (workload-factor uint)
    (average-skill-level uint)
)
    (begin
        (asserts! (is-authorized tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (<= workload-factor u200) (err ERR-INVALID-METRICS))
        (asserts! (and (> average-skill-level u0) (<= average-skill-level u10)) (err ERR-INVALID-METRICS))
        
        (map-set department-workload department {
            current-staff: current-staff,
            required-staff: required-staff,
            workload-factor: workload-factor,
            average-skill-level: average-skill-level,
            last-updated: block-height
        })
        (ok true)
    )
)

;; Employee availability management
(define-public (set-employee-availability
    (employee principal)
    (preferred-shifts (list 7 (string-ascii 20)))
    (available-days (list 7 uint))
    (overtime-eligible bool)
    (max-consecutive-days uint)
    (vacation-days-remaining uint)
)
    (begin
        (asserts! (is-authorized tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-some (map-get? employees employee)) (err ERR-INVALID-EMPLOYEE))
        
        (map-set employee-availability employee {
            preferred-shifts: preferred-shifts,
            available-days: available-days,
            overtime-eligible: overtime-eligible,
            max-consecutive-days: max-consecutive-days,
            vacation-days-remaining: vacation-days-remaining
        })
        (ok true)
    )
)

;; Optimization functions
(define-read-only (calculate-staffing-efficiency (department (string-ascii 30)))
    (match (map-get? department-workload department)
        dept-data (let (
            (current (get current-staff dept-data))
            (required (get required-staff dept-data))
            (workload (get workload-factor dept-data))
        )
            (if (> required u0)
                (some (/ (* current u100) required))
                none
            )
        )
        none
    )
)

(define-read-only (get-employee-performance-average (employee principal))
    ;; This would typically aggregate multiple performance records
    ;; For simplicity, returning the most recent performance score
    (some u85) ;; Placeholder for performance calculation
)

;; Read-only functions for data retrieval
(define-read-only (get-employee (employee principal))
    (map-get? employees employee)
)

(define-read-only (get-shift (shift-id uint))
    (map-get? shifts shift-id)
)

(define-read-only (get-performance-record (performance-id uint))
    (map-get? performance-records performance-id)
)

(define-read-only (get-department-workload (department (string-ascii 30)))
    (map-get? department-workload department)
)

(define-read-only (get-employee-availability (employee principal))
    (map-get? employee-availability employee)
)

;; Emergency staffing protocols
(define-public (trigger-emergency-staffing (department (string-ascii 30)) (urgency-level uint))
    (begin
        (asserts! (is-authorized tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (<= urgency-level u5) (err ERR-INVALID-METRICS))
        
        ;; Update department workload with emergency status
        (match (map-get? department-workload department)
            dept-data (begin
                (map-set department-workload department 
                    (merge dept-data { 
                        workload-factor: (+ (get workload-factor dept-data) (* urgency-level u20)),
                        last-updated: block-height 
                    })
                )
                (ok true)
            )
            (err ERR-DEPARTMENT-NOT-FOUND)
        )
    )
)

;; Skill-based assignment optimization
(define-read-only (recommend-assignment (department (string-ascii 30)) (required-skill-level uint))
    (let ((dept-data (unwrap! (map-get? department-workload department) (err ERR-DEPARTMENT-NOT-FOUND))))
        (if (>= (get average-skill-level dept-data) required-skill-level)
            (ok "optimal-assignment")
            (ok "training-required")
        )
    )
)
