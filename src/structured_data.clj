(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (+ x y)
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[left _] [right _]] rectangle]
    (Math/abs (- right left))
  )
)

(defn height [rectangle]
  (let [[[_ bottom] [_ top]] rectangle]
    (Math/abs (- top bottom)) 
  )
)

(defn square? [rectangle]
  (== (width rectangle) (height rectangle))
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
  (let [[[left bottom] [right top]] rectangle
        [x y] point
       ]
    (and (<= left x right) (<= bottom y top)) 
  )
)

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and 
      (contains-point? outer bottom-left)
      (contains-point? outer top-right)
    )
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (> (author-count book) 1)
)

(defn add-author [book new-author]
  (let [orig-authors (:authors book)
        new-authors (conj orig-authors new-author)
       ]
    (assoc book :authors new-authors)
  )
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)
  )
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (apply clojure.set/union (map :authors books)) 
)

(defn all-author-names [books]
  (set (map :name (authors books)))
)

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)
       ]
    (str
      name
      (if (not= birth nil)
        (str  
          " ("  
          birth  
          " - " 
          death 
          ")"
        )
        ""
      )
    )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str
    (:title book)
    ", written by "
    (authors->string (:authors book)) 
  )
)

(defn books->string [books]
  (let [book-count (count books)]
    (str
      (cond
        (= book-count 0) "No books"
        (= book-count 1) "1 book. "
        :else (str book-count " books. ")
      )
      (apply str (interpose ". " (map book->string books)))
      "."
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; %________%
