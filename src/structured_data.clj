(ns structured-data)
(require 'clojure.set)

(defn do-a-thing [x]
  ( let [ xx ( + x x ) ]
     ( Math/pow xx xx )
  )
)

(defn spiff [v]
   (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
   (let [[x y z] v]
         (+ x z)
    )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
         (- x2 x1)
    )
  )

(defn height [rectangle]
   (let [[[x1 y1] [x2 y2]] rectangle]
         (- y2 y1)
    )
  )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
        (cond
          (= (- x2 x1) (- y2 y1)) true
          :else false
        )
  )
)

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
        (* (- x2 x1) (- y2 y1))
    )
  )

(defn contains-point? [rectangle point]
  (let [[[[x1 y1] [x2 y2]] [p1 p2]] [rectangle point]]
         (if (and (<= x1 p1 x2) (<= y1 p2 y2))
             true
             false
          )
        )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
        (if (and (contains-point? outer (point x1 y1)) (contains-point? outer (point x2 y2)))
              true
              false
           )
        )
  )

(defn title-length [book]
 (count (get book :title))
  )

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
        true
        false
    )
  )

(defn add-author [book new-author]
  (assoc book  :authors (conj (get book :authors) new-author ))
  )

(defn alive? [author]
  (if (not (contains? author :death-year))
        true
        false
    )
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [sec (fn [x] (get x 1) )]
         (map sec collection)
    )
  )

(defn titles [books]
  (let [title (fn [x] (:title x))]
        (map title books)
    )
  )

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
      true                                                    ;
      false
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
  (if (= (count a-seq) (count (set a-seq)))
         false
         true
    )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (if (contains? (:authors book) author)
        true
        false
      )
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (if (contains? author :birth-year)
         (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")" )
         (str  (:name author))
        )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
   (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (cond
      (= (count books) 0) "No books."
      (= (count books) 1) (str (count books) " book. " (str (apply str (interpose ". " (map book->string books))) "."))
      :else (str (count books) " books. " (str (apply str (interpose ". " (map book->string books))) "."))
      )  
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books )
  )

(defn author-by-name [name authors]
   (first (filter (fn [x] (= (:name x) name) ) authors))
  )

(defn living-authors [authors]
 (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
        false
        true
        )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
