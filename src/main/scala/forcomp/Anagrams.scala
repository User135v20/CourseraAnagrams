package forcomp

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {

    def charLists(word: List[Char]): List[List[Char]] ={
      word match {
        case Nil => Nil
        case List(x) => List(word)
        case x :: xs =>
          val (first,second) = word span(y => y == x)
          first :: charLists(second)
      }
    }

    def charCounts (list: List[List[Char]]): Occurrences = {
      list match {
        case Nil => Nil
        case List(x) => List((x.head, x.length))
        case x :: xs => List((x.head,x.length)) ::: charCounts(xs)
      }
    }
    if (w.filter(_.isLetter).isEmpty) Nil
    else charCounts(charLists(w.filter(_.isLetter).toLowerCase.toList.sorted))
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {

    def sumDuplicates(list: Occurrences): Int ={
      list match {
        case Nil => 0
        case List(x) => x._2
        case x :: xs => x._2 + sumDuplicates(xs)
      }
    }

    def unionDuplicates(occurrences: Occurrences) : Occurrences ={
      occurrences match {
        case Nil => Nil
        case List(x) => occurrences
        case x :: xs =>
          if (xs.exists(y => y._1 == x._1)) {
            (x._1, sumDuplicates(occurrences.takeWhile(el => el._1 == x._1))) ::
              unionDuplicates(occurrences.dropWhile(el => el._1 == x._1))
          }
          else x :: unionDuplicates(xs)
      }
    }
    s match {
      case Nil => Nil
      case List(x) => wordOccurrences(x)
      case x :: xs => unionDuplicates((wordOccurrences(x) ::: sentenceOccurrences(xs)).sorted)
    }
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {

    def addWord(word: Word, map: Map[Occurrences, List[Word]]):Map[Occurrences, List[Word]]  ={
      val sequence = wordOccurrences(word)
      if (map.isEmpty || !map.contains(sequence))
        map + (sequence -> List(word))
      else
        map.updated(sequence, (map(sequence) :+ word).sorted)
    }

    def wordIter(list: List[Word], map: Map[Occurrences, List[Word]]): Map[Occurrences, List[Word]] ={
      list match {
        case Nil => Map()
        case List(x) => addWord(x,map)
        case x :: xs => wordIter(xs,addWord(x,map))
      }
    }
    wordIter(dictionary, Map())
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences get (wordOccurrences(word)) match {
      case None => Nil
      case Some(list: List[Word]) => list
    }
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    // создает лист (Char,Int). Позволяет из ('a',3) создать ('a',3),('a',2),('a',1)
    def simpleListCreater(tuple: (Char,Int)): List[Occurrences] = {
      tuple._2 match {
        case 0 => Nil
        case _ => (List((tuple._1,tuple._2))) :: simpleListCreater((tuple._1,tuple._2-1))
      }
    }

    def matchInputList(list: Occurrences): List[Occurrences] ={
      list match {
        case Nil => List(Nil)
        case List(x) => simpleListCreater(x)
        case x :: xs => simpleListCreater(x) ++ matchInputList(xs)
      }
    }
    //объединяет полученные листы и созданный. т.к. предусмотрен случай Nil дает на выходе
    //  добавление к результату созданного листа, в рез-те получаем перебор всех вариантов
    def unionSimpleList(firstList: List[Occurrences], secondList: List[Occurrences]): List[Occurrences] = {
      (firstList, secondList) match {
        case (Nil,Nil) => Nil
        case ( _ , Nil) => firstList ::: secondList
        case (Nil, _ ) => secondList ::: firstList
        case (x :: xs, List(y)) => (x ::: y)  :: unionSimpleList(xs,secondList)
      //case (List(x), y :: ys) => (x ::: y) :: unionSimpleList(firstList,ys)
        case (x :: xs, y :: ys) => (List(x ::: y) ::: unionSimpleList(List(x),ys) ::: unionSimpleList(xs,List(y)) ::: unionSimpleList(xs,ys))
      }
    }
    def unionList(resList: List[Occurrences], summand : Occurrences): List[Occurrences] = {
      summand match {
        case Nil => resList
        case List(x) => unionSimpleList(resList, matchInputList(summand)) ::: resList
        case x :: xs => unionList(unionSimpleList(resList, matchInputList(List(x))) ::: resList, xs)
      }
    }
    List(List.empty) ::: unionList(List(),occurrences)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {

    def remove(from: Occurrences, el: (Char, Int)): Occurrences = {
      val first = from.filter(y => y._1 == el._1)
      val tailList = from.filter(x => x != first.head)
      first.head._2 - el._2 match {
        case x if x > 0 => (tailList :+ (first.head._1, x)).sorted
        case _ => tailList.sorted
      }
    }

    def subtractingLists(list: Occurrences, deductible: Occurrences): Occurrences = {
      deductible match {
        case Nil => list
        case List(x) => remove(list,x)
        case x :: xs => subtractingLists(remove(list,x),xs)
      }
    }
    subtractingLists(x,y)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    // последовательность листов символ-значение исходной фразы
    val OCCURENCES = sentenceOccurrences(sentence)

    def getWord(occ: Occurrences): List[Word] = {
      dictionaryByOccurrences get (occ) match {
        case None => Nil
        case Some(x) => x
      }
    }
    // функция, которая возвращает список слов, которые могут быть составлены из входных последовательностей символов
    def iterFindWord(list: List[Occurrences]): List[Word] = {
      list match {
        case Nil => Nil
        case List(x) => getWord(x)
        case x :: xs => getWord(x) ::: iterFindWord(xs)
      }
    }
    // объединяет списки слов в список последовательностей
    def union(first: List[Word], second: List[Word]): List[Sentence] = {
      second match {
        case Nil => List(first)
        case List(x) => List(first :+ x)
        case x :: xs => (first :+ x) :: union(first,xs)
      }
    }
    // добавляет слово к списку последовательности слов
    def unionSent(first: List[Sentence], second: List[Word]): List[Sentence] = {
      (first,second) match {
        case (Nil, Nil) => Nil
        case (Nil, y :: ys) => Nil
        case (x::xs, Nil) => first
        case (x :: xs, List(y)) => List(x :+ y) ::: unionSent(xs,second)
        case (x :: xs, y :: ys)  => List(x :+ y) ::: unionSent(first,ys) ::: unionSent(xs,second)
      }
    }
    // проверяет остались ли символы после вычитания символов получившейся последовательности символов из исходной
    def checkFreeSymbol(occInput: Occurrences, occSource: Occurrences): Boolean = {
      subtract(occSource, occInput) match {
        case Nil => true
        case _ => false
      }
    }
    // формирование элемента итогового листа
    def createElResList (el: List[Sentence]) : List[Sentence] = {
      if (checkFreeSymbol(sentenceOccurrences(el.head), OCCURENCES))
        el
      else
        unionSent(el, iterFindWord(combinations(subtract(OCCURENCES,sentenceOccurrences(el.head)))))
    }
    // формирование итогового листа
    def createResList(list: List[Sentence]): List[Sentence] = {
      list match {
        case Nil => Nil
        case List(x) => list ::: createElResList(list)
        case x :: xs => List(x) ::: createElResList(List(x)) ::: createResList(xs)
      }
    }
    // создание листа (последовательности слов) из листа слов.
    // Необходимо для формирования списка, в дальнейшем слова будут добавляться
    // к последовательности слов т.е. к List[Sentence], а тут к List[Word]
    def createFirstList(list: List[Word]): List[Sentence] = {
      list match {
        case Nil => Nil
        case List(x) => union(list, iterFindWord(combinations(subtract(OCCURENCES,wordOccurrences(list.head)))))
        case x :: xs => union(List(x), iterFindWord(combinations(subtract(OCCURENCES,wordOccurrences(List(x).head))))) ::: createFirstList(xs)
      }
    }
    // запуск функции
    if (sentence.isEmpty) List(List.empty)
    else {
      // фильтрация отсекает все комбинации слов, где использованы не все символы, входящей фразы
      createResList(createFirstList(iterFindWord(combinations(OCCURENCES)))).filter(x=>checkFreeSymbol(sentenceOccurrences(x),OCCURENCES))
    }
  }
}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
