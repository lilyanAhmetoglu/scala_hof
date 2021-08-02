

  object Trie {

    // Alphabet size
    val ALPHABET_SIZE: Int = 26

    class TrieNode() {


      var children: Array[TrieNode] = new Array[TrieNode](ALPHABET_SIZE)

      // end of a word
      var isEndOfWord: Boolean = false

      for (i <- 0 until ALPHABET_SIZE) children(i) = null

    }

    var root: TrieNode = _

    // just marks leaf node
    def insert(key: String): Unit = {
      var level: Int = 0
      val length: Int = key.length
      var index: Int = 0
      var pCrawl: TrieNode = root
      level = 0
      while (level < length) {
        index = key.charAt(level) - 'a'
        if (pCrawl.children(index) == null)
          pCrawl.children(index) = new TrieNode()
        pCrawl = pCrawl.children(index) { level += 1; level - 1 }
      }
      // mark last node as leaf
      pCrawl.isEndOfWord = true
    }

    // Returns true if key presents in trie, else false
    def search(key: String): Boolean = {
      var level: Int = 0
      val length: Int = key.length
      var index: Int = 0
      var pCrawl: TrieNode = root
      level = 0
      while (level < length) {
        index = key.charAt(level) - 'a'
        if (pCrawl.children(index) == null) false
        pCrawl = pCrawl.children(index) { level += 1; level - 1 }
      }
      (pCrawl.isEndOfWord)
    }


    def main(text: Array[String]): Unit = {
      val keys: Array[String] =
        Array("the", "a", "there", "answer", "any", "by", "bye", "their")
      val output: Array[String] = Array("Not present in trie", "Present in trie")
      root = new TrieNode()
      var i: Int = 0
      i = 0
      while (i < keys.length) { insert(keys(i)) { i += 1; i - 1 } }
      // Search for different keys
      if (search("the") == true) println("the --- " + output(1))
      else println("the --- " + output(0))
      if (search("these") == true) println("these --- " + output(1))
      else println("these --- " + output(0))
      if (search("their") == true) println("their --- " + output(1))
      else println("their --- " + output(0))
      if (search("thaw") == true) println("thaw --- " + output(1))
      else println("thaw --- " + output(0))
    }

  }

  class Trie {}



