package recfun

object Main {
  def main(args: Array[String]) {
  	
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
   
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      def iterator (rowIterator: Int, previousRow: Array[Int]): Array[Int] = {
        if (rowIterator == r) {
          previousRow
        } else {
          var newRow:Array[Int] = new Array[Int](previousRow.length + 1)
          newRow(0) = 1
          var newRowIndex = 1;
          var stopIndex:Int = previousRow.length-2
          //var index = 0
          for(i <- 0 to stopIndex ) {
            newRow(newRowIndex) = previousRow(i) + previousRow(i + 1)
            newRowIndex = newRowIndex + 1
          }
          newRow(newRow.length-1) = 1;
          iterator(rowIterator+1, newRow)
        }
      }
      var lastRow = iterator(0, Array(1))
      return lastRow(c)
    }
  
  /**
   * Exercise 2
   */
  def balance (chars: List[Char]): Boolean = {
		def iterator (iteratorChars: List[Char], switch: Int): Boolean =
			if(iteratorChars.isEmpty) {
				switch == 0
			} else if (switch == -1) {
				false
			} else {
				iterator(iteratorChars.tail, validateChar(iteratorChars.head, switch));
			}
		
		
		def validateChar(iteratorChar: Char, switch: Int): Int = {
			if (iteratorChar == '(') {
				switch +1
			} else if (iteratorChar == ')'){
				switch - 1
			} else {
				switch
			}
		}
		iterator(chars, 0)
	}
	 
  /**
   * Exercise 3
   */
   	 def countChange(money: Int, coins: List[Int]): Int = {
    	def iterator (money: Int, coins:List[Int], count: Int): Int = {
    		if (money < 0) {
    			count
    		} else {
    			if (coins.isEmpty) {
    				if (money == 0) {
    						count + 1
    					
    				} else {
    					count
    				}
    		} else {
    		 iterator(money - coins.head, coins, count) - iterator(money, coins.tail, count)
    			}
    		}
    	}
    	
    	val result = iterator(money, coins, 0)
    	if(result < 0) - result else result
    }   
  }
