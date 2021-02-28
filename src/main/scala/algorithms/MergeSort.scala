package algorithms

/**
 * Merge sort is a divide and conquer algorithm
 * Copies all elements from the target array into a helper array, keeping track of where the start of
 * the left and right halves should be
 * Then iterate through the helper, copying the smaller elements from each half into the array
 */
object MergeSort {

  def sort(list: List[Int]): Unit = {

    def mergesort(list: List[Int], helper: Array[Int], low: Int, high: Int): Unit = {
      if(low < high) {
        val middle: Int = (low + high) / 2
        mergesort(list, helper, low, middle) // sort left half
        mergesort(list, helper, middle + 1, high) // sort right half
        mergeHalves(list.toArray, helper, low, middle, high) // merge the two
      }
    }

    def mergeHalves(list: Array[Int], helper: Array[Int], low: Int, middle: Int, high: Int): Unit = {

      // copy all the data into the temp array
      for(i <- low until high) {
        helper(i) = list(i)
      }

      var helperLeft: Int = low // counter for left array
      var helperRight: Int = middle + 1 // counter for right array
      var current: Int = low

      // iterate through helper array, compare the left and right half, copying back the smaller element
      // from the two halves into the original array
      while(helperLeft <= middle && helperRight <= high) {
        if(helper(helperLeft) <= helper(helperRight)) {
          list(current) = helper(helperRight)
          helperLeft += 1
        }
        else {
          list(current) = helper(helperRight)
          helperRight +=1
        }
        current += 1
      }

      // copy the rest of the left side of the array into the target array
      val remaining: Int = middle - helperLeft
      for(i <- 0 until remaining){
        list(current+1) = helper(helperLeft + i)
      }
    }
    mergesort(list, new Array[Int](list.length), 0, list.length - 1)
  }
}
