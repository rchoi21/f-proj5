//
// F# image processing functions.
//
// More details?
//
// Rebekah Choi UIC 11/24/2022
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //


  //
  // grayscale helper function for list of tuples
  //
  let rec grayscaleProcessTL (list: (int*int*int) list) (tupleList: (int*int*int) list) =
    match list with
    | [] -> List.rev tupleList // reverse final list
    | hd::tl -> let (r, g, b) = List.head list
                // printfn "(r,g,b): %A" (r, g, b)
                let weightedAvg = (int)((float)r * 0.299 + (float)g * 0.587 + (float)b * 0.114)
                let (avgR, avgG, avgB) = (weightedAvg, weightedAvg, weightedAvg)
                let newTupleList = (avgR, avgG, avgB)::tupleList
                // printfn "newTupleList: %A" newTupleList
                grayscaleProcessTL tl newTupleList

  //
  // grayscale helper function for list of list of tuples
  //
  let rec grayscaleProcessLL (image: (int*int*int) list list) (newImg: (int*int*int) list list) =
    match image with
    | [] -> List.rev newImg // reverse final list
    | hd::tl -> let L = List.head image
                // printfn "L: %A" L
                let grayTupleList = grayscaleProcessTL L []
                let updatedNewImg = grayTupleList::newImg 
                // printfn "updatedNewImg: %A" updatedNewImg
                grayscaleProcessLL tl updatedNewImg

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // TODO: process given img in order to make a grayscale image
    // printfn "image: %A" image
    let newImg = grayscaleProcessLL image [] // kind of bothers me with the rec keyword in function
    newImg


  //
  // threshold helper function for list of tuples
  //
  let rec thresholdProcessTL (list: (int*int*int) list) (tupleList: (int*int*int) list) threshold =
    match list with
    | [] -> List.rev tupleList // reverse final list
    | hd::tl -> let (r, g, b) = List.head list
                // printfn "(r,g,b): %A" (r, g, b)
                if r <= threshold then
                  if g <= threshold then
                    if b <= threshold then // when r && g && b <= threshold
                      let (newR, newG, newB) = (0, 0, 0)
                      let newTupleList = (newR, newG, newB)::tupleList
                      // printfn "newTupleList: %A" newTupleList
                      thresholdProcessTL tl newTupleList threshold
                    else // when (r && g  <= threshold) && b > threshold
                      let (newR, newG, newB) = (0, 0, 255)
                      let newTupleList = (newR, newG, newB)::tupleList
                      // printfn "newTupleList: %A" newTupleList
                      thresholdProcessTL tl newTupleList threshold
                  else // when g > threshold...
                    if b <= threshold then // when (r && b <= threshold) && g > threshold
                      let (newR, newG, newB) = (0, 255, 0)
                      let newTupleList = (newR, newG, newB)::tupleList
                      // printfn "newTupleList: %A" newTupleList
                      thresholdProcessTL tl newTupleList threshold
                    else // when r <= threshold && (g && b > threshold)
                      let (newR, newG, newB) = (0, 255, 255)
                      let newTupleList = (newR, newG, newB)::tupleList
                      // printfn "newTupleList: %A" newTupleList
                      thresholdProcessTL tl newTupleList threshold
                else // when r > threshold...
                  if g <= threshold then
                    if b <= threshold then // when (g && b <= threshold) && r > threshold
                      let (newR, newG, newB) = (255, 0, 0)
                      let newTupleList = (newR, newG, newB)::tupleList
                      // printfn "newTupleList: %A" newTupleList
                      thresholdProcessTL tl newTupleList threshold
                    else // when g <= threshold && (r && b > threshold)
                      let (newR, newG, newB) = (255, 0, 255)
                      let newTupleList = (newR, newG, newB)::tupleList
                      // printfn "newTupleList: %A" newTupleList
                      thresholdProcessTL tl newTupleList threshold
                  else // when g > threshold...
                    if b <= threshold then // when b <= threshold && (r && g > threshold)
                      let (newR, newG, newB) = (255, 255, 0)
                      let newTupleList = (newR, newG, newB)::tupleList
                      // printfn "newTupleList: %A" newTupleList
                      thresholdProcessTL tl newTupleList threshold
                    else // when r && g && b > threshold
                      let (newR, newG, newB) = (255, 255, 255)
                      let newTupleList = (newR, newG, newB):: tupleList
                      // printfn "newTupleList: %A" newTupleList
                      thresholdProcessTL tl newTupleList threshold

  //
  // threshold helper function for list of list of tuples
  //
  let rec thresholdProcessLL (image: (int*int*int) list list) (newImg: (int*int*int) list list) threshold =
    match image with
    | [] -> List.rev newImg // reverse final list
    | hd::tl -> let L = List.head image
                // printfn "L: %A" L
                let thresholdTupleList = thresholdProcessTL L [] threshold
                let updatedNewImg = thresholdTupleList::newImg 
                // printfn "updatedNewImg: %A" updatedNewImg
                thresholdProcessLL tl updatedNewImg threshold

  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // TODO: make the image into 8 colors only based on threshold
    let newImg = thresholdProcessLL image [] threshold
    newImg

  //
  // flip horizontal helper function for list of list of tuples
  //
  let rec flipHorizontalProcessLL (image: (int*int*int) list list) (newImg: (int*int*int) list list) =
    match image with
    | [] -> List.rev newImg
    | hd::tl -> let tupleList = List.head image
                let reversedTupleList = List.rev tupleList
                let updatedNewImg = reversedTupleList::newImg
                flipHorizontalProcessLL tl updatedNewImg
    


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // printfn "image: %A" image
    // TOOD: reverse each list of tuples in image
    let newImg = flipHorizontalProcessLL image []
    // printfn "newImg: %A" newImg
    newImg

  //
  // edge detection helper function for calculating "difference" between pixels
  //
  let findPixDiff (r1, g1, b1) (r2, g2, b2) = 
    let difference = sqrt((float)((r1 - r2)*(r1 - r2) + (g1 - g2)*(g1 - g2) + (b1 - b2)*(b1 - b2)))
    difference

  //
  // edge detection helper function for comparing two tuple Lists and making a new edge tuple list
  //
  let rec findEdges (tupleList1: (int*int*int) list) (tupleList2: (int*int*int) list) (edgList: (int*int*int) list) threshold =
    match tupleList1 with
    | [] -> List.rev edgList
    | hd::tl -> if (List.isEmpty tl = false) then // hd is not the last tuple in list
                    let diffRight = findPixDiff hd (List.head tl)
                    let diffDown = findPixDiff hd (List.head tupleList2)
                    if diffRight > ((float)threshold) || diffDown > ((float)threshold) then
                        let newList = (0,0,0)::edgList
                        findEdges tl (List.tail tupleList2) newList threshold
                    else
                        let newList = (255,255,255)::edgList
                        findEdges tl (List.tail tupleList2) newList threshold
                else // if last tuple on list, do nothing
                    findEdges tl (List.tail tupleList2) edgList threshold

  //
  // edge detection helper function for adding the edge tuple lists to a new image
  //
  let rec makeEdgeImg (image: (int*int*int) list list) (edgeImg: (int*int*int) list list) threshold =
    match image with
    | [] -> List.rev edgeImg
    | hd::tl -> if List.isEmpty tl then // if last tuple list, do nothing
                    makeEdgeImg tl edgeImg threshold
                else
                    let tupleList2 = List.head tl   
                    // printfn "hd: %A" hd
                    let edgList = findEdges hd tupleList2 [] threshold
                    let newImg = edgList::edgeImg
                    makeEdgeImg tl newImg threshold

  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "signigicantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compares each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    // TODO: process image to detect an edge 
    let newImg = makeEdgeImg image [] threshold
    newImg

  //
  // helper function #1 for RotateRight90: from an image list, 
  // it makes a new image without the first tuple of every 
  // tuple list within the image. 
  //
  let rec colListImg (image: (int*int*int) list list) (tailImg: (int*int*int) list list) =
    match image with
    | [] -> List.rev tailImg
    | hd::tl -> let rowTail = List.tail hd // get list of tuples w/o first tuple
                let newImg = rowTail::tailImg
                colListImg tl newImg

  //
  // helper function #2 for RotateRight90: from an image list, 
  // it makes a new tuple list with the first tuple of every 
  // tuple list within the image. 
  //
  let rec getColumnList (image: (int*int*int) list list) (list: (int*int*int) list) =
    match image with
    | [] -> list
    | hd::tl -> let colList = (List.head hd)::list // append the first tuple to tuple acc
                getColumnList tl colList

  //
  // helper function #3 for RotateRight90: from an image list, 
  // it makes a new image list using helper functions #1 & #2. 
  //
  let rec makeRotatedImg (image: (int*int*int) list list) (rotatedImg: (int*int*int) list list) =
    match image with 
    | [] -> List.rev rotatedImg
    | hd::tl -> let colList = getColumnList image []
                // printfn "colList: %A" colList
                let smallerImg = colListImg image []
                // printfn "smallerImg: %A" smallerImg
                let newImg = colList::rotatedImg
                // printfn "newImg (in prog): %A" newImg
                if (List.isEmpty (List.head smallerImg)) then
                    // printfn "smth"
                    makeRotatedImg [] newImg
                else
                    makeRotatedImg smallerImg newImg

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // TODO: TRANSPOSE inverse "MATRIX" (MATH 310 HAS PURPOSE LOLL) Edit: not really...
    let newImg = makeRotatedImg image []
    newImg