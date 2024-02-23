// Project Name: Image processing in F# 
// Project Description: Assignment is to implement the five image // processing functions found in the F# file “Library.fs”: //Grayscale, Threshold, FlipHorizontal, EdgeDetect, and RightRotate90.
// Name: Abdullah Irfan
// Netid: airfan6
// Date: 10/28/2023



  let rec Threshold_ (row:(int*int*int) list) 
                     (thresh:int)
                     (depth:int) =
    match row with
    | [] -> row
    | (R,G,B)::tl -> let red = findThreshold R thresh depth
                     let green = findThreshold G thresh depth
                     let blue = findThreshold B thresh depth
                     (red,green,blue)::Threshold_ tl thresh depth 

  let rec threshTail (width:int)
                     (height:int)
                     (depth:int)
                     (image:(int*int*int) list list)
                     (thresh:int)
                     (result:(int*int*int) list list) = 
    match image with 
      | [] -> List.rev result
      | hd::tl -> threshTail width height depth tl thresh (Threshold_ hd thresh depth::result)

  let rec Threshold (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) 
                    (thresh:int) = 
    threshTail width height depth image thresh []
    // for now, just return the image back, i.e. do nothing:



 
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    match image with 
    | [] -> image
    | hd::tl -> List.rev hd::FlipHorizontal width height depth tl
    // for now, just return the image back, i.e. do nothing:



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
  // let rec EdgeDetect (width:int)
  //              (height:int)
  //              (depth:int)
  //              (image:(int*int*int) list list)
  //              (threshold:int) = 
  //   // for now, just return the image back, i.e. do nothing:
  //   image

  let rec EdgeDetect (width:int)
                 (height:int)
                 (depth:int)
                 (image:(int*int*int) list list)
                 (threshold:int) =
    let EdgeCal pt1 pt2 = 
      match pt1, pt2 with
      | (x1,y1,z1),(x2,y2,z2) -> (sqrt ((float)(x1-x2) ** 2.0 + (float)(y1-y2) ** 2.0 + (float)(z1-z2) ** 2.0 ))


    let rec GetNext LT PT H W =
    //printfn "Current Point %A %A %A" PT H W

      if W = width || H = height then 
        (1,1,1)
      else  
        let NextPoint = (image.Item(H-1).Item(W))
        let BellowPoint = (image.Item(H).Item(W-1))
        //printfn "Next Point %A" NextPoint
        //printfn "Point Bellow is %A" BellowPoint
        let CalNext = EdgeCal PT NextPoint
        //printfn "Next Point Calc %A" CalNext
        let CalDown = EdgeCal PT BellowPoint
        //printfn "Bellow Point Calc %A" CalDown
        if CalNext > (float)threshold || CalDown > (float)threshold then
        //printfn "Edge Found"
          (0,0,0)
        //printfn ""
        else  
        //printfn "No Edge"
          (255,255,255)
        //printfn ""
    let rec PointExt LT H W = 
      match LT with
      | [] -> []
      | hd::tl when (GetNext LT hd H (W+1)) = (0,0,0) || (GetNext LT hd H (W+1)) = (255,255,255) -> (GetNext LT hd H (W+1))::PointExt tl H (W+1)
      | _ -> []


    let rec access LT H W=
      match LT with
      | [] -> []
      | hd::tl when (PointExt hd (H+1) W) <> [] -> (PointExt hd (H+1) W)::access tl (H+1) W
      | _ -> []

    let Width = 0
    let Height = 0
    let image2 = access image Height Width// New image list creation

    //printfn "%A" image
    //printfn "%A" image2
    image2
    
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
    let img = List.transpose image
    FlipHorizontal width height depth img
    // for now, just return the image back, i.e. do nothing:
