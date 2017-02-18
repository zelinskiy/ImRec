import cv2
import numpy as np

def handleSlidebar(x):
    drawHough()

def drawHough():
    print("redraw")
    p1 = cv2.getTrackbarPos('p1','image')
    p2 = cv2.getTrackbarPos('p2','image')
    param1 = cv2.getTrackbarPos('param1','image')
    param2 = cv2.getTrackbarPos('param2','image')
    minRadius = cv2.getTrackbarPos('minRadius','image')
    maxRadius = cv2.getTrackbarPos('maxRadius','image')


    bimg = cv2.GaussianBlur(src=img,ksize=(3,3),sigmaX=0, sigmaY=0)
    #bimg = cv2.medianBlur(img,5)
    cimg = cv2.cvtColor(bimg,cv2.COLOR_GRAY2BGR)

    circles = cv2.HoughCircles(bimg,cv2.cv.CV_HOUGH_GRADIENT,param1,param2,
                                param1=50,param2=30,minRadius=minRadius,maxRadius=maxRadius)
    circles = np.uint16(np.around(circles))
    for i in circles[0,:]:
        cv2.circle(cimg,(i[0],i[1]),i[2],(0,255,0),2)
        cv2.circle(cimg,(i[0],i[1]),2,(0,0,255),3)

    cv2.imshow('image',cimg)


img = cv2.imread('test4.png',0)
cv2.namedWindow('image')
cv2.createTrackbar('param1','image',1,100,handleSlidebar)
cv2.setTrackbarPos('param1','image',1)

cv2.createTrackbar('param2','image',1,2000,handleSlidebar)
cv2.setTrackbarPos('param2','image',300)

cv2.createTrackbar('minRadius','image',0,1000,handleSlidebar)
cv2.setTrackbarPos('minRadius','image',0)

cv2.createTrackbar('maxRadius','image',0,1000,handleSlidebar)
cv2.setTrackbarPos('maxRadius','image',0)

drawHough()

while(True):
    if cv2.waitKey(1) == 27:
        break
cv2.destroyAllWindows()
