import cv2
import numpy as np

def nothing(x):
    print("1")
    pass
cv2.namedWindow('image')
cv2.createTrackbar('B','image',1,2000,nothing)

img = cv2.imread('test2.png',0)
prevPos = 1

while(True):
    bimg = cv2.medianBlur(img,5)
    cimg = cv2.cvtColor(bimg,cv2.COLOR_GRAY2BGR)

    param2 = cv2.getTrackbarPos('B','image')
    if(param2 != prevPos):
        circles = cv2.HoughCircles(bimg,cv2.HOUGH_GRADIENT,1,param2,
                                param1=50,param2=30,minRadius=0,maxRadius=0)

        circles = np.uint16(np.around(circles))
        for i in circles[0,:]:
            cv2.circle(cimg,(i[0],i[1]),i[2],(0,255,0),2)
            cv2.circle(cimg,(i[0],i[1]),2,(0,0,255),3)

        cv2.imshow('image',cimg)


    if cv2.waitKey(1) == 27:
        break
cv2.destroyAllWindows()
