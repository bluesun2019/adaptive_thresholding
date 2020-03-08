if(!suppressWarnings(require(jpeg))){
  install.packages('jpeg')
  require(jpeg)
}# 该包对.jpg的导入和导出效果很好。
img<-readJPEG("picture.jpg")# 必须转录地址。此处地址可更改。
img<-img[,,1]*0.3+img[,,2]*0.59+img[,,3]*0.11 # 将RGB转化为灰度矩阵
thresh<-function(image,s,t){
  w<-dim(image)[1]
  h<-dim(image)[2]
  intImg<-matrix(,w,h)
  out<-matrix(,w,h)
  for (i in 1:w){
    sum<-0
    for(j in 1:h){
      sum<-sum+image[i,j] # f(x,y)+I(x,y-1)-I(x-1,y-1)
      if(i==1){intImg[i,j]<-sum}
      else{
        intImg[i,j]<-intImg[i-1,j]+sum}
    }
  }
  for(i in 1:w){
    for(j in 1:h){
      x1<-max(i-round(s/2),1)
      x2<-min(i+round(s/2),w)
      y1<-max(j-round(s/2),1)
      y2<-min(j+round(s/2),h)
      count<-(x2-x1)*(y2-y1)
      if(x1!=1&&y1!=1){
        sum<-intImg[x2,y2]-intImg[x1-1,y2]-intImg[x2,y1-1]+intImg[x1-1,y1-1]
      }
      else 
      {
        if(x1!=1&&y1==1){
          sum<-intImg[x2,y2]-intImg[x1-1,y2]     
        }
        else {
          if(x1==1&&y1!=1){
            sum<-intImg[x2,y2]-intImg[x2,y1-1]
          }
          else{sum<-intImg[x2,y2]}
        }
      }
      if(image[i,j]*count<=sum*(100-t)/100){
        out[i,j]<-0 
      }
      else{
        out[i,j]<-1 # R中的像素范围为[0,1]
      } 
    }
  }
  out
}
image<-thresh(img,1/8*dim(image)[1],15)
writeJPEG(image,sprintf("newpic.jpg"))# 打印图像并保存至工作目录