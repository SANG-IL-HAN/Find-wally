library(jpeg)
library(imager)

problem=readJPEG("C:/Users/hsi33/Desktop/wally_problem.jpg")
problem2=load.image("C:/Users/hsi33/Desktop/wally_problem.jpg")
wally=readJPEG("C:/Users/hsi33/Desktop/wally_answer.jpg")


big_row=647
big_col=800
big_mul=(big_row-1)*(big_col-1)

small_row=53
small_col=44
small_mul=(small_row-1)*(small_col-1)

big=matrix(0,nrow=big_row,ncol=big_col)
small=matrix(0,nrow=small_row,ncol=small_col)

for(i in 1:small_row){
  for(j in 1:small_col){
    small[i,j]=0.3*wally[i,j,1]+0.3*wally[i,j,2]+0.3*wally[i,j,3]
  }
}

for(i in 1:big_row){
  for(j in 1:big_col){
    big[i,j]=0.3*problem[i,j,1]+0.3*problem[i,j,2]+0.3*problem[i,j,3]
  }
}

big2=matrix(0,nrow=small_mul)
small2=matrix(0,nrow=small_mul)

for(k in 1:small_row)
{
  for(l in 1:small_col)
  {
    small2[(k-1)*small_row+l]=small[k,l]
  }
}

for(i in 0:(big_row-small_row))
  {
  for(j in 0:(big_col-small_col))
    {
    for(k in 1:small_row)
      {
      for(l in 1:small_col)
        {
        big2[(k-1)*small_row+l]=big[i+k,j+l]
        }
       }
    corVal=cor(big2[1:small_mul],small2[1:small_mul])
    
    if(corVal>=0.9)
    {
      save_i=i;
      save_j=j;
    }
  }
}

for(x in 1:small_col){
  for(y in 1:small_row){
    problem2[save_j+x,save_i+y,1,1]=problem2[save_j+x,save_i+y,1,1]+0.5
    problem2[save_j+x,save_i+y,1,2]=problem2[save_j+x,save_i+y,1,2]+0.5
    problem2[save_j+x,save_i+y,1,3]=problem2[save_j+x,save_i+y,1,3]+0.5
  }
}

plot(problem2)
