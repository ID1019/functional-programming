defmodule Conv do

  ## 1x1 kernel

  def map(l1, kernel) do
    Enum.map(l1, kernel)
  end

  ## 3x3 kernel
  
  def map([n1|_]=n, [l1|_]=l, [s1|_]=s,  kernel) do
    ## first column is duplicated
    map([n1|n], [l1|l], [s1|s], [],  kernel)
  end

  def map([n1,n2,n3|_]=n, [l1,l2,l3|_]=l, [s1,s2,s3|_]=s, line, kernel) do
    k = kernel.([n1,n2,n3,l1,l2,l3,s1,s2,s3])
    [_|nr] = n
    [_|lr] = l
    [_|sr] = s    
    map(nr, lr, sr, [k|line], kernel)
  end
  def map([n1,n2], [l1,l2], [s1,s2], line, kernel) do
    ## last column is duplicated
    k = kernel.([n1,n2,n2,l1,l2,l2,s1,s2,s2])
    Enum.reverse([k|line])
  end


  ## 5x5 kernel 
  def map([nn1|_]=nn, [n1|_]=n, [l1|_]=l, [s1|_]=s, [ss1|_]=ss,  kernel) do
    ## first column is triplicated
    map([nn1,nn1|nn], [n1,n1|n], [l1,l1|l], [s1,s1|s], [ss1,ss1|ss], [],  kernel)
  end
  
  def map([nn1,nn2,nn3,nn4,nn5|_]=nn, [n1,n2,n3,n4,n5|_]=n, [l1,l2,l3,l4,l5|_]=l, [s1,s2,s3,s4,s5|_]=s, [ss1,ss2,ss3,ss4,ss5|_]=ss, line, kernel) do
    k = kernel.([nn1,nn2,nn3,nn4,nn5,n1,n2,n3,n4,n5,l1,l2,l3,l4,l5,s1,s2,s3,s4,s5,ss1,ss2,ss3,ss4,ss5])
    [_|nnr] = nn
    [_|nr] = n    
    [_|lr] = l
    [_|sr] = s
    [_|ssr] = ss        
    map(nnr, nr, lr, sr, ssr, [k|line], kernel)
  end

  def map([nn1,nn2,nn3,nn4], [n1,n2,n3,n4], [l1,l2,l3,l4], [s1,s2,s3,s4], [ss1,ss2,ss3,ss4], line, kernel) do
    ## last column is triplicated
    k3 = kernel.([nn1,nn2,nn3,nn4,nn4,n1,n2,n3,n4,n4,l1,l2,l3,l4,l4,s1,s2,s3,s4,s4,ss1,ss2,ss3,ss4,ss4])
    k4 = kernel.([nn2,nn3,nn4,nn4,nn4,n2,n3,n4,n4,n4,l2,l3,l4,l4,l4,s2,s3,s4,s4,s4,ss2,ss3,ss4,ss4,ss4])        
    Enum.reverse([k4,k3|line])
  end
  def map([nn1,nn2,nn3], [n1,n2,n3], [l1,l2,l3], [s1,s2,s3], [ss1,ss2,ss3], line, kernel) do
    ## last column is triplicated
    k = kernel.([nn1,nn2,nn3,nn3,nn3,n1,n2,n3,n3,n3,l1,l2,l3,l3,l3,s1,s2,s3,s3,s3,ss1,ss2,ss3,ss3,ss3])    
    Enum.reverse([k|line])
  end
  
  

end


      
      

