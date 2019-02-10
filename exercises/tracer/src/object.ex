defprotocol Object do

  def intersect(object, ray)

  def color(object)

  def transparency(object)

  def brilliance(object)    
  
  def normal(object, pos) 

  
end

  
