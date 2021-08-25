-- This tests a specific case of GetSream and input abstraction that
-- creates a prediction of parse failure, as opposed to predicting a
-- parse progress. It used to trigger a bug when the prediction was
-- wrongly always assuming to return prediction progress.

def A = block
  @x = GetStream
  @this = Take 2 x
  @next = Drop 2 x
  SetStream this
  @y = Match "aad"
  SetStream next

def B = block
  @x = GetStream
  @this = Take 2 x
  @next = Drop 2 x
  SetStream this
  @y = Match "aac"
  SetStream next

def Main = block
  (A | B)
  END
