import Font

-- ResourceDict.ddl: definition of parser for resource
-- dictionaries. Currently only extracts the font dictionary.
def ResourceDict = {
  @d = Dict;
  @fontDict = TryLookup d "Font" empty;
  -- TODO: update
}
  
