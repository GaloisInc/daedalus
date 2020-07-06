def Main = { @ds = Many (1..) ('0' .. '9');
             ^ for (v = 0; d in ds) (v * 10 + (d - '0'))
           }
