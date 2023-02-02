import Lexemes

def Date =
  struct
    day_name    : Date_day_name
    day         : uint 8
    month       : Date_month
    year        : uint 16
    year_short  : bool
    time        : Date_time_of_day

def Date_IMF_fixdate : Date =
  block
    day_name = Date_day_name
    $[',']
    $sp
    day   = Date_day
    $sp
    month = Date_month
    $sp
    year  = Date_year
    year_short = false
    $sp
    time  = Date_time_of_day
    $sp
    Match "GMT"

def Date_rfc850_date : Date =
  block
    day_name = Date_day_name_1
    $[',']
    $sp
    day = Date_day
    $['-']
    month = Date_month
    $['-']
    year = 10 * DigitNum + DigitNum as uint 16
    year_short = true
    $sp
    time = Date_time_of_day

def Date_asctime_date =
  block
    day_name = Date_day_name
    $sp
    month = Date_month
    $sp
    day   = Date_day <| { $sp; DigitNum }
    $sp
    time = Date_asctime_date
    $sp
    year = Date_year
    year_short = false


def Date_day_name =
  First
    Mon = @Match "Mon"
    Tue = @Match "Tue"
    Wed = @Match "Wed"
    Thu = @Match "Thu"
    Fri = @Match "Fri"
    Sat = @Match "Sat"
    Sun = @Match "Sun"

def Date_day_name_1 : Date_day_name =
  First
    Mon = @Match "Monday"
    Tue = @Match "Tuesday"
    Wed = @Match "Wednesday"
    Thu = @Match "Thursday"
    Fri = @Match "Friday"
    Sat = @Match "Saturday"
    Sun = @Match "Sunday"



def Date_day =
  block
    $$ = 10 * DigitNum + DigitNum
    $$ < 32 is true

def Date_month =
  First
    Jan = @Match "Jan"
    Feb = @Match "Feb"
    Mar = @Match "Mar"
    Apr = @Match "Apr"
    May = @Match "May"
    Jun = @Match "Jun"
    Jul = @Match "Jul"
    Aug = @Match "Aug"
    Sep = @Match "Sep"
    Oct = @Match "Oct"
    Nov = @Match "Nov"
    Dec = @Match "Dec"


def Date_year : uint 16 =
    1000 * (DigitNum as ?auto) +
     100 * (DigitNum as ?auto) +
      10 * (DigitNum as ?auto) +
           (DigitNum as ?auto)


def Date_time_of_day =
  block
    hour = Date_hour
    $[':']
    minute = Date_minute
    $[':']
    second = Date_second

def Date_hour =
  block
    $$ = 10 * DigitNum + DigitNum
    $$ < 24 is true

def Date_minute =
  block
    $$ = 10 * DigitNum + DigitNum
    $$ < 60 is true

def Date_second =
  block
    $$ = 10 * DigitNum + DigitNum
    $$ <= 60 is true    -- leap seconds
