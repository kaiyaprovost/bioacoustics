from datetime import timezone, datetime

format = "%Y%m%d_%H%M%S"

testtime = "20250607_115000"

my_datetime = datetime.strptime(testtime,format)
dt2 = my_datetime.replace(tzinfo=timezone.utc)

print(datetime.now().astimezone().strftime("%Y-%m-%d %H:%M:%S %z %Z"))
print(my_datetime.astimezone().strftime("%Y-%m-%d %H:%M:%S %z %Z"))
print(dt2.strftime("%Y-%m-%d %H:%M:%S %z %Z"))
## datetime is using the local time zone for this
## it is assuming all the times I give it are in my local time zone 
## but it does convert time zones with EST/EDT information


## my code should always use datetime libraries and objects
## only right before output should you comvert to a str

## make sure time stamps parsed correctly

## print out the datetime output when read in