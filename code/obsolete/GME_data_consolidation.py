import pandas as pd
import os
import datetime
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm

start_timer = datetime.datetime.now()

os.chdir(r"C:\Users\jerep\Downloads\GME_2016\MGP")

# iterate over dates
start_date = datetime.date(2016,1,1)
oneday = datetime.timedelta(days=1)

# worker function
def process_date(i):
    # processed date
    current_date = start_date + i * oneday
    current_filename = current_date.strftime("%Y%m%d") + "MGPOffertePubbliche.xml"

    # read xml
    temp_df = pd.read_xml(open(current_filename))

    # remove unnecessary readings
    temp_df = temp_df.iloc[1:, 2:]

    return temp_df

dfs = []

# use a threadpool to run concurrent worker threads
def main():
    with ProcessPoolExecutor() as executor:
        
        # submit all the tasks to the executor
        future_to_date = {executor.submit(process_date, i): i for i in range(366)}

        for future in tqdm(as_completed(future_to_date)):
            result = future.result()
            if result is not None:
                dfs.append(result)


    df = pd.concat(dfs, axis=0)
    df.to_csv("2016_MGP.csv", index=False)

    print("finished!")
    print(datetime.datetime.now()-start_timer)

if __name__ == '__main__':
    main()