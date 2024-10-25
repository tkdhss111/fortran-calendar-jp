# fortran-calendar-jp （暦サーバー）

A Simple Japanese Calendar Server

- Downloads "National Holidays (国民の祝日)" calendar file (syukujitu.csv) 
 for the coming year from the Cabinet Office (内閣府) on March 1st every year.\
 Email notice is to be sent after the download.\
 The original Shift-JIS encoding is to be coverted to UTF-8 (sykujitu_utf8.csv).\
 Each holiday is renamed to make its name distingushable from others.\
 Gym Day (体育の日) is to be renamed to Sport Day (スポーツの日).\
 Note that Spring/Autumnal Equinox Day may change depending on the year.

- Fills in information regarding special days (i.e., Golden Week, Obon week and Year-end/New Year Days).\
 Each term length of special days may change depending on weekend position.\
 Each special day is renamed to make its name distingushable from others.

- Reads and fills in information regarding abnormal days such as earthquake days from an input file.\
 The input file "abnormaldays.csv" shall be located in the working directory. 

[CSV file format of abnomaldays]
 |        name|yyyy-mm-dd|
 |:---:       |:---:     |
 |東日本大震災|2011-03-11|
 |東日本大震災|2011-03-12|
 |:           |:         |

# Dependencies

- fortran-datetime :: dt_mo.f90
- fortran-logger :: logger_mo.f90

# Install

Go to the "install" subdirectory and 
edit fortran-calendar-jp.servie and fortran-calendar-jp.timer with your working directory.

```
make release
sudo make service 
make watchdog
```

# Uninstall

Go to the "install" subdirectory and 

```
sudo make stop 
sudo make uninstall 
```

# Limitation

Wareki (和暦) notations such as Showa (昭和), Heisei (平成) and Reiwa (令和) are ***not*** included.
