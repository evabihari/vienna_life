# gnuplot script
# input parameter: gnuplot -e 'type="SUM"; currency="HUF"' test.gp
set terminal postscript eps enhanced color solid lw 2 font 'Helvetica,12
if (exist("type")) print type
if (!exist("type")) type="SUM"
if (!exist("currency")) currency="HUF"
set terminal x11 
set key below
set key left invert
set grid y
set yrange [9000 :*]
set y2range [10800000 :*]
set decimalsign locale; set decimalsign ","
set decimal locale 'en_US.UTF-8'
set format y "%'.f"
set format y2 "%'.f" 
set ylabel "SUM" tc lt 1
set ytics nomirror
set xtics nomirror rotate by -45 scale 0 font ",10" 
set key noinvert box
set xdata time
set timefmt '%Y-%m-%d'
## set xrange ['2016-01-28':'2016-03-01']
set xrange ['2016-01-28':*]
set format x "%y-%m-%d"
set datafile separator "|"
set yrange [9000 :*]
