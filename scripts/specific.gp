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
set title "data/2017-11-21-HUF1.dat egyenlegek " 
set y2tics 
set yrange   [600000 : *] 
set y2range   [4500000 :*] 
plot "data/2017-11-21-HUF1.dat" using 1:(stringcolumn(2) eq "SUM"? column(3):1/0) with boxes title "SUM" lc rgb "orange"  axes x1y2
replot "data/2017-11-21-HUF1.dat" using 1:(stringcolumn(2) eq "EQUILOR TBSZ"? column(3):1/0) title "EQUILOR TBSZ" lc rgb "blue"
replot "data/2017-11-21-HUF1.dat" using 1:(stringcolumn(2) eq "EQUILOR"? column(3):1/0) title "EQUILOR" lc rgb "red"
replot "data/2017-11-21-HUF1.dat" using 1:(stringcolumn(2) eq "EQUILOR ENEFI"? column(3):1/0) title "EQUILOR ENEFI" lc rgb "yellow"
set output '| /usr/local/bin/ps2pdf - /Users/evabihari/Erlang_apps/vienna_life_value/data/2017-11-21-HUF1.dat.pdf 
set size 1,1 
set term post portrait color "Times-Roman" 12 
replot 
