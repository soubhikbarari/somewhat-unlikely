cd "D:\Dropbox\state representation\Replication"

use tracts_historical.dta, clear
log using tracts_historical_log.txt, replace text
twoway (lpoly whtshare7 NEAR_DIST if NEAR_DIST<40,  graphregion(color(gs13))lcolor(white) lpattern(shortdash) lwidth(medthick)) (lpoly whtshare1 NEAR_DIST if NEAR_DIST<40, lcolor(white) lwidth(medthick)) (lpoly blkshare7 NEAR_DIST if NEAR_DIST<40, lcolor(black) lpattern(shortdash)) (lpoly blkshare1 NEAR_DIST if NEAR_DIST<40, lcolor(black)) (lpoly hspshare7 NEAR_DIST if NEAR_DIST<40, lcolor(gs8) lpattern(shortdash)) (lpoly hspshare1 NEAR_DIST if NEAR_DIST<40, lcolor(gs8) legend(label(1 "White share 1970") label(2 "White share 2010") label(3 "Black share 1970") label(4 "Black share 2010") label(5 "Hisp. share 1970") label(6 "Hisp. share 2010") region(color(gs13))) xtitle(Kilometers from city center) ytitle(Population share) scheme(s1mono))
graph export tracts.pdf, as(pdf) replace


** CPI source http://www.bls.gov/cpi/cpid1411.pdf   Page 72.

gen AVHHIN7_cpi = AVHHIN7/.388
gen AVHHIN8_cpi = AVHHIN8/.726
gen AVHHIN9_cpi = AVHHIN9/1.24
gen AVHHIN0_cpi = AVHHIN0/1.666
gen AVHHIN1_cpi = AVHHIN1A/2.14537

gen bands = 0
replace bands = 1 if NEAR_DIST<5
replace bands = 2 if NEAR_DIST>5 & NEAR_DIST<10
replace bands = 3 if NEAR_DIST>10 & NEAR_DIST<15
replace bands = 4 if NEAR_DIST>15 & NEAR_DIST<20
replace bands = 5 if NEAR_DIST>20 & NEAR_DIST<25
replace bands = 6 if NEAR_DIST>25 & NEAR_DIST<30
replace bands = 7 if NEAR_DIST>30 & NEAR_DIST<35
replace bands = 8 if NEAR_DIST>35 & NEAR_DIST<40

egen sd7_band_city = sd(AVHHIN7_cpi) if AVHHIN7_cpi~=0, by(Primary_Cities bands)
egen sd8_band_city = sd(AVHHIN8_cpi) if AVHHIN8_cpi~=0, by(Primary_Cities bands)
egen sd9_band_city = sd(AVHHIN9_cpi) if AVHHIN9_cpi~=0, by(Primary_Cities bands)
egen sd0_band_city = sd(AVHHIN0_cpi) if AVHHIN0_cpi~=0, by(Primary_Cities bands)
egen sd1_band_city = sd(AVHHIN1_cpi) if AVHHIN1_cpi~=0, by(Primary_Cities bands)

gen str8 bands_label = ""
replace bands_label = "<5 km" if bands==1
replace bands_label = "5-10" if bands==2
replace bands_label = "10-15" if bands==3
replace bands_label = "15-20" if bands==4
replace bands_label = "20-25" if bands==5
replace bands_label = "25-30" if bands==6
replace bands_label = "30-35" if bands==7
replace bands_label = "35-40 km" if bands==8

graph box AVHHIN7_cpi AVHHIN1_cpi if AVHHIN7_cpi>0 & AVHHIN1_cpi>0, over(bands_label, sort(bands)) nooutsides  legend(label(1 "1970") label(2 "2010")) ytitle(1984 USD) title("Distribution of ave. tract-level houshold income" "by distance band from city center, 1970 & 2010") note(" ")


graph export income_box.pdf, as(pdf) replace

log close



