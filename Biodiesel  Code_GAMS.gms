*Material selection for equipment items model ==========================================================================================================

set
    i       equipment i   /i1*i16/
    k       floor   /1,2,3/   
    s       candidate area /s1*s7/
    IP(i)   set of pertinent process unit /i1, i3, i7, i8, i12, i14, i15/   
    kk      protection device configuration /1*7/
    Ki      set of protection device configurations suitable for installation on item i /K1*K6/
    kt(k)   first floor /1/
    sec     section /sec1, sec2/
    te      material /te1*te7/
    typ     type of equipment /typ1*typ5/
    st      type of structure /st1, st2/ ;
    
Alias (i,j);
    
Alias (sec,secx);
    
parameter
    vol(i)          volume of equipment(m^3)
    vols(i)         volume of equipment for storage(m^3)
    p(te)           density of material(kg per m^3) /te1 7300, te2 7500, te3 8900, te4 4540, te5 7870, te6 7850, te7 8000/
    denf(i)         density of fluid inside equipment(kg per m^3)/i1 1100.357, i2 850.541, i3 850.289, i4 929.969, i5 916.665, i6 916.26, i7 890.626, i8 876.874, i9 876.874, i10 876.299, i11 1114.348, i12 1114.348, i13 1198.918, i14 1083.001, i15 845.656, i16 929.969/ 
    massf(i)        mass of fluid inside equipment (kg)
    group(i,sec)    set of section with equipment;
           
parameter
        alpha(i)   width of equipments i (metre)
          / i1   0.84
            i2   0.3
            i3   1.4
            i4   1.2
            i5   0.51
            i6   0.4
            i7   1.3
            i8   0.58
            i9   0.205    
            i10  0.4
            i11  0.5
            i12  0.84
            i13  0.84
            i14  0.51
            i15  0.51
            i16  0.4              /
            
        beta(i)  length of equipment i (m)
          / i1   0.84
            i2   0.5
            i3   1.4
            i4   1.2
            i5   1.2
            i6   1.1
            i7   1.3
            i8   1.44
            i9   0.35     
            i10  1.32     
            i11  1.2
            i12  0.84
            i13  0.84
            i14  1.28
            i15  1.28
            i16  1.64             /
            
        hita(i) height of equipment i (m)
          / i1   1
            i2   0.45
            i3   2.1
            i4   1.7
            i5   0.525
            i6   0.7
            i7   1.8
            i8   5.2
            i9   0.27       
            i10  0.64     
            i11  2.5
            i12  1
            i13  1
            i14  4.6
            i15  4.6
            i16  0.64             /;
            
        
table group(i,sec) equipment in each groups
            sec1    sec2    
        i1   1
        i2   1
        i3   1
        i4   1             
        i5   1        
        i6   1        
        i7   1                        
        i8   1               
        i9           1   
        i10          1
        i11          1
        i12          1
        i13          1
        i14          1
        i15          1
        i16          1
        ;
        

$ontext
=====================================================================================================
te1 = cast iron
te2 = stainless steel
te3 = nickel
te4 = titanium
te5 = stainless steel 316
te6 = carbon steel
te7 = stainless steel 304
=====================================================================================================
$offtext

$ontext
=====================================================================================================
typ1 = pump
typ2 = compressors
typ3 = heat ex
typ4 = heater
typ5 = vessel
=====================================================================================================
$offtext

table fm(te,typ) material factor of each equipment type
                typ1    typ2    typ3    typ4    typ5
        te1     1
        te2     2       2.5     3       1.7     1.7
        te3     3.5     5       1.2             5.4
        te4     9.7             2.6             7.7
        te5                     1.1             2.1
        te6                                     1
        te7                                     1.7;
        
display fm;

parameter Pd(i)         design pressure (psig)
          Po(i)         operating pressure (psig) /i1 1, i2 43.319, i3 1, i4 1, i5 43.319, i6 43.319, i7 43.319, i8 1, i9 1.258, i10 1.258, i11 1, i12 1.258, i13 1.258, i14 1, i15 1, i16 1/
          tp(i)         wall thickness to withstand the internal pressure (inches)
          str           maximum allowable stress (psi) /15000/
          weld          fractional weld efficiency /1/
          orr(i)        1 indicated that i is vertical vessel /i1 1, i3 1, i4 1, i7*i8 1, i12*i15 1/
          HoAndOrr(i)   1 indicated that i is horizontal and vertical vessel /i1 1, i3 1, i4 1, i7*i8 1, i12*i15 1/
          heater(i)     1 indicated that i is heater /i6 1/
          heatex(i)     1 indicated that i is heat exchanger /i10 1, i16 1/
          reactor(i)    1 indicated that i is Reactor /i7 1, i12 1/
          tow(i)        1 indicated that i is tower vessel /i8 1, i14*i15 1/
          dis(i)        1 indicated that i is distillation column /i8 1, i14*i15 1/
          pump(i)       1 indicated that i is pump /i2 1, i5 1, i9 1/
          extrac(i)     1 indicated that i is Liquid Liquid extraction /i11 1/
          
          cpl(i)        cost for platform
          
          cbf(i)        base cost of fired heater
          ener(i)       heat duty of fired heater(btu per hour) /i6 74872/
          fpf(i)        pressure factor for fired heater     

          cbh(i)        base cost of double pipe heat ex
          ah(i)         area of heat ex (sq ft) and screener /i10 0.04037, i16 0.8234/
          fp(i)         factor for heat ex
          
          cpa(i)        cost for agitator
          hp(i)         hp for agitator (hp) /i7 60, i12 60/
          
          cbt(i)        base cost for trays
          fnt(i)        number of tray factor
          nt(i)         number of tray  /i8 7, i14*i15 6/
          ftt(i)        type of tray    /i8 1, i14*i15 1/
          ftm(i)        material factor for distillation
          ct(i)         cost for installed tray
          
          sp(i)         s factor for pump
          flow(i)       flowrate(gal per minute) of pump /i2 0.917, i5 5.245, i9 6.375/
          head(i)       pump head (ft) of pump /i2 117.6, i5 138.7, i9 34.3/
          cbp(i)        base pump cost
          ft(i)         factor for centrifugal pump /i2 1.70, i5 1.70, i9 1.00/
          pcc(i)        horse power for pump
          pb(i)         pump brake horsepower of pump (hp) /i2 0.0784, i5 0.5701, i9 0.1641/
          cbm(i)        base cost of motor for pump
          cpm(i)        motor cost for pump
          fym           factor for motor(open drip-proof enclosure) /1/
          

          cet(i)        cost of Liquid Liquid extraction ($ per 10^5)
          shd(i)        Height multiplied by Diameter to the power of one point five /i11 17.226/
          capweight(st) weight capacity for structure(kg per m^2) /st1 1000, st2 1500/
          costst(st)    structure cost for weight($ per m^2) /st1 222, st2 300/;
          
          
vol(i)$(Po(i)>1000) = alpha(i)*beta(i)*hita(i)*0.4;

vol(i)$((Po(i)>500) and (Po(i)<1000)) = alpha(i)*beta(i)*hita(i)*0.3;

vol(i)$(Po(i)<500) = alpha(i)*beta(i)*hita(i)*0.2;

vols(i) = (alpha(i)*beta(i)*hita(i))-vol(i);

massf(i) = vols(i)*denf(i);

display vol, vols, massf;

Pd(i)$((Po(i)>0) and (Po(i)<5) and (HoAndOrr(i))) = 10;

Pd(i)$((Po(i)>10) and (Po(i)<1000) and (HoAndOrr(i)))   = exp(0.60608+(0.91615*log(Po(i)))+(0.0015655*(log(Po(i)))**2));

Pd(i)$((Po(i)>1000) and (HoAndOrr(i))) = 1.1*Po(i);

tp(i)$HoAndOrr(i)   = ((Pd(i)*alpha(i)*39.37)/((2*str*weld)-(1.2*Pd(i))))+0.2;

display Pd, tp;


*vertical vessel platform

cpl(i)$orr(i)       = 361.8*((alpha(i)*3.281)**0.7396)*((hita(i)*3.281)**0.70684);

display Pd, tp, cpl;


*cost for fired heater

cbf(i)$heater(i)    = exp(0.32325+0.766*log(ener(i)));

fpf(i)$heater(i)    = 0.986-0.0035*(Po(i)/500)+0.0175*(Po(i)/500)**2;

display cbf, fpf;

*cost for heat ex

cbh(i)$heatex(i)    = exp(7.146+0.16*log(ah(i)));

fp(i)$heatex(i)     = 0.851+0.1292*(Po(i)/600)+0.0198*(Po(i)/600)**2;

display cbh, fp;


*cost for agitator

cpa(i)$reactor(i)   = (3620*hp(i)**0.57)/10**5;

display cpa;


*cost for centrifugal pump

sp(i)$pump(i)       = flow(i)*head(i)**0.5;

cbp(i)$pump(i)      = exp(9.7171-(0.6019*log(sp(i)))+(0.0519*(log(sp(i)))**2));

pcc(i)$pump(i)      = (pb(i)+1)/(0.8+0.0319*log(pb(i)+1)-(0.00182*log(pb(i)+1)**2));

cbm(i)$pump(i)      = exp(5.8259+0.13141*log(pcc(i))+(0.053255*(log(pcc(i)))**2)+(0.028628*(log(pcc(i)))**3)-(0.0035549*(log(pcc(i)))**4));

cpm(i)$pump(i)      = fym*cbm(i)*821.1/500;

display cbp, pcc, cbm, cpm;


*tower vessel platform

cpl(i)$(tow(i) and dis(i))  = 300.9*((alpha(i)*3.281)**0.63316)*((hita(i)*3.281)**0.80161);

* trays for distillation

cbt(i)$dis(i)       = 468*exp(0.1739*alpha(i)*3.281);

fnt(i)$dis(i)       = 2.25/(1.0414**nt(i));

*316 stainless steel is used since it has widele range of fabrication properties (table 7.1 in ?????????????)

ftm(i)$dis(i)       = 1.401+(0.0724*alpha(i)*3.281);

ct(i)$dis(i)        = nt(i)*fnt(i)*ftt(i)*ftm(i)*cbt(i)*(821.1/500)/10**5;

display Pd, tp, cpl, cbt, ct;


*cost for Liquid Liquid extraction

cet(i)$extrac(i)    = ((320*shd(i)**0.84)*821.1/500)/10**5 ;

display cet;

*========================================================================================================

positive variable
    mass(i)     mass of equipment i (kg)
    Tm(sec)     total mass of that section sec (kg)
    Tn(te)      total materials used
    cpv(i)      equipment cost of vessel ($ per 10^5)
    cpf(i)      cost for fired heater ($ per 10^5)
    cph(i)      cost for heat ex ($ per 10^5)
    cpump(i)    total cost pump ($ per 10^5)
    cpt(i)      total cost of distillation column ($ per 10^5)
    cpr(i)      cost for reactor ($ per 10^5)
    w(i)        weight of equipment (lb)
    cvv(i)      cost for pressure vessel and tower for distillation absorption and stripping
    cpp(i)      pump cost exclude motor
    CP(i)       equipment cost ($ per 10^5)
    
Integer variable
    guess(sec)  first guess area for 22 sections(m^2);
    
variable
    wei         objective weight variable;
    
binary variable
    zz(i,te)      1 if material te is selected for equipment i
    bi(sec)       test
    wd(st,sec)    weight decision for structure;
    
equation
    masc        'mass costaints'
    mat         'material constaints'
    mat1        'material constaints'
    num         'number of materials used'
    totalmass   'section mass'
    test        'test av weight'
    test1       'test av weight'
    weighte     'weight of equipment'
    costhv      'base cost of vertical vessel'
    cvessel     'cost of vessel'
    cvessel2    'cost of vessel'
    cheat       'cost of heater'
    cheat2      'cost of heater'
    cheatx      'cost of heat ex'
    cheatx2     'cost of heat ex'
    cmotor      'cost of motor in pump'
    cmotor2     'cost of motor in pump'
    tcpump      'total cost of pump'
    cdis        'cost of distillation column'
    cequip      'cost of each equipments'
    creac       'cost of reactor'
    obj         'objective function';
    
masc(i)                .. mass(i) =e= sum(te, p(te)*vol(i)*zz(i,te));

mat(i)                 .. sum(te, zz(i,te)) =e= 1;

mat1(i)$extrac(i)         .. sum(te$(ord(te)=6), zz(i,te)) =e= 1;

num(te)                .. Tn(te) =e= sum(i, zz(i,te));

totalmass(sec)         .. Tm(sec) =e= sum(i$group(i,sec), mass(i));

test(sec)              .. Tm(sec) =l= sum(st, wd(st,sec)*capweight(st)*guess(sec))-sum(i$group(i,sec), massf(i));

test1(sec)             .. sum(st, wd(st,sec)) =e= 1;

* for vessel ===================================================================================================================================

weighte(i)$HoAndOrr(i)  .. w(i) =e= sum(te, 3.14*(alpha(i)+tp(i))*(hita(i)+0.8*alpha(i))*2.2046*tp(i)*p(te)*zz(i,te));

costhv(i)$orr(i)    .. cvv(i) =e= exp(7.0132+0.18255*log(w(i)+1)+(0.02297*(log(w(i)+1)**2)));

cvessel(i,typ)$((ord(typ)=5) and HoAndOrr(i))  .. cpv(i) =e= (sum(te$fm(te,typ), zz(i,te)*(fm(te,typ)*cvv(i)+cpl(i))*(821.1/500)))/10**5;

cvessel2(i,typ)$((ord(typ)=5) and HoAndOrr(i)) .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;  

* for heater ===================================================================================================================================

cheat(i,typ)$((ord(typ)=4) and heater(i))    .. cpf(i) =e= (sum(te$fm(te,typ), fpf(i)*fm(te,typ)*cbf(i)*zz(i,te)*821.1/500))/10**5;

cheat2(i,typ)$((ord(typ)=4) and heater(i))   .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;

* for heat ex ===================================================================================================================================

cheatx(i,typ)$((ord(typ)=3) and heatex(i))   .. cph(i) =e= (sum(te$fm(te,typ), fp(i)*fm(te,typ)*cbh(i)*zz(i,te)*(821.1/500)))/10**5;

cheatx2(i,typ)$((ord(typ)=3) and heatex(i))  .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;

* for reactor ===================================================================================================================================

creac(i)$reactor(i) .. cpr(i) =e= (cpv(i)+cpa(i))*821.1/500;

* for pump ===================================================================================================================================

cmotor(i,typ)$((ord(typ)=1) and pump(i))     .. cpp(i) =e= sum(te$fm(te,typ), ft(i)*fm(te,typ)*cbp(i)*zz(i,te)*(821.1/500));

cmotor2(i,typ)$((ord(typ)=1) and pump(i))    .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;

tcpump(i)$pump(i)       .. cpump(i) =e= (cpp(i)+cpm(i))/10**5;

* for distillation column ===================================================================================================================================

cdis(i)$dis(i)          .. cpt(i) =e= cpv(i)+ct(i);

* for each equipments ===================================================================================================================================

cequip(i)           .. CP(i) =e= cpv(i)+cpf(i)+cph(i)+cpump(i)+cpt(i)+cpr(i)+cet(i);

*====================================================================================================================================================

obj                 .. wei =e= sum(i, CP(i))+(sum((st,sec), wd(st,sec)*costst(st)*guess(sec))/10**5);

wei.lo=0;

option limrow = 500;

model weight /masc,
              mat,
              mat1,
              num,
              totalmass,
              test,
              test1,
              weighte,
              costhv,
              cvessel,
              cvessel2,
              cheat,
              cheat2,
              cheatx,
              cheatx2
              cmotor,
              cmotor2,
              tcpump,
              cdis,
              creac,     
              cequip,
              obj/      ;

solve weight using minlp minimizing wei;

display zz.l, Tn.l, mass.l, Tm.l, wei.l, cvv.l, cpv.l, cpf.l, cpump.l, cpt.l, cph.l ;

parameter Mmax(sec) maximun mass of that section;

Mmax(sec)=smax(i$group(i,sec),mass.l(i)+massf(i));

display Mmax;

*Multi-floor process plant layout model =================================================================================================================================================================================================================================================================================================================================================================================================================================================================

Parameter   pre(IP)       'IP value' /i1 1, i3 3, i7 7, i8 8, i12 12, i14 14, i15 15/
            wax(i,sec)    'test for heavy weight'
            haz(sec)      'section that has hazadous equipment' /sec1*sec2 1/;
            

scalar Kn 'total of k';
          
Kn = card(k);

          
wax(i,sec)$((mass.l(i)+massf(i)=Mmax(sec))and(group(i,sec)))=1;

display wax;


table f(i,j) flow relationship between item i and j
            i1  i2  i3  i4  i5  i6  i7  i8  i9  i10  i11  i12  i13  i14  i15  i16
    i1          1         
    i2              1                             
    i3                              1 
    i4                      1
    i5                          1           
    i6                              1   
    i7                                  1                  
    i8              1                                                                                      
    i9                                          1                                                                                                          
    i10                                              1                                                                            
    i11                                                   1              1 
    i12                                                        1
    i13                                                             1
    i14
    i15                                                                       1
    i16
    ;

table Cc(i,j) connection costs ($ per ft)
            i1      i2      i3      i4      i5      i6      i7      i8      i9      i10      i11      i12      i13      i14      i15      i16
    i1              8.28
    i2                      8.28                                                         
    i3                                                      25.42 
    i4                                      18.14
    i5                                              18.14                       
    i6                                                      18.38                                           
    i7                                                              20.77                               
    i8                      7.26                                            19.8  
    i9                                                                              19.81                                                                
    i10                                                                                      19.81         
    i11                                                                                               8.09                       18.86                                                                                 
    i12                                                                                                         8.83
    i13                                                                                                                 8.64
    i14
    i15                                                                                                                                   4.6
    i16                             4.14
    ;


table Cv(i,j)   vertical cost ($ per m yr)
            i1      i2      i3      i4      i5      i6      i7      i8      i9      i10      i11      i12      i13      i14      i15      i16
    i1              74.54
    i2                      74.54                                                         
    i3                                                      126.2 
    i4                                      459.37
    i5                                              459.37                       
    i6                                                      459.37                                           
    i7                                                              585.57                               
    i8                      51.66                                           533.91  
    i9                                                                              533.91                                                                
    i10                                                                                      533.91         
    i11                                                                                               92.77                      462.18                                                                                 
    i12                                                                                                        109.96
    i13                                                                                                                 104.2
    i14
    i15                                                                                                                                   17.50
    i16                             17.50
    ;


parameter
    Ch(i,j)   horizontal cost ($ per m yr);
    
Ch(i,j) = Cv(i,j)/10;

display Ch;

parameter
    Xbar(s)         candidate x coordinate /s1 15, s2 25, s3 25, s4 20, s5 30, s6 40, s7 40/
    ybar(s)         candidate y coordinare /s1 10, s2 15, s3 10, s4 30, s5 30, s6 35, s7 30/
    AR(s)           candidate area;
    
AR(s) = xbar(s)*ybar(s);

Scalar
    FC1             building factory cost ($ per m2) /218/
    LC              land cost ($ per m2)  /676/;          

parameter M(sec)      upper bound
          H(sec)      floor height
          FC2(sec)    area dependent floor construction cost ;
          
H(sec) = smax(i$group(i,sec), hita(i))+1;

FC2(sec) = sum(st, wd.l(st,sec)*costst(st));

display Kn, AR, H, FC2;

BINARY VARIABLES     
    V(i,sec,k)      'item i is assigned to floor k'
    Z(i,j,sec)      'i and j are allocated to the same floor'
    O(i)            'rotation'
    Ea(i,j)         'non1'
    Eb(i,j)         'non2'
    QS(s,sec)       'binary variable for linearity'
    Wx(i,j)         '1 if i is right of j'
    Wy(i,j)         '1 if i is above of j'
    OO(sec)         'orientation of s'
    Eaa(sec,secx)   'non s 1'
    Ebb(sec,secx)   'non s 2';
    
integer variables
    NF(sec)         'number of floor';
    
positive variables
    l(i)            'length of item i'
    d(i)            'depth of dimension i'
    vmainx(i)       'maintenance spacing on x coordinate'
    vmainy(i)       'maintenance spacing on y coordinate'
    x(i,sec)        'coordinates of geometrical x centre of item i'
    y(i,sec)        'coordinates of geometrical y centre of item i'
    R(i,j,sec)      'relative distance in x coordinates between items i and j, if i is to right of j'
    Le(i,j,sec)     'relative distance in x coordinates between items i and j, if i is to left of j'
    A(i,j,sec)      'relative distance in y coordinates between items i and j, if i above j'
    B(i,j,sec)      'relative distance in y coordinates between items i and j, if i below j'
    U(i,j,sec)      'relative distance in z coordinates between items i and j, if i is higher than j'
    De(i,j,sec)     'relative distance in z coordinates between items i and j, if i is lower than j'
    Xmax(sec)       'dimension of floor area on x axis'
    Ymax(sec)       'dimension of floor area on y axis'
    NQ(s,sec)       'new variable'
    ll(sec)         'length of s'
    dd(sec)         'delth of s'
    xxx(sec)        'x coordinate of s'
    yyy(sec)        'y coordinate of s'
    RR(sec,secx)    'Relative distance r'
    Lee(sec,secx)   'relative distance l'
    AA(sec,secx)    'relative distance a'
    BB(sec,secx)    'relative distance b';
    
    
variables    
    TD(i,j,sec)     'total rectilinear distance between items i and j'
    TDD(sec,secx)   'total rectilinear distance between s'
    FA(sec)         'floor area'
    xxxmax          'maximux x for s'
    yyymax          'maximum y for s'
    FAs             'area of s'
    Qs1             'obj for section1'
    Qs2             'obj for section2'
    QQ              'obj for s';
    
*safety part

Parameter
    F1(IP)          'general process hazards factor of item i' /i1 1.65, i3 1.65, i7 2.15, i8 1.65, i12 1.15, i14 1.45, i15 1.15/
    F2(IP)          'special process hazards factor of item i' /i1 1.46, i3 1.3, i7 1.3, i8 1.3, i12 1.1, i14 0.9, i15 0.9/
    F3(IP)          'process unit hazards factor of item i'
    Fi(IP)          'fire and explosion index of item i'
    MF(IP)          'material factor of item i' /i1 16, i3 16, i7 16, i8 16, i12 4, i14 4, i15 4/
    Dei(IP)         'the distance of exposure i'  
    DF(IP)          'damage factor' /i1 0.126, i3 0.111, i7 0.179, i8 0.111, i12 0.049, i14 0.05, i15 0.045/
    UU              'appropriate upper bound for safety'
    Mm              'appropriate upper bound for aoe eq'
    mainx(i)        'maintanance distance x axis'
    mainy(i)        'maintanance distance y axis'
    ss              'upper bound of s';

F3(IP)  = F1(IP)*F2(IP);
Fi(IP)  = F3(IP)*MF(IP);
Dei(IP) = 0.256*Fi(IP);

UU(IP)  = sum( i, CP.l(i))*DF(IP);
Mm      = sum( IP, Dei(IP));
mainx(i)$HoAndOrr(i)  = hita(i);
mainy(i)$HoAndOrr(i)  = hita(i);
mainx(i)$pump(i)      = 0.8;
mainy(i)$pump(i)      = 0.8;
mainx(i)$heatex(i)    = 0.5;
mainy(i)$heatex(i)    = beta(i)+1.5;
mainx(i)$heater(i)    = 3;
mainy(i)$heater(i)    = beta(i)+15;
mainx(i)$extrac(i)    = 1;
mainy(i)$extrac(i)    = 1;
mainx(i)$reactor(i)   = 4;
mainy(i)$reactor(i)   = 4;

table WS(i,j)   spacing for worker (m)
             i1      i2      i3      i4      i5      i6      i7      i8      i9      i10      i11      i12      i13      i14      i15      i16             
    i1               3.05    4.6     4.6     3.05    15.2    7.6     4.6     3.05    3.05     4.6      4.6      4.6      4.6      4.6      3.05
    i2                       3.05    3.05    1.5     15.2    3.05    3.05    1.5     3.05     3.05     3.05     3.05     3.05     3.05     3.05
    i3                               4.6     3.05    15.2    7.6     4.6     3.05    3.05     4.6      4.6      4.6      4.6      4.6      3.05
    i4                                       3.05    15.2    7.6     4.6     3.05    3.05     4.6      4.6      4.6      4.6      4.6      3.05                                          
    i5                                               15.2    3.05    3.05    1.5     3.05     3.05     3.05     3.05     3.05     3.05     3.05                                       
    i6                                                       15.2    15.2    15.2    15.2     15.2     15.2     15.2     15.2     15.2     15.2
    i7                                                               7.6     3.05    4.6      7.6      4.6      7.6      7.6      7.6      4.6                                                
    i8                                                                       3.05    3.05     4.6      4.6      4.6      4.6      4.6      3.05
    i9                                                                               3.05     3.05     3.05     3.05     3.05     3.05     3.05
    i10                                                                                       3.05     3.05     3.05     3.05     3.05     1.5                                                                               
    i11                                                                                                4.6      4.6      4.6      4.6      3.05        
    i12                                                                                                         7.6      7.6      7.6      3.05
    i13                                                                                                                  4.6      4.6      3.05                                                                                    
    i14                                                                                                                           4.6      3.05
    i15                                                                                                                                    3.05
    i16                                                                                                                                          ;
    
M(sec)  = max(sum( i, (alpha(i)+mainx(i))*ord(sec)), sum( i, (beta(i)+mainy(i))*ord(sec)),sum((i,j), ((alpha(i)/2)+WS(i,j))*ord(sec)),sum((i,j), ((beta(i)/2)+WS(i,j))*ord(sec)));

display UU, Dei, mainx, mainy, M;

table grammar(IP,Ki) loss control credit factor
                K1      K2      K3      K4      K5      K6
        i1      1       0.9     0.76    0.68    0.517   0.465
        i3      1       0.9     0.76    0.68    0.517   0.465 
        i7      0.9     0.75    0.405   0.365   0.194   0.117 
        i8      1       0.9     0.76    0.68    0.517   0.465
        i12     0.9     0.75    0.405   0.365   0.194   0.117
        i14     1       0.9     0.76    0.68    0.517   0.465
        i15     1       0.9     0.76    0.68    0.517   0.465  ;
        
table Pik(IP,Ki) protection device cost
                K1      K2      K3      K4      K5      K6
        i1              5000    20000   30000   50000   55000 
        i3              5000    20000   30000   50000   55000  
        i7      5000    15000   35000   40000   90000   125000 
        i8              5000    20000   30000   50000   55000 
        i12     5000    15000   35000   40000   90000   125000
        i14             5000    20000   30000   50000   55000 
        i15             5000    20000   30000   50000   55000  ;

Variables
        ohmz(IP)        'base maximum probable property damage cost'
        ohm(IP)         'actual probable property damage cost';
        
Positive variable
        QZ(i,Ki)        'new variable for safety'
        Ve(i)           'value of the area of explosure of i'
        Din(i,j,sec)    'total rectilinear distance between items i and j if Din is smaller than Dei'
        Dout(i,j,sec)   'total rectilinear distance between items i and j if Din is larger than Dei';
        
Binary variable
        Yy(IP,j)        '1 if j is allocated within the area of exposure of i'
        Zik(IP,Ki)      '1 if protection device is equipped'
        Wx(i,j)         '1 if i is right of j'
        Wy(i,j)         '1 if i is above of j'
        Wz(i,j)         '1 if i upper than j'
        Wxx(sec,secx)   '1 if sec is right of secx'
        Wyy(sec,secx)   '1 if sec is above of secx' ;
        
*section 1 ==================================================================================================================================

equations
         Floors1          Should be assigned to one floor 
         Floorrs1         weightest equipment should be assigned to the first floor
         Floor1s1         floor constraints value of z 
         Floor2s1         floor constraints value of z 
         Floor3s1         floor constraints value of z 
         Numfloors1       number of floor 
         Orient1s1        equipment orientation constraints length 
         Orient2s1        equipment orientation constraints depth 
         maintenance1s1   maintenance spacing required
         maintenance2s1   maintenance spacing required
         Non1s1           non overlapping constraints 
         Non2s1           non overlapping constraints 
         Non3s1           non overlapping constraints 
         Non4s1           non overlapping constraints
         
         riskzone1s1      worker spacing constrains
         riskzone2s1      worker spacing constrains
         riskzone3s1      worker spacing constrains
         riskzone4s1      worker spacing constrains

         Distance1s1      distance constraint 
         Distance2s1      distance constraint 
         Distance3s1      distance constraint 
         Distance4s1      distance constraint 
         distance11s1     absolute distance constraints
         distance12s1     absolute distance constraints 
         distance13s1     absolute distance constraints
         distance14s1     absolute distance constraints 
         distance15ps1    absolute distance constraints
         distance16ps1    absolute distance constraints
         Coordinate1s1    coordinate constaints 
         Coordinate2s1    coordinate constaints 
         Coordinate3s1    coordinate constaints 
         Coordinate4s1    coordinate constaints 
         Areals1          plant area linearity 
         Areals11         lowerbound of section area
         Qcons1           QS constraint 
         Xcons1           x constaint 
         Ycons1           y constaint 
         news1            linearization constraints 
         new1s1           linearization constraints 
         Objls1           objective function linear
         aoes1            area of exposure constaint
         aoe1s1           area of exposure constaint
         aoe2s1           area of exposure constaint
         aoe3s1           area of exposure constaint
         values1          the value of exposure
         maxxs1           maximum probable property damage constaint
         max1s1           Actual probable property damage constaint
         max2s1           one configuration can be installed per pertinent process unit;
         
Floors1(i,sec)$((group(i,sec))and(mass.l(i)+massf(i)<Mmax(sec))and(ord(sec)=1)) ..    sum(k, V(i,sec,k)) =e= 1;

Floorrs1(kt,sec)$((ord(sec)=1)) .. sum(i$(group(i,sec)and(mass.l(i)+massf(i)=Mmax(sec))), V(i,sec,kt)) =g= sum(i$(group(i,sec)),wax(i,sec));

Floor1s1(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=1)) ..   Z(i,j,sec) =g= V(i,sec,k)+V(j,sec,k)-1;

Floor2s1(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=1)) ..    Z(i,j,sec) =l= 1-V(i,sec,k)+V(j,sec,k);

Floor3s1(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=1)) ..    Z(i,j,sec) =l= 1+V(i,sec,k)-V(j,sec,k);

Numfloors1(i,sec)$(group(i,sec)and(ord(sec)=1)) .. NF(sec) =g= sum( k, ord(k)*V(i,sec,k) );

Orient1s1(i,sec)$(group(i,sec) and (ord(sec)=1))      .. l(i) =e= (alpha(i)*O(i))+(beta(i)*(1-O(i)));

Orient2s1(i,sec)$(group(i,sec) and (ord(sec)=1))      .. d(i) =e= alpha(i)+beta(i)-l(i);

maintenance1s1(i,sec)$(group(i,sec) and (ord(sec)=1)) .. vmainx(i) =e= (mainx(i)*O(i))+(mainy(i)*(1-O(i)));

maintenance2s1(i,sec)$(group(i,sec) and (ord(sec)=1)) .. vmainy(i) =e= mainx(i)+mainy(i)-vmainx(i);

Non1s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1))       .. x(i,sec)-x(j,sec)+(M(sec)*(1-Z(i,j,sec)+Ea(i,j)+Eb(i,j))) =g= (( l(i)+l(j) )/2)+(vmainx(i)+vmainx(j));

Non2s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1))       .. x(j,sec)-x(i,sec)+(M(sec)*(2-Z(i,j,sec)-Ea(i,j)+Eb(i,j))) =g= (( l(i)+l(j) )/2)+(vmainx(i)+vmainx(j));

Non3s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1))       .. y(i,sec)-y(j,sec)+(M(sec)*(2-Z(i,j,sec)+Ea(i,j)-Eb(i,j))) =g= (( d(i)+d(j) )/2)+(vmainy(i)+vmainy(j));

Non4s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1))       .. y(j,sec)-y(i,sec)+(M(sec)*(3-Z(i,j,sec)-Ea(i,j)-Eb(i,j))) =g= (( d(i)+d(j) )/2)+(vmainy(i)+vmainy(j));


riskzone1s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1))       .. x(i,sec)-x(j,sec)+(M(sec)*(1-Z(i,j,sec)+Ea(i,j)+Eb(i,j))) =g= WS(i,j)+(l(j)/2);

riskzone2s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1))       .. x(j,sec)-x(i,sec)+(M(sec)*(2-Z(i,j,sec)-Ea(i,j)+Eb(i,j))) =g= WS(i,j)+(l(j)/2);

riskzone3s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1))       .. y(i,sec)-y(j,sec)+(M(sec)*(2-Z(i,j,sec)+Ea(i,j)-Eb(i,j))) =g= WS(i,j)+(d(j)/2);

riskzone4s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1))       .. y(j,sec)-y(i,sec)+(M(sec)*(3-Z(i,j,sec)-Ea(i,j)-Eb(i,j))) =g= WS(i,j)+(d(j)/2);


Distance1s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1) and f(i,j))  .. R(i,j,sec)-Le(i,j,sec) =e= x(i,sec)-x(j,sec);

Distance2s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1) and f(i,j))  .. A(i,j,sec)-B(i,j,sec) =e= y(i,sec)-y(j,sec);

Distance3s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1) and f(i,j))  .. U(i,j,sec)-De(i,j,sec) =e= H(sec)*sum( k, ord(k)*(V(i,sec,k)-V(j,sec,k)));

Distance4s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1) and f(i,j))  .. TD(i,j,sec) =e= R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec)+U(i,j,sec)+De(i,j,sec);

distance11s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1)) .. R(i,j,sec) =l= M(sec)*Wx(i,j);

distance12s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1)) .. Le(i,j,sec) =l= M(sec)*(1-Wx(i,j));

distance13s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1)) .. A(i,j,sec) =l= M(sec)*Wy(i,j);

distance14s1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1)) .. B(i,j,sec) =l= M(sec)*(1-Wy(i,j));

distance15ps1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1)).. U(i,j,sec) =l= M(sec)*Wz(i,j);

distance16ps1(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1)).. De(i,j,sec) =l= M(sec)*(1-Wz(i,j));

Coordinate1s1(i,sec)$(group(i,sec)and(ord(sec)=1))   .. x(i,sec) =g= (l(i)/2)+1;

Coordinate2s1(i,sec)$(group(i,sec)and(ord(sec)=1))   .. y(i,sec) =g= (d(i)/2)+vmainy(i);

Coordinate3s1(i,sec)$(group(i,sec)and(ord(sec)=1))   .. x(i,sec)+(l(i)/2)+vmainx(i) =l= Xmax(sec);

Coordinate4s1(i,sec)$(group(i,sec)and(ord(sec)=1))   .. y(i,sec)+(d(i)/2)+vmainy(i) =l= Ymax(sec);


aoes1(IP,j,sec) $((pre(IP)<>ord(j)) and (group(IP,sec)) and (group(j,sec)) and (ord(sec)=1))   .. TD(IP,j,sec) =e= Din(IP,j,sec) + Dout(IP,j,sec);

aoe1s1(IP,j,sec) $((pre(IP)<>ord(j)) and (group(IP,sec)) and (group(j,sec)) and (ord(sec)=1))  .. Din(IP,j,sec) =l= Dei(IP)*Yy(IP,j);

aoe2s1(IP,j,sec) $((pre(IP)<>ord(j)) and (group(IP,sec)) and (group(j,sec)) and (ord(sec)=1))  .. Dout(IP,j,sec) =g= Dei(IP)*(1-Yy(IP,j));

aoe3s1(IP,j,sec) $((pre(IP)<>ord(j)) and (group(IP,sec)) and (group(j,sec)) and (ord(sec)=1))  .. Dout(IP,j,sec) =l= Mm*(1-Yy(IP,j));

values1(IP,sec)$(group(IP,sec) and (ord(sec)=1))       .. Ve(IP) =e= CP.l(IP)+sum( j$((pre(IP)<>ord(j)) and group(j,sec)), (CP.l(j)*Yy(IP,j))-((CP.l(j)/Dei(IP))*Din(IP,j,sec)));

maxxs1(IP,sec)$(group(IP,sec) and (ord(sec)=1))    .. ohmz(IP) =e= DF(IP)*Ve(IP);

max1s1(IP,sec)$(group(IP,sec) and (ord(sec)=1))    .. ohm(IP) =e= sum( Ki, grammar(IP,Ki)*ohmz(IP)*Zik(IP,Ki));

max2s1(IP,sec)$(group(IP,sec) and (ord(sec)=1))    .. sum( Ki, Zik(IP,Ki)) =e= 1    ;


Areals1(sec)$(ord(sec)=1)  .. FA(sec) =e= sum( s, AR(s)*QS(s,sec));

Areals11(sec)$(ord(sec)=1) .. FA(sec)*NF(sec) =g= guess.l(sec);

Qcons1(sec)$(ord(sec)=1)   .. sum( s, QS(s,sec)) =e= 1;

Xcons1(sec)$(ord(sec)=1)   .. Xmax(sec) =e= sum( s, Xbar(s)*QS(s,sec));

Ycons1(sec)$(ord(sec)=1)   .. Ymax(sec) =e= sum( s, Ybar(s)*QS(s,sec));

news1(s,sec)$(ord(sec)=1)  .. NQ(s,sec) =l= Kn*QS(s,sec);

new1s1(sec)$(ord(sec)=1)   .. NF(sec) =e= sum( s, NQ(s,sec));

Objls1        .. Qs1 =e= sum( (i,j,sec)$((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1) and f(i,j)), (( Cc(i,j)*TD(i,j,sec)/0.3048)+( Cv(i,j)*De(i,j,sec) )+( Ch(i,j)*(R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec))))/10**5 )+(sum((s,sec)$(ord(sec)=1), AR(s)*NQ(s,sec)*FC2(sec))/10**5)+(LC*sum(sec$(ord(sec)=1), FA(sec))/10**5)+ sum( (IP,sec)$(group(IP,sec)and(ord(sec)=1)), ohm(IP)) + sum( (IP,Ki,sec)$(group(IP,sec)and(ord(sec)=1)), Pik(IP,Ki)*Zik(IP,Ki)/10**5) ;

Qs1.lo=0;

Model linearitys1      
        /Floors1,
         Floorrs1,
         Floor1s1,
         Floor2s1,
         Floor3s1,
         Numfloors1,
         Orient1s1,
         Orient2s1,
         maintenance1s1,
         maintenance2s1,
         Non1s1,
         Non2s1,
         Non3s1,
         Non4s1,
        
         riskzone1s1,
         riskzone2s1,
         riskzone3s1,
         riskzone4s1,

         Distance1s1,
         Distance2s1,
         Distance3s1,
         Distance4s1,
         distance11s1,
         distance12s1,
         distance13s1,
         distance14s1,
         distance15ps1,
         distance16ps1,
         Coordinate1s1,
         Coordinate2s1,
         Coordinate3s1,
         Coordinate4s1,
         Areals1,
         Areals11,
         Qcons1,
         Xcons1,
         Ycons1,
         news1,
         new1s1,
         aoes1,
         aoe1s1,
         aoe2s1,
         aoe3s1,
         values1,
         maxxs1,
         max1s1,
         max2s1,
         Objls1/;

option limrow = 1000;

solve linearitys1 using minlp minimizing Qs1;

display x.l, y.l, l.l, d.l, V.l, O.l, Qs1.l, Xmax.l, Ymax.l, FA.l ;

*sec 2 =======================================================================================================
        
equations
         Floors2          Should be assigned to one floor 
         Floorrs2         test
         Floor1s2         floor constraints value of z 
         Floor2s2         floor constraints value of z 
         Floor3s2         floor constraints value of z 
         Numfloors2       number of floor 
         Orient1s2        equipment orientation constraints length 
         Orient2s2        equipment orientation constraints depth 
         maintenance1s2   maintenance spacing required
         maintenance2s2   maintenance spacing required
         Non1s2           non overlapping constraints 
         Non2s2           non overlapping constraints 
         Non3s2           non overlapping constraints 
         Non4s2           non overlapping constraints
        
         riskzone1s2      worker spacing constrains
         riskzone2s2      worker spacing constrains
         riskzone3s2      worker spacing constrains
         riskzone4s2      worker spacing constrains

         Distance1s2      distance constraint 
         Distance2s2      distance constraint 
         Distance3s2      distance constraint 
         Distance4s2      distance constraint 
         distance11s2     absolute distance constraints 
         distance12s2     absolute distance constraints 
         distance13s2     absolute distance constraints 
         distance14s2     absolute distance constraints 
         distance15ps2    absolute distance constraints
         distance16ps2    absolute distance constraints
         distancef1s2     distance constaints
         Coordinate1s2    coordinate constaints 
         Coordinate2s2    coordinate constaints 
         Coordinate3s2    coordinate constaints 
         Coordinate4s2    coordinate constaints 
         Areals2          plant area linearity 
         Areals21         lower bound of area
         Qcons2           QS constraint 
         Xcons2           x constaint 
         Ycons2           y constaint 
         news2            linearization constraints 
         new1s2           linearization constraints 
         Objls2           objective function linear
         aoes2            area of exposure constaint
         aoe1s2           area of exposure constaint
         aoe2s2           area of exposure constaint
         aoe3s2           area of exposure constaint
         values2          the value of exposure
         maxxs2           maximum probable property damage constaint
         max1s2           Actual probable property damage constaint
         max2s2           one configuration can be installed per pertinent process unit;
         
Floors2(i,sec)$((group(i,sec))and(mass.l(i)+massf(i)<Mmax(sec))and(ord(sec)=2)) ..    sum(k, V(i,sec,k)) =e= 1;

Floorrs2(kt,sec)$((ord(sec)=2)) .. sum(i$(group(i,sec)and(mass.l(i)+massf(i)=Mmax(sec))), V(i,sec,kt)) =g= sum(i$(group(i,sec)),wax(i,sec));

Floor1s2(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=2)) ..   Z(i,j,sec) =g= V(i,sec,k)+V(j,sec,k)-1;

Floor2s2(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=2)) ..    Z(i,j,sec) =l= 1-V(i,sec,k)+V(j,sec,k);

Floor3s2(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=2)) ..    Z(i,j,sec) =l= 1+V(i,sec,k)-V(j,sec,k);

Numfloors2(i,sec)$(group(i,sec)and(ord(sec)=2)) .. NF(sec) =g= sum( k, ord(k)*V(i,sec,k) );

Orient1s2(i,sec)$(group(i,sec) and (ord(sec)=2))      .. l(i) =e= (alpha(i)*O(i))+(beta(i)*(1-O(i)));

Orient2s2(i,sec)$(group(i,sec) and (ord(sec)=2))      .. d(i) =e= alpha(i)+beta(i)-l(i);

maintenance1s2(i,sec)$(group(i,sec) and (ord(sec)=2)) .. vmainx(i) =e= (mainx(i)*O(i))+(mainy(i)*(1-O(i)));

maintenance2s2(i,sec)$(group(i,sec) and (ord(sec)=2)) .. vmainy(i) =e= mainx(i)+mainy(i)-vmainx(i);

Non1s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2))       .. x(i,sec)-x(j,sec)+(M(sec)*(1-Z(i,j,sec)+Ea(i,j)+Eb(i,j))) =g= (( l(i)+l(j) )/2)+(vmainx(i)+vmainx(j));

Non2s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2))       .. x(j,sec)-x(i,sec)+(M(sec)*(2-Z(i,j,sec)-Ea(i,j)+Eb(i,j))) =g= (( l(i)+l(j) )/2)+(vmainx(i)+vmainx(j));

Non3s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2))       .. y(i,sec)-y(j,sec)+(M(sec)*(2-Z(i,j,sec)+Ea(i,j)-Eb(i,j))) =g= (( d(i)+d(j) )/2)+(vmainy(i)+vmainy(j));

Non4s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2))       .. y(j,sec)-y(i,sec)+(M(sec)*(3-Z(i,j,sec)-Ea(i,j)-Eb(i,j))) =g= (( d(i)+d(j) )/2)+(vmainy(i)+vmainy(j));


riskzone1s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2))       .. x(i,sec)-x(j,sec)+(M(sec)*(1-Z(i,j,sec)+Ea(i,j)+Eb(i,j))) =g= WS(i,j)+(l(j)/2);

riskzone2s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2))       .. x(j,sec)-x(i,sec)+(M(sec)*(2-Z(i,j,sec)-Ea(i,j)+Eb(i,j))) =g= WS(i,j)+(l(j)/2);

riskzone3s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2))       .. y(i,sec)-y(j,sec)+(M(sec)*(2-Z(i,j,sec)+Ea(i,j)-Eb(i,j))) =g= WS(i,j)+(d(j)/2);

riskzone4s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2))       .. y(j,sec)-y(i,sec)+(M(sec)*(3-Z(i,j,sec)-Ea(i,j)-Eb(i,j))) =g= WS(i,j)+(d(j)/2);


Distance1s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2) and f(i,j))  .. R(i,j,sec)-Le(i,j,sec) =e= x(i,sec)-x(j,sec);

Distance2s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2) and f(i,j))  .. A(i,j,sec)-B(i,j,sec) =e= y(i,sec)-y(j,sec);

Distance3s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2) and f(i,j))  .. U(i,j,sec)-De(i,j,sec) =e= H(sec)*sum( k, ord(k)*(V(i,sec,k)-V(j,sec,k)));

Distance4s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2) and f(i,j))  .. TD(i,j,sec) =e= R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec)+U(i,j,sec)+De(i,j,sec);

distance11s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2)) .. R(i,j,sec) =l= M(sec)*Wx(i,j);

distance12s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2)) .. Le(i,j,sec) =l= M(sec)*(1-Wx(i,j));

distance13s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2)) .. A(i,j,sec) =l= M(sec)*Wy(i,j);

distance14s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2)) .. B(i,j,sec) =l= M(sec)*(1-Wy(i,j));

distance15ps2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2)).. U(i,j,sec) =l= M(sec)*Wz(i,j);

distance16ps2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2)).. De(i,j,sec) =l= M(sec)*(1-Wz(i,j));

distancef1s2(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2)) .. TD(i,j,sec) =e= TD(j,i,sec);

Coordinate1s2(i,sec)$(group(i,sec)and(ord(sec)=2)) .. x(i,sec) =g= (l(i)/2)+vmainx(i);

Coordinate2s2(i,sec)$(group(i,sec)and(ord(sec)=2)) .. y(i,sec) =g= (d(i)/2)+vmainy(i);

Coordinate3s2(i,sec)$(group(i,sec)and(ord(sec)=2))   .. x(i,sec)+(l(i)/2)+vmainx(i) =l= Xmax(sec);

Coordinate4s2(i,sec)$(group(i,sec)and(ord(sec)=2))   .. y(i,sec)+(d(i)/2)+vmainy(i) =l= Ymax(sec);

aoes2(IP,j,sec) $((pre(IP)<>ord(j)) and (group(IP,sec)) and (group(j,sec)) and (ord(sec)=2))   .. TD(IP,j,sec) =e= Din(IP,j,sec) + Dout(IP,j,sec);

aoe1s2(IP,j,sec) $((pre(IP)<>ord(j)) and (group(IP,sec)) and (group(j,sec)) and (ord(sec)=2))  .. Din(IP,j,sec) =l= Dei(IP)*Yy(IP,j);

aoe2s2(IP,j,sec) $((pre(IP)<>ord(j)) and (group(IP,sec)) and (group(j,sec)) and (ord(sec)=2))  .. Dout(IP,j,sec) =g= Dei(IP)*(1-Yy(IP,j));

aoe3s2(IP,j,sec) $((pre(IP)<>ord(j)) and (group(IP,sec)) and (group(j,sec)) and (ord(sec)=2))  .. Dout(IP,j,sec) =l= Mm*(1-Yy(IP,j));

values2(IP,sec)$(group(IP,sec) and (ord(sec)=2))       .. Ve(IP) =e= CP.l(IP)+sum( j$((pre(IP)<>ord(j)) and group(j,sec)), (CP.l(j)*Yy(IP,j))-((CP.l(j)/Dei(IP))*Din(IP,j,sec)));

maxxs2(IP,sec)$(group(IP,sec) and (ord(sec)=2))    .. ohmz(IP) =e= DF(IP)*Ve(IP);

max1s2(IP,sec)$(group(IP,sec) and (ord(sec)=2))    .. ohm(IP) =e= sum( Ki, grammar(IP,Ki)*ohmz(IP)*Zik(IP,Ki));

max2s2(IP,sec)$(group(IP,sec) and (ord(sec)=2))    .. sum( Ki, Zik(IP,Ki)) =e= 1    ;

Areals2(sec)$(ord(sec)=2)  .. FA(sec) =e= sum( s, AR(s)*QS(s,sec));

Areals21(sec)$(ord(sec)=2) .. FA(sec)*NF(sec) =g= guess.l(sec);

Qcons2(sec)$(ord(sec)=2)   .. sum( s, QS(s,sec)) =e= 1;

Xcons2(sec)$(ord(sec)=2)   .. Xmax(sec) =e= sum( s, Xbar(s)*QS(s,sec));

Ycons2(sec)$(ord(sec)=2)   .. Ymax(sec) =e= sum( s, Ybar(s)*QS(s,sec));

news2(s,sec)$(ord(sec)=2)  .. NQ(s,sec) =l= Kn*QS(s,sec);

new1s2(sec)$(ord(sec)=2)   .. NF(sec) =e= sum( s, NQ(s,sec));

Objls2        .. Qs2 =e= sum( (i,j,sec)$((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2) and f(i,j)), (( Cc(i,j)*TD(i,j,sec)/0.3048)+( Cv(i,j)*De(i,j,sec) )+( Ch(i,j)*(R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec))))/10**5 )+(sum((s,sec)$(ord(sec)=2), AR(s)*NQ(s,sec)*FC2(sec))/10**5)+(LC*sum(sec$(ord(sec)=2), FA(sec))/10**5)+ sum( (IP,sec)$(group(IP,sec)and(ord(sec)=2)), ohm(IP)) + sum( (IP,Ki,sec)$(group(IP,sec)and(ord(sec)=2)), Pik(IP,Ki)*Zik(IP,Ki)/10**5) ;

Qs2.lo=0;

Model linearitys2      
        /Floors2,
         Floorrs2,
         Floor1s2,
         Floor2s2,
         Floor3s2,
         Numfloors2,
         Orient1s2,
         Orient2s2,
         maintenance1s2,
         maintenance2s2,
         Non1s2,
         Non2s2,
         Non3s2,
         Non4s2,
         
         riskzone1s2,
         riskzone2s2,
         riskzone3s2,
         riskzone4s2,
         
         Distance1s2,
         Distance2s2,
         Distance3s2,
         Distance4s2,
         distance11s2,
         distance12s2,
         distance13s2,
         distance14s2,
         distance15ps2,
         distance16ps2,
         distancef1s2,
         Coordinate1s2,
         Coordinate2s2,
         Coordinate3s2,
         Coordinate4s2,
         Areals2,
         Areals21,
         Qcons2,
         Xcons2,
         Ycons2,
         news2,
         new1s2,
         aoes2,
         aoe1s2,
         aoe2s2,
         aoe3s2,
         values2,
         maxxs2,
         max1s2,
         max2s2,
         Objls2/;

option limrow = 1000;

solve linearitys2 using minlp minimizing Qs2;

display x.l, y.l, l.l, d.l, V.l, O.l, Qs2.l, R.l, Le.l, A.l, B.l, TD.l ;


*Layout of two sections objective model  ===========================================================================================================================

parameter
        alphag(sec)     'dimension of s section'
        betag(sec)      'dimension of s section';
        
alphag(sec) = Xmax.l(sec);
betag(sec)  = Ymax.l(sec);
ss          = max(sum( sec, alphag(sec)), sum( sec, betag(sec)));

display alphag, betag, ss;

Parameter csec(sec,secx) connection cost between sections ($ per m)
          hcsec(sec,secx) horizontal cost between sections ($ per m year) ;
          
csec(sec,secx)=sum((i,j)$((group(i,sec)) and (group(j,secx)) and (f(i,j)=0) and (ord(i)<>ord(j)) and (ord(sec)<>ord(secx))), Cc(i,j));

hcsec(sec,secx)=sum((i,j)$((group(i,sec)) and (group(j,secx)) and (f(i,j)=0) and (ord(i)<>ord(j)) and (ord(sec)<>ord(secx))), Ch(i,j));

display csec, hcsec;

equation
        length         length s eq
        delth          delth s eq
        nons1          non overlapping for s
        nons2          non overlapping for s
        nons3          non overlapping for s
        nons4          non overlapping for s
        relative1      relative s1
        relative2      relative s2
        relative3      relative s3
        relative4      s4
        relative5      s5
        relative6      s6
        relative7      s7
        coordinates1   coordinate s
        coordinates2   coordinate s
        coordinates3   coordinate s
        coordinates4   coordinate s
        areas          area for s
        objs           obj for s    ;
        
length(sec) .. ll(sec) =e= (alphag(sec)*OO(sec))+(betag(sec)*(1-OO(sec)));

delth(sec)  .. dd(sec) =e= alphag(sec)+betag(sec)-ll(sec);

nons1(sec,secx)$(ord(sec)<>ord(secx))     .. (xxx(sec)-xxx(secx))+(ss*(Eaa(sec,secx)+Ebb(sec,secx))) =g= ((ll(sec)+ll(secx))*0.8);
                                
nons2(sec,secx)$(ord(sec)<>ord(secx))     .. xxx(secx)-xxx(sec)+(ss*(1-Eaa(sec,secx)+Ebb(sec,secx))) =g= ((ll(sec)+ll(secx))*0.8);
                                
nons3(sec,secx)$(ord(sec)<>ord(secx))     .. yyy(sec)-yyy(secx)+(ss*(1+Eaa(sec,secx)-Ebb(sec,secx))) =g= ((dd(sec)+dd(secx))*0.8);
                                
nons4(sec,secx)$(ord(sec)<>ord(secx))     .. yyy(secx)-yyy(sec)+(ss*(2-Eaa(sec,secx)-Ebb(sec,secx))) =g= ((dd(sec)+dd(secx))*0.8);

relative1(sec,secx)$(ord(sec)<>ord(secx)) .. RR(sec,secx)-Lee(sec,secx) =e= xxx(sec)-xxx(secx);
                                
relative2(sec,secx)$(ord(sec)<>ord(secx)) .. AA(sec,secx)-BB(sec,secx) =e= yyy(sec)-yyy(secx);
                                
relative3(sec,secx)$(ord(sec)<>ord(secx)) .. TDD(sec,secx) =e= RR(sec,secx)+Lee(sec,secx)+AA(sec,secx)+BB(sec,secx);

relative4(sec,secx)$(ord(sec)<>ord(secx)) .. RR(sec,secx) =l= ss*Wxx(sec,secx);
                                          
relative5(sec,secx)$(ord(sec)<>ord(secx)) .. Lee(sec,secx) =l= ss*(1-Wxx(sec,secx));
                                          
relative6(sec,secx)$(ord(sec)<>ord(secx)) .. AA(sec,secx) =l= ss*Wyy(sec,secx);
                                          
relative7(sec,secx)$(ord(sec)<>ord(secx)) .. BB(sec,secx) =l= ss*(1-Wyy(sec,secx));

coordinates1(sec) .. xxx(sec) =g= ll(sec)/2;
                                
coordinates2(sec) .. yyy(sec) =g= dd(sec)/2;

coordinates3(sec) .. xxx(sec)+(ll(sec)*0.5) =l= xxxmax;

coordinates4(sec) .. yyy(sec)+(dd(sec)*0.5) =l= yyymax;

areas             .. FAs =e= xxxmax*yyymax;

objs              .. QQ =e= sum( (sec,secx), (csec(sec,secx)+hcsec(sec,secx))*TDD(sec,secx))+((LC+FC1)*FAs);

QQ.lo=0;

model final
    /length,
     delth,
     nons1,
     nons2,
     nons3,
     nons4,
     relative1,
     relative2,
     relative3,
     relative4,
     relative5,
     relative6,
     relative7,
     coordinates1,
     coordinates2,
     coordinates3,
     coordinates4,
     areas,
     objs/;
     
solve final using minlp minimizing QQ;

parameter
        newX(i,sec)         final x cordinate
        newY(i,sec)         final Y cordinate
        newL(i,sec)         final length
        newD(i,sec)         final delth
        miniX               minimum X coordinate
        miniY               minimum y coordinate;
        
miniX=smin(sec,xxx.l(sec));

miniY=smin(sec,yyy.l(sec));

newL(i,sec)$group(i,sec) = (l.l(i)*OO.l(sec))+(d.l(i)*(1-OO.l(sec)));

newD(i,sec)$group(i,sec) = l.l(i)+d.l(i)-newL(i,sec);
        
newX(i,sec)$(group(i,sec)and(xxx.l(sec)>miniX)) = (((x.l(i,sec)-(0.5*Xmax.l(sec)))+xxx.l(sec))*OO.l(sec))+((1-OO.l(sec))*((y.l(i,sec)-(0.5*Ymax.l(sec)))+xxx.l(sec)));

newX(i,sec)$(group(i,sec)and(xxx.l(sec)=miniX)) = (x.l(i,sec)*OO.l(sec))+(y.l(i,sec)*(1-OO.l(sec)));
        
newY(i,sec)$(group(i,sec)and(yyy.l(sec)>=miniY)) = (((y.l(i,sec)-(0.5*Ymax.l(sec)))+yyy.l(sec))*OO.l(sec))+((1-OO.l(sec))*(yyy.l(sec)-(X.l(i,sec)-(0.5*Xmax.l(sec)))));

display  newX, newY, newL, newD, miniX, miniY;
