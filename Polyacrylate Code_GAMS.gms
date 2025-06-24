*Material selection for equipment items model ==========================================================================================================

set
    i       equipment i   /i1*i11/
    k       floor   /1,2,3/   
    s       candidate area /s1*s7/
    IP(i)   set of pertinent process unit /i1, i2, i3, i11/   
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
    denf(i)         density of fluid inside equipment(kg per m^3)/i1 979.386, i2 966.773, i3 1002.12, i4 1002.12, i5 1074.2, i6 1078.02, i7 999.323, i8 999.323, i9 1.335, i10 1.452, i11 1.452/ 
    massf(i)        mass of fluid inside equipment (kg)
    group(i,sec)    set of section with equipment;
           
parameter
        alpha(i)   width of equipments i (metre)
          / i1   1.28
            i2   1.29
            i3   1.29
            i4   1.28
            i5   1.5
            i6   2.8
            i7   5
            i8   1.2
            i9   5     
            i10  1.25
            i11  0.4          /
            
        beta(i)  length of equipment i (m)
          / i1   1.28
            i2   1.29
            i3   1.29
            i4   1.28
            i5   1.8
            i6   4.5
            i7   5
            i8   1.2
            i9   5       
            i10  1.8      
            i11  1.1        /
            
        hita(i) height of equipment i (m)
          / i1   3.84
            i2   3.88
            i3   3.88
            i4   3.84
            i5   1.86
            i6   5.5
            i7   7.5
            i8   3.6
            i9   7        
            i10  1.67     
            i11  0.7           /;
            
        
table group(i,sec) equipment in each groups
            sec1    sec2    
        i1   1
        i2   1
        i3   1
        i4   1             
        i5   1        
        i6           1
        i7           1                
        i8           1       
        i9           1   
        i10          1
        i11          1          ;
        

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
          Po(i)         operating pressure (psig) /i1 1, i2 28.8154, i3 28.8154, i4 1.2582, i5 1.2582, i6 1.2582, i7 1, i8 1, i9 1, i10 1.983, i11 1.2582/
          tp(i)         wall thickness to withstand the internal pressure (inches)
          str           maximum allowable stress (psi) /15000/
          weld          fractional weld efficiency /1/
          orr(i)        1 indicated that i is vertical vessel /i1*i4 1, i7*i9 1/
          HoAndOrr(i)   1 indicated that i is horizontal and vertical vessel /i1*i4 1, i7*i9 1/
          comp(i)       1 indicated that i is compressor /i10 1/
          heater(i)     1 indicated that i is heater /i11 1/
          dry(i)        1 indicated that i is dryer /i6 1/
          centri(i)     1 indicated that i is Centrifuge Solid Separator /i5 1/
          reactor(i)    1 indicated that i is Reactor /i2*i3 1/
          cpl(i)        cost for platform
          
          fd            electric motor drive for compressors /1.25/
          pc(i)         horsepower(Hp) of compressor /i10 81.1793/
          cb(i)         base cost of compressors
          ev(i)         evaporation rate of spray dryer(lb per hr)/i6 1309.15/
          cbf(i)        base cost of fired heater
          ener(i)       heat duty of fired heater(btu per hour) /i11 1700190/   
          fpf(i)        pressure factor for fired heater
          
          cpd(i)        cost of spray dryer ($ per 10^5)
          cpa(i)        cost for agitator
          
          hp(i)         hp for agitator (hp) /i2*i3 60/
          ctf(i)        cost for Centrifuge Solid Separator
          
          bd(i)         Bowl diameter for Batch tob drive centifuge (in)/i5 9.84/
          
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

* cost for compressors (centrifugal type)

cb(i)$comp(i) = exp(7.58+0.8*log(pc(i)));

display cb;

*cost for fired heater

cbf(i)$heater(i)    = exp(0.32325+0.766*log(ener(i)));

fpf(i)$heater(i)    = 0.986-0.0035*(Po(i)/500)+0.0175*(Po(i)/500)**2;

display cbf, fpf;

*cost for dryer (stainless steel is used in this eq)

cpd(i)$dry(i)      = (exp(8.2938+(0.8526*log(ev(i)))-(0.0229*log(ev(i))**2))*821.1/500)/10**5;

display cpd;

*cost for agitator

cpa(i)$reactor(i)   = (3620*hp(i)**0.57)/10**5;

display cpa;

*cost for Centrifuge Solid separator

ctf(i)$centri(i)    = (2000*bd(i)**0.95)/10**5;

display ctf; 
*========================================================================================================

positive variable
    mass(i)     mass of equipment i (kg)
    Tm(sec)     total mass of that section sec (kg)
    Tn(te)      total materials used
    cpv(i)      equipment cost of vessel ($ per 10^5)
    cpf(i)      cost for fired heater ($ per 10^5)
    ccomp(i)    cost of compressors ($ per 10^5)
    w(i)        weight of equipment (lb)
    cvv(i)      cost for pressure vessel and tower for distillation absorption and stripping
    CP(i)       equipment cost ($ per 10^5)
    cpr(i)      cost for reactor ($ per 10^5)
    
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
    mat2        'material constaints'
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
    ccompress   'cost of compressors'
    ccompress2  'cost of compressors'
    cequip      'cost of each equipments'
    creac       'cost of reactor'
    obj         'objective function';
    
masc(i)                .. mass(i) =e= sum(te, p(te)*vol(i)*zz(i,te));

mat(i)                 .. sum(te, zz(i,te)) =e= 1;

mat1(i)$dry(i)         .. sum(te$(ord(te)=7), zz(i,te)) =e= 1;

mat2(i)$centri(i)      .. sum(te$(ord(te)=7), zz(i,te)) =e= 1;

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

* for compressors ===================================================================================================================================

ccompress(i,typ)$((ord(typ)=2) and comp(i))  .. ccomp(i) =e= (sum(te$fm(te,typ), fd*fm(te,typ)*cb(i)*zz(i,te)*821.1/500))/10**5;

ccompress2(i,typ)$((ord(typ)=2) and comp(i)) .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;

* for reactor ===================================================================================================================================

creac(i)$reactor(i) .. cpr(i) =e= (cpv(i)+cpa(i))*821.1/500;

* for each equipments ===================================================================================================================================

cequip(i)           .. CP(i) =e= cpv(i)+cpf(i)+ccomp(i)+cpd(i)+cpr(i)+ctf(i);

*====================================================================================================================================================

obj                 .. wei =e= sum(i, CP(i))+(sum((st,sec), wd(st,sec)*costst(st)*guess(sec))/10**5);

wei.lo=0;

option limrow = 500;

model weight /masc,
              mat,
              mat1,
              mat2,
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
              ccompress,
              ccompress2,
              creac,     
              cequip,
              obj/      ;

solve weight using minlp minimizing wei;

display zz.l, Tn.l, mass.l, Tm.l, wei.l, cvv.l, cpv.l, cpf.l, ccomp.l ;

parameter Mmax(sec) maximun mass of that section;

Mmax(sec)=smax(i$group(i,sec),mass.l(i)+massf(i));

display Mmax;

*Multi-floor process plant layout model =================================================================================================================================================================================================================================================================================================================================================================================================================================================================

Parameter   pre(IP)       'IP value' /i1 1, i2 2, i3 3, i11 11/
            wax(i,sec)    'test for heavy weight'
            haz(sec)      'section that has hazadous equipment' /sec1*sec2 1/;
            

scalar Kn 'total of k';
          
Kn = card(k);

          
wax(i,sec)$((mass.l(i)+massf(i)=Mmax(sec))and(group(i,sec)))=1;

display wax;


table f(i,j) flow relationship between item i and j
            i1  i2  i3  i4  i5  i6  i7  i8  i9  i10 i11 
    i1          1          
    i2              1                               
    i3                  1
    i4                      1
    i5                                  
    i6                              1
    i7                                  1   1                   
    i8                                                                                                    
    i9                                           1                                                                              
    i10                                               1                                                                      
    i11                         1                                                             ;                                                                                                                                

table Cc(i,j) connection costs ($ per ft)
            i1      i2      i3      i4      i5      i6      i7      i8      i9      i10     i11                  
    i1              55.44
    i2                      57.31                                                          
    i3                              55.59       
    i4                                      56.47                    
    i5                                              29              50.31                
    i6                                                      1042.34                                         
    i7                                                              13.27   928.95                            
    i8                                                     
    i9                                                                              929.03        
    i10                                                                                     894.52       
    i11                                             1136.58                                                                                                                                
        ;                                                                                                                                


table Cv(i,j)   vertical cost ($ per m yr)
            i1      i2      i3      i4      i5      i6      i7      i8      i9      i10     i11              
    i1              6036.85   
    i2                      6049.48 
    i3                              6049.48     
    i4                                      6259.89 
    i5                                              1526.85         4733.04
    i6                                                      4458.13                                 
    i7                                                              250.08  4208.05          
    i8                                                     
    i9                                                                              4208.34                                                        
    i10                                                                                     4208.34
    i11                                             4208.35
    ;                                                                                                                   
 

parameter
    Ch(i,j)   horizontal cost ($ per m yr);
    
Ch(i,j) = Cv(i,j)/10;

display Ch;

parameter
    Xbar(s)         candidate x coordinate /s1 10, s2 25, s3 20, s4 25, s5 30, s6 40, s7 45/
    ybar(s)         candidate y coordinare /s1 15, s2 20, s3 30, s4 15, s5 10, s6 10, s7 20/
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
    Qs3             'obj for section3'
    QQ              'obj for s';
    
*safety part

Parameter
    F1(IP)          'general process hazards factor of item i' /i1 1.85, i2 2.35, i3 2.35, i11 0.85/
    F2(IP)          'special process hazards factor of item i' /i1 1.58, i2 1.42, i3 1.42, i11 0.8/
    F3(IP)          'process unit hazards factor of item i'
    Fi(IP)          'fire and explosion index of item i'
    MF(IP)          'material factor of item i' /i1 24, i2 4, i3 4, i11 1/
    Dei(IP)         'the distance of exposure i'  
    DF(IP)          'damage factor' /i1 0.657, i2 0.084, i3 0.084, i11 0.011/
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
mainx(i)$comp(i)      = 0.8;
mainy(i)$comp(i)      = 0.8;
mainx(i)$heater(i)    = 3;
mainy(i)$heater(i)    = beta(i)+15;
mainx(i)$dry(i)       = 1;
mainy(i)$dry(i)       = 1;
mainx(i)$reactor(i)   = 4;
mainy(i)$reactor(i)   = 4;
mainx(i)$centri(i)    = 1;
mainy(i)$centri(i)    = 1;

M(sec)  = max(sum( i, (alpha(i)+mainx(i))*ord(sec)), sum( i, (beta(i)+mainy(i))*ord(sec)));

display UU, Dei, mainx, mainy, M;

table grammar(IP,Ki) loss control credit factor
                K1      K2      K3      K4      K5      K6
        i1      1       0.9     0.76    0.68    0.517   0.465
        i2      0.9     0.75    0.405   0.365   0.194   0.117 
        i3      0.9     0.75    0.405   0.365   0.194   0.117 
        i11     1       0.9     0.76    0.68    0.517   0.465 ;
        
table Pik(IP,Ki) protection device cost
                K1      K2      K3      K4      K5      K6
        i1              5000    20000   30000   50000   55000        
        i2      5000    15000   35000   40000   90000   125000         
        i3      5000    15000   35000   40000   90000   125000
        i11             5000    20000   30000   50000   55000   ;

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



