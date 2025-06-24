*Material selection for equipment items model ==========================================================================================================

set
    i       equipment i   /i1*i9/
    k       floor   /1,2/
    IP(i)   set of pertinent process unit /i2, i3, i4, i5, i6, i7, i8, i9/   
    kk      protection device configuration /1*7/
    Ki      set of protection device configurations suitable for installation on item i /K1*K6/
    s       candidate area /s1*s7/
    kt(k)   first floor /1/
    sec     section /sec1/
    te      material /te1*te7/
    typ     type of equipment /typ1*typ5/
    st      type of structure /st1, st2/ ;
    
Alias (i,j);
    
Alias (sec,secx);
    
parameter
    vol(i)          volume of equipment(m^3)
    vols(i)         volume of equipment for storage(m^3)
    p(te)           density of material(kg per m^3) /te1 7300, te2 7500, te3 8900, te4 4540, te5 7870, te6 7850, te7 8000/
    denf(i)         density of fluid inside equipment(kg per m^3)/i1 1050.18, i2 245.009, i3 351.75, i4 551.659, i5 1141.14, i6 926.698, i7 1078.23, i8 1103.67, i9 699.679/ 
    massf(i)        mass of fluid inside equipment (kg)
    group(i,sec)    set of section with equipment;
    
parameter
        alpha(i)  y width of equipments i (metre)
          / i1   5.4
            i2   4.8
            i3   4.1
            i4   4.1
            i5   0.5
            i6   1.0
            i7   2.7
            i8   2.7
            i9   2.0           /
            
        beta(i)  x length of equipment i (m)
          / i1   5.4
            i2   4.8
            i3   4.1
            i4   4.1
            i5   1.25
            i6   2.5
            i7   2.7
            i8   2.7
            i9   2.0          /
            
       hita(i) height of equipment i (m)
          / i1   8.1
            i2   7.2
            i3   6.2
            i4   6.2
            i5   4.5
            i6   6.0
            i7   4.1
            i8   4.1
            i9   3.0    /;
            
table group(i,sec) equipment in each groups
            sec1   
        i1  1
        i2  1
        i3  1
        i4  1             
        i5  1        
        i6  1        
        i7  1               
        i8  1      
        i9  1   ;
        
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
          Po(i)         operating pressure (psig) /i1 1990.8, i2 1990.8, i3 1990.8, i4 1990.8, i5 1990.8, i6 1990.8, i7 1990.8, i8 1990.8, i9 1990.8/
          tp(i)         wall thickness to withstand the internal pressure (inches)
          str           maximum allowable stress (psi) /15000/
          weld          fractional weld efficiency /1/
          orr(i)        1 indicated that i is vertical vessel /i1*i9 1/
          HoAndOrr(i)   1 indicated that i is horizontal and vertical vessel /i1*i9 1/
          tow(i)        1 indicated that i is tower vessel /i5*i6  1/
          dis(i)        1 indicated that i is distillation column /i5*i6  1/
          reactor(i)    1 indicated that i is reactor /i2*i3 1, i7 1, i9 1/
          cpl(i)        cost for platform
          cbt(i)        base cost for trays
          fnt(i)        number of tray factor
          nt(i)         number of tray  /i5 5, i6 10/
          ftt(i)        type of tray    /i5*i6 1/
          ftm(i)        material factor for distillation
          ct(i)         cost for installed tray
          cpa(i)        cost for agitator
          hp(i)         hp for agitator(hp) /i2 60, i3 60, i7 60, i9 60/ 
          capweight(st) weight capacity for structure(kg per m^2) /st1 1000, st2 1500/
          costst(st)    structure cost for weight($ per m^2) /st1 222, st2 300/;
        
vol(i)$(Po(i)>1000) = alpha(i)*beta(i)*hita(i)*0.4;

vol(i)$((Po(i)>500) and (Po(i)<1000)) = alpha(i)*beta(i)*hita(i)*0.3;

vol(i)$(Po(i)<500) = alpha(i)*beta(i)*hita(i)*0.2;

vols(i) = (alpha(i)*beta(i)*hita(i))-vol(i);

massf(i) = vols(i)*denf(i);

display vol, vols, massf;

Pd(i)$HoAndOrr(i)   = exp(0.60608+(0.91615*log(Po(i)))+(0.0015655*(log(Po(i)))**2));

tp(i)$HoAndOrr(i)   = ((Pd(i)*alpha(i)*39.37)/((2*str*weld)-(1.2*Pd(i))));

display Pd, tp;


*vertical vessel platform

cpl(i)$orr(i)       = 361.8*((alpha(i)*3.281)**0.7396)*((hita(i)*3.281)**0.70684);

*tower vessel platform

cpl(i)$(tow(i) and dis(i))  = 300.9*((alpha(i)*3.281)**0.63316)*((hita(i)*3.281)**0.80161);

* trays for distillation

cbt(i)$dis(i)       = 468*exp(0.1739*alpha(i)*3.281);

fnt(i)$dis(i)       = 2.25/(1.0414**nt(i));

*316 stainless steel is used since it has widele range of fabrication properties (table 7.1 in ?????????????)

ftm(i)$dis(i)       = 1.401+(0.0724*alpha(i)*3.281);

ct(i)$dis(i)        = nt(i)*fnt(i)*ftt(i)*ftm(i)*cbt(i)*(821.1/500)/10**5;

display Pd, tp, cpl, cbt, ct;

*cost for agitators (turbine, closed vessel)

cpa(i)$reactor(i)   = (3620*hp(i)**0.57)/10**5;

display cpa;


*========================================================================================================

positive variable
    mass(i)     mass of equipment i (kg)
    Tm(sec)     total mass of that section sec (kg)
    Tn(te)      total materials used
    cpv(i)      equipment cost of vessel ($ per 10^5)
    cpt(i)      total cost of distillation column ($ per 10^5)
    cpr(i)      cost for reactor ($ per 10^5)
    w(i)        weight of equipment (lb)
    cvv(i)      cost for pressure vessel and tower for distillation absorption and stripping
    CP(i)       equipment cost ($ per 10^5)
    
Integer variable
    guess(sec)  first guess area for 1 sections(m^2);
    
variable
    wei         objective weight variable;
    
binary variable
    zz(i,te)      1 if material te is selected for equipment i
    wd(st,sec)    weight decision for structure;
    
equation
    masc        'mass costaints'
    mat         'material constaints'
    num         'number of materials used'
    totalmass   'section mass'
    test        'test av weight'
    test1       'test av weight'
    weighte     'weight of equipment'
    costhv      'base cost of vertical vessel'
    cvessel     'cost of vessel'
    cvessel2    'cost of vessel'
    cdis        'cost of distillation column'
    creac       'cost of reactor'
    cequip      'cost of each equipments'
    obj         'objective function';
    
masc(i)             .. mass(i) =e= sum(te, p(te)*vol(i)*zz(i,te));

mat(i)              .. sum(te, zz(i,te)) =e= 1;

num(te)             .. Tn(te) =e= sum(i, zz(i,te));

totalmass(sec)      .. Tm(sec) =e= sum(i$group(i,sec), mass(i));

test(sec)           .. Tm(sec) =l= sum(st, wd(st,sec)*capweight(st)*guess(sec))-sum(i$group(i,sec), massf(i));

test1(sec)          .. sum(st, wd(st,sec)) =e= 1;

* for vessel ===================================================================================================================================

weighte(i)$HoAndOrr(i)  .. w(i) =e= sum(te, 3.14*(alpha(i)*3.281+tp(i))*(hita(i)*3.281+0.8*alpha(i)*3.281)*tp(i)*p(te)*zz(i,te));

costhv(i)$orr(i)    .. cvv(i) =e= exp(7.0132+0.18255*log(w(i)+1)+(0.02297*(log(w(i)+1)**2)));

cvessel(i,typ)$((ord(typ)=5) and HoAndOrr(i))  .. cpv(i) =e= (sum(te$fm(te,typ), zz(i,te)*(fm(te,typ)*cvv(i)+cpl(i))*(821.1/500)))/10**5;

cvessel2(i,typ)$((ord(typ)=5) and HoAndOrr(i)) .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;  

* for distillation column ===================================================================================================================================

cdis(i)$dis(i)          .. cpt(i) =e= cpv(i)+ct(i);

* for reactor ===================================================================================================================================

creac(i)$reactor(i) .. cpr(i) =e= (cpv(i)+cpa(i))*821.1/500;

* for each equipments ===================================================================================================================================

cequip(i)           .. CP(i) =e= cpv(i)+cpt(i)+cpr(i);

*====================================================================================================================================================

obj                 .. wei =e= sum(i, CP(i))+(sum((st,sec), wd(st,sec)*costst(st)*guess(sec))/10**5);

wei.lo=0;

option limrow = 500;

model weight /masc,
              mat,
              num,
              totalmass,
              test,
              test1,
              weighte,
              costhv,
              cvessel,
              cvessel2,
              cdis,
              creac,
              cequip,
              obj/      ;

solve weight using minlp minimizing wei;

display zz.l, Tn.l, mass.l, Tm.l, wei.l, cvv.l, cpv.l, cpt.l;

parameter Mmax(sec) maximun mass of that section;

Mmax(sec)=smax(i$group(i,sec),mass.l(i)+massf(i));

display Mmax;




*Multi-floor process plant layout model =================================================================================================================================================================================================================================================================================================================================================================================================================================================================

Parameter pre(IP)       'IP value' /i2 2, i3 3, i4 4, i5 5, i6 6, i7 7, i8 8, i9 9/
          wax(i,sec)    'test for heavy weight'
          haz(sec)      'section that has hazadous equipment' /sec1 1/;
          
scalar Kn 'total of k';
          
Kn = card(k);

wax(i,sec)$((mass.l(i)+massf(i)=Mmax(sec))and(group(i,sec)))=1;

display wax;

table f(i,j) flow relationship between item i and j
            i1  i2  i3  i4  i5  i6  i7  i8  i9  
    i1          1           1   1
    i2              1
    i3                  1
    i4                      1   1
    i5                                      1
    i6                              1
    i7                                  1   
    i8                                      1                                                                      
    i9                                            ;                                                                         
                                                                                                                                                                                                
        
table Cc(i,j) connection costs ($ per ft)
            i1      i2      i3      i4      i5      i6      i7      i8      i9      
    i1              290.63                  78.48   308.95
    i2                      246.98
    i3                              201.71      
    i4                                      142.43  152.67           
    i5                                                                      61.89                      
    i6                                                      113.98                                                 
    i7                                                              112.79    
    i8                                                                      77.13
    i9                                                                          ;                                                                   
      
             
table Cv(i,j)   vertical cost ($ per m yr)
             i1      i2      i3      i4      i5      i6      i7      i8      i9          
    i1               58388                   13643   34580
    i2                       58388
    i3                               58388      
    i4                                       5574    52814             
    i5                                                                       8745                      
    i6                                                       32096                                                 
    i7                                                               32096     
    i8                                                                       8745 
    i9                                                                              ;
    
parameter
    Ch(i,j)   horizontal cost ($ per m yr);
    
Ch(i,j) = Cv(i,j)/10;

display Ch;

parameter
    Xbar(s)         candidate x coordinate /s1 50, s2 60, s3 60, s4 65, s5 70, s6 80, s7 50/
    ybar(s)         candidate y coordinare /s1 10, s2 20, s3 30, s4 30, s5 10, s6 20, s7 50/
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

display AR, Kn, H, FC2;

Binary Variable  
    V(i,sec,k)      'item i is assigned to floor k' 
    Z(i,j,sec)      'i and j are allocated to the same floor'
    O(i)            'rotation'
    Ea(i,j)         'non1'
    Eb(i,j)         'non2'
    QS(s,sec)       'binary variable for linearity'
    Wx(i,j)         '1 if i is right of j'
    Wy(i,j)         '1 if i is above of j';

    
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
    NQ(s,sec)       'new variable';
    
variables    
    TD(i,j,sec)     'total rectilinear distance between items i and j'
    TDD(sec,secx)   'total rectilinear distance between s'
    FA(sec)         'floor area'
    Qs1             'obj for section1';
    
*safety part

Parameter
    F1(IP)          'general process hazards factor of item i' /i2 1.8, i3 1.3, i4 1, i5 1, i6 1, i7 1.3, i8 0.5, i9 1.3/
    F2(IP)          'special process hazards factor of item i' /i2 7.29, i3 8.22, i4 8.22, i5 6.65, i6 7.29, i7 6.49, i8 6.31, i9 5.66/
    F3(IP)          'process unit hazards factor of item i'
    Fi(IP)          'fire and explosion index of item i'
    MF(IP)          'material factor of item i' /i2 4, i3 4, i4 4, i5 4, i6 4, i7 4, i8 4, i9 4/
    Dei(IP)         'the distance of exposure i'  
    DF(IP)          'damage factor' /i2 0.182, i3 0.182, i4 0.182, i5 0.149, i6 0.164, i7 0.182, i8 0.081, i9 0.165/
    UU              'appropriate upper bound for safety'
    Mm              'appropriate upper bound for aoe eq'
    mainx(i)        'maintanance distance x axis'
    mainy(i)        'maintanance distance y axis'
    ss              'upper bound of s'
    
    PS(i)           'spacing for the public (m)' /i1 9.21, i2 8, i3 8, i4 8, i5 8, i6 8.2, i7 3.02, i8 2, i9 3/;
    
    
F3(IP)  = F1(IP)*F2(IP);
Fi(IP)  = F3(IP)*MF(IP);
Dei(IP) = 0.256*Fi(IP);

UU(IP)  = sum( i, CP.l(i))*DF(IP);
Mm      = sum( IP, Dei(IP));
mainx(i)$reactor(i)   = 4;
mainy(i)$reactor(i)   = 4;
mainx(i)$HoAndOrr(i)  = hita(i);
mainy(i)$HoAndOrr(i)  = hita(i);

table WS(i,j)   spacing for worker (m)
             i1      i2      i3      i4      i5      i6      i7      i8      i9          
    i1               30.5    30.5    30.5    30.5    30.5    30.5    30.5    30.5
    i2                       4.6     7.6     7.6     7.6     4.6     7.6     4.6
    i3                               7.6     7.6     7.6     4.6     7.6     4.6
    i4                                       4.6     4.6     7.6     4.6     7.6                                           
    i5                                               4.6     7.6     4.6     7.6                                       
    i6                                                       7.6     4.6     7.6                                       
    i7                                                               7.6     4.6                                                        
    i8                                                                       7.6
    i9                                                                             ;


M(sec)  = max(sum( i, (alpha(i)+mainx(i))*ord(sec)), sum( i, (beta(i)+mainy(i))*ord(sec)),sum((i,j), ((alpha(i)/2)+WS(i,j))*ord(sec)),sum((i,j), ((beta(i)/2)+WS(i,j))*ord(sec)));

display UU, Dei, mainx, mainy, M;

table grammar(IP,Ki) loss control credit factor
                K1      K2      K3      K4      K5      K6
        i2      0.9     0.75    0.405   0.365   0.194   0.117
        i3      0.9     0.75    0.405   0.365   0.194   0.117 
        i4      1       0.9     0.76    0.68    0.517   0.465
        i5      1       0.9     0.76    0.68    0.517   0.465
        i6      1       0.9     0.76    0.68    0.517   0.465
        i7      0.9     0.75    0.405   0.365   0.194   0.117
        i8      1       0.9     0.76    0.68    0.517   0.465
        i9      0.9     0.75    0.405   0.365   0.194   0.117 ;
        
table Pik(IP,Ki) protection device cost
                K1      K2      K3      K4      K5      K6
        i2      5000    15000   35000   40000   90000   125000        
        i3      5000    15000   35000   40000   90000   125000         
        i4              5000    20000   30000   50000   55000
        i5              5000    20000   30000   50000   55000
        i6              5000    20000   30000   50000   55000
        i7      5000    15000   35000   40000   90000   125000
        i8              5000    20000   30000   50000   55000
        i9      5000    15000   35000   40000   90000   125000 ;
        


Variables
        ohmz(IP)        'base maximum probable property damage cost'
        ohm(IP)         'actual probable property damage cost';
        
Positive variable
        QZ(i,Ki)        'new variable for safety'
        Ve(i)           'value of the area of explosure of i'
        Din(i,j,sec)    'total rectilinear distance between items i and j if Din is smaller than Dei'
        Dout(i,j,sec)   'total rectilinear distance between items i and j if Din is larger than Dei';
        
Binary Variable
        Yy(IP,j)        '1 if j is allocated within the area of exposure of i'
        Zik(IP,Ki)      '1 if protection device is equipped'
        Wx(i,j)         '1 if i is right of j'
        Wy(i,j)         '1 if i is above of j'
        Wz(i,j)         '1 if i upper than j';
        
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
         
         riskzone21s1     public spacing constrains
         riskzone22s1     public spacing constrains
         riskzone23s1     public spacing constrains
         riskzone24s1     public spacing constrains
         
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
         max2s1           one configuration can be installed per pertinent process unit;;
         
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


riskzone21s1(i,sec) $(group(i,sec)and(ord(sec)=1))   .. x(i,sec) =g= PS(i)*0.8;

riskzone22s1(i,sec) $(group(i,sec)and(ord(sec)=1))   .. y(i,sec) =g= PS(i)*0.8;

riskzone23s1(i,sec) $(group(i,sec)and(ord(sec)=1))   .. x(i,sec)+PS(i)*0.5 =l= Xmax(sec);

riskzone24s1(i,sec) $(group(i,sec)and(ord(sec)=1))   .. y(i,sec)+PS(i)*1 =l= Ymax(sec);




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

Objls1        .. Qs1 =e= sum( (i,j,sec)$((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1) and f(i,j)), (( Cc(i,j)*TD(i,j,sec)/0.3048)+( Cv(i,j)*De(i,j,sec) )+( Ch(i,j)*(R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec))))/10**5 )+(sum( (s,sec)$(ord(sec)=1), AR(s)*NQ(s,sec)*FC2(sec))/10**5)+(LC*sum(sec$(ord(sec)=1), FA(sec))/10**5)+ sum( (IP,sec)$(group(IP,sec)and(ord(sec)=1)), ohm(IP)) + sum( (IP,Ki,sec)$(group(IP,sec)and(ord(sec)=1)), Pik(IP,Ki)*Zik(IP,Ki)/10**5) ;
 

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
         
         riskzone21s1,
         riskzone22s1,
         riskzone23s1,
         riskzone24s1,
         
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


