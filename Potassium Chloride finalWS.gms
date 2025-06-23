*Material selection for equipment items model ==========================================================================================================

set
    i       equipment i   /i1*i20/
    k       floor   /1,2,3/   
    s       candidate area /s1*s7/
    kt(k)   first floor /1/
    sec     section /sec1, sec2, sec3/
    te      material /te1*te7/
    typ     type of equipment /typ1*typ5/
    st      type of structure /st1, st2/ ;
    
Alias (i,j);
    
Alias (sec,secx);
    
parameter
    vol(i)          volume of equipment(m^3)
    vols(i)         volume of equipment for storage(m^3)
    p(te)           density of material(kg per m^3) /te1 7300, te2 7500, te3 8900, te4 4540, te5 7870, te6 7850, te7 8000/
    denf(i)         density of fluid inside equipment(kg per m^3)/i1 1029.73, i2 1080.5, i3 1222.57, i4 1222.7, i5 990.534, i6 1281.75, i7 873.514, i8 1.265, i9 81.373, i10 1.185, i11 0.645, i12 1.278, i13 1209.247, i14*i20 161.504/ 
    massf(i)        mass of fluid inside equipment (kg)
    group(i,sec)    set of section with equipment;
           
parameter
        alpha(i)   width of equipments i (metre)
          / i1   3.8
            i2   1.890
            i3   0.31
            i4   2
            i5   2.4
            i6   2.3
            i7   4
            i8   2.2
            i9   0.8        
            i10  0.935
            i11  7.2       
            i12  0.7     
            i13  4.4
            i14  6               
            i15  1.1               
            i16  4.9            
            i17  2.2          
            i18  6.5              
            i19  2.2               
            i20  4.9          /
            
        beta(i)  length of equipment i (m)
          / i1   3.8
            i2   3.035
            i3   0.75
            i4   7
            i5   2.5
            i6   6.8
            i7   5.5
            i8   4.2
            i9   3.2       
            i10  2.02      
            i11  7.2       
            i12  0.7       
            i13  4.4               
            i14  6               
            i15  1.85              
            i16  6.3              
            i17  3.5             
            i18  6.5               
            i19  3.2               
            i20  6.2           /
            
        hita(i) height of equipment i (m)
          / i1   5.8
            i2   4.09
            i3   0.42
            i4   3
            i5   3.6
            i6   2.3
            i7   7
            i8   3.6
            i9   0.8         
            i10  1.1       
            i11  10       
            i12  2.6       
            i13  6.6               
            i14  9               
            i15  2.05              
            i16  3.1              
            i17  1.5              
            i18  9.7              
            i19  1.5              
            i20  4    /;
            
        
table group(i,sec) equipment in each groups
            sec1    sec2    sec3   
        i1   1
        i2   1
        i3   1
        i4   1             
        i5   1        
        i6   1        
        i7           1                
        i8           1       
        i9           1   
        i10          1
        i11          1
        i12          1
        i13                  1
        i14                  1
        i15                  1
        i16                  1 
        i17                  1
        i18                  1
        i19                  1
        i20                  1             ;
        

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
          Po(i)         operating pressure (psig) /i1 1, i2 1, i3 43.319, i4*i6 42.141, i7 1.984, i8 2.709, i9 1.984, i10 2.709, i11 1, i12 0.533, i13*i20 1.258/
          tp(i)         wall thickness to withstand the internal pressure (inches)
          str           maximum allowable stress (psi) /15000/
          weld          fractional weld efficiency /1/
          hor(i)        1 indicated that i is horizontal vessel /i6 1/
          orr(i)        1 indicated that i is vertical vessel /i1 1, i5 1, i11 1, i13*i14 1, i18 1/
          HoAndOrr(i)   1 indicated that i is horizontal and vertical vessel /i1 1, i5*i6 1, i11 1, i13*i14 1, i18 1/
          comp(i)       1 indicated that i is compressor /i10 1/
          pump(i)       1 indicated that i is pump /i3 1/
          heatex(i)     1 indicated that i is heat exchanger /i9 1/
          heater(i)     1 indicated that i is heater /i8 1/
          screener1(i)  1 indicated that i is screener 1 deck /i16 1/
          screener2(i)  1 indicated that i is screener 2 decks /i20 1/
          dry(i)        1 indicated that i is dryer /i7 1/
          cyclone(i)    1 indicated that i is cyclones /i12 1/
          hydro(i)      1 indicated that i is hydroclones /i4 1/
          hammer(i)     1 indicated that i is hammer mills /i17 1, i19 1/
          crystal(i)    1 indicated that i is crystallizer /i2 1/
          granulator(i) 1 indicated that i is roll presses /i15 1/
          cpl(i)        cost for platform
          fd            electric motor drive for compressors /1.25/
          pc(i)         horsepower(Hp) of compressor /i10 441.102/
          cb(i)         base cost of compressors
          sp(i)         s factor for pump
          flow(i)       flowrate(gal per minute) of pump /i3 349.966/
          head(i)       pump head (ft) of pump /i3 118.662/
          cbp(i)        base pump cost
          ft            factor for centrifugal pump /1.70/
          pcc(i)        horse power for pump
          pb(i)         pump brake horsepower of pump (hp) /i3 13.002/
          cbm(i)        base cost of motor for pump
          cpm(i)        motor cost for pump
          fym           factor for motor(open drip-proof enclosure) /1/
          cbh(i)        base cost of double pipe heat ex
          ah(i)         area of heat ex (sq ft) and screener /i9 290.322, i16 60, i20 192/
          ev(i)         evaporation rate of spray dryer(lb per hr)/i7 330.693/
          fp(i)         factor for heat ex
          cbf(i)        base cost of fired heater
          ener(i)       heat duty of fired heater(btu per hour) /i8 17378800/   
          fpf(i)        pressure factor for fired heater
          cpd(i)        cost of spray dryer ($ per 10^5)
          cpc(i)        cost of cyclone ($ per 10^5)
          chc(i)        cost of hydrocyclone ($ per 10^5)
          gasflow(i)    gas flowrate(ft^3 per min) of cyclone /i12 37912.05/
          liqrate(i)    liquid feed rate (gal per min) of hydrocyclone /i4 15.263/
          cpg(i)        cost of hammer mill crushers ($ per 10^5)      
          sfeed(i)      mass feed rate(ton per hr) of crusher /i17 65.670, i19 30.397/
          cps1(i)       cost for vibrating screens 1 desk ($ per 10^5)
          cps2(i)       cost for vibrating screens 2 deshs ($ per 10^5)
          cpcr(i)       cost for crystallizeer ($ per 10^5)
          volume(i)     volume of crystallizer (ft^3) /i2 88.287/
          cpgr(i)       cost for granulator ($ per 10^5)
          sflow(i)      wet solids flow rate of roll presses granulator (lb per hr) /i15 275637.314/ 
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


*horizontal vessel platform

cpl(i)              = 2005*(alpha(i)*3.281)**0.20294;

*vertical vessel platform

cpl(i)$orr(i)       = 361.8*((alpha(i)*3.281)**0.7396)*((hita(i)*3.281)**0.70684);

display Pd, tp, cpl;

* cost for compressors (centrifugal type)

cb(i)$comp(i) = exp(7.58+0.8*log(pc(i)));

display cb;

*cost for centrifugal pump

sp(i)$pump(i)       = flow(i)*head(i)**0.5;

cbp(i)$pump(i)      = exp(9.7171-(0.6019*log(sp(i)))+(0.0519*(log(sp(i)))**2));

pcc(i)$pump(i)      = pb(i)/(0.8+0.0319*log(pb(i))-(0.00182*log(pb(i))**2));

cbm(i)$pump(i)      = exp(5.8259+0.13141*log(pcc(i))+(0.053255*(log(pcc(i)))**2)+(0.028628*(log(pcc(i)))**3)-(0.0035549*(log(pcc(i)))**4));

cpm(i)$pump(i)      = fym*cbm(i)*821.1/500;

display cbp, pcc, cbm, cpm;

*cost for heat ex

cbh(i)$heatex(i)    = exp(7.146+0.16*log(ah(i)));

fp(i)$heatex(i)     = 0.851+0.1292*(Po(i)/600)+0.0198*(Po(i)/600)**2;

display cbh, fp;

*cost for fired heater

cbf(i)$heater(i)    = exp(0.32325+0.766*log(ener(i)));

fpf(i)$heater(i)    = 0.986-0.0035*(Po(i)/500)+0.0175*(Po(i)/500)**2;

display cbf, fpf;

*cost for vibrating screens 1 desk (carbon steel is used in this eq)

cps1(i)$screener1(i)  = ((1400*ah(i)**0.71)*821.1/500)/10**5;

display cps1;

*cost for vibrating screens 2 desks

cps2(i)$screener2(i)  = ((1230*ah(i)**0.78)*821.1/500)/10**5;

display cps2;

*cost for dryer (stainless steel is used in this eq)

cpd(i)$dry(i)      = (exp(8.2938+(0.8526*log(ev(i)))-(0.0229*log(ev(i))**2))*821.1/500)/10**5;

display cpd;

*cost for cyclones (carbon steel is used in this eq)

cpc(i)$cyclone(i)   = (exp(9.2227-(0.7892*log(gasflow(i)))+(0.08487*log(gasflow(i))**2))*821.1/500)/10**5;

display cpc;

*cost for hydrocyclones 

chc(i)$hydro(i)   = ((240*(liqrate(i))**0.5)*821.1/500)/10**5;

display chc;

*cost for crushers (includes motor and drive, and did not specific material used)

cpg(i)$hammer(i)  = ((3800*sfeed(i)**0.78)*821.1/500)/10**5;

display cpg;

*cost for crystallizer

cpcr(i)$crystal(i)   = ((40900*(volume(i))**0.41)*821.1/500)/10**5;

display cpcr;

*cost for granulator

cpgr(i)$granulator(i)   = (exp(10.8549-(0.4467*log(sflow(i)))+(0.06136*log(sflow(i))**2))*821.1/500)/10**5;

display cpgr;

*========================================================================================================

positive variable
    mass(i)     mass of equipment i (kg)
    Tm(sec)     total mass of that section sec (kg)
    Tn(te)      total materials used
    cpv(i)      equipment cost of vessel ($ per 10^5)
    cpf(i)      cost for fired heater ($ per 10^5)
    ccomp(i)    cost of compressors ($ per 10^5)
    cpump(i)    total cost pump ($ per 10^5)
    cph(i)      cost for heat ex ($ per 10^5)
    w(i)        weight of equipment (lb)
    cvv(i)      cost for pressure vessel and tower for distillation absorption and stripping
    cpp(i)      pump cost exclude motor
    CP(i)       equipment cost ($ per 10^5)
    
Integer variable
    guess(sec)  first guess area for 7 sections(m^2);
    
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
    mat3        'material constaints'
    mat4        'material constaints'
    mat5        'material constaints'
    mat6        'material constaints'
    mat7        'material constaints'
    mat8        'material constaints'
    num         'number of materials used'
    totalmass   'section mass'
    test        'test av weight'
    test1       'test av weight'
    weighte     'weight of equipment'
    costv       'base cost of horizontal vessel'
    costhv      'base cost of vertical vessel'
    cvessel     'cost of vessel'
    cvessel2    'cost of vessel'
    cheat       'cost of heater'
    cheat2      'cost of heater'
    ccompress   'cost of compressors'
    ccompress2  'cost of compressors'
    cmotor      'cost of motor in pump'
    cmotor2     'cost of motor in pump'
    tcpump      'total cost of pump'
    cheatx      'cost of heat ex'
    cheatx2     'cost of heat ex'
    cequip      'cost of each equipments'
    obj         'objective function';
    
masc(i)                .. mass(i) =e= sum(te, p(te)*vol(i)*zz(i,te));

mat(i)                 .. sum(te, zz(i,te)) =e= 1;

mat1(i)$screener1(i)   .. sum(te$(ord(te)=6), zz(i,te)) =e= 1;
   
mat2(i)$dry(i)         .. sum(te$(ord(te)=7), zz(i,te)) =e= 1;

mat3(i)$cyclone(i)     .. sum(te$(ord(te)=6), zz(i,te)) =e= 1;

mat4(i)$hammer(i)      .. sum(te$(ord(te)=7), zz(i,te)) =e= 1;

mat5(i)$crystal(i)     .. sum(te$(ord(te)=7), zz(i,te)) =e= 1;

mat6(i)$screener2(i)   .. sum(te$(ord(te)=6), zz(i,te)) =e= 1;

mat7(i)$hydro(i)       .. sum(te$(ord(te)=7), zz(i,te)) =e= 1;

mat8(i)$granulator(i)  .. sum(te$(ord(te)=7), zz(i,te)) =e= 1;

num(te)                .. Tn(te) =e= sum(i, zz(i,te));

totalmass(sec)         .. Tm(sec) =e= sum(i$group(i,sec), mass(i));

test(sec)              .. Tm(sec) =l= sum(st, wd(st,sec)*capweight(st)*guess(sec))-sum(i$group(i,sec), massf(i));

test1(sec)             .. sum(st, wd(st,sec)) =e= 1;

* for vessel ===================================================================================================================================

weighte(i)$HoAndOrr(i)  .. w(i) =e= sum(te, 3.14*(alpha(i)+tp(i))*(hita(i)+0.8*alpha(i))*2.2046*tp(i)*p(te)*zz(i,te));

costv(i)$hor(i)     .. cvv(i) =e= exp(8.9552-0.233*log(w(i)+1)+(0.04333*(log(w(i)+1)**2)));

costhv(i)$orr(i)    .. cvv(i) =e= exp(7.0132+0.18255*log(w(i)+1)+(0.02297*(log(w(i)+1)**2)));

cvessel(i,typ)$((ord(typ)=5) and HoAndOrr(i))  .. cpv(i) =e= (sum(te$fm(te,typ), zz(i,te)*(fm(te,typ)*cvv(i)+cpl(i))*(821.1/500)))/10**5;

cvessel2(i,typ)$((ord(typ)=5) and HoAndOrr(i)) .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;  

* for heater ===================================================================================================================================

cheat(i,typ)$((ord(typ)=4) and heater(i))    .. cpf(i) =e= (sum(te$fm(te,typ), fpf(i)*fm(te,typ)*cbf(i)*zz(i,te)*821.1/500))/10**5;

cheat2(i,typ)$((ord(typ)=4) and heater(i))   .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;

* for compressors ===================================================================================================================================

ccompress(i,typ)$((ord(typ)=2) and comp(i))  .. ccomp(i) =e= (sum(te$fm(te,typ), fd*fm(te,typ)*cb(i)*zz(i,te)*821.1/500))/10**5;

ccompress2(i,typ)$((ord(typ)=2) and comp(i)) .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;

* for pump ===================================================================================================================================

cmotor(i,typ)$((ord(typ)=1) and pump(i))     .. cpp(i) =e= sum(te$fm(te,typ), ft*fm(te,typ)*cbp(i)*zz(i,te)*(821.1/500));

cmotor2(i,typ)$((ord(typ)=1) and pump(i))    .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;

tcpump(i)$pump(i)       .. cpump(i) =e= (cpp(i)+cpm(i))/10**5;

* for heat ex ===================================================================================================================================

cheatx(i,typ)$((ord(typ)=3) and heatex(i))   .. cph(i) =e= (sum(te$fm(te,typ), fp(i)*fm(te,typ)*cbh(i)*zz(i,te)*(821.1/500)))/10**5;

cheatx2(i,typ)$((ord(typ)=3) and heatex(i))  .. sum(te$fm(te,typ), zz(i,te)*fm(te,typ)/fm(te,typ)) =e= 1;


* for each equipments ===================================================================================================================================

cequip(i)           .. CP(i) =e= cpv(i)+cpf(i)+ccomp(i)+cpump(i)+cph(i)+cps1(i)+cps2(i)+cpd(i)+cpc(i)+cpg(i)+chc(i)+cpcr(i)+cpgr(i);

*====================================================================================================================================================

obj                 .. wei =e= sum(i, CP(i))+(sum((st,sec), wd(st,sec)*costst(st)*guess(sec))/10**5);

wei.lo=0;

option limrow = 500;

model weight /masc,
              mat,
              mat1,
              mat2,
              mat3,
              mat4,
              mat5,
              mat6,
              mat7,
              mat8,
              num,
              totalmass,
              test,
              test1,
              weighte,
              costv,
              costhv,
              cvessel,
              cvessel2,
              cheat,
              cheat2,
              ccompress,
              ccompress2,
              cmotor,
              cmotor2,
              tcpump,
              cheatx,
              cheatx2,
              cequip,
              obj/      ;

solve weight using minlp minimizing wei;

display zz.l, Tn.l, mass.l, Tm.l, wei.l, cvv.l, cpv.l, cpf.l, ccomp.l, cpump.l, cph.l;

parameter Mmax(sec) maximun mass of that section;

Mmax(sec)=smax(i$group(i,sec),mass.l(i)+massf(i));

display Mmax;


*Multi-floor process plant layout model =================================================================================================================================================================================================================================================================================================================================================================================================================================================================

Parameter wax(i,sec)    'test for heavy weight';


scalar Kn 'total of k';
          
Kn = card(k);

          
wax(i,sec)$((mass.l(i)+massf(i)=Mmax(sec))and(group(i,sec)))=1;

display wax;


table f(i,j) flow relationship between item i and j
            i1  i2  i3  i4  i5  i6  i7  i8  i9  i10 i11 i12 i13 i14 i15 i16 i17 i18 i19 i20 
    i1          1          
    i2              1                               
    i3                  1
    i4      1                   1
    i5      1                    
    i6                      1       
    i7                                      1                   
    i8                              1                                                                      
    i9                                  1               1                                                                          
    i10                                     1                                                                                  
    i11                                                                                         
    i12                                             1       
    i13                                                         1
    i14                                                             1
    i15                                                                 1
    i16                                                                     1   1
    i17                                                                         1
    i18                                                                                 1
    i19                                                                         1
    i20                                                         1                   1           ;                                                                                                                                

table Cc(i,j) connection costs ($ per ft)
            i1      i2      i3      i4      i5      i6      i7      i8      i9      i10     i11     i12     i13     i14     i15     i16     i17     i18     i19     i20               
    i1              181.97
    i2                      135.57                                                          4869.59
    i3                              135.56      
    i4      66.15                                   122.4                
    i5      71.98                      
    i6                                      98.33           537.5                                                 
    i7                                                                      2424.72                         537.5    
    i8                                                      2840.25
    i9                                                              2131.31                         2393.22
    i10                                                                     2081.53                     
    i11                                                                                                                                                                             
    i12                                                                                     2498.19         387
    i13                                                                                                             537.5                               
    i14                                                                                                                     1290                                                                                                                          
    i15                                                                                                                             1290
    i16                                                                                                                                     967.5   967.5            
    i17                                                                                                                                             537.5
    i18                                                                                                                                                             1290          
    i19                                                                                                                                             1096.5
    i20                                                                                                             967.5                                  537.5          ;                                                                                                                                


table Cv(i,j)   vertical cost ($ per m yr)
            i1      i2      i3      i4      i5      i6      i7      i8      i9      i10     i11     i12     i13     i14     i15     i16     i17     i18     i19     i20               
    i1              90963.5
    i2                      53511.6                                                         37451.9
    i3                              53511.6      
    i4      8802.36                                 44709.2                
    i5      10619.5                      
    i6                                      21238.9         23470.3                                                 
    i7                                                                      33003.4                         15716.9    
    i8                                                      25250
    i9                                                              25250                           33003.4
    i10                                                                     25250                     
    i11                                                                                                                                                                             
    i12                                                                                     28122.3         4881.12
    i13                                                                                                             20598                              
    i14                                                                                                                     52797.6                                                                                                                          
    i15                                                                                                                             52597.6
    i16                                                                                                                                     27636.3 25161.3            
    i17                                                                                                                                             27636.3
    i18                                                                                                                                                             65589.9          
    i19                                                                                                                                             12792.1
    i20                                                                                                             32199.6                                  12792.1          ;                                                                                                                                
                                                                                                  

parameter
    Ch(i,j)   horizontal cost ($ per m yr);
    
Ch(i,j) = Cv(i,j)/10;

display Ch;

parameter
    Xbar(s)         candidate x coordinate /s1 20, s2 30, s3 30, s4 30, s5 40, s6 40, s7 100/
    ybar(s)         candidate y coordinare /s1 35, s2 20, s3 40, s4 50, s5 55, s6 60, s7 80/
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
    mainx(i)        'maintanance distance x axis'
    mainy(i)        'maintanance distance y axis'
    ss              'upper bound of s';

mainx(i)$HoAndOrr(i)  = hita(i);
mainy(i)$HoAndOrr(i)  = hita(i);
mainx(i)$comp(i)      = 0.8;
mainy(i)$comp(i)      = 0.8;
mainx(i)$pump(i)      = 0.8;
mainy(i)$pump(i)      = 0.8;
mainx(i)$heatex(i)    = 0.5;
mainy(i)$heatex(i)    = beta(i)+1.5;
mainx(i)$heater(i)    = 3;
mainy(i)$heater(i)    = beta(i)+15;
mainx(i)$screener1(i) = 1;
mainy(i)$screener1(i) = 1;
mainx(i)$screener2(i) = 1;
mainy(i)$screener2(i) = 1;
mainx(i)$dry(i)       = 1;
mainy(i)$dry(i)       = 1;
mainx(i)$cyclone(i)   = 1;
mainy(i)$cyclone(i)   = 1;
mainx(i)$hydro(i)     = 1;
mainy(i)$hydro(i)     = 1;
mainx(i)$hammer(i)    = 1;
mainy(i)$hammer(i)    = 1;
mainx(i)$crystal(i)   = 1;
mainy(i)$crystal(i)   = 1;
mainx(i)$granulator(i)= 1;
mainy(i)$granulator(i)= 1;


table WS(i,j)   spacing for worker (m)
             i1      i2      i3      i4      i5      i6      i7      i8      i9     i10     i11     i12     i13     i14     i15     i16     i17     i18     i19     i20        
    i1               4.575   3.05    4.575   4.575   4.575   4.575   15.25   3.05   15.25   4.575   4.575   4.575   4.575                           4.575                             
    i2                       3.05    4.575   4.575   4.575   4.575   15.25   3.05   15.25   4.575   4.575   4.575   4.575                           4.575        
    i3                               3.05    3.05    3.05    3.05    15.25   3.05   9.15    3.05    3.05    3.05    3.05                            3.05                              
    i4                                       4.575   4.575   4.575   15.25   3.05   15.25   4.575   4.575   4.575   4.575                           4.575                                                                       
    i5                                               4.575   4.575   15.25   3.05   15.25   4.575   4.575   4.575   4.575                           4.575                                                                        
    i6                                                       4.575   15.25   3.05   15.25   4.575   4.575   4.575   4.575                           4.575                                                                                  
    i7                                                               15.25   3.05   15.25   4.575   4.575   4.575   4.575                           4.575                                                                                   
    i8                                                                       15.25  15.25   15.25   15.25   15.25   15.25                           15.25                       
    i9                                                                              9.15    3.05    3.05    3.05    3.05                            3.05
    i10                                                                                     15.25   15.25   15.25   15.25                           15.25
    i11                                                                                             4.575   4.575   4.575                           4.575
    i12                                                                                                     4.575   4.575                           4.575
    i13                                                                                                             4.575                           4.575
    i14                                                                                                                                             4.575
    i15
    i16
    i17
    i18
    i19
    i20                                 ;


M(sec)  = max(sum( i, (alpha(i)+mainx(i))*ord(sec)), sum( i, (beta(i)+mainy(i))*ord(sec)),sum((i,j), ((alpha(i)/2)+WS(i,j))*ord(sec)),sum((i,j), ((beta(i)/2)+WS(i,j))*ord(sec)));

display mainx, mainy, M;

        
Positive variable
        Ve(i)           'value of the area of explosure of i'
        Din(i,j,sec)    'total rectilinear distance between items i and j if Din is smaller than Dei'
        Dout(i,j,sec)   'total rectilinear distance between items i and j if Din is larger than Dei';
        
Binary variable
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
         Objls1           objective function linear;
         
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

Areals1(sec)$(ord(sec)=1)  .. FA(sec) =e= sum( s, AR(s)*QS(s,sec));

Areals11(sec)$(ord(sec)=1) .. FA(sec)*NF(sec) =g= guess.l(sec);

Qcons1(sec)$(ord(sec)=1)   .. sum( s, QS(s,sec)) =e= 1;

Xcons1(sec)$(ord(sec)=1)   .. Xmax(sec) =e= sum( s, Xbar(s)*QS(s,sec));

Ycons1(sec)$(ord(sec)=1)   .. Ymax(sec) =e= sum( s, Ybar(s)*QS(s,sec));

news1(s,sec)$(ord(sec)=1)  .. NQ(s,sec) =l= Kn*QS(s,sec);

new1s1(sec)$(ord(sec)=1)   .. NF(sec) =e= sum( s, NQ(s,sec));

Objls1        .. Qs1 =e= sum( (i,j,sec)$((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=1) and f(i,j)), (( Cc(i,j)*TD(i,j,sec)/0.3048)+( Cv(i,j)*De(i,j,sec) )+( Ch(i,j)*(R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec))))/10**5 )+(sum( (s,sec)$(ord(sec)=1), AR(s)*NQ(s,sec)*FC2(sec))/10**5)+(LC*sum(sec$(ord(sec)=1), FA(sec))/10**5) ;

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
         Objls2           objective function linear;
         
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

Areals2(sec)$(ord(sec)=2)  .. FA(sec) =e= sum( s, AR(s)*QS(s,sec));

Areals21(sec)$(ord(sec)=2) .. FA(sec)*NF(sec) =g= guess.l(sec);

Qcons2(sec)$(ord(sec)=2)   .. sum( s, QS(s,sec)) =e= 1;

Xcons2(sec)$(ord(sec)=2)   .. Xmax(sec) =e= sum( s, Xbar(s)*QS(s,sec));

Ycons2(sec)$(ord(sec)=2)   .. Ymax(sec) =e= sum( s, Ybar(s)*QS(s,sec));

news2(s,sec)$(ord(sec)=2)  .. NQ(s,sec) =l= Kn*QS(s,sec);

new1s2(sec)$(ord(sec)=2)   .. NF(sec) =e= sum( s, NQ(s,sec));

Objls2        .. Qs2 =e= sum( (i,j,sec)$((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=2) and f(i,j)), (( Cc(i,j)*TD(i,j,sec)/0.3048)+( Cv(i,j)*De(i,j,sec) )+( Ch(i,j)*(R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec))))/10**5 )+(sum((s,sec)$(ord(sec)=2), AR(s)*NQ(s,sec)*FC2(sec))/10**5)+(LC*sum(sec$(ord(sec)=2), FA(sec))/10**5);

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
         Objls2/;

option limrow = 1000;

solve linearitys2 using minlp minimizing Qs2;

display x.l, y.l, l.l, d.l, V.l, O.l, Qs2.l, R.l, Le.l, A.l, B.l, TD.l ;

*sec 3 =======================================================================================================
        
equations
         Floors3          Should be assigned to one floor 
         Floorrs3         weightest equipment should be assigned to the first floor
         Floor1s3         floor constraints value of z 
         Floor2s3         floor constraints value of z 
         Floor3s3         floor constraints value of z 
         Numfloors3       number of floor 
         Orient1s3        equipment orientation constraints length 
         Orient2s3        equipment orientation constraints depth 
         maintenance1s3   maintenance spacing required
         maintenance2s3   maintenance spacing required
         Non1s3           non overlapping constraints 
         Non2s3           non overlapping constraints 
         Non3s3           non overlapping constraints 
         Non4s3           non overlapping constraints
         
         riskzone1s3      worker spacing constrains
         riskzone2s3      worker spacing constrains
         riskzone3s3      worker spacing constrains
         riskzone4s3      worker spacing constrains
         

         Distance1s3      distance constraint 
         Distance2s3      distance constraint 
         Distance3s3      distance constraint 
         Distance4s3      distance constraint 
         distance11s3     absolute distance constraints
         distance12s3     absolute distance constraints 
         distance13s3     absolute distance constraints
         distance14s3     absolute distance constraints 
         distance15ps3    absolute distance constraints
         distance16ps3    absolute distance constraints
         Coordinate1s3    coordinate constaints 
         Coordinate2s3    coordinate constaints 
         Coordinate3s3    coordinate constaints 
         Coordinate4s3    coordinate constaints 
         Areals3          plant area linearity 
         Areals31         lowerbound of section area
         Qcons3           QS constraint 
         Xcons3           x constaint 
         Ycons3           y constaint 
         news3            linearization constraints 
         new1s3           linearization constraints 
         Objls3           objective function linear;
         
Floors3(i,sec)$((group(i,sec))and(mass.l(i)+massf(i)<Mmax(sec))and(ord(sec)=3)) ..    sum(k, V(i,sec,k)) =e= 1;

Floorrs3(kt,sec)$((ord(sec)=3)) .. sum(i$(group(i,sec)and(mass.l(i)+massf(i)=Mmax(sec))), V(i,sec,kt)) =g= sum(i$(group(i,sec)),wax(i,sec));

Floor1s3(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=3)) ..   Z(i,j,sec) =g= V(i,sec,k)+V(j,sec,k)-1;

Floor2s3(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=3)) ..    Z(i,j,sec) =l= 1-V(i,sec,k)+V(j,sec,k);

Floor3s3(i,j,sec,k) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec))and(ord(sec)=3)) ..    Z(i,j,sec) =l= 1+V(i,sec,k)-V(j,sec,k);

Numfloors3(i,sec)$(group(i,sec)and(ord(sec)=3)) .. NF(sec) =g= sum( k, ord(k)*V(i,sec,k) );

Orient1s3(i,sec)$(group(i,sec) and (ord(sec)=3))      .. l(i) =e= (alpha(i)*O(i))+(beta(i)*(1-O(i)));

Orient2s3(i,sec)$(group(i,sec) and (ord(sec)=3))      .. d(i) =e= alpha(i)+beta(i)-l(i);

maintenance1s3(i,sec)$(group(i,sec) and (ord(sec)=3)) .. vmainx(i) =e= (mainx(i)*O(i))+(mainy(i)*(1-O(i)));

maintenance2s3(i,sec)$(group(i,sec) and (ord(sec)=3)) .. vmainy(i) =e= mainx(i)+mainy(i)-vmainx(i);

Non1s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3))       .. x(i,sec)-x(j,sec)+(M(sec)*(1-Z(i,j,sec)+Ea(i,j)+Eb(i,j))) =g= (( l(i)+l(j) )/2)+(vmainx(i)+vmainx(j));

Non2s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3))       .. x(j,sec)-x(i,sec)+(M(sec)*(2-Z(i,j,sec)-Ea(i,j)+Eb(i,j))) =g= (( l(i)+l(j) )/2)+(vmainx(i)+vmainx(j));

Non3s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3))       .. y(i,sec)-y(j,sec)+(M(sec)*(2-Z(i,j,sec)+Ea(i,j)-Eb(i,j))) =g= (( d(i)+d(j) )/2)+(vmainy(i)+vmainy(j));

Non4s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3))       .. y(j,sec)-y(i,sec)+(M(sec)*(3-Z(i,j,sec)-Ea(i,j)-Eb(i,j))) =g= (( d(i)+d(j) )/2)+(vmainy(i)+vmainy(j));


riskzone1s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3))       .. x(i,sec)-x(j,sec)+(M(sec)*(1-Z(i,j,sec)+Ea(i,j)+Eb(i,j))) =g= WS(i,j)+(l(j)/2);

riskzone2s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3))       .. x(j,sec)-x(i,sec)+(M(sec)*(2-Z(i,j,sec)-Ea(i,j)+Eb(i,j))) =g= WS(i,j)+(l(j)/2);

riskzone3s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3))       .. y(i,sec)-y(j,sec)+(M(sec)*(2-Z(i,j,sec)+Ea(i,j)-Eb(i,j))) =g= WS(i,j)+(d(j)/2);

riskzone4s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3))       .. y(j,sec)-y(i,sec)+(M(sec)*(3-Z(i,j,sec)-Ea(i,j)-Eb(i,j))) =g= WS(i,j)+(d(j)/2);



Distance1s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3) and f(i,j))  .. R(i,j,sec)-Le(i,j,sec) =e= x(i,sec)-x(j,sec);

Distance2s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3) and f(i,j))  .. A(i,j,sec)-B(i,j,sec) =e= y(i,sec)-y(j,sec);

Distance3s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3) and f(i,j))  .. U(i,j,sec)-De(i,j,sec) =e= H(sec)*sum( k, ord(k)*(V(i,sec,k)-V(j,sec,k)));

Distance4s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3) and f(i,j))  .. TD(i,j,sec) =e= R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec)+U(i,j,sec)+De(i,j,sec);

distance11s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3)) .. R(i,j,sec) =l= M(sec)*Wx(i,j);

distance12s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3)) .. Le(i,j,sec) =l= M(sec)*(1-Wx(i,j));

distance13s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3)) .. A(i,j,sec) =l= M(sec)*Wy(i,j);

distance14s3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3)) .. B(i,j,sec) =l= M(sec)*(1-Wy(i,j));

distance15ps3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3)).. U(i,j,sec) =l= M(sec)*Wz(i,j);

distance16ps3(i,j,sec) $((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3)).. De(i,j,sec) =l= M(sec)*(1-Wz(i,j));

Coordinate1s3(i,sec)$(group(i,sec)and(ord(sec)=3))   .. x(i,sec) =g= (l(i)/2)+1;

Coordinate2s3(i,sec)$(group(i,sec)and(ord(sec)=3))   .. y(i,sec) =g= (d(i)/2)+vmainy(i);

Coordinate3s3(i,sec)$(group(i,sec)and(ord(sec)=3))   .. x(i,sec)+(l(i)/2)+vmainx(i) =l= Xmax(sec);

Coordinate4s3(i,sec)$(group(i,sec)and(ord(sec)=3))   .. y(i,sec)+(d(i)/2)+vmainy(i) =l= Ymax(sec);

Areals3(sec)$(ord(sec)=3)  .. FA(sec) =e= sum( s, AR(s)*QS(s,sec));

Areals31(sec)$(ord(sec)=3) .. FA(sec)*NF(sec) =g= guess.l(sec);

Qcons3(sec)$(ord(sec)=3)   .. sum( s, QS(s,sec)) =e= 1;

Xcons3(sec)$(ord(sec)=3)   .. Xmax(sec) =e= sum( s, Xbar(s)*QS(s,sec));

Ycons3(sec)$(ord(sec)=3)   .. Ymax(sec) =e= sum( s, Ybar(s)*QS(s,sec));

news3(s,sec)$(ord(sec)=3)  .. NQ(s,sec) =l= Kn*QS(s,sec);

new1s3(sec)$(ord(sec)=3)   .. NF(sec) =e= sum( s, NQ(s,sec));

Objls3        .. Qs3 =e= sum( (i,j,sec)$((ord(i)<>ord(j)) and (group(i,sec)) and (group(j,sec)) and (ord(sec)=3) and f(i,j)), (( Cc(i,j)*TD(i,j,sec)/0.3048)+( Cv(i,j)*De(i,j,sec) )+( Ch(i,j)*(R(i,j,sec)+Le(i,j,sec)+A(i,j,sec)+B(i,j,sec))))/10**5 )+(sum( (s,sec)$(ord(sec)=3), AR(s)*NQ(s,sec)*FC2(sec))/10**5)+(LC*sum(sec$(ord(sec)=3), FA(sec))/10**5) ;

Qs3.lo=0;

Model linearitys3      
        /Floors3,
         Floorrs3,
         Floor1s3,
         Floor2s3,
         Floor3s3,
         Numfloors3,
         Orient1s3,
         Orient2s3,
         maintenance1s3,
         maintenance2s3,
         Non1s3,
         Non2s3,
         Non3s3,
         Non4s3,
         
         riskzone1s3,
         riskzone2s3,
         riskzone3s3,
         riskzone4s3,
         

         Distance1s3,
         Distance2s3,
         Distance3s3,
         Distance4s3,
         distance11s3,
         distance12s3,
         distance13s3,
         distance14s3,
         distance15ps3,
         distance16ps3,
         Coordinate1s3,
         Coordinate2s3,
         Coordinate3s3,
         Coordinate4s3,
         Areals3,
         Areals31,
         Qcons3,
         Xcons3,
         Ycons3,
         news3,
         new1s3,
         Objls3/;

option limrow = 1000;

solve linearitys3 using minlp minimizing Qs3;


display x.l, y.l, l.l, d.l, V.l, O.l, Qs1.l, Xmax.l, Ymax.l, FA.l ;



*Layout of three sections objective model  ===========================================================================================================================

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
