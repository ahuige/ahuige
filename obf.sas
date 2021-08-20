%macro ahg0(m,str);
%*<spec> like ksh cmd: if missing(&m) then return &str </spec>;
    %if %AHGblank(&m) %then &str;
%mend;
%macro ahg1(m,str);
%*<spec> like ksh cmd: if not missing(&m) then return &str </spec>;
    %if not %AHGblank(&m) %then &str;
%mend;
%macro AHG2arr(prefix);
%*<spec> create dummy macro array xxx1..xxx99 and xxx_N </spec>;
  data _null_;
    infile datalines truncover;
    format line $1000.;
    input line 1-1000;
    call symput(compress("&prefix"||put(_n_,best.)),TRIM(line));
    call symput(compress("&prefix._N"),left(put(_n_,best.)));
%mend;
%macro AHGaddBase(all,base);
%*<spec> from 1 4 5 7 by adding base to like 11 14 15 17 </spec>;
%local _ahg_ahuige_303063 ;
  %do _ahg_ahuige_303063=1 %to %AHGcount(&all);
    %eval(%scan(&all,&_ahg_ahuige_303063)+&base)
  %end;
%mend;
%macro AHGaddcomma(mac,comma=%str(,) );
%*<spec> add comma or other sep into a mac var like abc xyz to abc,xyz </spec>;

%if %AHGnonblank(&mac) %then %sysfunc(tranwrd(     %sysfunc(compbl(&mac)),%str( ),&comma       ))   ;
%mend;
%macro AHGaddsasautos(dir,clear=0);
%local _ahg_ahuige_166171 ;
%let _ahg_ahuige_166171=%sysfunc(getoption(sasautos));
%if not %index(&dir,%str(%()) %then %let _ahg_ahuige_166171=(&_ahg_ahuige_166171);
%if not %index(&dir,%str(%'))  and not %index(&dir,%str(%"))  %then %let dir="&dir";
%let _ahg_ahuige_166171=&dir %substr(&_ahg_ahuige_166171,2,%eval(%length(&_ahg_ahuige_166171)-2));
%if &clear %then option sasautos=(&dir);
%else option sasautos=(&_ahg_ahuige_166171);
;

proc options option=sasautos;run;
%mend;

%macro AHGaddwords(sentence,words,dlm=%str( ));
%*<spec> add more words to sentence without dup </spec>;

  %AHGremoveWords(&sentence,&words,dlm=&dlm)&dlm&words
  
%mend;


%macro AHGallchar(dsn,into=);
%local _ahg_ahuige_785756 
;
%ahggettempname(_ahg_ahuige_785756);
data _null_;deletefromithere=1;run;
%AHGvarinfo(&dsn,out=&_ahg_ahuige_785756,info= name  type );

data &_ahg_ahuige_785756;
  set &_ahg_ahuige_785756(where=(type='C'));
run;

%AHGdistinctValue(&_ahg_ahuige_785756,name,into=&into,dlm=%str( ));


%mend;
%macro AHGallnum(dsn,into=);

%local _ahg_ahuige_591715 
;
%ahggettempname(_ahg_ahuige_591715);
%AHGvarinfo(&dsn,out=&_ahg_ahuige_591715,info= name  type );

data &_ahg_ahuige_591715;
  set &_ahg_ahuige_591715(where=(type='N'));
run;

%AHGdistinctValue(&_ahg_ahuige_591715,name,into=&into,dlm=%str( ));



%mend;
%macro AHGalltocharnew(dsn,out=%AHGbasename(&dsn),rename=,zero=0,width=100,name=0);
option mprint;
%local _ahg_huigea_8657 _ahg_ahuige_8657 _ahg_uigeah_8657 _ahg_igeahu_8657 _ahg_geahui_8657 %AHGwords(cmd,100)
;
%ahggettempname(_ahg_geahui_8657);
 
%AHGvarinfo(&dsn,out=&_ahg_geahui_8657,info= name  type  length num);
 
data _null_&_ahg_geahui_8657;
  set &_ahg_geahui_8657;
  format cmd $200.;
  if type='N' then cmd='input(left(put('||name||',best.)),$'||"&width"||'.) as '||name;
  else 
    do;
    if num>=&width then cmd=name;
    else cmd='put('||name||',$'||"&width"||'.) as '||name;
    end;
  call symput('cmd'||%AHGputn(_n_),cmd);
  call symput('_ahg_igeahu_8657',%AHGputn(_n_));
run;
option mprint;
 
proc sql noprint;
  create table &_ahg_geahui_8657(drop= AHGdrop) as
  select ' ' as AHGdrop 
    %do _ahg_huigea_8657=1 %to &_ahg_igeahu_8657;
%local _ahg___ahuig_8657 ;
    %if &zero %then %let _ahg___ahuig_8657=%AHGzero(&_ahg_huigea_8657,z&zero.);
    %else %let _ahg___ahuig_8657=&_ahg_huigea_8657;
  ,&&cmd&_ahg_huigea_8657 %if not %AHGblank(&rename) %then as &rename&_ahg___ahuig_8657;
  %end;
  from &dsn
  ;quit;

%AHGrenamedsn(&_ahg_geahui_8657,&out);

%if &name %then
%do;
  data &out;
    set &out;
    array _allchar_ _character_;
    if _n_=1 then 
      do;
      do over _allchar_;
        _allchar_=vname(_allchar_);
      end;
      output;
      set &out;
      end;
    output;
  run;
%end;
option mprint;

%mend;
%macro AHGalltochar(dsn,out=%AHGbasename(&dsn),prefix=gha);
%local _ahg_igeahu_561036 _ahg_ahuige_561036 
;
%ahggettempname(_ahg_ahuige_561036);
data &_ahg_ahuige_561036;
  set sashelp.vcolumn(where=(
    %AHGequaltext(libname,"%AHGlibname(&dsn)")
    and  %AHGequaltext(memname,"%AHGbasename(&dsn)")
    and %AHGequaltext(type,'num')
    )
);
%local _ahg_geahui_561036 _ahg_uigeah_561036 _ahg_huigea_561036 ;
data &_ahg_ahuige_561036;
  format putcmd renamecmd dropcmd $1000. ;
  retain putcmd renamecmd dropcmd ' ';
  set &_ahg_ahuige_561036 end=end;
  putcmd=trim(putcmd)||' '||trim(name)||'='|| "put(" ||"&prefix"||trim(name)||',best8.);';
  renamecmd=trim(renamecmd)||' '||trim(name)||'='|| "&prefix"||trim(name);
  dropcmd=trim(dropcmd)||" &prefix"||trim(name);
  if end then 
  do;
  call symput('_ahg_geahui_561036',putcmd);
  call symput('_ahg_uigeah_561036',renamecmd);
  call symput('_ahg_huigea_561036',dropcmd);
  end;
run;
%*pm(putcmd renamecmd dropcmd);


data &out(drop=&_ahg_huigea_561036);
  set  &dsn(rename=(&_ahg_uigeah_561036));
  %unquote(&_ahg_geahui_561036);
run;

%exit:
%mend;




%macro AHGalltonum(dsn,out=%AHGbasename(&dsn),rename=,zero=0,width=100);
%local _ahg_huigea_342111 _ahg_ahuige_342111 _ahg_uigeah_342111 _ahg_igeahu_342111 _ahg_geahui_342111 %AHGwords(cmd,100)
;
%ahggettempname(_ahg_geahui_342111);
%AHGvarinfo(&dsn,out=&_ahg_geahui_342111,info= name  type  length num);
data _null_;
  set &_ahg_geahui_342111;
  format cmd $200.;
  if type='C' then cmd='input( '||name||',best. ) as '||name;
  else cmd=name ;
    call symput('cmd'||left(_n_),cmd);
  call symput('_ahg_igeahu_342111',_n_);
run;

%AHGdatadelete(data=&_ahg_geahui_342111);

proc sql noprint;
  create table &out(drop= AHGdrop) as
  select ' ' as AHGdrop 
    %do _ahg_huigea_342111=1 %to &_ahg_igeahu_342111;
%local _ahg___ahuig_342111 ;
    %if &zero %then %let _ahg___ahuig_342111=%AHGzero(&_ahg_huigea_342111,z&zero.);
    %else %let _ahg___ahuig_342111=&_ahg_huigea_342111;
  ,&&cmd&_ahg_huigea_342111 %if not %AHGblank(&rename) %then as &rename&_ahg___ahuig_342111;
  %end;
  from &dsn
  ;quit;

%mend;




%macro AHGamp(myMAC);
&&&mymac
%mend;
%macro AHGanySlash(dir,toSlash,compress=1);    
%local _ahg_ahuige_651273 ;
%if %AHGblank(&toslash) %then 
%do;
%if %index(&dir,/) %then %do;%let _ahg_ahuige_651273=%str(/); %let toslash=\; %end;
%if %index(&dir,\) %then %do;%let _ahg_ahuige_651273=\; %let toslash=%str(/); %end;

%end;


%if &toslash=\ %then %let _ahg_ahuige_651273=%str(/);
%else %let _ahg_ahuige_651273=%str(\);
%if not &compress %then %sysfunc(tranwrd(&dir,&_ahg_ahuige_651273,&toslash));
%else %sysfunc(compress(%sysfunc(tranwrd(&dir,&_ahg_ahuige_651273,&toslash))));

%mend;

%macro ahgarr(id,dlm,Arr=ahgarr);
  %if %AHGblank(&dlm) %then %let dlm=@;
  %scan(&&&arr,&id, &dlm)
%mend;
%macro AHGbareName(dsn);
  %ahgbasename(%ahgpurename(&dsn))
%mend;
%macro AHGbasename(dsn);
  %if %index(&dsn,.) %then %scan(&dsn,2,%str(.%());
  %else %scan(&dsn,1,%str(.%());
%mend;
%macro AHGblank(string);
  %if %length(%bquote(&string)) %then 0 ;
  %else 1;
%mend;
%macro AHGbody(dsn,html,open=0);
%if %AHGblank(&html)  %then %let html=%AHGtempdir%ahgdelimit%AHGrdm.html;
data _null_;
  file "&html";
  if _n_=1 then put "<html><body>";
  set &dsn end=myend;
  put line;
  if myend then put '</body></html>';
run;
%if &open %then x "start  &html ";;
%mend;

%macro AHGbool(vars);
%local _ahg_ahuige_301161 _ahg_huigea_301161 ;
 1  
%do _ahg_huigea_301161=1 %to %AHGcount(&vars);
%let _ahg_ahuige_301161=%scan(&vars,&_ahg_huigea_301161);
and 
 (catx('',&_ahg_ahuige_301161)>'' and compress(catx('',&_ahg_ahuige_301161)) not in ('0','.'))
%end; 
%mend;
%macro AHGcatchprx(dsn,prxptn,out=,open=1,compact=0,lazy=1);
 
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn)ct;
data &out(label="&dsn Temp dataset");
  set &dsn;
    array allchar _character_;
    do over allchar;
    if prxmatch(&prxptn,allchar) then 
    do;    
      %if &compact %then 
      %do;
      keep dsn_____ findn_____ findvar_____ findstr_____;
      dsn_____=put("&dsn",$25.);
      findn_____=_n_;
      findvar_____=put(vname(allchar),$25.);
      findstr_____=put(allchar,$200.);
      %end; 
    output;%if &lazy and not &compact %then return;; end;
    end;



run;

%if &open %then %AHGopendsn(&out,justopen=1);


%mend;
%macro AHGcatch(dsn,value,out=,strict=1,open=1,justopen=0,compact=0,lazy=1);
%local _ahg_ahuige_179761 ;
%if %bquote(%substr(%bquote(&value),1,1))=%str(%')
or %bquote(%substr(%bquote(&value),1,1))=%str(%") %then %let _ahg_ahuige_179761=char;
%else %let _ahg_ahuige_179761=num;
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn)ct;
data &out(label="&dsn Temp dataset");
  set &dsn;
  %if &_ahg_ahuige_179761=char %then
    %do;
    array allchar _character_;
    do over allchar;
    if
    %if &strict %then  upcase(allchar)=%upcase(&value);

    %else index(upcase(allchar),%upcase(&value));
    then do;    
      %if &compact %then 
      %do;
      keep dsn_____ findn_____ findvar_____ findstr_____;
      dsn_____=put("&dsn",$25.);
      findn_____=_n_;
      findvar_____=put(vname(allchar),$25.);
      findstr_____=put(allchar,$200.);
      %end; 
    output;%if &lazy and not &compact %then return;; end;
    end;
    %end;
  %else
    %do;
    array allnum _numeric_;
    do over allnum;
    if allnum=&value then do;
    %if &compact %then 
      %do;
      keep dsn_____ findn_____ findvar_____ findnum_____;
      dsn_____=put("&dsn",$25.);
      findn_____=_n_;
      findvar_____=put(vname(allnum),$25.);
      findnum_____= allnum;
      %end; 
    output;%if &lazy and not &compact %then return;; end;
    end;
    %end;

run;

%if &open %then %AHGopendsn(&out,justopen=&justopen);


%mend;
%macro AHGcharToNum(dsn,vars,out=);
  %if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%local _ahg_huigea_76416 _ahg_ahuige_76416 ;
  %let _ahg_huigea_76416=_;
  data &out;
    set &dsn;
    %do _ahg_ahuige_76416=1 %to %AHGcount(&vars);
    %scan(&vars,&_ahg_ahuige_76416)&_ahg_huigea_76416=input(%scan(&vars,&_ahg_ahuige_76416),best.);
    %end;

    drop
    %do _ahg_ahuige_76416=1 %to %AHGcount(&vars);
    %scan(&vars,&_ahg_ahuige_76416) 
    %end;
    ;
    rename 
    %do _ahg_ahuige_76416=1 %to %AHGcount(&vars);
    %scan(&vars,&_ahg_ahuige_76416)&_ahg_huigea_76416=%scan(&vars,&_ahg_ahuige_76416) 
    %end;
    ;
  run;
  
%mend;
%macro AHGclearglobalmac(begin=);
%local _ahg_ahuige_368389 _ahg_uigeah_368389 ;
%if %AHGblank(&begin) %then %let _ahg_uigeah_368389=0;
%else %let _ahg_uigeah_368389=%length(&begin);
%ahggettempname(_ahg_ahuige_368389);

  data &_ahg_ahuige_368389;
    set sashelp.vmacro(keep=name scope);
    where scope='GLOBAL' and (substr(upcase(name),1,&_ahg_uigeah_368389)=upcase("&begin") or %AHGblank(&begin));
  run;  
  
  

%local _ahg_huigea_368389 
;
    proc sql noprint;
    select '/* clear '||name||'*/'||' %symdel '|| name || '/NOWARN ;' into :_ahg_huigea_368389 separated by ' '
    from &_ahg_ahuige_368389
    ;
    quit;
    %PUT %NRBQUOTE(&_ahg_huigea_368389);
    &_ahg_huigea_368389;

%mend;
%macro AHGclearlog(opt);
  %if %AHGblank(&opt) %then %let opt=log lst tree;
  %if %AHGonwin %then
  %do;
    %if %AHGpos(&opt,log) %then dm "clear log";;
    %if %AHGpos(&opt,lst) %then dm "clear lst";;
    %if %AHGpos(&opt,tree) %then dm 'odsresults; clear';;
  %end;
%mend;

%macro AHGcolumn2Mac(dsn,mac,vars,global=0,dlm=%str( ),quote=);
  %if &global %then %global &mac;
%local _ahg_huigea_266768 _ahg_ahuige_266768 ;
  %let _ahg_ahuige_266768=sdksf4543534534;
 
  data _null_;
    format  &_ahg_ahuige_266768 $10000.;
    retain &_ahg_ahuige_266768 '';
    set &dsn end=end;
    %do _ahg_huigea_266768=1 %to %AHGcount(&vars);
    &_ahg_ahuige_266768=Trim(&_ahg_ahuige_266768)||"&dlm"||&quote(left(%scan(&vars,&_ahg_huigea_266768)));
    %end;

    if end then call symput("&mac",compbl(&_ahg_ahuige_266768));
  
  run;
 
%mend;
%macro AHGcopylib(inlib,tolib,exclude=,n=99999999999);
  %if %AHGblank(&tolib) %then %let tolib=work;
%local _ahg_huigea_694904 _ahg_ahuige_694904 _ahg_uigeah_694904 ;
  %AHGdsnInLib(lib=&inlib,list=alldsn,lv=1);
  %do _ahg_huigea_694904=1 %to %AHGcount(&_ahg_ahuige_694904)
;
  %let _ahg_uigeah_694904= %scan(&_ahg_ahuige_694904,&_ahg_huigea_694904,%str( ));
  %if not %sysfunc(indexw(%upcase(&exclude),%upcase(&_ahg_uigeah_694904))  ) %then
  %do;
  data &tolib..&_ahg_uigeah_694904;
    set  &inlib..&_ahg_uigeah_694904(obs=&n);
  run;
  %end;
  %end;

%mend;
%macro AHGcount(line,dlm=%str( ));
%local _ahg_ahuige_658965 _ahg_huigea_658965 ;
  %let _ahg_ahuige_658965=1;
  %do %until(&_ahg_huigea_658965=yes);
      %if  %qscan(%bquote(&line),&_ahg_ahuige_658965,&dlm) eq %str() %then
      %do;
      %let _ahg_huigea_658965=yes;
      %eval(&_ahg_ahuige_658965-1)
      %end;
    %else %let _ahg_ahuige_658965=%eval(&_ahg_ahuige_658965+1);
  %end;

%mend;
%macro  AHGcreateHashex(HashID,Pairs,dlm=%str( ),dlm2=%str( ));
%AHGclearglobalmac(begin=&hashid);
%local _ahg_huigea_66685 ;
%global &hashid.list;
%let &hashid.list=;

%if &dlm ne %str( ) or &dlm2 ne %str( ) %then
  %do _ahg_huigea_66685= 1 %to %AHGcount(&pairs,dlm=&dlm);
  %let &hashid.list=&&&hashid.list %AHGscan2(&pairs,&_ahg_huigea_66685,1,dlm=&dlm,dlm2=&dlm2);
%local _ahg_ahuige_66685 ;
  %let _ahg_ahuige_66685=&hashid&_ahg_huigea_66685;
  %global  &_ahg_ahuige_66685;
  %let &_ahg_ahuige_66685=%AHGscan2(&pairs,&_ahg_huigea_66685,2,dlm=&dlm,dlm2=&dlm2);
  %end;
%else
  %do;
%local _ahg_uigeah_66685 ;
    %let _ahg_uigeah_66685=&pairs;
    %let _ahg_huigea_66685=0;
    %do %while(not %AHGblank(&_ahg_uigeah_66685));
    %ahgincr(_ahg_huigea_66685);
%local _ahg_ahuige_66685 ;
    %let &hashid.list=&&&hashid.list %AHGleft(localpairs);
    %let _ahg_ahuige_66685= &hashid&_ahg_huigea_66685 ;
    %global &_ahg_ahuige_66685;
    %let &_ahg_ahuige_66685=%AHGleft(localpairs);
    %end;
  %end;

%mend;

%macro AHGdatadelete(lib = , data = );
  proc datasets 
    %if %length(&lib) %then %do; lib = &lib %end;
    %else %do; lib = work %end;
    %if not %length(&data) %then %do; kill %end;
    memtype = data nolist   nodetails
  ;
    %if %length(&data) %then %do; delete &data; %end;
  run;
  quit;
%mend ;

%macro AHGdatakeep(lib = , data = );
%*<spec> keep only  <data= in>  in <lib=> </spec>;

  proc datasets 
    %if %length(&lib) %then %do; lib = &lib %end;
    %else %do; lib = work %end;
    %if not %length(&data) %then %do; kill %end;
    memtype = data nolist
  ;
    %if %length(&data) %then %do; save &data; %end;
  run;
  quit;
%mend  ;


%macro AHGdatanodupkey(data = , out = , by = );
  %if %AHGblank(&out) %then %let out=%AHGbasename(&data);

  proc sort data = &data out = &out nodupkey;
    by &by;
  run;
%mend ;

%macro AHGdatasort(data = , out = , by = );
  %if %AHGblank(&out) %then %let out=%AHGbarename(&data);
  proc sort 
    %if %length(&data) %then data = &data;
    %if %length(&out) %then out = &out;
  ;
    by &by;


  run;
%mend ;
%macro AHGdefault(mac,default,global=1);
  %if  &global %then %global &mac;
  %if %AHGblank(%bquote(%trim(&&&mac))) %then %let &mac=&default  ;
%mend;
%macro AHGdelimit;
%if %AHGpos(&sysscp,win)%then%str(\);
%else%str(/);
%mend;
%macro AHGdel(mac,like=0,startwith=1);
%*<spec> del mac var exactly match or  looks like or start with </spec>;

%local _ahg_igeahu_55481 ;
%if not &like %then
  %do _ahg_igeahu_55481=1 %to %AHGcount(&mac);
    %symdel %scan(&mac,&_ahg_igeahu_55481);
  %end;
%else
  %do ;
%local  _ahg_geahui_55481 _ahg_uigeah_55481 _ahg_huigea_55481 ;
     %let mac=%upcase(&mac);
     %do _ahg_huigea_55481=1 %to %AHGcount(&mac);
         %let _ahg_geahui_55481=%scan(&mac,&_ahg_huigea_55481);  
         %let _ahg_uigeah_55481=;
         proc sql noprint;
          select name into :_ahg_uigeah_55481 separated by ' '
          from sashelp.vmacro
          where upcase(name) like %if &startwith %then "&_ahg_geahui_55481%";%else "%"||"&_ahg_geahui_55481%"; 
          order by name
          ;quit;

%local  _ahg_igeahu_55481 _ahg_ahuige_55481 ;
        %do _ahg_igeahu_55481=1 %to %AHGcount(&_ahg_uigeah_55481);
          %let _ahg_ahuige_55481=%scan(&_ahg_uigeah_55481,&_ahg_igeahu_55481);
          %if not %AHGblank(&_ahg_ahuige_55481) %then  %symdel &_ahg_ahuige_55481;
        %end;
    %end;
  %end;
%mend;
%macro AHGdelta(msg);
  %put '####################delta    ';
  %if not %AHGblank(&msg) %then %put           &msg             ;
  %put '          #             ';
  %put '        ####           ';
  %put '      ########          ';
  %put '    ############        ';
  %put '  ################      ';
  %put '####################    ';
%mend;
%macro AHGdim(str,by=2,dlm=%str( ));
  %sysfunc(ceil(%sysevalf(%AHGcount(&str)/&by )))
%mend;
%macro AHGdistinctvalue(dsn,var,sort=1,into=,dlm=@,quote=0);
%local _ahg_ahuige_723398 _ahg_huigea_723398 
;
%let _ahg_huigea_723398=1;

%if   &quote %then %AHGvarisnum(&dsn,&var,into=_ahg_huigea_723398);

%let _ahg_ahuige_723398=&var;
%if %eval(&quote and not &_ahg_huigea_723398 )%then %let _ahg_ahuige_723398=quote(&var);

%if not &sort %then
  %do;
  data _null_;
    format line&var $32333.;
    retain line&var;
    set &dsn(keep=&var) end=end;
    line&var=catx("&dlm",line&var,&var);
    if end then call symput("&into",line&var);
  run;
  %end;
%else 
    %do;
    proc sql noprint;
    select distinct 

    &_ahg_ahuige_723398 
    into :&into separated by "&dlm"
    from &dsn
    ;quit;
    %end;
%let &into=%trim(%bquote(&&&into));

%mend;
%macro AHGdropvar(dsn,IDs,out=);
  %if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%local _ahg_uigeah_704744 _ahg_ahuige_704744 _ahg_huigea_704744 ;
  %AHGvarlist(&dsn,into=_ahg_huigea_704744,dlm=%str( ),global=0);
  %let _ahg_ahuige_704744=%ahgcount(&ids);
    data &out;
    set &dsn(drop=
    %do _ahg_uigeah_704744=1 %to &_ahg_ahuige_704744;
        %scan(&_ahg_huigea_704744,%scan(&ids,&_ahg_uigeah_704744)) 
    %end;
    );
  run;
  
%mend;

%macro ahgD(d=%str(,),sep=i);
%*<spec> use ',' as dlm for loop i: skip 1, from 2 to n from select ... as , </spec>;
%if &&&sep ne 1 %then &d;
%MEND;
%macro AHGdsninlib(lib=WORK,list=dsnlist,lv=2,mask=,global=0);
%local _ahg_huigea_859272 ;
%if %lowcase(&lib)=work %then %let lv=1;
%ahggettempname(_ahg_huigea_859272);
%if &global %then %global &list;
  proc datasets lib=&lib nolist;

    contents data=_all_ memtype=data out=work.&_ahg_huigea_859272 noprint;
  run;


%local _ahg_ahuige_859272 ;
   %if &lv=1 %then %let _ahg_ahuige_859272=MEMNAME;
   %else  %let _ahg_ahuige_859272="&lib.."||MEMNAME;
  proc sql noprint;
    select  &_ahg_ahuige_859272 into :&list  separated by ' '
  from sashelp.vstable
  where upcase("&lib")=libname  and not %AHGeqv(memname, "&_ahg_huigea_859272")  %if not %AHGblank(&mask) %then %str(and upcase(memname) like %upcase(&mask));
      
  ;
  quit;
  
%AHGdatadelete(data=&_ahg_huigea_859272);  

%mend;


%macro AHGdsnOfFmt(fmt,lib=work,out=&fmt,var=&fmt);
/* create a dsn with all possible format values
with a variable name =fmt 
*/
proc format library=&lib CNTLOUT=&out(where=(fmtname=upcase("&fmt")) keep=fmtname start label type );
run;
%local _ahg_ahuige_254148 ;
%AHGdistinctValue(&out,type,into=_ahg_ahuige_254148,dlm=@);

data &out;
  set &out;
  %if &_ahg_ahuige_254148=N %then &var=input(left(start),best.);
  %else &var=start;
  ;
  keep start &var label;
run;
%mend;
%macro AHGdsn(dsn,out=,where=1);
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
    data &out;
        set &dsn;
        if &where;
    run;
%mend;
%macro AHGdsnwild(lib,dsn,into=_into_,last=0);
%*<spec> put wildcard dsname into :into </spec>;
%local _ahg_ahuige_299307 ;
  %AHGdsninlib(lib=&lib,list=alldsn,lv=2,mask=,global=0);
%local _ahg_huigea_299307 ;
  ;
  %let dsn=%sysfunc(tranwrd(&dsn,-,%str(\w*)));
  ;
  %do _ahg_huigea_299307=1 %to %AHGcount(%str(&_ahg_ahuige_299307));
  %if ((%AHGamp(&into)=) or &last) and %sysfunc(prxmatch(/^(&lib\.)?&dsn$/i,%scan(&_ahg_ahuige_299307,&_ahg_huigea_299307,%str( )))) %then %let &into=%scan(&_ahg_ahuige_299307,&_ahg_huigea_299307,%str( ));
  %end;
%mend;
  
  %macro AHGemptyDSN(dsn,out=empty%AHGbasename(&dsn));

    data &out;
      ahuige32984932184093284593='';
      output;
      set &dsn(where=(0));
      drop ahuige32984932184093284593;

    run;
    
  %mend;
  
%macro AHGeqm(txt1,txt2);
  %AHGequalmactext(&txt1,&txt2)
%mend;
%macro AHGequalmactext(text1,text2);
  (%upcase(&text1)=%upcase(&text2))
%mend;
%macro AHGequaltext(text1,text2);
  (upcase(&text1)=upcase(&text2))
%mend;
%macro AHGeqV(txt1,txt2);
  %AHGequaltext(&txt1,&txt2)
%mend;
%macro AHGfiledatetime(file,dtfmt=mmddyy10.,t=_,tmfmt=time5.);
%local _ahg__ahuige_571604 _ahg_geahui_571604 _ahg_igeahu_571604 _ahg_ahuige_571604 ;
  %IF not %sysfunc(prxmatch(m/[\%str(,)\&\%str(;)\s]/,%bquote(&file)))%then
    %do;
    %if %sysfunc(fileexist(%bquote(&file))) and (not %AHGispath(%bquote(&file))) %then
      %do;
%local _ahg__huigea_571604 _ahg___ahuig_571604 _ahg__uigeah_571604 _ahg_huigea_571604 
;
%local _ahg_uigeah_571604 
;
      %let _ahg__huigea_571604=%sysfunc(filename(onefile,%bquote(&file)));                                                                                       
      %let _ahg___ahuig_571604=%sysfunc(fopen(&_ahg_huigea_571604));    
      %let _ahg_ahuige_571604=%qsysfunc(finfo(&_ahg___ahuig_571604,Last Modified));
      
      %if (not %index(&sysscp,WIN)) %then
          %let _ahg_igeahu_571604=%SYSFUNC(inputn(%SYSFUNC(PRXCHANGE(S/(^\w{3})\s*(\w+)\s*(\d+)\s*(\d+:\d+:\d+)\s*(\d+)/\3\2\5 \4/,1,&_ahg_ahuige_571604)),datetime20.));
      %else %let _ahg_igeahu_571604=%sysfunc(inputn(%bquote(&_ahg_ahuige_571604) ,datetime20.));
      %let _ahg__ahuige_571604=%sysfunc(datepart(&_ahg_igeahu_571604));
      %let _ahg_geahui_571604=%sysfunc(timepart(&_ahg_igeahu_571604));
      
      %let _ahg_igeahu_571604=%sysfunc(putn(&_ahg__ahuige_571604,yymmdd10.))&t%sysfunc(putn(&_ahg_geahui_571604,time10.));
      %let _ahg_igeahu_571604=%sysfunc(prxchange(s/\s(\d:)/ 0\1/,-1,&_ahg_igeahu_571604));
      %let _ahg___ahuig_571604=%sysfunc(fclose(&_ahg___ahuig_571604)); 
      %sysfunc(compress(&_ahg_igeahu_571604))

      %end; 
    %end;
%mend;
%macro AHGfiledt(file,into=,dtfmt=mmddyy10.,tmfmt=time5.);
%local _ahg_ahuige_298742 _ahg_igeahu_298742 _ahg_uigeah_298742 _ahg_huigea_298742 ;
  %if %sysfunc(fileexist(&file)) %then
  %do;
  %AHGpipe(dir &file /tw,rcmac=thetime,start=6,end=6);
  %let _ahg_ahuige_298742=%sysfunc(putn(%sysfunc(inputn(%substr(%bquote(&_ahg_huigea_298742),1,10),&dtfmt)),yymmdd10.));
  %let _ahg_igeahu_298742=%sysfunc(putn(%sysfunc(inputn(%substr(%bquote(&_ahg_huigea_298742),11,6),&tmfmt)),time5.));

  %let &into=%sysfunc(translate(&_ahg_ahuige_298742 &_ahg_igeahu_298742,___,:-%str( )));
  ;
  %end; 
%mend;

%macro AHGfilelike(path,regex,exclude);
%local _ahg_igeahu_990036 _ahg_uigeah_990036 _ahg_ahuige_990036 ;
 %if  %AHGblank(&exclude) %then %let exclude='m/```/';
%ahggettempname(_ahg_uigeah_990036);
%if %AHGonwin %then %AHGpipe(%bquote(dir  &path /O:D /b/s),rcmac=rcpipe,start=1,end=999999999,dsn=&_ahg_uigeah_990036,global=0);
%else %AHGpipe(%bquote(find  %cpipath(&path,unix) ),rcmac=rcpipe,start=1,end=999999999,dsn=&_ahg_uigeah_990036,global=0);

data new&_ahg_uigeah_990036;
  format str $500.;
  set &_ahg_uigeah_990036(  keep=line) ;
  where prxmatch(&regex,line) and  not prxmatch(&exclude,line) ;
  keep line  ;
  str=catx(' ','%let one= %cpipath(',line,',unix);%put &one;');
  call execute(str);
  str=catx(' ','%let one= %cpipath(',line,',win);%let _ahg_ahuige_990036=&_ahg_ahuige_990036@&one;');
  call execute(str);
  line=catx(' ','<a  href="',line,'">',line,'</a>');
  output;
run;

%AHGprt;

data new&_ahg_uigeah_990036;
  format line $500.;
%local _ahg_huigea_990036 ;
  %do _ahg_huigea_990036=1 %to %AHGcount(&_ahg_ahuige_990036,dlm=@);
  line=strip(scan("&_ahg_ahuige_990036",&_ahg_huigea_990036,'@'));
  line=catx(' ','<a  href="',line,'">',line,'</a>');
  output;
  %end;
run;

%AHGprt;
 
%mend;
%macro AHGfilename(file);
%local _ahg_huigea_321588 _ahg_ahuige_321588 ;
  %let _ahg_huigea_321588=%scan(&file,%AHGcount(&file,dlm=/),/);
  %let _ahg_huigea_321588=%scan(&_ahg_huigea_321588,%AHGcount(&_ahg_huigea_321588,dlm=\),\);
  &_ahg_huigea_321588
%mend;
%macro AHGfilesindir(dir,dlm=@,fullname=0,extension=,mask=,include=,except=,into=q,case=0,print=0);    
%local _ahg_geahui_410199 _ahg__uigeah_410199 _ahg__igeahu_410199 _ahg__huigea_410199 _ahg_huigea_410199 _ahg__ahuige_410199 _ahg___ahuig_410199 _ahg_igeahu_410199 ;
  %let _ahg__igeahu_410199=%substr(X%AHGrandom,1,8);
  %let _ahg__huigea_410199=%sysfunc(filename(fileref,&dir)); 
  %let _ahg_geahui_410199=%sysfunc(dopen(&_ahg__igeahu_410199));                                                                                                      
                                                                                                                                        
  %let _ahg__uigeah_410199=%sysfunc(dnum(&_ahg_geahui_410199)); 
%local _ahg_ahuige_410199 ;
  %ahggettempname(_ahg_ahuige_410199);
  data &_ahg_ahuige_410199;
  format file $200.;
  %do _ahg_huigea_410199= 1 %to &_ahg__uigeah_410199;                                                                                                                
     file="%qsysfunc(dread(&_ahg_geahui_410199,&_ahg_huigea_410199))";
     output;
  %end;
  run;
  proc sql noprint;
    select  %if &fullname %then "&dir%AHGdelimit"||;file into :&into   separated by "&dlm"
  from &_ahg_ahuige_410199
  where 1=1 
  %if %AHGnonblank(&mask) %then 
  %if &case=0 %then %str(and upcase(file) like upcase(&mask));
  %else and file like &mask ;


%local _ahg_uigeah_410199 ;
  %if %AHGnonblank(&include) %then 
  %do;
    and ( 1
    %do _ahg__ahuige_410199=1 %to %AHGcount(&include); 
    %let _ahg_uigeah_410199=%scan(&include,&_ahg__ahuige_410199,%str( ));
    %if &case=0 %then %str(or (index(upcase(file),upcase("&_ahg_uigeah_410199")))        );
    %else or (index( file ,"&_ahg_uigeah_410199")  );
    %end;
    )
  %end;

  %if %AHGnonblank(&except) %then 
  %do _ahg__ahuige_410199=1 %to %AHGcount(&except); 
  %let _ahg_uigeah_410199=%scan(&except,&_ahg__ahuige_410199,%str( ));
  %if &case=0 %then %str(and not (index(upcase(file),upcase("&_ahg_uigeah_410199")))        );
  %else and not (index( file ,"&_ahg_uigeah_410199")  );
  %end;
  order by file
  ;
  quit;
  %let _ahg__huigea_410199=%sysfunc(dclose(&_ahg_geahui_410199)); 
  %let _ahg__huigea_410199=%sysfunc(filename(filrf));
  %if &print %then ;
%mend;
  




%MACRO AHGfill(line,nums);

%let line=%sysfunc(prxchange(s/(\D*)\d+/$1```/, -1, &line));

%local _ahg_huigea_975002 _ahg_ahuige_975002 _ahg_uigeah_975002 ;
%do _ahg_huigea_975002=1 %to %AHGcount(&nums);
  %let _ahg_uigeah_975002=%scan(&nums,&_ahg_huigea_975002,%str( ));
  %let _ahg_uigeah_975002=s/([^`]*)```/$1 &_ahg_uigeah_975002/;
  %let line=%sysfunc(prxchange(&_ahg_uigeah_975002, 1, &line));
%end;
&line

%mend;

%macro AHGfmtmac(dsn,var=,url=);
%local _ahg_ahuige_444532 ;
%ahggettempname(_ahg_ahuige_444532)
%AHGvarinfo(&dsn,out=&_ahg_ahuige_444532,info= name  type  length num fmt);

data _null_;
  set &_ahg_ahuige_444532;
  format command $200.;
  command=' %global &url.type'||name||';'||' %global &url.fmt'||name||';';
  call execute(command);
run;

data _null_;
  set &_ahg_ahuige_444532;
  call symput("&url.type"||name,type);
  call symput("&url.fmt"||name,fmt);
run;

%mend;

%macro AHGfmtValueLabel(fmt,ValueMac, LabelMac,dlm=@,out=);
%if %AHGblank(&out) %then %let out=&fmt.fmt;
proc format CNTLOUT=&out(where=(fmtname=upcase("&fmt")) keep=fmtname start label);
run;


proc sql noprint;
  select start,Label into :&valuemac  separated by "&dlm", :&labelmac separated by "&dlm"
  from &out
  order by start
  ;

quit;
%mend;

%macro AHGfreeloop(dsn,byvars
,cmd=
,out=outAhuige
,in=Ahuige
,url=
,bydsn=&url.BY
,execute=1
,del=1
,addLoopVar=0
,low=0
,up=99999999
,title=1
,printstr=dataset:&dsn @cmd:&cmd @ by:&byvars);
/*
1 New dsn: &url.by(1)  &url&outone.&_ahg_uigeah_295653 (N*O)
2  New Mac: &url.N

*/
%if %AHGblank(&url) %then %let url=_%substr(%AHGrandom,1,3);
%if %AHGblank(&cmd) %then %let cmd= put abc ;
%let cmd=%nrstr(%%)&cmd;
%global &url.N;    
%let &url.N=0;


proc sql noprint;
  create table &bydsn as
  select distinct %AHGaddcomma(&byvars)
  from &dsn
  order by  %AHGaddcomma(&byvars)
  ;quit;
%local _ahg_uigeah_295653 _ahg_ahuige_295653 ;

%AHGnobs(&bydsn,into=&url.N);

data
%do _ahg_uigeah_295653=1 %to &&&url.N;
&url&_ahg_uigeah_295653
%end;
;
  set &bydsn;
  %do _ahg_uigeah_295653=1 %to &&&url.N;
  if _n_=&_ahg_uigeah_295653 then output &url&_ahg_uigeah_295653 ;
  %end;
run;

%do _ahg_uigeah_295653=1 %to &&&url.N;
%local _ahg_huigea_295653 ;

  data _null_;
    set &url&_ahg_uigeah_295653;
    call symput('_ahg_huigea_295653',compbl(left(%AHGaddcomma(&byvars,comma=%str(||)))));
  run;

%if &del %then
  %do;
  %AHGmergedsn(&url&_ahg_uigeah_295653,&dsn,&in,by=&byvars,joinstyle=left/*left right full matched*/);
  %end;
%else
  %do;
  %AHGmergedsn(&url&_ahg_uigeah_295653,&dsn,&url&_ahg_uigeah_295653,by=&byvars,joinstyle=left/*left right full matched*/);
  data &in ;
    set  &url&_ahg_uigeah_295653;
  run;
  %end;


;
%if &execute=1 %then
  %do;
  %put ######################freeloopNo&_ahg_uigeah_295653;
  %put &printstr;
  %if &title %then title "&_ahg_huigea_295653";;
  %if %eval(&low<=&_ahg_uigeah_295653) and %eval(&_ahg_uigeah_295653<=&up) %then
    %do;
    %unquote(&cmd);
%local _ahg_geahui_295653 _ahg_igeahu_295653 ;
      %do _ahg_geahui_295653=1 %to %AHGcount(&out);
        %let _ahg_igeahu_295653=%scan(&out,&_ahg_geahui_295653);
        data &url&_ahg_igeahu_295653&_ahg_uigeah_295653;
          set  &_ahg_igeahu_295653;
          %if &addloopvar %then
          %do;
          point=&_ahg_uigeah_295653;
          set &bydsn point=point;
          %end;
        run;
      %end;
    %end;
  
  
  %end;
  
  
%end;



%mend;

%macro AHGfreesplit(dsn,byvars,outPref=,bydsn=);
%*<spec> split dataset by var like by age to age21 age14.... </spec>;

%AHGdatadelete(data=&outpref:);
%AHGdel(&outpref,like=1);
%global &outpref.N;


proc sql;
  create table &bydsn as
  select distinct %AHGaddcomma(&byvars)
  from &dsn
  group by  %AHGaddcomma(&byvars)
  ;quit;
%local _ahg_huigea_289095 _ahg_ahuige_289095 ;



%AHGnobs(&bydsn,into=&outpref.N);


data
%do _ahg_huigea_289095=1 %to &&&outpref.N;
&outpref&_ahg_huigea_289095
%end;
;
  set &bydsn;
  %do _ahg_huigea_289095=1 %to &&&outpref.N;
  if _n_=&_ahg_huigea_289095 then output &outpref&_ahg_huigea_289095 ;
  %end;
  run;

%do _ahg_huigea_289095=1 %to &&&outpref.N;
%AHGmergedsn(&outpref&_ahg_huigea_289095,&dsn,&outpref&_ahg_huigea_289095,by=&byvars,joinstyle=left/*left right full matched*/);
%end;


%mend;
%macro AHGfreqCoreEX(dsn,var,out=,by=,print=0,rename=1
,keep=value cell frequency percent
);
%if %AHGblank(&out) %then %let out=&sysmacroname;
%local _ahg_ahuige_597743 ;
%ahggettempname(_ahg_ahuige_597743);
data &_ahg_ahuige_597743;
  set &dsn;
run;

proc freq data=&_ahg_ahuige_597743(keep=&var %if %AHGnonblank(&by) %then  &by; rename=(&var=value));
    table value;
    ods output OneWayFreqs=%AHGbarename(&out);
    %if %AHGnonblank(&by) %then by  &by;;
run;
%if not &rename %then 
  %do;
  data  %AHGbarename(&out)(keep=&keep &var %if %AHGnonblank(&by) %then  &by;);
    set %AHGbarename(&out);
    rename value=&var;
    cell=catx(' ',%AHGputn(frequency),' (',%AHGputn(percent,6.1),')');
  run;
  %end;
%else 
  %do;
  data  %AHGbarename(&out)(keep=&keep value %if %AHGnonblank(&by) %then  &by;);
    set %AHGbarename(&out);
    cell=catx(' ',%AHGputn(frequency),' (',%AHGputn(percent,6.1),')');
  run;
  %end;
%if &print %then %AHGprt;
%mend;

%macro AHGfreqCore(dsn,var,by=,out=,print=0,rename=1,
keep=cell frequency percent,tran=
,tranBy=
,cell=put(frequency,4.)||' ('||left(put(percent,5.1))||')'
);
%if %AHGblank(&out) %then %let out=&sysmacroname;
ods listing close;
proc freq data=&dsn(keep=&var &by  );
    table &var;
    %if not %AHGblank(&by) %then by &by;;
    ods output OneWayFreqs=&out(keep=&var  CUMFREQUENCY percent  frequency &by);
run;
ods listing;

%if %AHGpos(&keep,cell) %then 
%do;
data &out;
  set &out;
  cell=&cell;
run;
%end;

%if &rename %then 
%do;
data &out;
  set &out(rename=(&var=value));
run;
%end;


%if not %AHGblank(&tran) %then 
%do;

data &out.Notran;
  set &out;
run;
%if not %AHGblank(&tranby) %then %AHGdatasort(data =&out , out = , by =&tranby ) ;

proc transpose data=&out out=&out(drop=_name_);
  var 
  %if %AHGpos(&keep,cell)  %then cell;
  %else 
%AHGremoveWords(&keep,value &var,dlm=%str( )) ;
  ;
  id &tran;
  ;
  %if not %AHGblank(&tranby) %then by &tranby; ;
run;

%end;
%else 
  %do;
  data &out;
    set &out(keep=&keep &by %if not &rename %then &var; %else value;);
  run;
  %end;



%if &print %then %AHGprt;
%mend;
%macro AHGfreqDefault(dsn,var,expected=,out=,by=,print=0,rename=1
,keep=value cell frequency percent
);
%if %AHGblank(&out) %then %let out=&sysmacroname;
%local _ahg_huigea_697565 _ahg_geahui_697565 _ahg_ahuige_697565 ;

%ahggettempname(_ahg_geahui_697565);
%ahggettempname(_ahg_huigea_697565);
%ahggettempname(_ahg_ahuige_697565);
proc sql;
  create table &_ahg_huigea_697565 as
  select &var as value %if %AHGnonblank(&by) %then , %AHGaddcomma(&by);
  from  &dsn
  %if %AHGnonblank(&by) %then order by %AHGaddcomma(&by);
  
  ;
  quit;


proc freq data=&_ahg_huigea_697565;
    table value/missing;
    ods output OneWayFreqs=&_ahg_geahui_697565.0;
    %if %AHGnonblank(&by) %then by  &by;;
run;
%local _ahg_igeahu_697565 ;
%AHGvarisnum(&_ahg_geahui_697565.0,value,into=_ahg_igeahu_697565);

proc sql;
  create table _&_ahg_ahuige_697565 as
  select distinct 'Analysis result' as %AHG_(result) %if %AHGnonblank(&by) %then , %AHGaddcomma(&by);
  from &_ahg_geahui_697565.0
  ;
  quit;
  

data &_ahg_ahuige_697565;
  %if &_ahg_igeahu_697565 ne 1 %then format value $200.;
  %else format value 8.;;
  %* dummy statement for exclude default output statement;
  if 0 then output;
  %if %AHGnonBLANK(&expected) %THEN
  %DO;
%local _ahg_uigeah_697565 ;
    %do _ahg_uigeah_697565=1 %to %sysfunc(countw(&expected,@));
      %if &_ahg_igeahu_697565=1 %then value=%scan(&expected,&_ahg_uigeah_697565,@);
      %else value="%scan(&expected,&_ahg_uigeah_697565,@)";
      ;
      Frequency=0;
      Percent=0; 
      output;
    %end; 
  %END;
run;  

proc sql;
  create table __&_ahg_ahuige_697565 as
  select *
  from _&_ahg_ahuige_697565, &_ahg_ahuige_697565
  ;
  quit;

%AHGmergedsn(__&_ahg_ahuige_697565,&_ahg_geahui_697565.0,&_ahg_geahui_697565
,by=%if %AHGnonblank(&by) %then  &by; value ,
fuzzby=1,
joinstyle=full/*left right full matched*/);



 

  data  %AHGbarename(&out);
    set &_ahg_geahui_697565;
    %if not &rename %then rename value=&var;;
    value=left(compbl(value));
    if missing(frequency) then frequency=0;
    if missing(percent) then percent=0;
    cell=catx(' ',%AHGputn(frequency),' (',%AHGputn(percent,6.1),')');
    keep &keep value %if %AHGnonblank(&by) %then  &by;;
  run;



  
  
  
%if &print %then %AHGprt;
%mend;%macro AHGfuncloop(func,loopvar=ahuige,loops=,dlm=%str( ),execute=yes,pct=1);
%local _ahg_huigea_100648 _ahg_igeahu_100648 _ahg_ahuige_100648 _ahg_uigeah_100648 ;
  %let _ahg_igeahu_100648=%AHGcount(&loops,dlm=&dlm);
  %do _ahg_huigea_100648=1 %to &_ahg_igeahu_100648;
  %let _ahg_ahuige_100648=%sysfunc(tranwrd(&func,&loopvar,%scan(&loops,&_ahg_huigea_100648,&dlm)));
  %if &pct %then %let _ahg_uigeah_100648=%nrstr(%%)&_ahg_ahuige_100648;
  %else %let _ahg_uigeah_100648=&_ahg_ahuige_100648;
  %if &execute=yes or &execute=y %then %unquote(&_ahg_uigeah_100648);
  %else %put &_ahg_uigeah_100648;
  %end;
%mend;
%macro AHGfuzzStr(dsn,vars,out=,replace=0);
%local _ahg_ahuige_997027 _ahg_huigea_997027 ;
%if %AHGblank(&out) %then %let out=%ahggettempname(out);
data &out;
  set &dsn; 
  %do _ahg_ahuige_997027=1 %to %AHGcount(&vars);
  %let _ahg_huigea_997027=%scan(&vars,&_ahg_ahuige_997027);
  %if &replace=0 %then
    %do;
    if vtype(&_ahg_huigea_997027)='N' then %AHG_(&_ahg_huigea_997027)=&_ahg_huigea_997027;
    else %AHG_(&_ahg_huigea_997027)=upcase(left(compbl(&_ahg_huigea_997027)));
    %end;
  %else
    %do;
    if vtype(&_ahg_huigea_997027) ne 'N' then &_ahg_huigea_997027=upcase(left(compbl(&_ahg_huigea_997027)));
    %end;

  %end;
run;
%mend;%macro ahggettempnameNeat(dsn,start=,useit=0);
%ahgincr(global__loop__for_temp_data);
%let &dsn=%sysfunc(translate(&dsn.__&global__loop__for_temp_data,_,.));
%mend;
%macro AHGgettempname(tempname,start=,useit=0);
  
  %if %AHGblank(&start) %then %let start=T_&tempname;
  %if %length(&start)>10 %then %let start=%substr(&start,1,10);
%local _ahg_huigea_616415 _ahg_ahuige_616415 ;
  %do %until (not %sysfunc(exist(&&&tempname))  );
  %let _ahg_huigea_616415=;
  %do _ahg_ahuige_616415=1 %to 7;
  %let _ahg_huigea_616415=&_ahg_huigea_616415%sysfunc(byte(%sysevalf(65+%substr(%sysevalf(%sysfunc(ranuni(0))*24),1,2))) ); 
  %end;
  %let &tempname=&start._&_ahg_huigea_616415;
  %end;
  %put &tempname=&&&tempname;
  %if &useit %then
  %do;
  data &&&tempname;
  run;
  %end;


%mend;
%macro AHGgetwords(words,from,num,dlm1st=0,dlm=%str( )/*right*/);
%*<spec> like substrbyword(sentence,from, to ) </spec>;
%local _ahg_ahuige_178720 _ahg_huigea_178720 ;
  %let _ahg_huigea_178720=;
  %do _ahg_ahuige_178720=0 %to %eval(&num-1);
    %if &_ahg_ahuige_178720 gt &dlm1st %then %let _ahg_huigea_178720=&_ahg_huigea_178720&dlm;
    %let _ahg_huigea_178720=&_ahg_huigea_178720%scan(&words,%eval(&_ahg_ahuige_178720+&from),&dlm);
  %end;
  &_ahg_huigea_178720
%mend;
%macro AHGgrep(all,ptn,r=0,dlm=%str( ));
%*<spec> grep: keep words contain ptn /or not : Mary Tom Mike Joe,M Jo  </spec>;
%local _ahg_ahuige_250282 _ahg_huigea_250282 ;

%do _ahg_ahuige_250282=1 %to %AHGcount(%str(&all),dlm=&dlm);
%let _ahg_huigea_250282=%scan(&all,&_ahg_ahuige_250282,&dlm);
%let ptn=%sysfunc(tranwrd(%sysfunc(compbl(&ptn)),%str( ),|));
%if &r ne (%sysfunc(prxmatch(/(&ptn)/i,&_ahg_huigea_250282))>0) %then &_ahg_huigea_250282;
%end;

%mend;
%macro AHGhashvalue(hashid,handle);
%local _ahg_ahuige_450690 _ahg_huigea_450690 ;
  %let indx=%AHGindex(&&&hashid.list,&handle);
  %let _ahg_huigea_450690=&&&hashid&indx;
  &_ahg_huigea_450690
%mend;
%macro AHGidx(str,sub,case=0);
  %AHGin(&sub,&str,case=&case)
%mend;
%macro AHGincr(mac,by=1);
  %let &mac=%eval(&by+&&&mac);
%mend;
%macro AHGindex2(str,dlm);
%local _ahg_huigea_559119 _ahg_uigeah_559119 _ahg_ahuige_559119 ;
%let _ahg_ahuige_559119=0;
%do _ahg_huigea_559119=1 %to %length(&str);
%if %qsubstr(&str,&_ahg_huigea_559119,1) = &dlm and (&_ahg_ahuige_559119=0) and (&_ahg_uigeah_559119=1) %then %let _ahg_ahuige_559119=&_ahg_huigea_559119;
%if %qsubstr(&str,&_ahg_huigea_559119,1) eq &dlm %then %let _ahg_uigeah_559119=1;
%end;
&_ahg_ahuige_559119
%mend;
%macro AHGindex(full,sub,dlm=%str( ),case=0,lastone=0);
%local _ahg_huigea_339142 _ahg_ahuige_339142 ;
  %if not &case %then
    %do;
    %let full=%upcase(&full);
    %let sub=%upcase(&sub);
    %end;
  %let _ahg_huigea_339142=0;
  %do _ahg_ahuige_339142=1 %to %AHGcount(&full,dlm=&dlm);
  %if %scan(&full,&_ahg_ahuige_339142,&dlm)=&sub %then 
    %do;
    %let _ahg_huigea_339142=&_ahg_ahuige_339142;
    %if not &lastone %then %goto indexExit;
    %end;
  %end;
  %indexExit:
  &_ahg_huigea_339142
%mend;
%macro AHGin(sub,str,case=0);
%*<spec> if one value contains another value auto-trim-ed </spec>;
  %if not &case %then index(left(trim(upcase(&str))),left(trim(upcase(&sub))));
  %else index(left(trim(&str)),left(trim(&sub)))
%mend;
%macro AHGinterval(fromID,toid,pre=ahuigetimePoint,url=From &fromid To &toid );
%if %AHGblank(&fromid) %then %let fromid=0;
data _null_;
%IF %AHGblank(&toid) %then  diff=time()-input("&&ahuigetimepoint&fromid",time8.);
%ELSE diff=input("&&ahuigetimepoint&toid",time8.)-input("&&ahuigetimepoint&fromid",time8.);
;
put "######ahuige Interval:&url ########## time used :" diff time8.;
run;


%mend;
%macro AHGisPath(item);
%local _ahg_huigea_278173 _ahg_uigeah_278173 _ahg_ahuige_278173 ;
%let _ahg_uigeah_278173=%sysfunc(filename(fname,&item));
%let _ahg_huigea_278173= %sysfunc (dopen(&_ahg_ahuige_278173));
%if &_ahg_huigea_278173 %then 
  %do;
  %let _ahg_uigeah_278173= %sysfunc (DCLOSE(&_ahg_huigea_278173));
  1
  %end;
%else 0;
%mend;
%macro AHGits(str);
%local _ahg_uigeah_539726 _ahg_huigea_539726 _ahg_geahui_539726 _ahg_ahuige_539726 _ahg_igeahu_539726 ;
%let _ahg_huigea_539726=%eval(%length(&str)/2);

%do _ahg_uigeah_539726=1 %to &_ahg_huigea_539726;
%let _ahg_ahuige_539726=%eval(&_ahg_uigeah_539726*2-1);
%let _ahg_geahui_539726=&_ahg_geahui_539726%sysfunc(byte(%substr(&str,&_ahg_ahuige_539726,2)));
%end;
&_ahg_geahui_539726
%mend;
%Macro AHGjuststat(dsn,var,out,by=);

%IF not %AHGblank(&by) %then %AHGdatasort(data =&dsn , out =&dsn , by =&by );
proc means data=&dsn n mean std median max min;
  output out=&out 
n=n mean=mean median=median std=std max=max min=min;
;
  var &var;
  by &by;
run;

%mend;
%macro AHGkeepvar(dsn,IDs,out=);
  %if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%local _ahg_uigeah_364475 _ahg_ahuige_364475 _ahg_huigea_364475 ;
  %AHGvarlist(&dsn,into=_ahg_huigea_364475,dlm=%str( ),global=0);
  %let _ahg_ahuige_364475=%ahgcount(&ids);
    data &out;
    set &dsn(keep=
    %do _ahg_uigeah_364475=1 %to &_ahg_ahuige_364475;
        %scan(&_ahg_huigea_364475,%scan(&ids,&_ahg_uigeah_364475)) 
    %end;
    );
  run;
  
%mend;


%macro AHGkill(dsns,
                dsetlist=_all_ ,   /* list of the SAS datasets to be deleted from the     */
               libkill= work      /* set the name of the library in which files are to   */
               )
               ;



%local _ahg_igeahu_984088 _ahg_huigea_984088 ;
     %if %AHGblank(&dsns) %then
           %do;

                proc datasets library=work kill memtype=data;

           %end;
     %else 
       %do _ahg_huigea_984088=1 %to %AHGcount(&dsns);
%local _ahg_ahuige_984088 _ahg_uigeah_984088 ;
       %let _ahg_ahuige_984088=%scan(&dsns,&_ahg_huigea_984088,%str( ));
       %if not %index(&_ahg_ahuige_984088,.) %then %let _ahg_ahuige_984088=work.&_ahg_ahuige_984088;
       %let _ahg_uigeah_984088=%scan(&_ahg_ahuige_984088,1);
       %let _ahg_ahuige_984088=%scan(&_ahg_ahuige_984088,2);
       proc datasets library=&_ahg_uigeah_984088 memtype=data;
                     delete &_ahg_ahuige_984088;
       run;
       quit;
       %end;

%mend  ;

%macro ahgkw(str,key);
%let &key=;
%if %sysfunc(prxmatch(/\b&key:/i,&str)) %then %let &key=%sysfunc(prxchange(s/(.*)(\b&key:)(\S+).*/\3/i,1,&str));
&&&key 
%mend;
%macro AHGlatest(path,latest);
%local _ahg_huigea_904285 _ahg_ahuige_904285 ;
%ahggettempname(_ahg_ahuige_904285);
%AHGpipe(%bquote(dir  &path /O:D),rcmac=rcpipe,start=1,end=999999999,dsn=&_ahg_ahuige_904285,global=0);
data &_ahg_ahuige_904285;
  set &_ahg_ahuige_904285(where=(index(line,'.sas7bdat')) keep=line) end=end;
  if end then line=prxchange('s/.*?(\w+)\.sas7bdat.*/\1/',1,line);
  if end then call symput("&latest",trim(line));;
run;
%mend;
%macro AHGleft(arrname,mac,dlm=%str( ),global=0);
  %let &arrname=%sysfunc(left(%str(&&&arrname)));

%local _ahg_huigea_79480 _ahg_ahuige_79480 _ahg_uigeah_79480 ;
  %let _ahg_ahuige_79480=%AHGcount(&&&arrname,dlm=&dlm);
  %if &_ahg_ahuige_79480<=1 %then 
    %do;
    %let _ahg_uigeah_79480=&&&arrname;
    %let  &arrname=;
    %end;
  %else
    %do;
    %let _ahg_uigeah_79480=%scan(&&&arrname,1,&dlm);
    %let &arrname=%substr(&&&arrname,%index(&&&arrname,&dlm)+1);
    %end;
  %if &global %then %global &mac;   
  %if %AHGblank(&mac) %then &_ahg_uigeah_79480;
  %else %let &mac=&_ahg_uigeah_79480;
%mend;
%macro AHGlibinfo(lib,out=,info=dsn name  label);
%local _ahg_huigea_454313 ;
%AHGdsnInLib(lib=&lib,list=dsnlist);
;
%local _ahg_uigeah_454313 ;
%macro dosomething(dsn);
%local _ahg_ahuige_454313 ;
%ahggettempname(_ahg_ahuige_454313);
%let _ahg_uigeah_454313=&_ahg_uigeah_454313 &_ahg_ahuige_454313;
%AHGvarinfo(&dsn,out=&_ahg_ahuige_454313,info=&info  );
%mend;
%AHGfuncloop(%nrbquote( dosomething(ahuige) ) ,loopvar=ahuige,loops=&_ahg_huigea_454313);

data &out;
  set &_ahg_uigeah_454313;
run;
%mend;
%macro AHGlibpath(lib,into);
%if not %index(&thedsn,.) %then %let lib=Work;
%else %let lib=%scan(&lib,1,.);

%if %upcase(lib)=WORK %then ;
%else %if %upcase(lib)=SASHELP %then ;
%else %if %upcase(lib)=SASUSER %then ;
%ELSE
%do;
data _null_;
  set sashelp.vlibnam(where=(upcase("&lib")=libname));
  call symput("&into",path);
run;
%end;

%mend;
%macro ahglike(string,word);
%local _ahg_huigea_280731 _ahg_ahuige_280731 ;
  %let _ahg_huigea_280731=;
  %do _ahg_ahuige_280731=1 %to %AHGcount(&string);
  %if  %AHGequalmactext(%sysfunc(compress(%scan(&string,&_ahg_ahuige_280731),0123456789)),&word) %then %let _ahg_huigea_280731=&_ahg_huigea_280731 %scan(&string,&_ahg_ahuige_280731);
  %end;
  &_ahg_huigea_280731
%mend;

%macro AHGlookslike(dsn);
%local _ahg_igeahu_622759 _ahg_ahuige_622759 _ahg_geahui_622759 _ahg_uigeah_622759 _ahg_huigea_622759 ;
%let _ahg_igeahu_622759=%scan(&dsn,-2,.);
%let _ahg_ahuige_622759=%scan(&dsn,-1,.);
%IF &_ahg_igeahu_622759 EQ %THEN %let _ahg_igeahu_622759=work;
%let _ahg_geahui_622759=%sysfunc(pathname(&_ahg_igeahu_622759));
%let _ahg_uigeah_622759=%str( )%AHGls(&_ahg_geahui_622759,ext=sas7bdat);
 &_ahg_igeahu_622759..%sysfunc(prxchange(s/.*?\s(\w*&_ahg_ahuige_622759\w*)\.sas7bdat.*/\1/i,1,%bquote(&_ahg_uigeah_622759))) ;

%mend;
%macro AHGlsd(dir, dlm=%str( ) );
%local _ahg__ahuige_388386 _ahg__uigeah_388386 _ahg__ahuige_388386 _ahg__huigea_388386 _ahg_ahuige_388386 ;
%let _ahg__huigea_388386=%sysfunc(filename(onedir,%bquote(&dir)));
%local _ahg_igeahu_388386 _ahg_huigea_388386 _ahg_geahui_388386 _ahg___ahuig_388386 _ahg_uigeah_388386 ;
%let _ahg_igeahu_388386=%sysfunc(dopen(&_ahg__uigeah_388386));
%let _ahg_geahui_388386=%sysfunc(dnum(&_ahg_igeahu_388386));

%do _ahg_huigea_388386=1 %to &_ahg_geahui_388386;
  %let _ahg___ahuig_388386=%trim(%sysfunc(dread(&_ahg_igeahu_388386,&_ahg_huigea_388386)));
  %IF not %sysfunc(prxmatch(m/[\%str(,)\&\%str(;)\s]/,%bquote(&_ahg___ahuig_388386))) %then
    %do;
      %let _ahg_ahuige_388386=&dir%AHGdelimit;
      %let _ahg_ahuige_388386=&_ahg_ahuige_388386.%superq(name);
      %if %AHGispath(&_ahg_ahuige_388386) %then
        %do;
        %if %AHGblank(&_ahg__ahuige_388386) %then %let _ahg__ahuige_388386=%superq(name);
        %else %let _ahg__ahuige_388386=&_ahg__ahuige_388386&dlm%superq(name);
        %end;
    %end;
%end;
%let _ahg_uigeah_388386=%sysfunc(dclose(&_ahg_igeahu_388386));
&_ahg__ahuige_388386

%mend;
%macro AHGls(dir,ext=%str(),dlm=%str( ),dt=0);
%local _ahg__ahuige_423584 _ahg__uigeah_423584 _ahg__ahuige_423584 _ahg__huigea_423584 _ahg_ahuige_423584 ;
%let _ahg__huigea_423584=%sysfunc(filename(onedir,%bquote(&dir)));
%local _ahg_igeahu_423584 _ahg_huigea_423584 _ahg_geahui_423584 _ahg___ahuig_423584 _ahg_uigeah_423584 ;
%let _ahg_igeahu_423584=%sysfunc(dopen(&_ahg__uigeah_423584));
%let _ahg_geahui_423584=%sysfunc(dnum(&_ahg_igeahu_423584));

%do _ahg_huigea_423584=1 %to &_ahg_geahui_423584;
  %let _ahg___ahuig_423584=%trim(%sysfunc(dread(&_ahg_igeahu_423584,&_ahg_huigea_423584)));
 
  %IF not %sysfunc(prxmatch(m/[\%str(~)\%str($)\%str(,)\&\%str(;)\s]/,%bquote(&_ahg___ahuig_423584))) %then
    %do;
    %if %index(%upcase(%superq(name)),%upcase(&ext)) or %AHGblank(%bquote(&ext)) %then 
      %do;
      
      %let _ahg_ahuige_423584=&dir%AHGdelimit;
      %let _ahg_ahuige_423584=&_ahg_ahuige_423584.%superq(name);
      %if not %AHGispath(&_ahg_ahuige_423584) %then
        %do;
        %if %AHGblank(&_ahg__ahuige_423584) %then %let _ahg__ahuige_423584=%superq(name);
        %else %let _ahg__ahuige_423584=&_ahg__ahuige_423584&dlm%superq(name);

        %if &dt and not %AHGispath(&_ahg_ahuige_423584) %then %let _ahg__ahuige_423584=&_ahg__ahuige_423584 %AHGfiledatetime(&_ahg_ahuige_423584);
        %end;
      %end;
    %end;
%end;
%let _ahg_uigeah_423584=%sysfunc(dclose(&_ahg_igeahu_423584));
&_ahg__ahuige_423584

%mend;
%macro AHGmacAndvalue(pairs,global=1,dlm=%str( ),dlm2=|);
%local _ahg_huigea_46422 _ahg_ahuige_46422 ;
%do _ahg_ahuige_46422=1 %to %AHGcount(&pairs,dlm=&dlm);
  %let _ahg_huigea_46422=%scan(&pairs,&_ahg_ahuige_46422,&dlm);
  %if &global %then %global %scan(&_ahg_huigea_46422,1,&dlm2);;
  %let %scan(&_ahg_huigea_46422,1,&dlm2)=%scan(&_ahg_huigea_46422,2,&dlm2);
%end;
%mend;
%macro ahgmacroot;
c:\hui\newsas
%mend;
%MACRO ahgmatch(ptn,str);
%sysfunc(prxchange(s/(\S*?)((\S*&ptn\S*)?)/\2/i,-1, &str))
%mend;

%macro AHGmergeall(
out=,
item1=,
item2=,
item3=,
item4=,
item5= ,
item6= ,
item7= ,
item8= ,
item9=
)
;
%local _ahg_huigea_509861 _ahg_uigeah_509861 _ahg_ahuige_509861 loopN LastDsn
item1 dsn1  vars1 by1 tempdsn1
item2 dsn2  vars2 by2 tempdsn2
item3 dsn3  vars3 by3 tempdsn3
item4 dsn4  vars4 by4 tempdsn4
item5 dsn5  vars5 by5 tempdsn5
item6 dsn6  vars6 by6 tempdsn6
item7 dsn7  vars7 by7 tempdsn7
item8 dsn8  vars8 by8 tempdsn8
item9 dsn9  vars9 by9 tempdsn9

;
%do _ahg_uigeah_509861=1  %to 9;
  %let _ahg_huigea_509861=%eval(10-&_ahg_uigeah_509861);
  %if %AHGblank(&&item&_ahg_huigea_509861)  %then   %let _ahg_ahuige_509861=%eval(&_ahg_huigea_509861-1);
%end;

%let loopn=%eval(&_ahg_ahuige_509861-1);

%do _ahg_huigea_509861=1 %to &_ahg_ahuige_509861;
  %let dsn&_ahg_huigea_509861=%scan(&&item&_ahg_huigea_509861,1,@);
  %let vars&_ahg_huigea_509861=%scan(&&item&_ahg_huigea_509861,2,@);
  %if &_ahg_huigea_509861 <= &loopn %then
  %do;
  %let by&_ahg_huigea_509861=%scan(&&item&_ahg_huigea_509861,3,@);
  %*pm(dsn&_ahg_huigea_509861  vars&_ahg_huigea_509861 by&_ahg_huigea_509861);
  %end;
%end;

%AHGdatanodupkey(data =&dsn1(keep=&vars1) , out =&out , by =&by1 );

%do _ahg_huigea_509861=1 %to &loopn;
  %let _ahg_uigeah_509861=%eval(&_ahg_huigea_509861+1);
  %ahggettempname(tempdsn&_ahg_uigeah_509861,start=%sysfunc(tranwrd(&&dsn&_ahg_uigeah_509861,.,_))_);
  %AHGdatanodupkey(data =&&dsn&_ahg_uigeah_509861(keep=&&by&_ahg_huigea_509861 /*it is i not j*/ &&vars&_ahg_uigeah_509861),
    out =&&tempdsn&_ahg_uigeah_509861 , by =&&by&_ahg_huigea_509861 /*it is i not j*/ );

  %AHGmergedsn(&out,&&tempdsn&_ahg_uigeah_509861,&out,rename=1,by=&&by&_ahg_huigea_509861,joinstyle=full/*left right full matched*/);
%end;
  

%mend;
%macro AHGmergedsn(dsn1,dsn2,outdsn,by=,rename=1
,keepINvar=0,joinstyle=full,check=1
,fuzzBy=0);
%if &fuzzby=1 %then %let rename=1;%*do not risk orignal data on fuzzby;
%if &check %then
%do;
%local _ahg_ahuige_373027 _ahg_huigea_373027 _ahg_geahui_373027 _ahg_igeahu_373027 ;
  %let _ahg_ahuige_373027=;
  %let _ahg_huigea_373027=;
  %AHGvarlist(&dsn1,into=_ahg_ahuige_373027,dlm=%str( ),global=0,print=1);
  %AHGvarlist(&dsn2,into=_ahg_huigea_373027,dlm=%str( ),global=0,print=1);
  ;
  %let _ahg_geahui_373027=%AHGremoveWords(&_ahg_ahuige_373027,&_ahg_huigea_373027 &by );
  ;
  ;
  %if not %AHGblank(&_ahg_geahui_373027) %then %let _ahg_igeahu_373027=%AHGremoveWords(&_ahg_ahuige_373027,&_ahg_geahui_373027 &by,dlm=%str( ));
  %else %let _ahg_igeahu_373027=%AHGremoveWords(&_ahg_ahuige_373027,&by,dlm=%str( ));
  ;
  %if %AHGnonblank(&_ahg_igeahu_373027) %then 
  %do;
  %put WARNING: &dsn1 has same variable names with &dsn2, They are &_ahg_igeahu_373027,
    these Variables will be removed from the left side dataset (&dsn1) before merging,
    ;
  
  %end;
%end;




%local _ahg___ahuig_373027 _ahg__ahuige_373027 _ahg_uigeah_373027 byfuzz;
;
%if &rename %then
  %do;
  %ahggettempname(_ahg___ahuig_373027,start=%sysfunc(tranwrd(%scan(&dsn1,1,%str(%()),.,_))_);
  %ahggettempname(_ahg__ahuige_373027,start=%sysfunc(tranwrd(%scan(&dsn2,1,%str(%()),.,_))_);
  %end;
%else
  %do;
  %let _ahg___ahuig_373027=&dsn1;
  %let _ahg__ahuige_373027=&dsn2;
  %end;
%if &fuzzby=1 %then
  %do;
  %do _ahg_uigeah_373027=1 %to %AHGcount(&by);
    %let byfuzz=&byfuzz %AHG_(%scan(&by,&_ahg_uigeah_373027));
  %end;  
  %AHGfuzzStr(&dsn1,&by,out =&_ahg___ahuig_373027 );
  %AHGfuzzStr(&dsn2,&by,out =&_ahg__ahuige_373027 );
  %let by=&byfuzz;
  %AHGdatasort(data =&_ahg___ahuig_373027, out =&_ahg___ahuig_373027 , by =&by );
  %AHGdatasort(data =&_ahg__ahuige_373027, out =&_ahg__ahuige_373027 , by =&by );
  %end;
%else 
  %do;
  %AHGdatasort(data =&dsn1, out =&_ahg___ahuig_373027 , by =&by );
  %AHGdatasort(data =&dsn2 , out =&_ahg__ahuige_373027 , by =&by );
  %end;
%local _ahg__huigea_373027 ;
%if %lowcase(&joinstyle)=full %then %let _ahg__huigea_373027=%str(ind1 or ind2);
%if %lowcase(&joinstyle)=matched %then %let _ahg__huigea_373027=%str(ind1 and ind2);
%if %lowcase(&joinstyle)=left %then %let _ahg__huigea_373027=%str(ind1 );
%if %lowcase(&joinstyle)=right %then %let _ahg__huigea_373027=%str(ind2 );
data &outdsn(%if &fuzzby=1 %then drop=&by;);
    merge  &_ahg___ahuig_373027(in=ind1 drop=&_ahg_igeahu_373027) &_ahg__ahuige_373027(in=ind2) ;
    by &by;
    if &_ahg__huigea_373027;
    %if &keepinvar=1 %then
    %do;
    %ahg_(ind1)=ind1;
    %ahg_(ind2)=ind2;
    %ahg_(both)=ind1 and ind2;
    %end;
run;
%AHGdatadelete(data=&_ahg___ahuig_373027 &_ahg__ahuige_373027);



%mend;
%macro AHGmergelike(dsn1,dsn2,out,by=,by1=,by2=,j=matched);
%if %AHGnonblank(&by) %then %let by1=&by;
%if %AHGnonblank(&by) %then %let by2=&by;
%local _ahg_uigeah_107836 _ahg_ahuige_107836 _ahg_huigea_107836 ;
%ahggettempname(_ahg_uigeah_107836);
%ahggettempname(_ahg_ahuige_107836);

%if %AHGeqm(&by1,&by2) %then %let _ahg_huigea_107836=&by1;


data &_ahg_uigeah_107836(drop=&_ahg_huigea_107836);
  set &dsn1;
  SDFALJPIJDASDFKJDFJKE=compbl(upcase(&by1));
run;

data &_ahg_ahuige_107836;
  set &dsn2;
  SDFALJPIJDASDFKJDFJKE=compbl(upcase(&by2));
run;

%AHGmergedsn(&_ahg_uigeah_107836,&_ahg_ahuige_107836,&out(drop=SDFALJPIJDASDFKJDFJKE),by=SDFALJPIJDASDFKJDFJKE,joinstyle=&j/*left right full matched*/);

%mend;
%macro AHGmergeprintEx(
dsns
,by=
,keep=
,drop=,
label=label
,out=mergeprintout,print=1
,prefix=ahuigecol
,clean=1
);

%local _ahg_uigeah_780805 _ahg_igeahu_780805 _ahg_huigea_780805 
;
%let _ahg_igeahu_780805=%AHGcount(&dsns);
%local %AHGwords(Printing,&_ahg_igeahu_780805)
;


%do _ahg_uigeah_780805=1 %to &_ahg_igeahu_780805;
%let printing&_ahg_uigeah_780805=;
%ahggettempname(printing&_ahg_uigeah_780805);
%end;
%do _ahg_uigeah_780805=1 %to &_ahg_igeahu_780805;
%local _ahg_ahuige_780805 ;
%let _ahg_ahuige_780805=;
%AHGvarlist(%scan(&dsns,&_ahg_uigeah_780805,%str( )),into=_ahg_ahuige_780805);
;
data &&printing&_ahg_uigeah_780805;
    set %scan(&dsns,&_ahg_uigeah_780805,%str( ))
( 
drop=&drop 
%do _ahg_huigea_780805=1 %to %AHGcount(&_ahg_ahuige_780805);
%if not %sysfunc(indexw(%upcase(&by &keep &drop),%upcase(%scan(&_ahg_ahuige_780805,&_ahg_huigea_780805))  )  )
   and  %lowcase(%scan(&_ahg_ahuige_780805,&_ahg_huigea_780805)) ne ahuigebylabel
  %then rename=(%scan(&_ahg_ahuige_780805,&_ahg_huigea_780805)=&prefix._%sysfunc(putn(&_ahg_uigeah_780805,z2.))_%sysfunc(putn(&_ahg_huigea_780805,z2.))    ) ;
%end;
);

run;

%end;
data &out;
  set &printing1;
run;
 %do _ahg_uigeah_780805=2 %to &_ahg_igeahu_780805;
%AHGmergedsn(&out,&&printing&_ahg_uigeah_780805  ,&out,by=&by,joinstyle=full/*left right full matched*/);
%end;   ;



%if &clean %then 
%do;
%AHGdatadelete(data=
%do _ahg_uigeah_780805=1 %to &_ahg_igeahu_780805;
 &&printing&_ahg_uigeah_780805  
 %end;
 );
%end;

%if &print %then
%do;
proc print &label noobs;
run;
%end;


%mend;

%macro AHGmergeprint(dsns,by=,drop=,label=label
,out=mergeprintout,print=1
,prefix=
,clean=1
,z=3
,keep=0
,length=500
);
%if %AHGblank(&prefix) %then %let prefix=%AHGrdm(9);
%local _ahg_geahui_684506 _ahg___ahuig_684506 
;
%let _ahg___ahuig_684506=%AHGcount(&dsns);
%local %AHGwords(Printing,&_ahg___ahuig_684506)
;


%do _ahg_geahui_684506=1 %to &_ahg___ahuig_684506;
%let printing&_ahg_geahui_684506=;
%ahggettempname(printing&_ahg_geahui_684506);
%end;

%local _ahg_huigea_684506 ;
%do _ahg_geahui_684506=1 %to &_ahg___ahuig_684506;
%local _ahg_ahuige_684506 _ahg_uigeah_684506 ;
%let _ahg_ahuige_684506=;
%let _ahg_uigeah_684506=;
%AHGvarlist(%scan(&dsns,&_ahg_geahui_684506,%str( )),into=_ahg_ahuige_684506);
%let _ahg_huigea_684506=&_ahg_huigea_684506 &_ahg_ahuige_684506;

%AHGallchar(%scan(&dsns,&_ahg_geahui_684506,%str( )),into=_ahg_uigeah_684506);
;
data &&printing&_ahg_geahui_684506;

%do j=1 %to %AHGcount(&_ahg_ahuige_684506);
%if %sysfunc(indexw(&_ahg_uigeah_684506,%scan(&_ahg_ahuige_684506,&j))) %then format &prefix._%sysfunc(putn(&_ahg_geahui_684506,z&z..))_%sysfunc(putn(&j,z&z..))   $&length.. ;
%else length &prefix._%sysfunc(putn(&_ahg_geahui_684506,z&z..))_%sysfunc(putn(&j,z&z..))   8;
;
%end;    

    set %scan(&dsns,&_ahg_geahui_684506,%str( ))
(
%do j=1 %to %AHGcount(&_ahg_ahuige_684506);
%if not %sysfunc(indexw(%upcase(&by),%upcase(%scan(&_ahg_ahuige_684506,&j))  )  )
   and  %lowcase(%scan(&_ahg_ahuige_684506,&j)) ne ahuigebylabel
  %then rename=(%scan(&_ahg_ahuige_684506,&j)=&prefix._%sysfunc(putn(&_ahg_geahui_684506,z&z..))_%sysfunc(putn(&j,z&z..))    ) ;
%end;
);


run;

%end;


data &out;
    merge  %do _ahg_geahui_684506=1 %to &_ahg___ahuig_684506; &&printing&_ahg_geahui_684506  %end;   ;
run;

%AHGuniq(&_ahg_huigea_684506,allvar);

%local _ahg_igeahu_684506 ;
%if not %AHGblank(&drop) %then %let _ahg_igeahu_684506=( drop= &drop) ;

%if &keep %then %AHGrenamekeep(&out,out=&out&_ahg_igeahu_684506 ,names=&_ahg_huigea_684506,keepall=0);

%if &clean %then %AHGdatadelete(data=%do _ahg_geahui_684506=1 %to &_ahg___ahuig_684506; &&printing&_ahg_geahui_684506  %end;);

%if &print %then
%do;
proc print &label noobs width=min
;
run;
%end;


%mend;

%macro AHGmergevar(dsn,out=,mode=/*3:5@7:9*/,space=" ");
  %if %AHGblank(&out) %then %let out=%AHGbasename(&dsn);
%local _ahg_ahuige_982534 
;
  %AHGalltocharnew(&dsn,out=&out,rename=AHGmv);
  %AHGvarlist(&out,into=_ahg_ahuige_982534,dlm=%str( ),global=0);
  ;


%local _ahg_huigea_982534 _ahg_uigeah_982534 _ahg_geahui_982534 _ahg___ahuig_982534 _ahg_igeahu_982534 ;
  %let _ahg_geahui_982534=%AHGcount(&mode,dlm=@); 
  %do _ahg_huigea_982534=1 %to &_ahg_geahui_982534;
    %AHGsetvarLen(&out,AHGmv%AHGscan2(&mode,&_ahg_huigea_982534,1,dlm=@,dlm2=:),$80);
    %do _ahg_uigeah_982534=%eval(1+%AHGscan2(&mode,&_ahg_huigea_982534,1,dlm=@,dlm2=:)) %to %AHGscan2(&mode,&_ahg_huigea_982534,2,dlm=@,dlm2=:);
    %let _ahg___ahuig_982534=&_ahg___ahuig_982534 AHGmv&_ahg_uigeah_982534  ;
    %end;
  %end;
  %let _ahg___ahuig_982534=drop %str( &_ahg___ahuig_982534;);
  ;

  data &out;
    set &out;
    &_ahg___ahuig_982534;
    %do _ahg_huigea_982534=1 %to &_ahg_geahui_982534;
      AHGmv%AHGscan2(&mode,&_ahg_huigea_982534,1,dlm=@,dlm2=:) =''
      %do _ahg_uigeah_982534=%AHGscan2(&mode,&_ahg_huigea_982534,1,dlm=@,dlm2=:) %to %AHGscan2(&mode,&_ahg_huigea_982534,2,dlm=@,dlm2=:);
      ||trim(left(AHGmv&_ahg_uigeah_982534)) ||&space
      %end;
      ;
    %end;
  run;
  %AHGprt;
%mend;
option nomlogic;

%macro AHGname(stats,but=);
%local _ahg_geahui_706318 _ahg_ahuige_706318 ;
%let _ahg_geahui_706318=%sysfunc(translate(%bquote(&stats),__,%str(%')%str(%")));
%let _ahg_geahui_706318=%sysfunc(compress(&_ahg_geahui_706318));
%local _ahg_uigeah_706318 _ahg_huigea_706318 _ahg_igeahu_706318 ;
%do _ahg_uigeah_706318=1 %to %length(&_ahg_geahui_706318);
%let _ahg_huigea_706318=%bquote(%substr(&_ahg_geahui_706318,&_ahg_uigeah_706318,1));
%if %SYSFUNC(NOTALNUM(%bquote(&_ahg_huigea_706318))) and not %index(&but,%bquote(&_ahg_huigea_706318)) %then %let _ahg_ahuige_706318=&_ahg_ahuige_706318._;
%else %let _ahg_ahuige_706318=&_ahg_ahuige_706318.%bquote(&_ahg_huigea_706318);
%end;
&_ahg_ahuige_706318
%mend; 
%macro AHGnobs(dsn,into=);
  %if %sysfunc(exist(&dsn))  %then
  %do;
  proc sql noprint;
  select strip(put(count(*),best.)) into :&into
  from &dsn
  ;quit;
  %end; 
  %else  %let &into=0;

%mend;
%macro AHGnonblank(str);
  not %AHGblank(&str)
%mend;
%macro AHGnt;
%index(&systcpiphostname,sddsrv)
%mend;
%macro AHGonWIN;
  %if %UPCASE(%substr(&sysscp,1,3)) =WIN  %then 1;
  %else 0;
%mend;
%macro AHGopendsn(dsn,relabel=1,justopen=0);
%if %AHGblank(&dsn) %then %let dsn=&syslast;

%if &justopen %then dm  "vt &dsn COLHEADING=NAMES " viewtable:&dsn view=form %str(;)  ;
%else 
  %do;
%local _ahg_huigea_372378 ;
  %ahggettempname(_ahg_huigea_372378,start=%AHGbarename(&dsn));
  data &_ahg_huigea_372378 %if &relabel %then (label="&dsn  Temp dataset ");;
    set &dsn;
  run;

%local _ahg_ahuige_372378 ;
  %let _ahg_ahuige_372378=0;
  %AHGnobs(sashelp.class,into=_ahg_ahuige_372378);

  %if &_ahg_ahuige_372378=0 %then 
  %do;
  %AHGdelta(msg=&dsn is  empty!);
  %end;
  %else dm  "vt &_ahg_huigea_372378 COLHEADING=labels " viewtable:&_ahg_huigea_372378 view=form   ;;
  %end;



%mend;
%macro AHGordvar(dsn,vars,out=,keepall=0);
%local _ahg_huigea_750733 ;
%ahggettempname(_ahg_huigea_750733);
%if &keepall %then
  %do;
%local _ahg_ahuige_750733 
;
  %let _ahg_ahuige_750733=;
  %ahggettempname(_ahg_ahuige_750733);  
  
  data &_ahg_ahuige_750733;
    set &dsn(drop=&vars);
  run;
  %end;
%if %AHGblank(&out) %then %let out=%AHGbasename(&dsn);
proc sql;
  create table &_ahg_huigea_750733 as
  select %AHGaddcomma(&vars)
  from  &dsn(keep=&vars)
;quit;



%if &keepall %then
%do;

data &_ahg_huigea_750733 ;
  merge &_ahg_huigea_750733 &_ahg_ahuige_750733;
run;
%end;
%else 
%do;
data &out;
  set &_ahg_huigea_750733;
run;
%end;


%mend;
%macro AHGpmByline(mac,dlm=`);
%local _ahg_ahuige_804000 _ahg_huigea_804000 ;
%let _ahg_ahuige_804000=%AHGcount(%superq(&mac),dlm=&dlm);
%do _ahg_huigea_804000=1 %to &_ahg_ahuige_804000;
%put %scan(%superq(&mac),&_ahg_huigea_804000,&dlm);
%end;
%mend;
%macro AHGpmlike(Ms,start=1);
%local _ahg_geahui_204081 _ahg_uigeah_204081 _ahg_igeahu_204081 ;
   %let ms=%upcase(&ms);
   %do _ahg_igeahu_204081=1 %to %AHGcount(&ms);
       %let _ahg_geahui_204081=%scan(&ms,&_ahg_igeahu_204081,%str( ));  
       %let _ahg_uigeah_204081=;
       proc sql noprint;
        select name into :_ahg_uigeah_204081 separated by ' '
        from sashelp.vmacro
        %if &start eq 1 %then where upcase(name) like  "&_ahg_geahui_204081%";
        %else where upcase(name) like "%"||"&_ahg_geahui_204081%";

        order by name
        ;quit;
      %if &start=1 %then %AHGsortnum(&_ahg_uigeah_204081,into=_ahg_uigeah_204081);
%local _ahg_ahuige_204081 _ahg_huigea_204081 ;
      %do _ahg_ahuige_204081=1 %to %AHGcount(&_ahg_uigeah_204081);
        %let _ahg_huigea_204081=%scan(&_ahg_uigeah_204081,&_ahg_ahuige_204081,%str( ));
        %put &_ahg_huigea_204081=&&&_ahg_huigea_204081;
      %end;
  %end;
%mend;

%macro AHGpm(Ms);
%local _ahg_huigea_130511 _ahg_ahuige_130511 ;
  %do _ahg_huigea_130511=1 %to %AHGcount(&ms);
    %let _ahg_ahuige_130511=%scan(&ms,&_ahg_huigea_130511,%str( ));
    %put &_ahg_ahuige_130511=&&&_ahg_ahuige_130511;
  %end;
%mend;

%macro AHGpop(arrname,mac,dlm=%str( ),global=0);
  %if &global %then %global &mac;
%local _ahg_ahuige_42021 ;
  %let _ahg_ahuige_42021=%sysfunc(reverse(%str(&&&arrname)));
  %AHGleft(stack,&mac,dlm=&dlm);
  %let  &arrname=%sysfunc(reverse(%str(&_ahg_ahuige_42021)));
  %let  &mac=%sysfunc(reverse(%str(&&&mac)));


%mend;

%macro AHGpos(string,word);
  %let string=%upcase(&string);
  %let word=%upcase(&word);
  %index(&string,&word)
%mend;
%macro AHGprocMeansby(dsn,var,by,out=stats,print=0,alpha=0.05
,stats=n mean median  min max
,split=\
,orie=
,labels=
,left=left
,statord=
);


%if &orie=vert and %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);
%if %AHGblank(&labels) %then %let labels=%sysfunc(translate(&stats,__,\.));
%if not %AHGblank(&labels) and %index(&labels,@)=0 %then %let labels=%AHGaddcomma(&labels,comma=@);

%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local _ahg_ahuige_776673 ;
%let _ahg_ahuige_776673=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local _ahg__igeahu_776673 _ahg___ahuig_776673 %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(IsStat,20);
%local _ahg____ahuig_776673 _ahg_geahui_776673 _ahg__uigeah_776673 ;

%let _ahg_geahui_776673=&dsn;


%do _ahg____ahuig_776673=1 %to %AHGcount(&stats);
  %let _ahg___ahuig_776673=%scan(&stats,&_ahg____ahuig_776673,%str( ));
  %let isStat&_ahg____ahuig_776673=0;
  %if not  (%index(&_ahg___ahuig_776673,%str(%")) or %index(&_ahg___ahuig_776673,%str(%'))) %then
  %do;
  %let isStat&_ahg____ahuig_776673=1;
  %let _ahg__uigeah_776673=&_ahg__uigeah_776673 &_ahg___ahuig_776673 ;  /*mystats are real stats*/
  %end;
%end;


%AHGsetstatfmt(statfmt=&_ahg__uigeah_776673);
%let _ahg__igeahu_776673=%AHGcount(&stats);

%do _ahg____ahuig_776673=1 %to &_ahg__igeahu_776673;
  %let _ahg___ahuig_776673=%scan(&stats,&_ahg____ahuig_776673,%str( ));
  %let mystat&_ahg____ahuig_776673=%scan(&_ahg___ahuig_776673,1,&split);
  %let myformat&_ahg____ahuig_776673=%scan(&_ahg___ahuig_776673,2,&split);
  %if %AHGblank(&&myformat&_ahg____ahuig_776673) and %str(&&isstat&_ahg____ahuig_776673) %then %let myformat&_ahg____ahuig_776673=&&&&formatof&&mystat&_ahg____ahuig_776673;
  %if &&isstat&_ahg____ahuig_776673 %then ;
%end;


  proc means data=&_ahg_geahui_776673 noprint alpha=&alpha;;
    var &var;
    by &by;
    output out=&out 
    %do _ahg____ahuig_776673=1 %to  &_ahg__igeahu_776673;
    %if &&isstat&_ahg____ahuig_776673 %then &&mystat&_ahg____ahuig_776673%str(=)&&mystat&_ahg____ahuig_776673;
    %end;
    ;
  run;


  


  proc sql noprint;
    create table old&out as
    select
       ' '
    %do _ahg____ahuig_776673=1 %to  %AHGcount(&stats);
      %if &&isstat&_ahg____ahuig_776673 %then ,&left(put(&&mystat&_ahg____ahuig_776673, &&myformat&_ahg____ahuig_776673)) as  &&mystat&_ahg____ahuig_776673 ;
      %else  ,&&mystat&_ahg____ahuig_776673;
    %end;
    ,%AHGaddcomma(&by)
    from &out
    ;quit;


%AHGrelabel(old&out,out=&out,pos=,labels=blank@&labels@%AHGaddcomma(&by,comma=@));

%if &orie=vert %then  
%do;

%local _ahg_huigea_776673 _ahg___ahuige_776673 _ahg__ahuige_776673 _ahg_uigeah_776673 ;
%AHGvarlist(&out,into=_ahg_huigea_776673,dlm=%str( ),global=0);
%local _ahg__huigea_776673 _ahg__geahui_776673 ;
%let _ahg__geahui_776673=1;
%let _ahg___ahuige_776673=%AHGcount(&_ahg_ahuige_776673,dlm=@);
;
%do _ahg____ahuig_776673=1 %to &_ahg___ahuige_776673;
  %let _ahg__ahuige_776673=%scan(&_ahg_ahuige_776673,&_ahg____ahuig_776673,@);
  %do _ahg__huigea_776673=1 %to %AHGcount(&_ahg__ahuige_776673);
  %let _ahg__geahui_776673=%eval(&_ahg__geahui_776673+1);
  %if &_ahg__huigea_776673=1 %then %let _ahg_uigeah_776673= &_ahg_uigeah_776673   %str(theVerticalvar&_ahg____ahuig_776673=) %scan(&_ahg_huigea_776673,&_ahg__geahui_776673);
  %else  %let _ahg_uigeah_776673= &_ahg_uigeah_776673 ||'  '|| %scan(&_ahg_huigea_776673,&_ahg__geahui_776673);
  %if &_ahg__huigea_776673=%AHGcount(&_ahg__ahuige_776673) %then  %let _ahg_uigeah_776673= &_ahg_uigeah_776673 %str(;);
  %end;
%end;

%local _ahg_igeahu_776673 ;
%ahggettempname(_ahg_igeahu_776673);

data &_ahg_igeahu_776673;
  set &out;
  keep &by %do _ahg____ahuig_776673=1 %to  &_ahg___ahuige_776673; theVerticalvar&_ahg____ahuig_776673  %end;  ;
    %unquote(&_ahg_uigeah_776673);
run;

data hori&out;
  set &out;
run;

data &out;
  set &_ahg_igeahu_776673;
  keep &by  
  %if not %AHGblank(&labels) %then label; 
  %if not %AHGblank(&statord) %then &statord;
  stat;
  array allvar(1:&_ahg___ahuige_776673) theVerticalvar1-theVerticalvar&_ahg___ahuige_776673;
  do i=1 to dim(allvar);
  %if not %AHGblank(&labels) %then label=scan("&labels",i,'@');;
  %if not %AHGblank(&statord) %then &statord=i; ;
  stat=input(allvar(i),$50.);

  output;
  end;

  
run;

%AHGdatadelete(data=&_ahg_igeahu_776673);




%end;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;
%macro AHGprocMeans(dsn,var,out=stats,print=0,alpha=0.05
,stats=n mean median  min max /*n @ min '-' max*/
,split=\
,orie=
,labels=
,left=left
,statord=
);
%if &orie=vert and %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);
%if not %AHGblank(&labels) and %index(&labels,@)=0 %then %let labels=%AHGaddcomma(&labels,comma=@);

%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local _ahg_ahuige_757062 ;
%let _ahg_ahuige_757062=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local _ahg__igeahu_757062 _ahg___ahuig_757062 %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(IsStat,20);
%local _ahg____ahuig_757062 _ahg_geahui_757062 _ahg__uigeah_757062 ;

%let _ahg_geahui_757062=&dsn;


%do _ahg____ahuig_757062=1 %to %AHGcount(&stats);
  %let _ahg___ahuig_757062=%scan(&stats,&_ahg____ahuig_757062,%str( ));
  %let isStat&_ahg____ahuig_757062=0;
  %if not  (%index(&_ahg___ahuig_757062,%str(%")) or %index(&_ahg___ahuig_757062,%str(%'))) %then
  %do;
  %let isStat&_ahg____ahuig_757062=1;
  %let _ahg__uigeah_757062=&_ahg__uigeah_757062 &_ahg___ahuig_757062 ;  /*mystats are real stats*/
  %end;
%end;


%AHGsetstatfmt(statfmt=&_ahg__uigeah_757062);
%let _ahg__igeahu_757062=%AHGcount(&stats);

%do _ahg____ahuig_757062=1 %to &_ahg__igeahu_757062;
  %let _ahg___ahuig_757062=%scan(&stats,&_ahg____ahuig_757062,%str( ));
  %let mystat&_ahg____ahuig_757062=%scan(&_ahg___ahuig_757062,1,&split);
  %let myformat&_ahg____ahuig_757062=%scan(&_ahg___ahuig_757062,2,&split);
  %if %AHGblank(&&myformat&_ahg____ahuig_757062) and %str(&&isstat&_ahg____ahuig_757062) %then %let myformat&_ahg____ahuig_757062=&&&&formatof&&mystat&_ahg____ahuig_757062;
  %if &&isstat&_ahg____ahuig_757062 %then ;
%end;


  proc means data=&_ahg_geahui_757062 noprint %if not %AHGblank(&alpha) %then alpha=&alpha;;
    var &var;
    output out=&out.s 
    %do _ahg____ahuig_757062=1 %to  &_ahg__igeahu_757062;
    %if &&isstat&_ahg____ahuig_757062 %then &&mystat&_ahg____ahuig_757062%str(=)&&mystat&_ahg____ahuig_757062;
    %end;
    ;
  run;


  


  proc sql;
    create table &out as
    select
        ' '
    %do _ahg____ahuig_757062=1 %to  %AHGcount(&stats);
      %if &&isstat&_ahg____ahuig_757062 %then ,&left(put(&&mystat&_ahg____ahuig_757062, &&myformat&_ahg____ahuig_757062)) as  &&mystat&_ahg____ahuig_757062 ;
      %else  ,&&mystat&_ahg____ahuig_757062;
    %end;
    from &out.s
    ;quit;


%AHGrelabel(&out,out=&out,pos=,labels=Label@&labels);

%if &orie=vert %then  
%do;

%local _ahg_huigea_757062 _ahg___ahuige_757062 _ahg__ahuige_757062 _ahg_uigeah_757062 ;
%AHGvarlist(&out,into=_ahg_huigea_757062,dlm=%str( ),global=0);
%local _ahg__huigea_757062 _ahg__geahui_757062 ;
%let _ahg__geahui_757062=1;
%let _ahg___ahuige_757062=%AHGcount(&_ahg_ahuige_757062,dlm=@);
;
%do _ahg____ahuig_757062=1 %to &_ahg___ahuige_757062;
  %let _ahg__ahuige_757062=%scan(&_ahg_ahuige_757062,&_ahg____ahuig_757062,@);
  %do _ahg__huigea_757062=1 %to %AHGcount(&_ahg__ahuige_757062);
  %let _ahg__geahui_757062=%eval(&_ahg__geahui_757062+1);
  %if &_ahg__huigea_757062=1 %then %let _ahg_uigeah_757062= &_ahg_uigeah_757062   %str(theVerticalvar&_ahg____ahuig_757062=) %scan(&_ahg_huigea_757062,&_ahg__geahui_757062);
  %else  %let _ahg_uigeah_757062= &_ahg_uigeah_757062 ||'  '|| %scan(&_ahg_huigea_757062,&_ahg__geahui_757062);
  %if &_ahg__huigea_757062=%AHGcount(&_ahg__ahuige_757062) %then  %let _ahg_uigeah_757062= &_ahg_uigeah_757062 %str(;);
  %end;
%end;

%local _ahg_igeahu_757062 ;
%ahggettempname(_ahg_igeahu_757062);

data &_ahg_igeahu_757062;
  set &out;
  keep  %do _ahg____ahuig_757062=1 %to  &_ahg___ahuige_757062; theVerticalvar&_ahg____ahuig_757062  %end;  ;
    %unquote(&_ahg_uigeah_757062);
run;

data hori&out;
  set &out;
run;

data &out;
  set &_ahg_igeahu_757062;
  keep 
  %if not %AHGblank(&labels) %then label; 
  %if not %AHGblank(&statord) %then &statord;
  stat;
  array allvar(1:&_ahg___ahuige_757062) theVerticalvar1-theVerticalvar&_ahg___ahuige_757062;
  do i=1 to dim(allvar);
  %if not %AHGblank(&labels) %then label=scan("&labels",i,'@');;
  %if not %AHGblank(&statord) %then &statord=i; ;
  stat=input(allvar(i),$50.);
  output;
  end;

  
run;

%AHGdatadelete(data=&_ahg_igeahu_757062);




%end;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;

%macro AHGprop(dsn,var,prop);
%local _ahg_uigeah_115063 _ahg_ahuige_115063 _ahg_huigea_115063 _ahg_igeahu_115063 ;

%if %sysfunc(exist(&dsn)) %then
%do;


%let _ahg_ahuige_115063=%sysfunc(open(&dsn,i));

%do _ahg_uigeah_115063=1 %to  %sysfunc(attrn(&_ahg_ahuige_115063,nvars));
   %if %AHGeqm(&var,%sysfunc(varname(&_ahg_ahuige_115063,&_ahg_uigeah_115063))) %then 
     %do;
     %if %AHGeqm(&prop,format) %then  %let _ahg_huigea_115063=%sysfunc(varfmt(&_ahg_ahuige_115063,&_ahg_uigeah_115063));
     %else %if %AHGeqm(&prop,label) %then  %let _ahg_huigea_115063=%sysfunc(varlabel(&_ahg_ahuige_115063,&_ahg_uigeah_115063));
     %else %if %AHGeqm(&prop,length) %then  
        %do;
        %let _ahg_huigea_115063=%sysfunc(varlength(&_ahg_ahuige_115063,&_ahg_uigeah_115063));
        %if %sysfunc(vartype(&_ahg_ahuige_115063,&_ahg_uigeah_115063))=C %then %let _ahg_huigea_115063=$&_ahg_huigea_115063;
        %end;
     %end;
      
%end;
%let _ahg_igeahu_115063=%sysfunc(close(&_ahg_ahuige_115063));
%end;
&_ahg_huigea_115063
%mend;
%macro AHGprt(dsn=_last_,label=label);
proc print data=&dsn noobs &label;run;
%mend;
%macro AHGpureName(dsn);
  %if %index(&dsn,%str(%()) %then %scan(&dsn,1,%str(%());
  %else &dsn;
%mend;
%macro AHGpush(arrname,value,dlm=%str( ));
  %if %AHGcount(&&&arrname,dlm=&dlm) >0 %then   %let &arrname=&&&arrname&dlm&value;
  %else %let &arrname=&value;
%mend;
%macro ahgputc(var,fmt);
%if %AHGblank(&fmt) %then %let fmt=best.;
input(left(&var),&fmt)
%mend;
%macro ahgputn(var,fmt);
%if %AHGblank(&fmt) %then %let fmt=best.;
left(put(&var,&fmt))
%mend;
%macro AHGqcount(str,dlm=%str(%'));
%local _ahg_geahui_818550 _ahg_huigea_818550 _ahg_uigeah_818550 _ahg_ahuige_818550 _ahg___ahuig_818550 _ahg_igeahu_818550 ;
%let _ahg_huigea_818550=0;   
%let str=&str%str( );
%do _ahg_geahui_818550=1 %to %length(&str);
  %if &_ahg_ahuige_818550=b and %qsubstr(&str,&_ahg_geahui_818550,1) eq %then %let _ahg___ahuig_818550=1;
  %if &_ahg_ahuige_818550=q and %qsubstr(&str,&_ahg_geahui_818550,1) eq &dlm %then %let _ahg___ahuig_818550=1;
  %if &_ahg___ahuig_818550=1 %then 
    %do;
    %let _ahg_ahuige_818550=;
    %let _ahg_uigeah_818550=&_ahg_uigeah_818550%qsubstr(&str,&_ahg_geahui_818550,1);
    %ahgincr(_ahg_huigea_818550)
    %let _ahg___ahuig_818550=0;
    %end;
  %else 
    %do;
    %if %AHGblank(&_ahg_ahuige_818550) and %qsubstr(&str,&_ahg_geahui_818550,1) eq &dlm %then %let _ahg_ahuige_818550=q  ;
    %else %if %AHGblank(&_ahg_ahuige_818550) and %qsubstr(&str,&_ahg_geahui_818550,1) gt %then %let _ahg_ahuige_818550=b  ;
    %let _ahg_uigeah_818550=&_ahg_uigeah_818550%qsubstr(&str,&_ahg_geahui_818550,1) ;
    %end;
%end;
&_ahg_huigea_818550
%mend;






%macro AHGqscan(str,n,dlm=%str(%'));
%local _ahg_geahui_715884 _ahg_huigea_715884 _ahg_uigeah_715884 _ahg_ahuige_715884 _ahg___ahuig_715884 _ahg_igeahu_715884 ;
%let _ahg_huigea_715884=0;   
%let str=&str%str( );
%do _ahg_geahui_715884=1 %to %length(&str);
  %if &_ahg_ahuige_715884=b and %qsubstr(&str,&_ahg_geahui_715884,1) eq %then %let _ahg___ahuig_715884=1;
  %if &_ahg_ahuige_715884=q and %qsubstr(&str,&_ahg_geahui_715884,1) eq &dlm %then %let _ahg___ahuig_715884=1;
  %if &_ahg___ahuig_715884=1 %then 
    %do;
    %let _ahg_ahuige_715884=;
    %let _ahg_uigeah_715884=&_ahg_uigeah_715884%qsubstr(&str,&_ahg_geahui_715884,1);
    %ahgincr(_ahg_huigea_715884)
    %if &_ahg_huigea_715884=&n %then &_ahg_uigeah_715884;
    %let _ahg_uigeah_715884=;
    %let _ahg___ahuig_715884=0;
    %end;
  %else 
    %do;
    %if %AHGblank(&_ahg_ahuige_715884) and %qsubstr(&str,&_ahg_geahui_715884,1) eq &dlm %then %let _ahg_ahuige_715884=q  ;
    %else %if %AHGblank(&_ahg_ahuige_715884) and %qsubstr(&str,&_ahg_geahui_715884,1) gt %then %let _ahg_ahuige_715884=b  ;
    %let _ahg_uigeah_715884=&_ahg_uigeah_715884%qsubstr(&str,&_ahg_geahui_715884,1) ;
    %end;
%end;
%mend;






%macro AHGrandom;
%local _ahg_ahuige_58843 
;
  %let _ahg_ahuige_58843=%sysfunc(normal(0));
  %let _ahg_ahuige_58843=%sysfunc(translate(&_ahg_ahuige_58843,00,.-));
  &_ahg_ahuige_58843
  %put random=&_ahg_ahuige_58843;
%mend;
%macro ahgrank(dsn,var,_n_=);
data _null_;
  set &dsn;
  %if %AHGnonblank(&_n_) %then
  %do;
  if _n_ in (&_n_) then 
  %end;
  do asfdklasjf=1 to length(&var);
  char=substr(&var,asfdklasjf,1);
  rank=rank(char);
  put _n_= char= rank=;
  end;
  
run;
%mend;
%macro AHGrdm(length,seed=0);
%local _ahg_ahuige_205158 _ahg_huigea_205158 ;
%if %AHGblank(&length) %then %let length=6;
%do _ahg_ahuige_205158=1 %to &length;
  %let _ahg_huigea_205158=&_ahg_huigea_205158%sysfunc(byte(%sysevalf(65+%substr(%sysevalf(%sysfunc(ranuni(&seed))*24),1,2))) ); 
%end;
&_ahg_huigea_205158
%mend;
%macro AHGreadline(file=,out=readlineout);
data &out;
  filename inf   "&file" ;
  infile inf truncover;;
  format  line $char800.;
  input line 1-800  ;
run;
%mend;
%macro AHGreLabel(dsn,out=,pos=,labels=,dlm=@);
  %if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%local _ahg_ahuige_414201 ;
  %if %AHGblank(&pos) %then %let pos=%AHGwords(,%AHGcount(&labels,dlm=&dlm));
  %AHGvarlist(&dsn,into=_ahg_ahuige_414201,dlm=%str( ),global=0);
  ;
  data &out;
    set &dsn;
%local _ahg_huigea_414201 ;
    label
    %do _ahg_huigea_414201=1 %to %AHGcount(&pos);
    %scan(&_ahg_ahuige_414201, %scan(&pos,&_ahg_huigea_414201))="%scan( &labels,&_ahg_huigea_414201,&dlm)"
    %end;
    ;
  run;
%mend;

%macro AHGremovewords(sentence,words,dlm=%str( ));
%local _ahg_ahuige_160958 _ahg_huigea_160958 _ahg___ahuig_160958 _ahg__ahuige_160958 _ahg_igeahu_160958 _ahg_uigeah_160958 _ahg_geahui_160958 
;
  %let sentence=%bquote(&sentence);
  %let words=%bquote(&words);
  %let _ahg___ahuig_160958=%AHGcount(&sentence,dlm=&dlm);
  %let _ahg__ahuige_160958=%AHGcount(&words,dlm=&dlm);

  %let _ahg_igeahu_160958=&dlm;
  %do _ahg_ahuige_160958=1 %to &_ahg___ahuig_160958;
    %let _ahg_uigeah_160958=0;
    %let _ahg_geahui_160958=%scan(&sentence, &_ahg_ahuige_160958,&dlm);
    %let _ahg_huigea_160958=0;
    %do %until (&_ahg_huigea_160958=&_ahg__ahuige_160958 or &_ahg_uigeah_160958) ;
        %ahgincr(_ahg_huigea_160958)
  
      %if %upcase(&_ahg_geahui_160958)= %upcase(%scan(&words, &_ahg_huigea_160958,&dlm)) %then %let _ahg_uigeah_160958=1;
    %end;
    %if &_ahg_uigeah_160958=0 %then %let _ahg_igeahu_160958=&_ahg_igeahu_160958&dlm&_ahg_geahui_160958;
  %end;
  %let _ahg_igeahu_160958=%sysfunc(tranwrd(&_ahg_igeahu_160958,&dlm&dlm,));
   &_ahg_igeahu_160958
%mend;
%macro AHGrenamedsn(dsn,out);
%if not %sysfunc(exist(&out)) %then
  %do;
  %if not %index(&dsn,.) %then %let dsn=work.&dsn;
%local _ahg_uigeah_361077 _ahg_huigea_361077 _ahg_ahuige_361077 ;
  %let _ahg_uigeah_361077=%scan(&dsn,1);
  %let _ahg_huigea_361077=%scan(&dsn,2);
  proc datasets library=&_ahg_uigeah_361077;
     change &_ahg_huigea_361077=%scan(&out,%AHGcount(&out,dlm=.));
  run;
  %end;
%else 
  %do; data %scan(&out,%AHGcount(&out,dlm=.));set &dsn;run;  %end; 


%mend;
%macro AHGrenamekeep(dsn,out=,pos=,names=,prefix=col,keepall=0);
  %if %AHGblank(&names) %then %let names=%AHGwords(&prefix,400);
  %if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%local _ahg_ahuige_127434 _ahg_huigea_127434 ;
  %AHGvarlist(&dsn,into=_ahg_ahuige_127434,dlm=%str( ),global=0);
  %let _ahg_huigea_127434=%sysfunc(min(%AHGcount(&_ahg_ahuige_127434),%AHGcount(&names)));
  
  %if %AHGblank(&pos) %then %let pos=%AHGwords(%str( ),&_ahg_huigea_127434);
  ;
 
  data &out;
    set &dsn;
%local _ahg_uigeah_127434 ;
    %if not &keepall %then
      %do;
      keep
      %do _ahg_uigeah_127434=1 %to &_ahg_huigea_127434;
        %scan(&_ahg_ahuige_127434, %scan(&pos,&_ahg_uigeah_127434))
      %end;
      ;
      %end;
    rename
    %do _ahg_uigeah_127434=1 %to &_ahg_huigea_127434;
    %scan(&_ahg_ahuige_127434, %scan(&pos,&_ahg_uigeah_127434))=%scan( &names,&_ahg_uigeah_127434)
    %end;
    ;
  run;
%mend;
%macro AHGrenameVar(dsn,out=,names=,prefix=AHG  ,zero=0);
  %if %AHGblank(&out) %then %let out=%AHGbasename(&dsn);
%local _ahg_huigea_374206 _ahg_uigeah_374206 _ahg_ahuige_374206 ;
  
  %AHGvarlist(&dsn,into=_ahg_ahuige_374206,dlm=%str( ),global=0);
  proc sql noprint;
  create table &out as
  select 
  %do _ahg_huigea_374206=1 %to %AHGcount(&_ahg_ahuige_374206); 
    
    %if &zero>0 %then %let _ahg_uigeah_374206=%AHGzero(&_ahg_huigea_374206,z&zero..);
    %else %let _ahg_uigeah_374206=&_ahg_huigea_374206;

    %if &_ahg_huigea_374206 ne 1 %then ,;

    %if %AHGblank(&names) %then %scan(&_ahg_ahuige_374206,&_ahg_huigea_374206) as &prefix._&_ahg_uigeah_374206 ;
    %else %if not %AHGblank(%scan(&names,&_ahg_huigea_374206)) %then %scan(&_ahg_ahuige_374206,&_ahg_huigea_374206) as  %scan(&names,&_ahg_huigea_374206)  ;
    %else   %scan(&_ahg_ahuige_374206,&_ahg_huigea_374206) ;
  %end;

  from &dsn
  ;
  quit;
%mend;
%macro AHGreportby(dsn,by,ls=123,ps=45,flow=flow,widthmax=50,which=,
whichlength=,sort=0,groupby=0,groupto=0,topline=,showby=0,
option=nowd nocenter headline,labelopt=%str(option label;));
%local _ahg__ahuige_61599 ;
  %if %AHGblank(&by) %then %let by=0; 
  %ahggettempname(_ahg__ahuige_61599);
  data &_ahg__ahuige_61599;
  %if &by=0 %then
  %do;
  ahuige34xbege5435='_';
  %let by=ahuige34xbege5435;
  %let showby=0;
  %end;
  set &dsn;
run;


%local _ahg_uigeah_61599 _ahg_ahuige_61599 _ahg_huigea_61599 ;
  &labelopt;
  %if &sort %then
  %do;
  proc sort data=&_ahg__ahuige_61599 ; by &by;run;
  %end;
  %AHGvarlist(&_ahg__ahuige_61599,into=_ahg_ahuige_61599,dlm=%str( ),global=0);

  %AHGvarinfo(&_ahg__ahuige_61599,out=varinfo34589,info= name  length);


%local _ahg__igeahu_61599 ;
  %AHGcolumn2mac(varinfo34589,infostr,name length);
%local _ahg___ahuig_61599 ;
  %let _ahg___ahuig_61599=%AHGrandom;
  %AHGcreatehashex(my&_ahg___ahuig_61599.hash,&_ahg__igeahu_61599);
  %put #####################;
  %let _ahg_huigea_61599=%AHGremoveWords(&_ahg_ahuige_61599,&by,dlm=%str( ));
  &labelopt;
  
  proc report data=&_ahg__ahuige_61599 &option ;
    column
    %if %AHGblank(&topline) %then  &by &_ahg_huigea_61599;
    %else %if %index( %bquote(&topline) , %str( %( )    ) %then &topline;
    %else ( &topline &by &_ahg_huigea_61599 );
    ;
    %do _ahg_uigeah_61599=1 %to  %AHGcount(&by);
    %if &showby %then
    %do;
    define %scan(&by,&_ahg_uigeah_61599)/order
    %if not &groupby %then display &flow;
    %else group;
    %end;
    %else  define %scan(&by,&_ahg_uigeah_61599)/order noprint;

    
    ;
    %end;
%local _ahg__uigeah_61599 ;
    %let _ahg__uigeah_61599=0;
    %do _ahg_uigeah_61599=1 %to %AHGcount(&_ahg_huigea_61599);
%local _ahg_igeahu_61599 ;
%local _ahg__huigea_61599 _ahg_geahui_61599 ;
    %let _ahg__huigea_61599=%scan(&_ahg_huigea_61599,&_ahg_uigeah_61599);
    %let _ahg_igeahu_61599=%AHGhashvalue(my&_ahg___ahuig_61599.hash,&_ahg__huigea_61599);
    %let _ahg_igeahu_61599=%sysfunc(min(&widthmax,%sysfunc(max(&_ahg_igeahu_61599,%length(&_ahg__huigea_61599)))));
    define  %scan(&_ahg_huigea_61599,&_ahg_uigeah_61599)  /
        %if %sysfunc(indexw(&which,&_ahg_uigeah_61599))  %then %do;%let _ahg__uigeah_61599=%eval(&_ahg__uigeah_61599+1);width=%scan(&whichlength,%AHGindex(&which,&_ahg_uigeah_61599))   %end;
    %else %str(width=)&_ahg_igeahu_61599;
        %if &_ahg_uigeah_61599<=&groupto %then group;
        %else display &flow;
          ;
    %end;
    by &by;


  run;
  
%mend;
%macro AHGreportwithformat(dsn,fmts=,groupvar=,split=#,order=data);
%local _ahg_ahuige_409163 _ahg_igeahu_409163 _ahg_huigea_409163 _ahg_uigeah_409163 ;
%local  %AHGwords(defstr,50)
;
  %AHGvarlist(&dsn,into=_ahg_ahuige_409163,dlm=%str( ),global=0);
  %ahggettempname(_ahg_igeahu_409163);
  %AHGvarinfo(&dsn,out=&_ahg_igeahu_409163,info=name type length num);
  data _null_;
    set &_ahg_igeahu_409163;
    format defstr $500.;
    group='       ';
    %do _ahg_uigeah_409163=1 %to %AHGcount(&fmts);
    if _n_=scan(scan("&fmts",&_ahg_uigeah_409163,' '),1,'\') then length=scan(scan("&fmts",&_ahg_uigeah_409163,' '),2,'\') ;
    %end;
    %do _ahg_uigeah_409163=1 %to %AHGcount(&groupvar);
    if %AHGequaltext("%scan(&groupvar,&_ahg_uigeah_409163)",name) then group=' group ';
    %end;
    defstr='define '||name||' /display width='||left(length)||group||" flow order=&order;";
    put defstr;
    call symput('defstr'||left(_n_),defstr);
    call symput('_ahg_huigea_409163',_n_);
  run;

  proc report data=&dsn nowindows headline split="&split" ;
    column &_ahg_ahuige_409163;
    %do _ahg_uigeah_409163=1 %to &_ahg_huigea_409163;
    &&defstr&_ahg_uigeah_409163
    %end;
  run;
%mend;
%macro AHGrPi(i);
%local _ahg_ahuige_284063 ;
%let _ahg_ahuige_284063=301415926535897;
%substr(&_ahg_ahuige_284063,%eval(%sysfunc(mod(&i,15))+1),1)
%mend;
%macro AHGRTFtotxt(rtf,out,txt,ptn=\b\f2\fs16,tailor=1);
  %if %AHGblank(&txt) %then %let txt=%tempdir\____SDD_OUTPUT.txt;
%local _ahg_huigea_86430 _ahg_uigeah_86430 _ahg_ahuige_86430 ;
  %let _ahg_uigeah_86430='_____________________________________________';
  %if not %AHGblank(&out) %then %let _ahg_huigea_86430=&out;
  %else %ahggettempname(_ahg_huigea_86430);
  %AHGreadline(file=&rtf,out=&_ahg_huigea_86430);
data &_ahg_huigea_86430;
    set &_ahg_huigea_86430;
    drop one newline delete i;
    line=tranwrd(line,'\\','\');
    if prxmatch( '/.*\\b\\f\d\d\\fs\d\d(.*)/',line) then    line=prxchange('s/.*\\b\\f\d\d\\fs\d\d(.*)/\1/',-1,line);
    else return;
    output;
    return;
run;


  %if &tailor %then
  %do;
  data tailored foot;
    set &_ahg_huigea_86430;
    if index(line,'\page') then newpage=1;
    retain newpage 0;
    if index(line,&_ahg_uigeah_86430) then linecount+1;
    if linecount<3 then output tailored;
    else if mod(linecount,3)=2 and not missing(line) and not index(line,&_ahg_uigeah_86430) then output tailored;
    keep line;
    if linecount=3 and not newpage then output foot;
  run;

  data &_ahg_huigea_86430;
    set tailored foot;
    file "&txt";
    put line;
  run;
  %end;



%mend;



%macro AHG_(name);
%substr(md5_&name%sysfunc(md5(%lowcase(&name)),$hex32.),1,24)
%mend;

%macro AHGscan2(mac,i,j,dlm=\,dlm2=#);
  %scan(%scan(&mac,&i,&dlm),&j,&dlm2)
%mend;
%macro AHGscanDim(str,dimNum,by=2,dlm=%str( ));
  %scan(&str,%eval(( &dimnum-1)*&by +1)) %scan(&str,%eval(( &dimnum-1)*&by +2)) %scan(&str,%eval(( &dimnum-1)*&by +3))
%mend;
%macro AHGscansub(line,Num,dlm=%str( ));
    %put cnt=%AHGcount(&line,dlm=&dlm);
    %do i=1 %to &num;
        %scan(%bquote(&line),&i,&dlm)&dlm
    %end;
%mend;


%macro AHGscanSubstr(words,from,num,dlm1st=0,dlm=%str( ),compress=0/*right*/);
%local _ahg_ahuige_955487 _ahg_huigea_955487 ;
  %let _ahg_huigea_955487=;
  %do _ahg_ahuige_955487=0 %to %eval(&num-1);
    %if &_ahg_ahuige_955487 gt &dlm1st %then %let _ahg_huigea_955487=&_ahg_huigea_955487&dlm;
    %let _ahg_huigea_955487=&_ahg_huigea_955487%scan(&words,%eval(&_ahg_ahuige_955487+&from),&dlm);
  %end;
  %if &compress %then %let _ahg_huigea_955487=%sysfunc(compress(&_ahg_huigea_955487));
  &_ahg_huigea_955487
%mend;
%macro AHGsethashvalue(hashid,handle,value);
%local _ahg_ahuige_421419 _ahg_huigea_421419 ;
  %let indx=%AHGindex(&&&hashid.list,&handle);
  %let &hashid&indx=&value;
  &_ahg_huigea_421419
%mend;
%macro AHGsetprintEX(dsns,out=setprint,print=0,prefix=,by=,ord=,keep=0,length=500,displayname=0);
option mprint;
%local _ahg_igeahu_113896 _ahg__igeahu_113896 _ahg_huigea_113896 _ahg_geahui_113896 _ahg__ahuige_113896 ;
%if %AHGblank(&prefix) %then %let prefix=%AHGrdm(9);
%let _ahg__igeahu_113896=%AHGcount(&dsns);
%local _ahg_uigeah_113896 _ahg_ahuige_113896 ;
%do _ahg_igeahu_113896=1 %to  &_ahg__igeahu_113896;
  %let _ahg_huigea_113896=%scan(&dsns,&_ahg_igeahu_113896,%str( ));
  %ahggettempname(_ahg_geahui_113896);
  %if  &displayname  %then
    %do;
%local _ahg__huigea_113896 _ahg__uigeah_113896 _ahg_ahuige_113896 
;
    %ahggettempname(_ahg__huigea_113896);
    %let _ahg_ahuige_113896=;
    %AHGvarlist(&_ahg_huigea_113896,into=_ahg_ahuige_113896,dlm=%str( ),global=0);
    data &_ahg__huigea_113896;
%local _ahg___ahuig_113896 ;
      %do _ahg___ahuig_113896=1 %to %ahgcount(&_ahg_ahuige_113896);
      %scan(&_ahg_ahuige_113896,&_ahg___ahuig_113896)="%scan(&_ahg_ahuige_113896,&_ahg___ahuig_113896)";

      %end;
    run;

    %AHGsetprint(&_ahg__huigea_113896 &_ahg_huigea_113896,out=&_ahg_geahui_113896,print=0);
    %end;
  %else 
    %do;
    data &_ahg_geahui_113896;
      set &_ahg_huigea_113896;
    run;
    %end;


  
  
  %let _ahg__ahuige_113896=&_ahg__ahuige_113896 &_ahg_geahui_113896;
  option mprint;
  data &_ahg_geahui_113896;
    %if %AHGnonblank(&ord) %then  &ord=&_ahg_igeahu_113896;;
    %if %AHGnonblank(&by) %then
    %do;
    format &by $40.; &by="&_ahg_huigea_113896";
    %end;
    set &_ahg_geahui_113896;
  run;

  %AHGalltocharnew(&_ahg_geahui_113896,out=&_ahg_geahui_113896);
  option mprint;
  %AHGvarlist(%scan(&dsns,&_ahg_igeahu_113896,%str( )),into=_ahg_ahuige_113896);
  %if %AHGcount(&_ahg_ahuige_113896)> %AHGcount(&_ahg_uigeah_113896) %then
        %let _ahg_uigeah_113896=&_ahg_uigeah_113896 %AHGscansubstr( &_ahg_ahuige_113896,%eval(%AHGcount(&_ahg_uigeah_113896)+1),%eval(%AHGcount(&_ahg_ahuige_113896)-%AHGcount(&_ahg_uigeah_113896)));
  option mprint;
  %AHGmergeprint(&_ahg_geahui_113896,out=&_ahg_geahui_113896,print=0,prefix=&prefix,length=&length);
  option mprint;

%end;

data &out;
  set &_ahg__ahuige_113896;
run;
option mprint;
%if &keep %then %AHGrenamekeep(&out,names=&_ahg_uigeah_113896,keepall=0);
%else  %AHGrenamekeep(&out,keepall=0);
%AHGtrimdsn(&out);

%if  &print %then %AHGprt;
option mprint;
%mend;

%macro AHGsetprint(dsns,out=setprint,print=0,prefix=,by=,ord=,keep=0,length=500);
option mprint;
%local _ahg_igeahu_850710 _ahg__ahuige_850710 _ahg_huigea_850710 _ahg_geahui_850710 _ahg___ahuig_850710 
;
%if %AHGblank(&prefix) %then %let prefix=%AHGrdm(9);
%let _ahg__ahuige_850710=%AHGcount(&dsns);
%local _ahg_uigeah_850710 _ahg_ahuige_850710 ;
%do _ahg_igeahu_850710=1 %to  &_ahg__ahuige_850710;
  %let _ahg_huigea_850710=%scan(&dsns,&_ahg_igeahu_850710,%str( ));
  %ahggettempname(_ahg_geahui_850710);
  %let _ahg___ahuig_850710=&_ahg___ahuig_850710 &_ahg_geahui_850710;
  option mprint;
  data &_ahg_geahui_850710;
    %if %AHGnonblank(&ord) %then  &ord=&_ahg_igeahu_850710;;
    %if %AHGnonblank(&by) %then 
    %do;
    format &by $40.; &by="&_ahg_huigea_850710";
    %end;
    set &_ahg_huigea_850710;
  run;
  
  %AHGalltocharnew(&_ahg_geahui_850710,out=&_ahg_geahui_850710);
  option mprint;
  %AHGvarlist(%scan(&dsns,&_ahg_igeahu_850710,%str( )),into=_ahg_ahuige_850710);
  %if %AHGcount(&_ahg_ahuige_850710)> %AHGcount(&_ahg_uigeah_850710) %then 
        %let _ahg_uigeah_850710=&_ahg_uigeah_850710 %AHGscansubstr( &_ahg_ahuige_850710,%eval(%AHGcount(&_ahg_uigeah_850710)+1),%eval(%AHGcount(&_ahg_ahuige_850710)-%AHGcount(&_ahg_uigeah_850710)));
  option mprint;
  %AHGmergeprint(&_ahg_geahui_850710,out=&_ahg_geahui_850710,print=0,prefix=&prefix,length=&length);
  option mprint;

%end;

data &out;
  set &_ahg___ahuig_850710;
run;
option mprint;
%if &keep %then %AHGrenamekeep(&out,names=&_ahg_uigeah_850710,keepall=0);
%else  %AHGrenamekeep(&out,keepall=0);
%AHGtrimdsn(&out);

%if  &print %then %AHGprt;
option mprint;
%mend;
%macro AHGsetstatfmt(statfmt=);
%local _ahg_huigea_160437 _ahg_ahuige_160437 _ahg_uigeah_160437 ;
%let _ahg_uigeah_160437=n\5. std\6.2 mean\6.1 median\6.1 min\6.1 max\6.1 lclm\6.2 uclm\6.2 p25\6.2 p50\6.2 p75\6.2;
%do _ahg_huigea_160437=1 %to %AHGcount(&statfmt);
  %if %index(%scan(&statfmt,&_ahg_huigea_160437,%str( )),\) %then %let _ahg_uigeah_160437=&_ahg_uigeah_160437 %scan(&statfmt,&_ahg_huigea_160437,%str( ));
%end;
%do _ahg_huigea_160437=1 %to %AHGcount(&_ahg_uigeah_160437);
%let _ahg_ahuige_160437=%nrstr(%global) formatof%scan(%scan(&_ahg_uigeah_160437,&_ahg_huigea_160437,%str( )),1,\);
%unquote(&_ahg_ahuige_160437);
%if %AHGblank(%scan(%scan(&_ahg_uigeah_160437,&_ahg_huigea_160437,%str( )),2,\)) %then %let formatof%scan(%scan(&_ahg_uigeah_160437,&_ahg_huigea_160437,%str( )),1,\)=6.2;
%else %let formatof%scan(%scan(&_ahg_uigeah_160437,&_ahg_huigea_160437,%str( )),1,\)=%scan(%scan(&_ahg_uigeah_160437,&_ahg_huigea_160437,%str( )),2,\);

%end;

%mend;
%macro AHGsetvarLen(dsn,var,len,out=);
  %if %AHGblank(&out) %then %let out=%AHGbasename(&dsn);
%local _ahg_huigea_988143 _ahg_ahuige_988143 ;
  %ahggettempname(_ahg_huigea_988143);
  %AHGvarlist(&dsn,into=_ahg_ahuige_988143,dlm=%str( ),global=0);
  data &_ahg_huigea_988143;
    length &var &len;
  run;
  data &out;
    merge &_ahg_huigea_988143 &dsn;
  run;
  %AHGordvar(&out,&_ahg_ahuige_988143,keepall=0);
%mend;
%macro AHGshrink(dsn,vars,lengths,out=,pre=z487);
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%local _ahg_ahuige_178477 ;
data &out;
  format
  %do _ahg_ahuige_178477=1 %to %AHGcount(&vars);
  %scan(&vars,&_ahg_ahuige_178477) 
  %scan(&lengths,&_ahg_ahuige_178477,%str( ))  
  %end;
  ;
  set &dsn
  (
  rename=(
  %do _ahg_ahuige_178477=1 %to %AHGcount(&vars);
  %scan(&vars,&_ahg_ahuige_178477)=&pre%scan(&vars,&_ahg_ahuige_178477)
  %end;
  )
  );
  %do _ahg_ahuige_178477=1 %to %AHGcount(&vars);
  %scan(&vars,&_ahg_ahuige_178477)=put(&pre%scan(&vars,&_ahg_ahuige_178477),%scan(&lengths,&_ahg_ahuige_178477,%str( )));
  %end;

  drop
  %do _ahg_ahuige_178477=1 %to %AHGcount(&vars);
   &pre%scan(&vars,&_ahg_ahuige_178477) 
  %end;
  ;

run;
%mend;
%macro ahgslash(file,where);
%LOCAL dt  spl lps;
%if %AHGindex(%bquote(&where),unix) %then %let spl=%str(/);
%else %if %AHGindex(%bquote(&where),win) %then %let spl=%str(\);
%else %let spl=%AHGdelimit;
%let lps=%sysfunc(compress(%str(\/),%bquote(&spl)));
%sysfunc(translate(%bquote(&file),%bquote(&spl),%bquote(&lps)))
%mend;
%macro AHGsmall(dsn,vars,out=);
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%AHGdatasort(data =&dsn , out =&out , by =&vars );

data &out;
  set &out;
  by &vars;
  if first.%scan(&vars,-1) then output;
run;
%mend;
%macro AHGsomeTempName(tempname,n,start=);
%local _ahg_uigeah_149032 _ahg_huigea_149032 ;
  %if %AHGblank(&start) %then %let start=T_&tempname;
%local _ahg_igeahu_149032 _ahg_ahuige_149032 
;


  %do %until ( %unquote(&_ahg_ahuige_149032)    );
    %let _ahg_igeahu_149032=%sysfunc(normal(0));
    %let _ahg_igeahu_149032=%sysfunc(translate(&_ahg_igeahu_149032,00,.-));
    %let _ahg_uigeah_149032=&start._%substr(&_ahg_igeahu_149032,1,5);
    %let &tempname=%AHGwords(&_ahg_uigeah_149032,&n);
    %let _ahg_ahuige_149032=  1 ;
      %do _ahg_huigea_149032=1 %to &n;
      %let _ahg_ahuige_149032=%str(&_ahg_ahuige_149032 and not %sysfunc(exist(&_ahg_uigeah_149032.&_ahg_huigea_149032)) )  ;
    ;
    %end;
  %end;
  %put &tempname=&&&tempname;



%mend;

%macro AHGsortnum(mac,into=sortoutmac,dlm=%str( ),nodup=0);

%local _ahg_uigeah_867303 _ahg_huigea_867303 _ahg_ahuige_867303 ;

  %ahggettempname(_ahg_ahuige_867303);

  data &_ahg_ahuige_867303;

  format item $200.;

  %do _ahg_uigeah_867303=1 %to %AHGcount(&mac,dlm=&dlm);

  item=left("%scan(&mac,&_ahg_uigeah_867303,&dlm)");
  length=length(compress(item));

  output;

  %end;

  run;

  proc sql noprint;

  select %if &nodup %then distinct; item into :&into separated by "&dlm"

  from &_ahg_ahuige_867303

  order by length,item

  ;quit;

%mend;

%macro AHGsort(mac,into=sortoutmac,dlm=%str( ),nodup=0);

%local _ahg_uigeah_219778 _ahg_huigea_219778 _ahg_ahuige_219778 ;

  %ahggettempname(_ahg_ahuige_219778);

  data &_ahg_ahuige_219778;

  format item $200.;

  %do _ahg_uigeah_219778=1 %to %AHGcount(&mac,dlm=&dlm);

  item=left("%scan(&mac,&_ahg_uigeah_219778,&dlm)");

  output;

  %end;

  run;

  proc sql noprint;

  select %if &nodup %then distinct; item into :&into separated by "&dlm"

  from &_ahg_ahuige_219778

  order by item

  ;quit;

%mend;
%macro AHGsortWords(words,into=,dlm=%str( ),length=100,nodup=1);
%local _ahg_huigea_695681 _ahg_ahuige_695681 ;
  %ahggettempname(_ahg_ahuige_695681);
  option nomprint;
  data &_ahg_ahuige_695681;
    length word $&length.;
    %do _ahg_huigea_695681=1 %to %AHGcount(&words,dlm=&dlm);
    word=scan("&words",&_ahg_huigea_695681,"&dlm");
    output;
    %end;
  run;

  proc sql noprint;
  select %if &nodup %then distinct; trim(word) as word into :&into separated by "&dlm"
  from &_ahg_ahuige_695681
  order by word
  ;
  quit;

  %AHGdatadelete(data=&_ahg_ahuige_695681);
  option mprint;

%mend;


%macro AHGsplitdsn(dsn,by,prefix=,into=,intoby=,nofmt=0,intobyvalues=);
%if %AHGblank(&prefix) %then %let prefix=splitdsn;
%local _ahg_ahuige_364141 _ahg_igeahu_364141 _ahg_uigeah_364141 _ahg_huigea_364141 ;
%ahggettempname(_ahg_huigea_364141);

%if &nofmt %then
%do;
data &_ahg_huigea_364141;
  format &by best.;
  set &dsn;
run;  
%end;
%else
%do;
data &_ahg_huigea_364141;
  set &dsn;
run;  
%end;


proc sql noprint;
  select count(distinct &by) into :_ahg_ahuige_364141
  from &_ahg_huigea_364141
  ;
  %if not %AHGblank(&intobyvalues) %then %let _ahg_ahuige_364141=%AHGcount(&intobyvalues,dlm=@);;
  %if not %AHGblank(&intoby) %then
  %do;
  proc sql noprint;
  select distinct &by into :&intoby separated by '@'
  from &_ahg_huigea_364141
  order by &by
  ;
  %end;
  %if not %AHGblank(&intobyvalues) %then %let intoby= &intobyvalues ;

  quit;


proc sort data=&_ahg_huigea_364141 ;
  by &by;
run;

%AHGsomeTempName(dsns,&_ahg_ahuige_364141,start=&prefix);
%if not %AHGblank(&into) %then %let &into=&_ahg_uigeah_364141;

data &_ahg_uigeah_364141;
  set &_ahg_huigea_364141;
  by &by;
  if first.&by then ahuigebycount4352+1;
  drop ahuigebycount4352;
  %do _ahg_igeahu_364141=1 %to &_ahg_ahuige_364141;
  if  ahuigebycount4352=&_ahg_igeahu_364141 then output %scan(&_ahg_uigeah_364141,&_ahg_igeahu_364141);
  %end;
run;

%mend;

%macro AHGsplitVar(dsn,inVar,toVars,out=,dlm=@,drop=1);
  %if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
  data &out;
    set &dsn;
%local _ahg_ahuige_22354 ;
    %do _ahg_ahuige_22354=1 %to %AHGcount(&tovars);
    %scan(&tovars,&_ahg_ahuige_22354)=scan(&invar,&_ahg_ahuige_22354,"&dlm");
    %end;
    %if &drop %then drop &invar;;
  run; 
                    
%mend;

%macro chn_ut_status(showall=0);

%local _ahg___ahuig_150567 _ahg_igeahu_150567 _ahg_uigeah_150567 _ahg__huigea_150567 _ahg__uigeah_150567 ;
%let _ahg__uigeah_150567=_%AHGrdm(6);



%if %sysfunc(exist(specs.trackingsheet)) %then
  %do;
  data sdtmAll&_ahg__uigeah_150567;
    format Category  name $100.;
    set specs.trackingsheet(rename=(output_name=name));
    keep name category insheet insheet;
    name=lowcase(name);
    category=upcase(category);
    if indexw('sdtm',trim(lowcase(category)));
    inSheet=1;
  run;
  %end;






%AHGpspipe(%str(ls |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&sdtm,out=sdtmInDir&_ahg__uigeah_150567);

data sdtmInDir&_ahg__uigeah_150567;
  set sdtmInDir&_ahg__uigeah_150567; drop line;
  WHERE index(line,'.sas7bdat');
  format   name $100. FileDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  FileDate=left(substr(line,index(line,' ')));
  sdtm=1;
run;

%local _ahg_geahui_150567 ;

%AHGnobs(SDTMAll&_ahg__uigeah_150567,into=_ahg_geahui_150567);

%AHGmergedsn(sdtmAll&_ahg__uigeah_150567,sdtmInDir&_ahg__uigeah_150567,sdtmstatus&_ahg__uigeah_150567,by=name,joinstyle=full);

%macro dummy;
%if &_ahg_geahui_150567>0 %then %AHGmergedsn(sdtmAll&_ahg__uigeah_150567,sdtmInDir&_ahg__uigeah_150567,sdtmstatus&_ahg__uigeah_150567,by=name,joinstyle=left/*left right full matched*/);
%else %AHGmergedsn(sdtmAll&_ahg__uigeah_150567,sdtmInDir&_ahg__uigeah_150567,sdtmstatus&_ahg__uigeah_150567,by=name,joinstyle=right/*left right full matched*/);
%mend;
/* 
#####################################3
*/ 

%if %sysfunc(exist(specs.trackingsheet)) %then
  %do;
data adamAll&_ahg__uigeah_150567;
  format Category  name $100.;
  set specs.trackingsheet(rename=(output_name=name));
  keep name category insheet;
  name=lowcase(name);
  category=upcase(category);
  if indexw('adam',trim(lowcase(category)));
  insheet=1;
run;
  %end;




%AHGpspipe(%str(ls |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&adam,out=adamInDir&_ahg__uigeah_150567);

data adamInDir&_ahg__uigeah_150567;
  set adamInDir&_ahg__uigeah_150567; drop line;
  WHERE index(line,'.sas7bdat');
  format name $100. FileDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  FileDate=left(substr(line,index(line,' ')));
  adam=1;
run;


%local _ahg_geahui_150567 ;

%AHGnobs(SDTMAll&_ahg__uigeah_150567,into=_ahg_geahui_150567);

%AHGmergedsn(adamAll&_ahg__uigeah_150567,adamInDir&_ahg__uigeah_150567,adamstatus&_ahg__uigeah_150567,by=name,joinstyle=full);


%macro dummy;
%if &_ahg_geahui_150567>0 %then %AHGmergedsn(adamAll&_ahg__uigeah_150567,adamInDir&_ahg__uigeah_150567,adamstatus&_ahg__uigeah_150567,by=name,joinstyle=left/*left right full matched*/);
%else %AHGmergedsn(adamAll&_ahg__uigeah_150567,adamInDir&_ahg__uigeah_150567,adamstatus&_ahg__uigeah_150567,by=name,joinstyle=right/*left right full matched*/);
%mend;

/* 
#####################################3
*/ 


%if %sysfunc(exist(specs.trackingsheet)) %then
  %do;
data tflAll&_ahg__uigeah_150567;
  format Category  name $100.;
  set specs.trackingsheet(rename=(output_name=name ));
  keep name category insheet program_name display_id;
  name=lowcase(name);
  program_name=scan(program_name,1,'.');
  if missing(name) then name=program_name;
  category=upcase(category);
  if missing(name) then delete;
  if  not indexw('adam sdtm setup',trim(lowcase(category)));
  insheet=1;
run;
  %end;


%AHGpspipe(%str(ls  |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&tfl_output,out=TFLInDir&_ahg__uigeah_150567);

data TFLInDir&_ahg__uigeah_150567;
  set TFLInDir&_ahg__uigeah_150567; drop line;
  WHERE index(line,'.rtf');
  format  name $100. FileDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  FileDate=left(substr(line,index(line,' ')));
run;



%local _ahg_geahui_150567 ;

%AHGnobs(TFLAll&_ahg__uigeah_150567,into=_ahg_geahui_150567);

%AHGmergedsn(TFLAll&_ahg__uigeah_150567,TFLInDir&_ahg__uigeah_150567,TFLstatus&_ahg__uigeah_150567,by=name,joinstyle=full);


%macro dummy;
%if &_ahg_geahui_150567>0 %then %AHGmergedsn(TFLAll&_ahg__uigeah_150567,TFLInDir&_ahg__uigeah_150567,TFLstatus&_ahg__uigeah_150567,by=name,joinstyle=left/*left right full matched*/);
%else  %AHGmergedsn(TFLAll&_ahg__uigeah_150567,TFLInDir&_ahg__uigeah_150567,TFLstatus&_ahg__uigeah_150567,by=name,joinstyle=right/*left right full matched*/);
%mend;


data AllFile&_ahg__uigeah_150567 one;
  set adamstatus&_ahg__uigeah_150567 sdtmstatus&_ahg__uigeah_150567 tflstatus&_ahg__uigeah_150567;;
  if missing(FileDate) then FileDate='-NA-';
  if not missing(program_name) then name=program_name;
run;

%AHGpspipe(%str(ls *.lst |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.programs_stat\sdtm\system_files,out=sdtmlog&_ahg__uigeah_150567);

data sdtmlog&_ahg__uigeah_150567;
  set sdtmlog&_ahg__uigeah_150567; drop line;
  WHERE index(line,'.lst');
  format   name $100. LstDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  LstDate=left(substr(line,index(line,' ')));
  if substr(name,1,1)='_' or name='' OR LstDate='LastWriteTime' then delete;
  output;
  if length(name)=2 then
  do;
  name='supp'||name;
  output;
  name=compress(tranwrd('relrec'||name,'supp',''));
  output;
  end;
run;


%AHGpspipe(%str(ls *.lst |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.programs_stat\adam\system_files,out=adamlog&_ahg__uigeah_150567);

data adamlog&_ahg__uigeah_150567;
  set adamlog&_ahg__uigeah_150567; drop line;
  format  name $100. LstDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  LstDate=left(substr(line,index(line,' ')));
  if substr(name,1,1)='_' or name='' OR LstDate='LastWriteTime' then delete;
  
run;

%AHGpspipe(%str(ls *.lst |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.programs_stat\tfl\system_files,out=tfllog&_ahg__uigeah_150567);
 
data tfllog&_ahg__uigeah_150567;
  set tfllog&_ahg__uigeah_150567; drop line;
  format   name $100. LstDate $500.;
  if 'lst' ne scan(scan(line,1,' '),2,'.') then delete;
  name=scan(scan(line,1,' '),1,'.');
  LstDate=left(substr(line,index(line,' ')));
  if substr(name,1,1)='_' or name='' OR LstDate='LastWriteTime' then delete;
run;




data allLog&_ahg__uigeah_150567;
  set adamlog&_ahg__uigeah_150567 sdtmlog&_ahg__uigeah_150567 tfllog&_ahg__uigeah_150567 ;;
run;



%AHGmergedsn( AllFile&_ahg__uigeah_150567 ,allLog&_ahg__uigeah_150567, AllFile&_ahg__uigeah_150567, by =name,joinstyle=left);


%AHGpspipe(%str(ls |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.replica_programs\sdtm\system_files,out=sdtm2nd&_ahg__uigeah_150567);

data sdtm2nd&_ahg__uigeah_150567;
  set sdtm2nd&_ahg__uigeah_150567; drop line;
  WHERE index(line,'.lst');
  format   name $100. QCdate $500.;
  name=substr(scan(scan(line,1,' '),1,'.'),4);
  QCdate=left(substr(line,index(line,' ')));
  OUTPUT;
  name='supp'||name;
  output;
run;



%AHGpspipe(%str(ls |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.replica_programs\adam\system_files,out=adam2nd&_ahg__uigeah_150567);

data adam2nd&_ahg__uigeah_150567;
  set adam2nd&_ahg__uigeah_150567; drop line;
  WHERE index(line,'.lst');
  format   name $100. QCdate $500.;
  name=substr(scan(scan(line,1,' '),1,'.'),4);
  QCdate=left(substr(line,index(line,' ')));
run;




%AHGpspipe(%str(ls |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.replica_programs\tfl\system_files,out=tfl2nd&_ahg__uigeah_150567);

data tfl2nd&_ahg__uigeah_150567;
  set tfl2nd&_ahg__uigeah_150567; drop line;
  WHERE index(line,'.lst');
  format   name $100. QCdate $500.;
  name=substr(scan(scan(line,1,' '),1,'.'),4);
  QCdate=left(substr(line,index(line,' ')));
run;


data allval&_ahg__uigeah_150567;
  set  sdtm2nd&_ahg__uigeah_150567  adam2nd&_ahg__uigeah_150567  tfl2nd&_ahg__uigeah_150567;
run;
  



%AHGmergedsn(AllFile&_ahg__uigeah_150567 , allval&_ahg__uigeah_150567,AllFile&_ahg__uigeah_150567 ,by=name,joinstyle=left/*left right full matched*/);

%local _ahg_ahuige_150567 _ahg_huigea_150567 _ahg__ahuige_150567 
;




proc sql noprint;
  select distinct max(filedate) into :_ahg_huigea_150567
  from allfile&_ahg__uigeah_150567
  where %AHGeqv(category,'sdtm') and not %AHGeqv(filedate,'-NA-');
  ;

  select distinct max(filedate) into :_ahg__ahuige_150567
  from allfile&_ahg__uigeah_150567
  where %AHGeqv(category,'adam')  and not %AHGeqv(filedate,'-NA-');
  ;
quit;


proc format ;
  value colorcd
  0=' '
  1='Log out-of-date /Not Done'
  2='QC out-of-date /Not Done'
  3='Source Dataset updated'
  9='File Not Created'
  ;
run;

data AllFile&_ahg__uigeah_150567;
  set AllFile&_ahg__uigeah_150567;
  array alldate filedate lstdate qcdate;

  do over alldate;
  if missing(alldate) then alldate='-NA-';
  end;



  do over alldate;
  alldate=trim(ALLDATE)||'``';
  end;


  if LstDate<FileDate then flag=1;

  if lstdate>QCdate and not (prxmatch('/\d{5}.*\d{5}/',name))then flag=2;

  if (%AHGeqv(category,'adam') and Filedate<"&_ahg_huigea_150567")  or 
( folder='tfl' and (Filedate<"&_ahg__ahuige_150567") )then flag=3;

  if missing(FileDate) or FileDate='-NA-' then flag=9;

  FLAG=max(flag,0);
  format comment $50.;
  comment=put(flag,colorcd.);

  format str $200. folder;

  drop program_name str  INSHEET SDTM ADAM loc ext folder ;
  if category='SDTM' THEN  category='  SDTM' ;
  if category='ADAM' THEN  category=' ADAM' ;
  if prxmatch('/relrec\w+/',name) then delete;
  if (not insheet) and sdtm then category='~  SDTM';
  if (not insheet) and adam then category='~ ADAM';


  format loc $100. folder $50. ext $20.;
  IF INDEX(category,'SDTM') then folder='sdtm';
  else if INDEX(category,'ADAM') then folder='adam';
  else folder='tfl';

  if folder='tfl' then ext='rtf     ';
  else ext='sas7bdat';

  if folder='tfl' then loc='programs_stat\\tfl_output';
  else loc='data\\'||folder;;

    str= "s/([^`]*)`([^`]*)`([^`]*)/\1`\2`%sysfunc(tranwrd(&__snapshot,\,\\))"||trim(loc)||"\\"||trim(name)||'.'||trim(ext)||'/';;
    filedate=prxchange(trim(str),1,filedate);
    str= "s/([^`]*)`([^`]*)`([^`]*)/\1`\2`%sysfunc(tranwrd(&__snapshot,\,\\))programs_stat\\"||trim(folder)||"\\system_files\\"||trim(name)||'.lst/';;
    lstdate=prxchange(trim(str),1,lstdate);
    str= "s/([^`]*)`([^`]*)`([^`]*)/\1`\2`%sysfunc(tranwrd(&__snapshot,\,\\))replica_programs\\"||trim(folder)||"\\system_files\\ir_"||trim(name)||'.lst/';;
    qcdate=prxchange(trim(str),1,qcdate);




  %if not &showall %then if (not insheet)  then delete;;
  if   missing(name) then delete;
  order=category;
  if not ( index(lowcase(category),'sdtm') or index(lowcase(category),'adam')) then order='TFL';
run;

%AHGalltocharNew(AllFile&_ahg__uigeah_150567);

data sdtmlabel&_ahg__uigeah_150567;
  format label $500.;
  set specs.meta_table:;
  keep name label  ;
  name=lowcase(dataset);
  where not missing(dataset);
run;

%AHGmergedsn(AllFile&_ahg__uigeah_150567,sdtmlabel&_ahg__uigeah_150567,AllFile&_ahg__uigeah_150567,by=name,joinstyle=left/*left right full matched*/);


data adamlabel&_ahg__uigeah_150567;
  set specs.meta_adam_table:;
  keep name label  ;
  name=lowcase(dataset);
  where not missing(dataset);
run;

%AHGmergedsn(AllFile&_ahg__uigeah_150567,adamlabel&_ahg__uigeah_150567,AllFile&_ahg__uigeah_150567,by=name,joinstyle=left/*left right full matched*/);

data tfllabel&_ahg__uigeah_150567;
  set specs.meta_tfl;
  keep display_id  DESCRIPTION  ;
  display_id= DISPLAY_IDENTIFIER;
  rename DESCRIPTION=label;
  where not missing(DISPLAY_IDENTIFIER);
run;

%AHGmergedsn(AllFile&_ahg__uigeah_150567,tfllabel&_ahg__uigeah_150567,AllFile&_ahg__uigeah_150567,by=display_id,joinstyle=left/*left right full matched*/);



%AHGdatasort(data = AllFile&_ahg__uigeah_150567, by =order category name);

data AllFile&_ahg__uigeah_150567;
  retain id 0;
  set AllFile&_ahg__uigeah_150567;
  by order category name;
  array cols  LABEL  NAME  CATEGORY  COMMENT  ;
  do over cols;
  cols=trim(cols)||'`'||trim(flag)||'`';
  end;
  
  if first.order then id=1;
  else id+1;
run;

%AHGordvar(AllFile&_ahg__uigeah_150567,id label name category filedate lstdate qcdate comment,out=,keepall=0);



option formdlim=' ' mprint;


%ahgcolorex(AllFile&_ahg__uigeah_150567,flag=flag,file=,label=%str(  filedate='File Date' lstdate='Log Date' qcdate='Validation Date'  )) ; 
 
%mend;


%macro AHGsumex(dsn,var,by=,out=stats,print=0,alpha=0.05
,stats=n mean median  min max
,orie=
,labels=
,left=left
,statord=
);

%local _ahg_uigeah_310780 _ahg___uigeah_310780 ;
%if %AHGblank(&statord) %then %let statord=ahgdummy%AHGrdm(10);
%ahggettempname(_ahg_uigeah_310780);
%if %AHGblank(&by) %then 
%do;
%let _ahg___uigeah_310780=missing;
%let by=%AHGrdm(10);
%end;
data &_ahg_uigeah_310780;
  set &dsn;
 %if &_ahg___uigeah_310780=missing %then  &by=1; ;
run;
%local _ahg__ahuige_310780 ;
%let _ahg__ahuige_310780=ahgxxxyyyzzz;
%macro ahgxxxyyyzzz(one);
  %IF not  (%index(&_ahg__uigeah_310780,%str(%")) or %index(&_ahg__uigeah_310780,%str(%'))) %THEN 1;
  %ELSE 0;
%mend;
%local _ahg_____ahuig_310780 ;
%let _ahg_____ahuig_310780=%AHGname(&stats,but=@);
%if %AHGblank(&statord) %then %let statord=ahgdummy%AHGrdm(10);
%if %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);
%macro dosomething;
%local _ahg_geahui_310780 _ahg__huigea_310780 _ahg__uigeah_310780 ;
%do _ahg_geahui_310780=1 %to %AHGcount(&stats,dlm=@);
  %let _ahg__uigeah_310780=%scan(&stats,&_ahg_geahui_310780,@);
  %let labels=&labels@;
  %do _ahg__huigea_310780=1 %to %AHGcount(&_ahg__uigeah_310780);
    %let labels=&labels %AHGscan2(&_ahg__uigeah_310780,&_ahg__huigea_310780,1,dlm=%str( ),dlm2=\);
  %end;
%end;
%let labels=%substr(&labels,2);
%mend;
%if %AHGblank(&labels) %then %doSomething ;
%if not %AHGblank(&labels) and not %index(&labels,@) %then %let labels=%AHGaddcomma(&labels,comma=@);
%local _ahg_____ahuig_310780 ;
%let _ahg_____ahuig_310780=%AHGname(&labels,but=@);

%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local _ahg_ahuige_310780 ;
%let _ahg_ahuige_310780=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local _ahg__geahui_310780 _ahg___ahuig_310780 %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(ISstat,20);
%local _ahg_geahui_310780 _ahg___igeahu_310780 _ahg__igeahu_310780 ;
%ahggettempname(_ahg___igeahu_310780);
%AHGdatasort(data =&_ahg_uigeah_310780 , out =&_ahg___igeahu_310780 , by = &by  );


%do _ahg_geahui_310780=1 %to %AHGcount(&stats);
  %let _ahg___ahuig_310780=%scan(&stats,&_ahg_geahui_310780,%str( ));
  %if %&_ahg__ahuige_310780(&_ahg___ahuig_310780) %then %let _ahg__igeahu_310780=&_ahg__igeahu_310780 &_ahg___ahuig_310780 ; /*mystats are real stats*/
%end;

%AHGsetstatfmt(statfmt=&_ahg__igeahu_310780);
%let _ahg__geahui_310780=%AHGcount(&stats);

%do _ahg_geahui_310780=1 %to &_ahg__geahui_310780;
  %let _ahg___ahuig_310780=%scan(&stats,&_ahg_geahui_310780,%str( ));
  %let mystat&_ahg_geahui_310780=%scan(&_ahg___ahuig_310780,1,\);
  %let myformat&_ahg_geahui_310780=%scan(&_ahg___ahuig_310780,2,\);
  %if %AHGblank(&&myformat&_ahg_geahui_310780) and %&_ahg__ahuige_310780(&&mystat&_ahg_geahui_310780) %then 
  %do;
  %global formatof&&mystat&_ahg_geahui_310780;
  %let myformat&_ahg_geahui_310780=&&&&formatof&&mystat&_ahg_geahui_310780;
  %if %AHGblank(&&myformat&_ahg_geahui_310780) %then %let myformat&_ahg_geahui_310780=7.2;
  %end;
  %if %&_ahg__ahuige_310780(&&mystat&_ahg_geahui_310780)  %then ;
%end;

  proc means data=&_ahg___igeahu_310780 noprint alpha=&alpha;;
    var &var;
    by &by;
    output out=&out 
    %do _ahg_geahui_310780=1 %to  &_ahg__geahui_310780;
    %if %&_ahg__ahuige_310780(&&mystat&_ahg_geahui_310780) %then  &&mystat&_ahg_geahui_310780%str(=)&&mystat&_ahg_geahui_310780;
    %end;
    ;
  run;

%macro ahgD(d=%str(,));
%if &_ahg_geahui_310780 ne 1 %then &d; 
%MEND;

  proc sql noprint;
    create table &out as
    select
    %do _ahg_geahui_310780=1 %to  %AHGcount(&stats);
      %if %&_ahg__ahuige_310780(&&mystat&_ahg_geahui_310780) %then %AHGd &left(put(&&mystat&_ahg_geahui_310780, &&myformat&_ahg_geahui_310780)) as  &&mystat&_ahg_geahui_310780 ;
      %else  %AHGd &&mystat&_ahg_geahui_310780;
    %end;
    %if not %AHGblank(&by) %then ,%AHGaddcomma(&by);
    from &out
    ;quit;

%if %substr(&sysmacroname,1,3)=AHG %then  
%do;

%local _ahg_huigea_310780 _ahg___geahui_310780 _ahg____ahuig_310780 _ahg____ahuige_310780 ;
%AHGvarlist(&out,into=_ahg_huigea_310780,dlm=%str( ),global=0);
%local _ahg_igeahu_310780 _ahg___huigea_310780 
;
%let _ahg___huigea_310780=0;
%let _ahg___geahui_310780=%AHGcount(&_ahg_ahuige_310780,dlm=@);
;
%do _ahg_geahui_310780=1 %to &_ahg___geahui_310780;
  %let _ahg____ahuig_310780=%scan(&_ahg_ahuige_310780,&_ahg_geahui_310780,@);
  %do _ahg_igeahu_310780=1 %to %AHGcount(&_ahg____ahuig_310780);
  %let _ahg___huigea_310780=%eval(&_ahg___huigea_310780+1);
  %if &_ahg_igeahu_310780=1 %then %let _ahg____ahuige_310780= &_ahg____ahuige_310780   %str(theVerticalvar&_ahg_geahui_310780=compbl%() %scan(&_ahg_huigea_310780,&_ahg___huigea_310780);
  %else  %let _ahg____ahuige_310780= &_ahg____ahuige_310780 ||'  '|| %scan(&_ahg_huigea_310780,&_ahg___huigea_310780);
  %if &_ahg_igeahu_310780=%AHGcount(&_ahg____ahuig_310780) %then  %let _ahg____ahuige_310780= &_ahg____ahuige_310780 %str(%););
  %end;
%end;

%local _ahg___ahuige_310780 ;
%ahggettempname(_ahg___ahuige_310780);

data &_ahg___ahuige_310780;
  set &out;
  keep &by %do _ahg_geahui_310780=1 %to  &_ahg___geahui_310780; theVerticalvar&_ahg_geahui_310780  %end;  ;
    %unquote(&_ahg____ahuige_310780);
run;

data hori&out;
  set &out;
run;

data &out;
  set &_ahg___ahuige_310780;
  keep &by  
  %if not %AHGblank(&labels) %then label; 
  %if not %AHGblank(&statord) %then &statord;
  stat;
  array allvar(1:&_ahg___geahui_310780) theVerticalvar1-theVerticalvar&_ahg___geahui_310780;
  do i=1 to dim(allvar);
  %if not %AHGblank(&labels) %then label=left(scan("%sysfunc(compress(&labels,%str(%'%")))",i,'@'));;
  %if not %AHGblank(&statord) %then &statord=i; ;
  stat=input(allvar(i),$50.);

  output;
  end;  
run;
%end;

%IF %AHGequalmactext(&orie,hori) %then
%do;
data &out;set &_ahg___ahuige_310780;run;

%AHGrelabel(&out,out=&out,labels=&by@&labels);
%end; 
data &out;
  set &out(drop=%if &_ahg___uigeah_310780=missing %then &by;  
    %if %substr(&statord,1,8)=ahgdummy %then &statord;);
run;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;
%macro AHGsumextrt(dsn,var,by=,trt=,out=stats,print=0,alpha=0.05
,stats=n mean median  min max
,orie=vert
,labels=
,left=left
,statord=
);
%local _ahg_ahuige_51637 _ahg____ahuige_51637 ;
%if %AHGblank(&statord) %then %let statord=ahgdummy%AHGrdm(10);
%ahggettempname(_ahg_ahuige_51637);
%if %AHGblank(&by) %then 
%do;
%let _ahg____ahuige_51637=missing;
%let by=%AHGrdm(10);
%end;
data &_ahg_ahuige_51637;
  set &dsn;
 %if &_ahg____ahuige_51637=missing %then  &by=1; ;
run;



%local _ahg__igeahu_51637 ;
%let _ahg__igeahu_51637=ahgxxxyyyzzz;
%macro ahgxxxyyyzzz(one);
  %IF not  (%index(&_ahg__huigea_51637,%str(%")) or %index(&_ahg__huigea_51637,%str(%'))) %THEN 1;
  %ELSE 0;
%mend;

%if %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);
%macro dosomething;
%local _ahg_huigea_51637 _ahg_igeahu_51637 _ahg__huigea_51637 ;
%do _ahg_huigea_51637=1 %to %AHGcount(&stats,dlm=@);
  %let _ahg__huigea_51637=%scan(&stats,&_ahg_huigea_51637,@);
  %let labels=&labels@;
  %do _ahg_igeahu_51637=1 %to %AHGcount(&_ahg__huigea_51637);
    %let labels=&labels %AHGscan2(&_ahg__huigea_51637,&_ahg_igeahu_51637,1,dlm=%str( ),dlm2=\);
  %end;
%end;
%let labels=%substr(&labels,2);
%mend;
%if %AHGblank(&labels) %then %doSomething ;
%if not %AHGblank(&labels) and not %index(&labels,@) %then %let labels=%AHGaddcomma(&labels,comma=@);
%local _ahg___huigea_51637 ;
%let _ahg___huigea_51637=%AHGname(&labels,but=@);
;
%let _ahg___huigea_51637=%sysfunc(tranwrd(&_ahg___huigea_51637,@,%str( )));
;

%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local _ahg____uigeah_51637 ;
%let _ahg____uigeah_51637=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local _ahg___ahuige_51637 _ahg_uigeah_51637 %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(ISstat,20);
%local _ahg_huigea_51637 _ahg___geahui_51637 _ahg___ahuig_51637 ;
%ahggettempname(_ahg___geahui_51637);


%do _ahg_huigea_51637=1 %to %AHGcount(&stats);
  %let _ahg_uigeah_51637=%scan(&stats,&_ahg_huigea_51637,%str( ));
  %if %&_ahg__igeahu_51637(&_ahg_uigeah_51637) %then %let _ahg___ahuig_51637=&_ahg___ahuig_51637 &_ahg_uigeah_51637 ; /*mystats are real stats*/
%end;

%AHGsetstatfmt(statfmt=&_ahg___ahuig_51637);
%let _ahg___ahuige_51637=%AHGcount(&stats);

%do _ahg_huigea_51637=1 %to &_ahg___ahuige_51637;
  %let _ahg_uigeah_51637=%scan(&stats,&_ahg_huigea_51637,%str( ));
  %let mystat&_ahg_huigea_51637=%scan(&_ahg_uigeah_51637,1,\);
  %let myformat&_ahg_huigea_51637=%scan(&_ahg_uigeah_51637,2,\);
  %if %AHGblank(&&myformat&_ahg_huigea_51637) and %&_ahg__igeahu_51637(&&mystat&_ahg_huigea_51637) %then 
  %do;
  %global formatof&&mystat&_ahg_huigea_51637;
  %let myformat&_ahg_huigea_51637=&&&&formatof&&mystat&_ahg_huigea_51637;
  %if %AHGblank(&&myformat&_ahg_huigea_51637) %then %let myformat&_ahg_huigea_51637=7.2;
  %end;
  %if %&_ahg__igeahu_51637(&&mystat&_ahg_huigea_51637)  %then ;
%end;

%AHGdatasort(data =&_ahg_ahuige_51637 , out = , by = &by &trt);

  proc means data=&_ahg_ahuige_51637 noprint %if %AHGnonblank(&alpha) %then alpha=&alpha;;
    var &var;
    by &by &trt;
    output 
%local _ahg___uigeah_51637 ;
    %let _ahg___uigeah_51637= %str(out=&out ) ;
    %do _ahg_huigea_51637=1 %to  &_ahg___ahuige_51637;
    %if %&_ahg__igeahu_51637(&&mystat&_ahg_huigea_51637) %then  %let _ahg___uigeah_51637=&_ahg___uigeah_51637 &&mystat&_ahg_huigea_51637  %bquote(=)  &&mystat&_ahg_huigea_51637;
    %end;
    &_ahg___uigeah_51637
    ;
  run;

%macro ahgD(d=%str(,));
%if &_ahg_huigea_51637 ne 1 %then &d; 
%MEND;

  proc sql noprint;
    create table temp&out as
    select
    %do _ahg_huigea_51637=1 %to  %AHGcount(&stats);
      %if %&_ahg__igeahu_51637(&&mystat&_ahg_huigea_51637) %then %AHGd &left(put(&&mystat&_ahg_huigea_51637, &&myformat&_ahg_huigea_51637)) as  &&mystat&_ahg_huigea_51637 ;
      %else  %AHGd &&mystat&_ahg_huigea_51637;
    %end;
    %AHG1(&by,%bquote(,%AHGaddcomma(&by)))
    %AHG1(&trt,%str(,&trt))
    from &out
    ;
    create table &out as
    select *
    from temp&out

    ;quit;

%if %substr(&sysmacroname,1,3)=AHG %then  
%do;

%local _ahg___igeahu_51637 _ahg_____ahuige_51637 _ahg__geahui_51637 _ahg____igeahu_51637 ;
%AHGvarlist(&out,into=_ahg___igeahu_51637,dlm=%str( ),global=0);
%local _ahg_geahui_51637 _ahg______ahuig_51637 
;
%let _ahg______ahuig_51637=0;
%let _ahg_____ahuige_51637=%AHGcount(&_ahg____uigeah_51637,dlm=@);
;
%do _ahg_huigea_51637=1 %to &_ahg_____ahuige_51637;
  %let _ahg__geahui_51637=%scan(&_ahg____uigeah_51637,&_ahg_huigea_51637,@);
  %do _ahg_geahui_51637=1 %to %AHGcount(&_ahg__geahui_51637);
  %let _ahg______ahuig_51637=%eval(&_ahg______ahuig_51637+1);
  %if &_ahg_geahui_51637=1 %then %let _ahg____igeahu_51637= &_ahg____igeahu_51637   %str(theVerticalvar&_ahg_huigea_51637=compbl%() %scan(&_ahg___igeahu_51637,&_ahg______ahuig_51637);
  %else  %let _ahg____igeahu_51637= &_ahg____igeahu_51637 ||'  '|| %scan(&_ahg___igeahu_51637,&_ahg______ahuig_51637);
  %if &_ahg_geahui_51637=%AHGcount(&_ahg__geahui_51637) %then  %let _ahg____igeahu_51637= &_ahg____igeahu_51637 %str(%););
  %end;
%end;

%local _ahg__uigeah_51637 ;
%ahggettempname(_ahg__uigeah_51637);

data &_ahg__uigeah_51637;
  set &out;
  keep   %AHG1(&trt,&trt) &by %do _ahg_huigea_51637=1 %to  &_ahg_____ahuige_51637; theVerticalvar&_ahg_huigea_51637  %end;  ;
    %unquote(&_ahg____igeahu_51637);
run;

data hori&out;
  set &out;
run;

data &out;
  set &_ahg__uigeah_51637;
  keep &by  
  %AHG1(&labels,label) 
  %AHG1(&statord,&statord) 
  %AHG1(&trt,&trt)
  stat;
  array allvar(1:&_ahg_____ahuige_51637) theVerticalvar1-theVerticalvar&_ahg_____ahuige_51637;
  do i=1 to dim(allvar);
  %if not %AHGblank(&labels) %then label=left(scan("%sysfunc(compress(&labels,%str(%'%")))",i,'@'));;
  %if not %AHGblank(&statord) %then &statord=i; ;
  stat=input(allvar(i),$50.);

  output;
  end;  
run;
%if not %AHGblank(&trt) %then
  %do;
  %AHGdatasort(data =&out , out = sort&out, by =&by &statord  label &trt );

  proc transpose data=sort&out out=&out(drop=_name_);
    var stat;
    by &by  
    &statord
    %if not %AHGblank(&labels) %then label;   
    ;
    id &trt;
  run;

%local _ahg_____ahuig_51637 _ahg__ahuige_51637 _ahg____huigea_51637 ;
  %AHGvarlist(&out,into=_ahg_____ahuig_51637 );
  %let _ahg____huigea_51637=%AHGremoveWords(&_ahg_____ahuig_51637,&by &statord label );
  %let _ahg__ahuige_51637=%AHGremoveWords(&_ahg_____ahuig_51637,&_ahg____huigea_51637);
  %AHGsortwords(&_ahg____huigea_51637,into=_ahg____huigea_51637);
  %AHGordvar(&out,&_ahg__ahuige_51637 &_ahg____huigea_51637,out=,keepall=0);
  %end;
%end;

%IF %AHGequalmactext(&orie,hori) %then
%do;
data &out;set &_ahg__uigeah_51637;run;
%local _ahg____geahui_51637 ;
%let _ahg____geahui_51637=%AHGrdm()_;
%if %AHGblank(&trt) %then %let &_ahg____geahui_51637.n=1;
%else 
  %do;
  %AHGfreeloop(&out,&trt
  ,cmd=put
  ,in=ahuige
  ,out=ahuige
  ,url=&_ahg____geahui_51637
  ,addloopvar=0);


  %macro dosomething(dsn);
    data &dsn;
      set &dsn(drop=&trt);
    run;
  %mend;

  %AHGfuncloop(%nrbquote(dosomething(ahuige) ) ,loopvar=ahuige,loops=%AHGwords(&_ahg____geahui_51637.AHUIGE,&&&_ahg____geahui_51637.n));


  %AHGmergePrintex(%AHGwords(&_ahg____geahui_51637.AHUIGE,&&&_ahg____geahui_51637.n)
  ,by=&by,drop=,out=&out,print=0,prefix=ahuigecol);

%local _ahg___igeahu_51637 ;
  %AHGvarlist(&out,into=_ahg___igeahu_51637);
  %let _ahg___igeahu_51637=&by %AHGremoveWords(&_ahg___igeahu_51637,&by,dlm=%str( ));
  %AHGordvar(&out,&_ahg___igeahu_51637,out=,keepall=0);
  %end;


%local _ahg____ahuig_51637 ;
  %AHGuniq(%do _ahg_huigea_51637=1 %to &&&_ahg____geahui_51637.n;  &_ahg___huigea_51637   %end;,longlabel);
    
  %AHGrenamekeep(&out,names=&by &_ahg____ahuig_51637,out=&out);
%end; 

data &out;
  set &out(drop=%if &_ahg____ahuige_51637=missing %then &by;  
    %if %AHGequalmactext(&orie,vert) and %substr(&statord,1,8)=ahgdummy %then &statord;
);
run;


%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;
%macro AHGsummary(dsn,var,trt=,by=,out=
,stats=n @ mean\9. @ median\9.2 @ min\9.2 '-' max\9.2
,orie=
,labels=
,obs=100
,Print=1
);

%AHGsumextrt(&dsn,&var,by=&by,trt=&trt ,out=&out
,stats=&stats
,orie=&orie
,labels=&labels
);

%AHGalltocharnew(&out);
%AHGtrimdsn(&out);

data &out;
  set &out(obs=&obs);
run;
%local _ahg_uigeah_76316 _ahg_ahuige_76316 _ahg_igeahu_76316 _ahg_huigea_76316 ;

%ahggettempname(_ahg_uigeah_76316)
%AHGvarinfo(%AHGpurename(&dsn),out=&_ahg_uigeah_76316,info= name label);
%AHGcolumn2mac(&_ahg_uigeah_76316(where=(upcase(name)=upcase("&var"))),varlb,label)
%AHGcolumn2mac(&_ahg_uigeah_76316(where=(upcase(name)=upcase("&trt"))),trtlb,label)
%AHGcolumn2mac(&_ahg_uigeah_76316(where=(upcase(name)=upcase("&by"))),bylb,label)

;

title;
title1 "Dataset:  &dsn   ";
title2 "Variable:  &var %AHG1(&_ahg_ahuige_76316,[&_ahg_ahuige_76316])";
title3 "Treatment: %AHG1(&trt,&trt) %AHG1(&_ahg_igeahu_76316,[&_ahg_igeahu_76316]) ";
Title4 "By: %AHG1(&by,&by)  %AHG1(&_ahg_huigea_76316,[&_ahg_huigea_76316])";
%if &print %then %AHGreportby(&out,0); 
%local _ahg_geahui_76316 ;
%ahggettempname(_ahg_geahui_76316);
data &_ahg_geahui_76316;
  format line $200.;
  line=repeat('#',200);output;
  line="End of  Dataset:%AHGpurename(&dsn)    Variable:&var   Treatment:&trt  By:&by";output;
  line=repeat('#',200);output;
run;
%if &print %then %AHGprt;
%mend;
%macro AHGsum(dsn,var,by,out=stats,print=0,alpha=
,stats=n mean median  min max
,orie=
,labels=
,left=left
,statord=
);


%if %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);


%macro dosomething;
%local _ahg_geahui_132241 _ahg__ahuige_132241 _ahg___huigea_132241 ;
%do _ahg_geahui_132241=1 %to %AHGcount(&stats,dlm=@);
  %let _ahg___huigea_132241=%scan(&stats,&_ahg_geahui_132241,@);
  %let labels=&labels@;
  %do _ahg__ahuige_132241=1 %to %AHGcount(&_ahg___huigea_132241);
    %let labels=&labels %AHGscan2(&_ahg___huigea_132241,&_ahg__ahuige_132241,1,dlm=%str( ),dlm2=\);
  %end;
%end;
%let labels=%substr(&labels,2);
%mend;
%if %AHGblank(&labels) %then %doSomething ;
%if not %AHGblank(&labels) and not %index(&labels,@) %then %let labels=%AHGaddcomma(&labels,comma=@);

%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local _ahg_ahuige_132241 ;
%let _ahg_ahuige_132241=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local _ahg__igeahu_132241 _ahg___ahuig_132241 %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(IsStat,20);
%local _ahg_geahui_132241 _ahg___uigeah_132241 _ahg__uigeah_132241 ;

%let _ahg___uigeah_132241=&dsn;


%do _ahg_geahui_132241=1 %to %AHGcount(&stats);
  %let _ahg___ahuig_132241=%scan(&stats,&_ahg_geahui_132241,%str( ));
  %let isStat&_ahg_geahui_132241=0;
  %if not  (%index(&_ahg___ahuig_132241,%str(%")) or %index(&_ahg___ahuig_132241,%str(%'))) %then
  %do;
  %let isStat&_ahg_geahui_132241=1;
  %let _ahg__uigeah_132241=&_ahg__uigeah_132241 &_ahg___ahuig_132241 ; /*mystats are real stats*/
  %end;
%end;


%AHGsetstatfmt(statfmt=&_ahg__uigeah_132241);
%let _ahg__igeahu_132241=%AHGcount(&stats);

%do _ahg_geahui_132241=1 %to &_ahg__igeahu_132241;
  %let _ahg___ahuig_132241=%scan(&stats,&_ahg_geahui_132241,%str( ));
  %let mystat&_ahg_geahui_132241=%scan(&_ahg___ahuig_132241,1,\);
  %let myformat&_ahg_geahui_132241=%scan(&_ahg___ahuig_132241,2,\);

  %if %AHGblank(&&myformat&_ahg_geahui_132241) and %str(&&isstat&_ahg_geahui_132241) %then 
  %do;
  %global formatof&&mystat&_ahg_geahui_132241;
  %let myformat&_ahg_geahui_132241=&&&&formatof&&mystat&_ahg_geahui_132241;
  %if %AHGblank(&&myformat&_ahg_geahui_132241) %then %let myformat&_ahg_geahui_132241=7.2;
  %end;
  %if &&isstat&_ahg_geahui_132241 %then ;
%end;


  proc means data=&_ahg___uigeah_132241 noprint %AHG1(&alpha,%str(alpha=&alpha));  ;
    var &var;
    by &by;
    output out=&out 
    %do _ahg_geahui_132241=1 %to  &_ahg__igeahu_132241;
    %if &&isstat&_ahg_geahui_132241 %then &&mystat&_ahg_geahui_132241%str(=)&&mystat&_ahg_geahui_132241;
    %end;
    ;
  run;




  proc sql noprint;
    create table old&out as
    select
    
    %do _ahg_geahui_132241=1 %to  %AHGcount(&stats);
      %if &&isstat&_ahg_geahui_132241 %then %AHGd &left(put(&&mystat&_ahg_geahui_132241, &&myformat&_ahg_geahui_132241)) as  &&mystat&_ahg_geahui_132241 ;
      %else  %AHGd &&mystat&_ahg_geahui_132241;
    %end;
    %if not %AHGblank(&by) %then ,%AHGaddcomma(&by);
    
    from &out
    ;quit;

%local _ahg___ahuige_132241 _ahg___igeahu_132241 _ahg_____ahuig_132241 ;
%let _ahg___igeahu_132241=%sysfunc(tranwrd(&labels,@,%str( )));
%let _ahg___ahuige_132241=%AHGqcount(&_ahg___igeahu_132241);
%do _ahg_geahui_132241=1 %to &_ahg___ahuige_132241;
%let _ahg_____ahuig_132241=&_ahg_____ahuig_132241@%AHGqscan(&_ahg___igeahu_132241,&_ahg_geahui_132241);
%end;
%AHGrelabel(old&out,out=&out,pos=,labels=&_ahg_____ahuig_132241 %if not %AHGblank(&by) %then @%AHGaddcomma(&by,comma=@););
%if %AHGequalmactext(&orie,hori)=1 and (not %AHGblank(&by)) %then 
%do;
%local _ahg_huigea_132241 ;
%AHGvarlist(&out,into=_ahg_huigea_132241);
%let _ahg_huigea_132241=&by %AHGremoveWords(&_ahg_huigea_132241,&by,dlm=%str( ));
%AHGordvar(&out,&_ahg_huigea_132241,out=,keepall=0);
%end;

%if %AHGequalmactext(&orie,vert) %then  
%do;

%local _ahg_huigea_132241 _ahg___geahui_132241 _ahg__huigea_132241 _ahg_uigeah_132241 ;
%AHGvarlist(&out,into=_ahg_huigea_132241,dlm=%str( ),global=0);
%local _ahg_igeahu_132241 _ahg____ahuig_132241 
;
%let _ahg____ahuig_132241=0;
%let _ahg___geahui_132241=%AHGcount(&_ahg_ahuige_132241,dlm=@);
;
%do _ahg_geahui_132241=1 %to &_ahg___geahui_132241;
  %let _ahg__huigea_132241=%scan(&_ahg_ahuige_132241,&_ahg_geahui_132241,@);
  %do _ahg_igeahu_132241=1 %to %AHGcount(&_ahg__huigea_132241);
  %let _ahg____ahuig_132241=%eval(&_ahg____ahuig_132241+1);
  %if &_ahg_igeahu_132241=1 %then %let _ahg_uigeah_132241= &_ahg_uigeah_132241   %str(theVerticalvar&_ahg_geahui_132241=) %scan(&_ahg_huigea_132241,&_ahg____ahuig_132241);
  %else  %let _ahg_uigeah_132241= &_ahg_uigeah_132241 ||'  '|| %scan(&_ahg_huigea_132241,&_ahg____ahuig_132241);
  %if &_ahg_igeahu_132241=%AHGcount(&_ahg__huigea_132241) %then  %let _ahg_uigeah_132241= &_ahg_uigeah_132241 %str(;);
  %end;
%end;

%local _ahg__geahui_132241 ;
%ahggettempname(_ahg__geahui_132241);

data &_ahg__geahui_132241;
  set &out;
  keep &by %do _ahg_geahui_132241=1 %to  &_ahg___geahui_132241; theVerticalvar&_ahg_geahui_132241  %end;  ;
    %unquote(&_ahg_uigeah_132241);
run;

data hori&out;
  set &out;
run;

data &out;
  set &_ahg__geahui_132241;
  keep &by  
  %if not %AHGblank(&labels) %then label; 
  %if not %AHGblank(&statord) %then &statord;
  stat;
  array allvar(1:&_ahg___geahui_132241) theVerticalvar1-theVerticalvar&_ahg___geahui_132241;
  do i=1 to dim(allvar);
  %if not %AHGblank(&labels) %then label=left(scan("%sysfunc(compress(&labels,%str(%'%")))",i,'@'));;
  %if not %AHGblank(&statord) %then &statord=i; ;
  stat=input(allvar(i),$50.);

  output;
  end;

  
run;





%end;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;
%macro AHGsumtrt(dsn,var,by,trt,out=stats,print=0,alpha=0.05
,stats=n mean median  min max
,orie=
,labels=
,left=left
,statord=
);

%if %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);


%macro dosomething;
%local _ahg_geahui_183435 _ahg__ahuige_183435 _ahg___geahui_183435 ;
%do _ahg_geahui_183435=1 %to %AHGcount(&stats,dlm=@);
  %let _ahg___geahui_183435=%scan(&stats,&_ahg_geahui_183435,@);
  %let labels=&labels@;
  %do _ahg__ahuige_183435=1 %to %AHGcount(&_ahg___geahui_183435);
    %let labels=&labels %AHGscan2(&_ahg___geahui_183435,&_ahg__ahuige_183435,1,dlm=%str( ),dlm2=\);
  %end;
%end;
%let labels=%substr(&labels,2);
%mend;
%if %AHGblank(&labels) %then %doSomething ;

%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%if %AHGequalmactext(&orie,hori) and  %AHGblank(&statord) %then %let statord=statord34325435;


%local _ahg_ahuige_183435 ;
%let _ahg_ahuige_183435=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local _ahg__igeahu_183435 _ahg___ahuig_183435 %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(IsStat,20);
%local _ahg_geahui_183435 _ahg_____ahuig_183435 _ahg__uigeah_183435 ;

%ahggettempname(_ahg_____ahuig_183435);

%if not %AHGblank(&by) %then %AHGdatasort(data =&dsn , out =&_ahg_____ahuig_183435 , by = &by &trt );
%else %let sortout=&dsn;


%do _ahg_geahui_183435=1 %to %AHGcount(&stats);
  %let _ahg___ahuig_183435=%scan(&stats,&_ahg_geahui_183435,%str( ));
  %let isStat&_ahg_geahui_183435=0;
  %if not  (%index(&_ahg___ahuig_183435,%str(%")) or %index(&_ahg___ahuig_183435,%str(%'))) %then
  %do;
  %let isStat&_ahg_geahui_183435=1;
  %let _ahg__uigeah_183435=&_ahg__uigeah_183435 &_ahg___ahuig_183435 ; /*mystats are real stats*/
  %end;
%end;

%AHGsetstatfmt(statfmt=&_ahg__uigeah_183435);
%let _ahg__igeahu_183435=%AHGcount(&stats);

%do _ahg_geahui_183435=1 %to &_ahg__igeahu_183435;
  %let _ahg___ahuig_183435=%scan(&stats,&_ahg_geahui_183435,%str( ));
  %let mystat&_ahg_geahui_183435=%scan(&_ahg___ahuig_183435,1,\);
  %let myformat&_ahg_geahui_183435=%scan(&_ahg___ahuig_183435,2,\);
  %if %AHGblank(&&myformat&_ahg_geahui_183435) and %str(&&isstat&_ahg_geahui_183435) %then 
  %do;
  %global formatof&&mystat&_ahg_geahui_183435;
  %let myformat&_ahg_geahui_183435=&&&&formatof&&mystat&_ahg_geahui_183435;
  %if %AHGblank(&&myformat&_ahg_geahui_183435) %then %let myformat&_ahg_geahui_183435=7.2;
  %end;
  %if &&isstat&_ahg_geahui_183435 %then ;
%end;

  proc means data=&_ahg_____ahuig_183435 noprint alpha=&alpha;;
    var &var;
    by &by &trt;
    output out=&out 
    %do _ahg_geahui_183435=1 %to  &_ahg__igeahu_183435;
    %if &&isstat&_ahg_geahui_183435 %then &&mystat&_ahg_geahui_183435%str(=)&&mystat&_ahg_geahui_183435;
    %end;
    ;
  run;

  proc sql noprint;
    create table old&out as
    select
    %do _ahg_geahui_183435=1 %to  %AHGcount(&stats);
      %if &&isstat&_ahg_geahui_183435 %then %ahgd &left(put(&&mystat&_ahg_geahui_183435, &&myformat&_ahg_geahui_183435)) as  &&mystat&_ahg_geahui_183435 ;
      %else  %ahgd &&mystat&_ahg_geahui_183435 as mystat&_ahg_geahui_183435;
    %end;
    ,%AHGaddcomma(&by &trt)
    from &out
    ;quit;

%local _ahg___igeahu_183435 _ahg____ahuige_183435 _ahg____uigeah_183435 ;
%let _ahg____ahuige_183435=%sysfunc(tranwrd(&labels,@,%str( )));
%let _ahg___igeahu_183435=%AHGqcount(&_ahg____ahuige_183435);
%do _ahg_geahui_183435=1 %to &_ahg___igeahu_183435;
%let _ahg____uigeah_183435=&_ahg____uigeah_183435%ahgd(d=@)%AHGqscan(&_ahg____ahuige_183435,&_ahg_geahui_183435);
%end;
%AHGrelabel(old&out,out=&out,pos=,labels=&_ahg____uigeah_183435@%AHGaddcomma(&by &trt,comma=@));

%if &orie=hori %then  
%do;

%AHGfreeloop(&out,&trt
,cmd=put
,in=ahuige
,out=ahuige
,url=stat_
,addloopvar=0);


%macro dosomething(dsn);
  data &dsn;
    set &dsn(drop=&trt);
  run;
%mend;

%AHGfuncloop(%nrbquote(dosomething(ahuige) ) ,loopvar=ahuige,loops=%AHGwords(stat_AHUIGE,&stat_n));




%AHGmergePrintex(%AHGwords(stat_AHUIGE,&stat_n)
,by=&by,drop=,out=&out,print=0,prefix=ahuigecol);

%local _ahg_huigea_183435 ;
%AHGvarlist(&out,into=_ahg_huigea_183435);
%let _ahg_huigea_183435=&by %AHGremoveWords(&_ahg_huigea_183435,&by,dlm=%str( ));
%AHGordvar(&out,&_ahg_huigea_183435,out=,keepall=0);
%end;

%if &orie=vert %then  
%do;

%local _ahg_huigea_183435 _ahg____huigea_183435 _ahg__huigea_183435 _ahg_uigeah_183435 ;
%AHGvarlist(&out,into=_ahg_huigea_183435,dlm=%str( ),global=0);
%local _ahg_igeahu_183435 _ahg___uigeah_183435 
;
%let _ahg___uigeah_183435=0;
%let _ahg____huigea_183435=%AHGcount(&_ahg_ahuige_183435,dlm=@);
;
%do _ahg_geahui_183435=1 %to &_ahg____huigea_183435;
  %let _ahg__huigea_183435=%scan(&_ahg_ahuige_183435,&_ahg_geahui_183435,@);
  %do _ahg_igeahu_183435=1 %to %AHGcount(&_ahg__huigea_183435);
  %let _ahg___uigeah_183435=%eval(&_ahg___uigeah_183435+1);
  %if &_ahg_igeahu_183435=1 %then %let _ahg_uigeah_183435= &_ahg_uigeah_183435   %str(theVerticalvar&_ahg_geahui_183435=) %scan(&_ahg_huigea_183435,&_ahg___uigeah_183435);
  %else  %let _ahg_uigeah_183435= &_ahg_uigeah_183435 ||'  '|| %scan(&_ahg_huigea_183435,&_ahg___uigeah_183435);
  %if &_ahg_igeahu_183435=%AHGcount(&_ahg__huigea_183435) %then  %let _ahg_uigeah_183435= &_ahg_uigeah_183435 %str(;);
  %end;
%end;

%local _ahg___huigea_183435 ;
%ahggettempname(_ahg___huigea_183435);

data &_ahg___huigea_183435;
  set &out;
  keep &by &trt %do _ahg_geahui_183435=1 %to  &_ahg____huigea_183435; theVerticalvar&_ahg_geahui_183435  %end;  ;
    %unquote(&_ahg_uigeah_183435);
run;

data hori&out;
  set &out;
run;

data new&out;
  set &_ahg___huigea_183435;
  keep &by &trt  
  %if not %AHGblank(&labels) %then label; 
  %if not %AHGblank(&statord) %then &statord;
  stat;
  array allvar(1:&_ahg____huigea_183435) theVerticalvar1-theVerticalvar&_ahg____huigea_183435;
  do i=1 to dim(allvar);
  %if not %AHGblank(&labels) %then label=left(scan("%sysfunc(compress(&labels,%str(%'%")))",i,'@'));;
  %if not %AHGblank(&statord) %then &statord=i; ;
  stat=input(allvar(i),$50.);
  output;
  end;
run;

%AHGdatasort(data =new&out , out = sort&out, by =&by &statord  label &trt );

proc transpose data=sort&out out=&out(drop=&statord _name_);
  var stat;
  by &by  
  &statord
  %if not %AHGblank(&labels) %then label;   
  ;
  id &trt;
run;

%local _ahg__geahui_183435 _ahg____ahuig_183435 _ahg___ahuige_183435 ;
%AHGvarlist(&out,into=_ahg__geahui_183435 );
%let _ahg___ahuige_183435=%AHGremoveWords(&_ahg__geahui_183435,&by &statord label );
%let _ahg____ahuig_183435=%AHGremoveWords(&_ahg__geahui_183435,&_ahg___ahuige_183435);
%AHGsortwords(&_ahg___ahuige_183435,into=_ahg___ahuige_183435);
%AHGordvar(&out,&_ahg____ahuig_183435 &_ahg___ahuige_183435,out=,keepall=0);


%end;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;
%macro AHGtime(id,pre=ahuigetimePoint);
%if %AHGblank(&id) %then %let id=0;
%global &pre&id;
data _null_;
  call symput("&pre&id",put(time(),time8.));
run;
%mend;
%macro AHGtitleft(prefix,ls=223);
%if %AHGblank(&prefix) %then %let prefix=AHG;
%local _ahg_ahuige_517326 ;
%do _ahg_ahuige_517326=1 %to 10;
%global &prefix.title&_ahg_ahuige_517326 &prefix.ft&_ahg_ahuige_517326;
%if not %AHGblank(%bquote(&&&prefix.title&_ahg_ahuige_517326)) %then title&_ahg_ahuige_517326 justify=left "%AHGapplyls(%bquote(&&&prefix.title&_ahg_ahuige_517326),&ls)";
%else title&_ahg_ahuige_517326;
;
%if not %AHGblank(%bquote(&&&prefix.ft&_ahg_ahuige_517326)) %then footnote&_ahg_ahuige_517326 justify=left "%AHGapplyls(%bquote(&&&prefix.ft&_ahg_ahuige_517326),&ls)";
%else footnote&_ahg_ahuige_517326 ;
; 
%end;
%mend;
%macro AHGtop(dsn,var,by,out=,n=5,desc=1);
  proc sql noprint;
    create table &out as
    select &var,&by
    from &dsn
    order by &by %if &desc %then descending;
    ;
    quit;
  data &out;
    set &out(obs=&n);
  run;
%mend;
%macro ahgtran(dsn,out=);
%local _ahg_uigeah_211489 _ahg_huigea_211489 _ahg_ahuige_211489 ;
%ahggettempname(_ahg_uigeah_211489);
%ahggettempname(_ahg_huigea_211489);
 
%AHGvarlist(&dsn,into=_ahg_ahuige_211489,dlm=%str( ),global=0,print=1);

data &_ahg_uigeah_211489;
  set &dsn;
  theid=_n_;
  dlm='############################';  
;
run;


proc transpose data=&_ahg_uigeah_211489 out=&_ahg_huigea_211489;
  var   &_ahg_ahuige_211489 dlm;
  by theid;
run;

data &out(where=(not missing(value)));
  set &_ahg_huigea_211489;
  format value $80.;
  keep _name_ value;
  col1=left(col1);
  begin=1;
  if length(trim(col1))<=50 then 
    do;
    value=col1;
    output;
    end;
  else 
    do end=1 to  length(col1) ;
    if (end-begin+1>=50 and substr(col1,end,1)=' ') or length(col1)=end then
      do;
      if begin ~= 1 then _name_='';
      value=substr(col1,begin,end-begin+1);
      output;
      begin=end+1;
      end;
    end;
run;

%mend;
%macro AHGtrimDsn(dsn,out=,min=3,left=1);
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%local _ahg_huigea_985242 ;
%AHGnobs(&dsn,into=_ahg_huigea_985242);
%if &_ahg_huigea_985242>0 %then
    %do;
%local _ahg_geahui_985242 _ahg__uigeah_985242 _ahg_uigeah_985242 _ahg_igeahu_985242 _ahg__huigea_985242 _ahg__ahuige_985242 _ahg_ahuige_985242 ;

    %AHGvarlist(&dsn,into=_ahg_ahuige_985242,dlm=%str( ),global=0);

    %AHGallchar(&dsn,into=_ahg__uigeah_985242);

    %let _ahg_igeahu_985242=%AHGcount(&_ahg__uigeah_985242);
    %let _ahg__huigea_985242=%AHGrdm(20);

    data test _null_;
      retain 
      %do _ahg_uigeah_985242=1 %to &_ahg_igeahu_985242;
      &_ahg__huigea_985242.&_ahg_uigeah_985242 
      %end;
      &min
      ;
      set &dsn end=end;
      %do _ahg_uigeah_985242=1 %to &_ahg_igeahu_985242;
      if length(%scan(&_ahg__uigeah_985242,&_ahg_uigeah_985242))> &_ahg__huigea_985242.&_ahg_uigeah_985242 then &_ahg__huigea_985242.&_ahg_uigeah_985242=length(%scan(&_ahg__uigeah_985242,&_ahg_uigeah_985242));
      %end;

      keep &_ahg__huigea_985242:;
      if end then  call symput('_ahg__ahuige_985242',compbl(''
      %do _ahg_uigeah_985242=1 %to &_ahg_igeahu_985242;
       ||trim(put(&_ahg__huigea_985242.&_ahg_uigeah_985242,best.))||' '
      %end;
      ))

      ;
    run;
%local _ahg___ahuig_985242 ;
    %let _ahg___ahuig_985242=%AHGrdm(25);
    data &out(rename=(
    %do _ahg_uigeah_985242=1 %to &_ahg_igeahu_985242;
      &_ahg___ahuig_985242&_ahg_uigeah_985242=%scan(&_ahg__uigeah_985242,&_ahg_uigeah_985242)  
    %end;
    ));
      format
      %do _ahg_uigeah_985242=1 %to &_ahg_igeahu_985242;
      &_ahg___ahuig_985242&_ahg_uigeah_985242 $%scan(&_ahg__ahuige_985242,&_ahg_uigeah_985242). 
      %end;
      ;
      drop 
      %do _ahg_uigeah_985242=1 %to &_ahg_igeahu_985242;
      %scan(&_ahg__uigeah_985242,&_ahg_uigeah_985242)  
      %end;
      ;
      set &dsn;
      %do _ahg_uigeah_985242=1 %to &_ahg_igeahu_985242;
      %if &left %then &_ahg___ahuig_985242&_ahg_uigeah_985242=left(%scan(&_ahg__uigeah_985242,&_ahg_uigeah_985242));
      %else &_ahg___ahuig_985242&_ahg_uigeah_985242=%scan(&_ahg__uigeah_985242,&_ahg_uigeah_985242);
      ;
      %end;
        
    run;

    %AHGordvar(&out,&_ahg_ahuige_985242,out=&out,keepall=0);
    %end;
%mend;

%macro AHGtwoDim(dsn,dmdsn,catVar,topCat,idVar,byVar
,withTotal=0,expected=,orie=vert,out=twoDim_out
,_Missing_=0,imputed=1);
%local _ahg_huigea_801499 _ahg_igeahu_801499 _ahg___ahuig_801499 _ahg_____ahuig_801499 _ahg__uigeah_801499 _ahg____ahuige_801499 _ahg___uigeah_801499 _ahg_geahui_801499 ;
%let _ahg_geahui_801499=%AHGaddcomma(&byvar);

%ahggettempname(_ahg___ahuig_801499);
%ahggettempname(_ahg_____ahuig_801499);
%ahggettempname(_ahg_huigea_801499);
%ahggettempname(_ahg_igeahu_801499);
%ahggettempname(_ahg__uigeah_801499);
%ahggettempname(_ahg____ahuige_801499);

%let _ahg___uigeah_801499=%AHGrdm();

%if %AHGblank(&byvar) %then %let byvar=%AHG_(everyone);
%if %AHGblank(&topcat) %then %let topcat=%AHG_(total);
%local _ahg__huigea_801499 ;
%let _ahg__huigea_801499=0;

%AHGvarisnum(&dsn,&catvar,into=_ahg__huigea_801499);
 
%AHGmergedsn(&dsn,&dmdsn,&_ahg____ahuige_801499,by=&idvar,keepINvar=1,joinstyle=left/*left right full matched*/);


proc sql;

  create table &_ahg_____ahuig_801499 as
  select distinct &_ahg_geahui_801499
  from &_ahg____ahuige_801499(keep=&byvar)
  ;
  create table &_ahg__uigeah_801499 as
  select *
  from &dmdsn,&_ahg_____ahuig_801499

  ;
  quit;
  
%AHGmergedsn(&_ahg____ahuige_801499,&_ahg__uigeah_801499,&_ahg____ahuige_801499.10,by=&idvar &byvar &topcat,keepINvar=1,joinstyle=full/*left right full matched*/);



proc sql;
  create table &_ahg_huigea_801499 as
  select *
  %if &_ahg__huigea_801499=1 %then ,%AHGbool(%ahgaddcomma(&catvar))  as %AHG_(atleast1) label="Presence",ifn(max(x),max(&catvar),0,0) as %AHG_(maxlevel) label="Max level";
  %else ,not Missing(&catvar)  as %AHG_(atleast1) label="Presence",ifc(not Missing(max(&catvar)),max(&catvar),'_Missing_','_Missing_') as %AHG_(maxlevel) label="Max level";
   
  from &_ahg____ahuige_801499.10
  group by  &_ahg_geahui_801499, &idvar
  ;
  quit;


%AHGdatasort(data = &_ahg_huigea_801499, out =&_ahg_igeahu_801499 , by = &byvar &topcat &idvar);
data &_ahg_igeahu_801499;
  set &_ahg_igeahu_801499;
  by &byvar &topcat &idvar;
  if first.&idvar then %AHG_(mainId)=1;
  if %AHG_(mainid)=1 and  %AHGbool(&byvar) then output;
run;

 
proc print;run;



%local _ahg____huigea_801499 _ahg___igeahu_801499 _ahg_ahuige_801499 ;
%ahggettempname(_ahg____huigea_801499);
%ahggettempname(_ahg___igeahu_801499);

%put varIsNum=&_ahg__huigea_801499 and  &imputed;

%if %eval(&_ahg__huigea_801499 and  &imputed) %then %let _ahg_ahuige_801499=%AHG_(maxlevel);
%else %let _ahg_ahuige_801499=&catvar;

%* forcefully test max level;
%let _ahg_ahuige_801499=%AHG_(maxlevel);

option byline;
proc freq data=&_ahg_igeahu_801499 ;
  tables  &_ahg_ahuige_801499*&topcat /Missing;
  
  by &byvar;
 

run;


 %AHGfreqDefault(&_ahg_igeahu_801499,&_ahg_ahuige_801499,expected=&expected,out=&_ahg___igeahu_801499,by=&topcat &byvar,print=1,rename=1
,keep=value cell frequency percent
);


%AHGfreesplit(&_ahg___igeahu_801499,&topcat ,outPref=__&topcat,bydsn=&_ahg____huigea_801499);

%local _ahg_uigeah_801499 ;
%ahggettempname(_ahg_uigeah_801499);

%local _ahg____ahuig_801499 ;
%AHGnobs(&_ahg____huigea_801499,into=_ahg____ahuig_801499);

%AHGmergeprintEx(
%AHGwords(__&topcat,&_ahg____ahuig_801499)
,by=&byvar value 
,keep=
,drop=&topcat
,label=label
,out=&out,print=0
,prefix=ahuigecol
,clean=0
);



%local _ahg__igeahu_801499 ;
 
%AHGvarlist(&out,into=_ahg__igeahu_801499);

%local _ahg___ahuige_801499 ;
%AHGvarlist(__&topcat.1,into=_ahg___ahuige_801499);
%let _ahg___ahuige_801499=%AHGcount(&_ahg___ahuige_801499);
%put ######;
;

%local _ahg___geahui_801499 _ahg__ahuige_801499 ;
%do _ahg___geahui_801499=1 %to %AHGcount(&_ahg__igeahu_801499);
%local v&_ahg___geahui_801499
;
  %let v&_ahg___geahui_801499=%scan(&_ahg__igeahu_801499,&_ahg___geahui_801499);
%end;

%local _ahg___huigea_801499 _ahg__geahui_801499 _ahg____uigeah_801499 ;

%if &byvar=%AHG_(everyone) %then %let _ahg____uigeah_801499=2;
%else  %let _ahg____uigeah_801499=1;;


%let _ahg__geahui_801499=%eval(1+%AHGcount(&byvar));
proc report data=&out;
  column 
  %do _ahg___geahui_801499=&_ahg____uigeah_801499 %to &_ahg__geahui_801499;
    &&v&_ahg___geahui_801499
  %end;
  %let onecolcnt=%eval(&_ahg___ahuige_801499-1-&_ahg__geahui_801499);
  %do _ahg___geahui_801499=1 %to &_ahg____ahuig_801499;
  ("&topcat &_ahg___geahui_801499" 
  %do _ahg__ahuige_801499=1 %to &onecolcnt;
    %let _ahg___huigea_801499=%eval(&_ahg__geahui_801499+
        (&_ahg___geahui_801499-1)*
        &onecolcnt
        +&_ahg__ahuige_801499);
    &&v&_ahg___huigea_801499
  %end;
  ) 
  %end;
  ;
  %do _ahg___geahui_801499=&_ahg____uigeah_801499 %to %AHGcount(&_ahg__igeahu_801499);
    define &&v&_ahg___geahui_801499 /display;
  %end;
  
run;

%exitp:
%mend ;
%macro AHGtype(dsn,var);
%local _ahg_ahuige_955875 ;
%let _ahg_ahuige_955875=  %sysfunc(open(&dsn,in));
%sysfunc(vartype(&_ahg_ahuige_955875,%sysfunc(varnum(&_ahg_ahuige_955875,&var))))
%mend;
%MACRO AHGuniq(mac,into);
%local _ahg_huigea_64691 _ahg_ahuige_64691 ;
%ahggettempname(_ahg_ahuige_64691);
data &_ahg_ahuige_64691;
  format word $100.;
  %do _ahg_huigea_64691=1 %to %AHGcount(&mac);
  word="%lowcase(%scan(&mac,&_ahg_huigea_64691))";
  i=&_ahg_huigea_64691;
  output;
  %end;
run;


%AHGdatasort(data = &_ahg_ahuige_64691, out = , by =word );

data &_ahg_ahuige_64691;
  set &_ahg_ahuige_64691;
  format ord $3.;
  retain ord;
  by word;
  if first.word then ord='1';
  else ord=%AHGputn(input(ord,best.)+1);
run;

%AHGdatasort(data = &_ahg_ahuige_64691, out = , by =i);

data &_ahg_ahuige_64691;
  set &_ahg_ahuige_64691;
  if ord ne '1' then word=compress(word||'_'||ord);
run;

proc sql noprint;
  select trim(word) into :&into separated by ' '
  from &_ahg_ahuige_64691
  ;
  quit;

%mend;
%macro AHGupdir(dir,n=1,dlm=\);
%local _ahg_ahuige_463801 _ahg_huigea_463801 ;
%let _ahg_huigea_463801=%bquote(s/(.*)\&dlm[^\&dlm]+\&dlm?/\1/);
%do _ahg_ahuige_463801=1 %to &n;
%let dir=%sysfunc(prxchange(&_ahg_huigea_463801,1,&dir));
%end;&dir
%mend;

%macro AHGupdir(dir,n=1,dlm=\);
%local _ahg_ahuige_852694 _ahg_huigea_852694 ;
%let _ahg_huigea_852694=%bquote(s/(.*)\&dlm[^\&dlm]+\&dlm?/\1/);
%do _ahg_ahuige_852694=1 %to &n;
%let dir=%sysfunc(prxchange(&_ahg_huigea_852694,1,&dir));
%end;&dir
%mend;

%macro AHGup(dir,level );
  %if %AHGblank(&level) %then %let level=1;
%local _ahg_uigeah_19792 _ahg_igeahu_19792 _ahg_ahuige_19792 _ahg_huigea_19792 ;
  %let dir=%sysfunc(compress(&dir));
  %let _ahg_uigeah_19792=%bquote(/);
  %if %index(&dir,\) %then %let _ahg_uigeah_19792=\;
  %if %bquote(%substr(&dir,1,1))=&_ahg_uigeah_19792 %then %let _ahg_huigea_19792=&_ahg_uigeah_19792%scan(&dir,1,&_ahg_uigeah_19792);
  %else %let _ahg_huigea_19792=%scan(&dir,1,&_ahg_uigeah_19792);
  %let _ahg_ahuige_19792=%AHGcount(&dir,dlm=&_ahg_uigeah_19792);
  %do _ahg_igeahu_19792=2 %to %eval(&_ahg_ahuige_19792-&level);
  %let _ahg_huigea_19792=&_ahg_huigea_19792&_ahg_uigeah_19792%scan(&dir,&_ahg_igeahu_19792,&_ahg_uigeah_19792);
  %end;
  &_ahg_huigea_19792
%mend;
%macro AHGuseLabel(dsn,out=,dlm=%str( ),remove=,to=);
%local _ahg_huigea_306154 _ahg_ahuige_306154 ;
%if %sysfunc(exist(&dsn)) %then
%do;


data _null_;
length varlist $ 32000;

tableid=open("&dsn",'i');
varlist=' ';
do i=1 to  attrn(tableid,'nvars');
   label=put(varlabel(tableid,i),$100.);
   %do _ahg_ahuige_306154=1 %to %AHGcount(&remove);
   label=tranwrd(upcase(label),upcase("%scan(&remove,&_ahg_ahuige_306154)"),"%scan(&to,&_ahg_ahuige_306154)");
   %end;
   caplabel=put('', $100.);
   j=0;
   do until (scan(label,j+1) eq ' ');
     j=sum(j,1);
     word=scan(label,j);
     word=lowcase(word);
     substr(word,1,1)=upcase(substr(word,1,1));
     caplabel=trim(caplabel)||word;
     caplabel=compress(caplabel,compress(caplabel,'abcdefghijklmnopqrstuvwxyz'||upcase('abcdefghijklmnopqrstuvwxyz0123456789')));

     if index('1234567890',substr(caplabel,1,1)) then caplabel='_'||caplabel;
   end;
   if length(caplabel)>32 then caplabel=compress(caplabel,'aeiouAEIOU');
   varlist=trim(varlist)||"&dlm ;rename "||varname(tableid,i)||'='||substr(caplabel,1,32)||';';
end;
call symput("_ahg_huigea_306154", varlist);
rc=close(tableid);
run;
;
%if not %AHGblank(&out) %then
%do;
data &out;
  set &dsn;
  &_ahg_huigea_306154;
run;
%end;


%end;

%mend;


%macro AHGuserDir;
\\gh3nas01\gh3nas_sales.grp\LCDDMAC\STATS\SA\Macro library\users\&sysuserid
%mend;
%macro AHGvarinfo(dsn,out=varinfoout,info= name  type  length num fmt);
%local _ahg_ahuige_337732 _ahg_huigea_337732 ;
%let _ahg_huigea_337732=name  type   length  format  informat label ;     

data &out(keep=&info);
length dsn $40 name $32  type $4  length 8 format $12  informat $10 label $50  num 8 superfmt fmt $12;
tableid=open("&dsn",'i');
varlist=' ';
dsn="&dsn";
do i=1 to  attrn(tableid,'nvars');
   %do _ahg_ahuige_337732=1 %to %AHGcount(&_ahg_huigea_337732);
   %if %scan(&_ahg_huigea_337732,&_ahg_ahuige_337732) ne num 
    and %scan(&_ahg_huigea_337732,&_ahg_ahuige_337732) ne fmt 
    and %scan(&_ahg_huigea_337732,&_ahg_ahuige_337732) ne superfmt 
    %then  %scan(&_ahg_huigea_337732,&_ahg_ahuige_337732)= var%scan(&_ahg_huigea_337732,&_ahg_ahuige_337732)(tableid,i);;
   %end;
   num=varnum(tableid,varname(tableid,i)) ;
   if type='C' then fmt='$'||compress(put(length,best.))||'.';
   else fmt=compress(put(length,best.))||'.';
   superfmt=format;
   if missing(superfmt) then superfmt=fmt;
   output;
end;
rc=close(tableid);
run;   

%mend;
%macro AHGvarisnum(dsn,var,into=varisnum);
%local _ahg_ahuige_496264 ;
%ahggettempname(_ahg_ahuige_496264);
%AHGvarinfo(&dsn,out=&_ahg_ahuige_496264,info= name type);
data _null_;
  set &_ahg_ahuige_496264(where=(%AHGequaltext(name,"&var")  ) );
  if type='N' then call symput("&into",'1');
  else call symput("&into",'0');
run;
%mend;
%macro AHGvarlistm(dsn,dlm=%str( ));
%local _ahg_uigeah_633626 _ahg_ahuige_633626 _ahg_huigea_633626 _ahg_igeahu_633626 ;

%if %sysfunc(exist(&dsn)) %then
%do;


%let _ahg_ahuige_633626=%sysfunc(open(&dsn,i));

%do _ahg_uigeah_633626=1 %to  %sysfunc(attrn(&_ahg_ahuige_633626,nvars));
   %let _ahg_huigea_633626=&_ahg_huigea_633626&dlm.%sysfunc(varname(&_ahg_ahuige_633626,&_ahg_uigeah_633626));
%end;

%let _ahg_igeahu_633626=%sysfunc(close(&_ahg_ahuige_633626));
%end;
&_ahg_huigea_633626

%mend;
%macro AHGvarlist(dsn,Into=,dlm=%str( ),global=0,withlabel=0,print=0);
%if %sysfunc(exist(&dsn)) %then
%do;

%if &global %then %global &into;;
data _null_;
length varlist $ 8000;

tableid=open("&dsn",'i');
varlist=' ';
do i=1 to  attrn(tableid,'nvars');
   varlist=trim(varlist)||"&dlm"||varname(tableid,i);
   %if &withlabel %then       varlist=trim(varlist)||"&dlm "||'/*'||trim(varlabel(tableid,i))||'*/';; ;
end;
call symput("&into", varlist);
rc=close(tableid);
run;
%end;
%else %let &into=;
%if &print %then ;

%mend;
%macro AHGwhere(subjid);
 1
%mend;
%macro AHGwildall(string,theword);
%local _ahg_huigea_121 _ahg_ahuige_121 _ahg_uigeah_121 ;
%do _ahg_huigea_121=1 %to %AHGcount(&string);
%let _ahg_ahuige_121=%scan(&string,&_ahg_huigea_121,%str( ));
%AHGwild(&_ahg_ahuige_121,&theword)
%end;

%mend;

%macro AHGwild(string,word);
%local _ahg_huigea_152208 _ahg_igeahu_152208 ;
  %let _ahg_igeahu_152208=%AHGcount(&word,dlm=@);

%local _ahg_uigeah_152208 _ahg___ahuig_152208 _ahg_ahuige_152208 _ahg_geahui_152208 ;
    %let _ahg_uigeah_152208=&string;
    %let _ahg_huigea_152208=0;
    %let _ahg_geahui_152208=0;
    %do %until( (&_ahg_huigea_152208=&_ahg_igeahu_152208) or &_ahg_geahui_152208) ;
      %ahgincr(_ahg_huigea_152208)
      %let _ahg_ahuige_152208=%scan(&word,&_ahg_huigea_152208,@);
      %let _ahg___ahuig_152208=%AHGpos(&_ahg_uigeah_152208,&_ahg_ahuige_152208);
      %if &_ahg___ahuig_152208>0 %then   %let _ahg_uigeah_152208=%substr(&_ahg_uigeah_152208,%eval(&_ahg___ahuig_152208+%length(&_ahg_ahuige_152208)));
      %else %let _ahg_geahui_152208=1;

    %end;
    %if (not &_ahg_geahui_152208) %then &string;
%mend;  
%macro AHGwinorunix;
    %if %UPCASE(%substr(&sysscp,1,3)) =WIN  %then WIN;
    %else UNIX;
%mend;
%macro AHGwintemp;
C:\Users\AHG\AppData\Local\Temp 
%mend;
%macro AHGwords(word,n,base=1);
%local _ahg_ahuige_884159 ;
%if not %index(&word,@) %then %let word=&word@;
%if %AHGcount(&n)=1 %then
  %do _ahg_ahuige_884159=%eval(&base) %to %eval(&n+&base-1);
  %sysfunc(tranwrd(&word,@,&_ahg_ahuige_884159))
  %end;
%else 
  %do _ahg_ahuige_884159=1 %to %AHGcount(&n) ;
  %sysfunc(tranwrd(&word,@,%scan(&n,&_ahg_ahuige_884159))) 
  %end;

%mend;





%macro AHGworkout(fromlib,dsns,tolib=work,pre=,where=%str(where 1));
  %if %AHGblank(&dsns) %then %AHGdsnInLib(lib=&fromlib,list=dsns,lv=1);;
%local _ahg_ahuige_871351 ;
  %do _ahg_ahuige_871351=1 %to %AHGcount(&dsns);
    data &tolib..&pre%scan(&dsns,&_ahg_ahuige_871351);
      set &fromlib..%scan(&dsns,&_ahg_ahuige_871351);
      &where ;
    run;
  %end;

%mend;
%macro AHGzero(n,length);
  %sysfunc(putn(&n,&length))
%mend;
%macro summary1(dsn,var,trt=,by=,out=
,stats=n @ mean\9. @ median\9.2 @ min\9.2 '-' max\9.2
,orie=
,labels=
,obs=100
,Print=1
);

%AHGsumextrt(&dsn,&var,by=&by,trt=&trt ,out=&out
,stats=&stats
,orie=&orie
,labels=&labels
);

%AHGalltocharnew(&out);
%AHGtrimdsn(&out);

data &out;
  set &out(obs=&obs);
run;
%local _ahg_uigeah_988583 _ahg_ahuige_988583 _ahg_igeahu_988583 _ahg_huigea_988583 ;

%ahggettempname(_ahg_uigeah_988583)
%AHGvarinfo(%AHGpurename(&dsn),out=&_ahg_uigeah_988583,info= name label);
%AHGcolumn2mac(&_ahg_uigeah_988583(where=(upcase(name)=upcase("&var"))),varlb,label)
%AHGcolumn2mac(&_ahg_uigeah_988583(where=(upcase(name)=upcase("&trt"))),trtlb,label)
%AHGcolumn2mac(&_ahg_uigeah_988583(where=(upcase(name)=upcase("&by"))),bylb,label)

;

title;
title1 "Dataset:  &dsn   ";
title2 "Variable:  &var %AHG1(&_ahg_ahuige_988583,[&_ahg_ahuige_988583])";
title3 "Treatment: %AHG1(&trt,&trt) %AHG1(&_ahg_igeahu_988583,[&_ahg_igeahu_988583]) ";
Title4 "By: %AHG1(&by,&by)  %AHG1(&_ahg_huigea_988583,[&_ahg_huigea_988583])";
%if &print %then %AHGreportby(&out,0); 
%local _ahg_geahui_988583 ;
%ahggettempname(_ahg_geahui_988583);
data &_ahg_geahui_988583;
  format line $200.;
  line=repeat('#',200);output;
  line="End of  Dataset:%AHGpurename(&dsn)    Variable:&var   Treatment:&trt  By:&by";output;
  line=repeat('#',200);output;
run;
%if &print %then %AHGprt;
%mend;

%macro AHGtwoDim(dsn=,dmdsn=,catVar=,topCat=,idVar=,byVar=
,withTotal=0,expected=,orie=vert,out=twoDim_out
,_Missing_=0,imputed=1);
%local new uniq _idvar _byvar fullset orig rdm _sqlby;
%if %AHGblank(&byvar) %then %let byvar=%AHG_(byvar);
%if %AHGblank(&topCat) %then %let topCat=%AHG_(topcat);
%if %AHGblank(&idvar) %then %let idvar=%AHG_(idvar);
%let _sqlby=%AHGaddcomma(&byvar);
%AHGgettempname(_idvar);
%AHGgettempname(_byvar);
%AHGgettempname(new);
%AHGgettempname(uniq);
%AHGgettempname(fullset);
%AHGgettempname(orig);

%let rdm=%AHGrdm();


%local varIsNum;
%let varIsNum=0;
/* %if %substr(&_Missing_,1,1)=%str(%') %then %let varIsNum=0; */
/* %else %let varIsNum=1; */

%AHGvarisnum(&dsn,&catVar,into=varIsNum);

%if &dmdsn ne %then
  %do;
   
  %AHGmergedsn(&dsn,&dmdsn,&orig,by=&idVar,keepINvar=1,joinstyle=left/*left right full matched*/);
  data &orig;
    set &orig;
    %AHG_(byvar)='_';
 
  run;
  
  
  proc sql;
  
    create table &_byvar as
    select distinct &_sqlBy
    from &orig(keep=&byVar)
    ;
    create table &fullset as
    select *,'_' as %AHG_(byvar),'_' as %AHG_(topcat),monotonic() as %AHG_(idvar); 
    from &dmdsn,&_byVar
  
    ;
    quit;
    
  %AHGmergedsn(&orig,&fullset,&orig.10,by=&idVar &byVar &topCat,keepINvar=1,joinstyle=full/*left right full matched*/);
  %end;
%else 
  %do;
  data &orig.10;
    set &dsn;
    %AHG_(byvar)='_';
    %AHG_(topcat)='_';
    %AHG_(idvar)=_n_; 
  run;
  %end;


proc sql;
  create table &new as
  select *
  %if &varIsNum=1 %then ,%AHGbool(%ahgaddcomma(&catvar),zero=0)  as %AHG_(atleast1) label="Presence",ifn(max(&catVar) ne .,max(&catVar),0,0) as %AHG_(maxlevel) label="Max level";
  %else ,not Missing(&catVar)  as %AHG_(atleast1) label="Presence",ifc(not Missing(max(&catVar)),max(&catVar),'_Missing_','_Missing_') as %AHG_(maxlevel) label="Max level";
   
  from &orig.10
  group by  &_sqlBy, &idVar
  ;
  quit;


%AHGdatasort(data = &new, out =&uniq , by = &byvar &topCat &idVar);
data &uniq;
  set &uniq;
  by &byvar &topCat &idVar;
  if first.&idVar then %AHG_(mainId)=1;
  if %AHG_(mainid)=1 and  %AHGbool(&byvar) then output;
run;

 
proc print;run;



%local byTopcat freqOut finalScore;
%AHGgettempname(byTopcat);
%AHGgettempname(freqout);

%put varIsNum=&varIsNum and  &imputed;

%if %eval(&varIsNum and  &imputed) %then %let finalScore=%AHG_(maxlevel);
%else %let finalScore=&catVar;

%* forcefully test max level;
%let finalScore=%AHG_(maxlevel);

option byline;
proc freq data=&uniq ;
  tables  &finalScore*&topCat /Missing;
  
  by &byvar;
 

run;


 %AHGfreqDefault(&uniq,&finalScore,expected=&expected,out=&freqout,by=&topCat &byvar,print=1,rename=1
,keep=value cell frequency percent
);


%AHGfreesplit(&freqout,&topCat ,outPref=__&topCat,bydsn=&bytopCat);

%local mergeprintout;
%AHGgettempname(mergeprintout);

%local topCatlv;
%AHGnobs(&bytopCat,into= topCatlv);

%AHGmergeprintEx(
%AHGwords(__&topCat,&topCatlv)
,by=&byvar value 
,keep=
,drop=&topCat
,label=label
,out=&out,print=0
,prefix=ahuigecol
,clean=0
);



%local vlist;
 
%AHGvarlist(&out,Into=vlist);

%local vCnt;
%AHGvarlist(__&topCat.1,Into=vCnt);
%let vCnt=%AHGcount(&vCnt);
%put ######;
%AHGpm(vCnt);

/* %goto exitp; */
%local i j;
%do i=1 %to %AHGcount(&vlist);
  %local v&i;
  %let v&i=%scan(&vlist,&i);
%end;

/* %AHGreportby(mergeprintout,0,topline=,showby=0, */
/* option=nowd nocenter headline,labelopt=%str(option label;)); */
%local justOne commonV beginFrom;

%if &byvar=%AHG_(byvar) %then %let beginFrom=2;
%else  %let beginFrom=1;;


%let commonV=%eval(1+%AHGcount(&byvar));
proc report data=&out;
  column 
  %do i=&beginFrom %to &commonV;
    &&v&i
  %end;
  %let oneColcnt=%eval(&vCnt-1-&commonV);
  %do i=1 %to &topCatlv;
  ("&topcat &i" 
  %do j=1 %to &oneColcnt;
    %let justone=%eval(&commonV+
        (&i-1)*
        &oneColcnt
        +&j);
    &&v&justone
  %end;
  ) 
  %end;
  ;
  %do i=&beginFrom %to %AHGcount(&vlist);
    define &&v&i /display;
  %end;
  
run;

%exitp:
%mend;

%macro AHGbool(vars,zero=);
%local _ahg_ahuige_79250 _ahg_huigea_79250 ;
 1  
%do _ahg_huigea_79250=1 %to %AHGcount(&vars);
%let _ahg_ahuige_79250=%scan(&vars,&_ahg_huigea_79250);
and 
 (catx('',&_ahg_ahuige_79250)>'' and compress(catx('',&_ahg_ahuige_79250)) not in ("&zero",'.'))
%end; 
%mend;
