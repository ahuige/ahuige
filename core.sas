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
	%local i;
	%do i=1 %to %AHGcount(&all);
		%eval(%scan(&all,&i)+&base)
	%end;
%mend;
%macro AHGaddcomma(mac,comma=%str(,) );
%*<spec> add comma or other sep into a mac var like abc xyz to abc,xyz </spec>;

%if %AHGnonblank(&mac) %then %sysfunc(tranwrd(     %sysfunc(compbl(&mac)),%str( ),&comma       ))   ;
%mend;
%macro AHGaddsasautos(dir,clear=0);
%local nowdir;
%let nowdir=%sysfunc(getoption(sasautos));
%if not %index(&dir,%str(%()) %then %let nowdir=(&nowdir);
%if not %index(&dir,%str(%'))  and not %index(&dir,%str(%"))  %then %let dir="&dir";
%let nowdir=&dir %substr(&nowdir,2,%eval(%length(&nowdir)-2));
%if &clear %then option sasautos=(&dir);
%else option sasautos=(&nowdir);
;

proc options option=sasautos;run;
%mend;

%macro AHGaddwords(sentence,words,dlm=%str( ));
%*<spec> add more words to sentence without dup </spec>;

	%AHGremoveWords(&sentence,&words,dlm=&dlm)&dlm&words
	
%mend;


%macro AHGallchar(dsn,into=);
%local allchar ;
%AHGgettempname(allchar);
data _null_;deletefromithere=1;run;
%AHGvarinfo(&dsn,out=&allchar,info= name  type );

data &allchar;
  set &allchar(where=(type='C'));
run;

%AHGdistinctValue(&allchar,name,into=&into,dlm=%str( ));


%mend;
%macro AHGallnum(dsn,into=);

/*data _null_;*/
/*  set &dsn;*/
/*  array into{1} $32000.   _temporary_;*/
/*  array allnum  _numeric_;*/
/*  do over allnum;*/
/*    into(1)=catx(' ',into(1),vname(allnum));*/
/*  end;*/
/*  call symput("&into",into(1));*/
/*  stop;*/
/*run;*/
%local allnum ;
%AHGgettempname(allnum);
%AHGvarinfo(&dsn,out=&allnum,info= name  type );

data &allnum;
  set &allnum(where=(type='N'));
run;

%AHGdistinctValue(&allnum,name,into=&into,dlm=%str( ));



%mend;
%macro AHGalltochar(dsn,out=%AHGbasename(&dsn),prefix=gha);
%local i info ;
%AHGgettempname(info);
data &info;
	set sashelp.vcolumn(where=(
		%AHGequaltext(libname,"%AHGlibname(&dsn)")
		and  %AHGequaltext(memname,"%AHGbasename(&dsn)")
		and %AHGequaltext(type,'num')
		)
);
%local putcmd renamecmd dropcmd;
data &info;
	format putcmd renamecmd dropcmd $1000. ;
	retain putcmd renamecmd dropcmd ' ';
	set &info end=end;
	putcmd=trim(putcmd)||' '||trim(name)||'='|| "put(" ||"&prefix"||trim(name)||',best8.);';
	renamecmd=trim(renamecmd)||' '||trim(name)||'='|| "&prefix"||trim(name);
	dropcmd=trim(dropcmd)||" &prefix"||trim(name);
	if end then 
	do;
	call symput('putcmd',putcmd);
	call symput('renamecmd',renamecmd);
	call symput('dropcmd',dropcmd);
	end;
run;
%*pm(putcmd renamecmd dropcmd);


data &out(drop=&dropcmd);
	set  &dsn(rename=(&renamecmd));
	%unquote(&putcmd);
run;

%exit:
%mend;




﻿%macro AHGalltocharnew(dsn,out=%AHGbasename(&dsn),rename=,zero=0,width=100,name=0);
option mprint;
%local i varlist informat nobs varinfo  %AHGwords(cmd,100);
%AHGgettempname(varinfo);
 
%AHGvarinfo(&dsn,out=&varinfo,info= name  type  length num);
 
data _null_&varinfo;
  set &varinfo;
  format cmd $200.;
  if type='N' then cmd='input(left(put('||name||',best.)),$'||"&width"||'.) as '||name;
  else 
    do;
    if num>=&width then cmd=name;
    else cmd='put('||name||',$'||"&width"||'.) as '||name;
    end;
  call symput('cmd'||%AHGputn(_n_),cmd);
  call symput('nobs',%AHGputn(_n_));
run;
option mprint;
 
proc sql noprint;
  create table &varinfo(drop= AHGdrop) as
  select ' ' as AHGdrop 
    %do i=1 %to &nobs;
    %local zeroI;
    %if &zero %then %let zeroI=%AHGzero(&i,z&zero.);
    %else %let zeroI=&i;
  ,&&cmd&i %if not %AHGblank(&rename) %then as &rename&zeroI;
  %end;
  from &dsn
  ;quit;

%AHGrenamedsn(&varinfo,&out);

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
%macro AHGalltonum(dsn,out=%AHGbasename(&dsn),rename=,zero=0,width=100);
%local i varlist informat nobs varinfo  %AHGwords(cmd,100);
%AHGgettempname(varinfo);
%AHGvarinfo(&dsn,out=&varinfo,info= name  type  length num);
data _null_;
	set &varinfo;
	format cmd $200.;
	if type='C' then cmd='input( '||name||',best. ) as '||name;
	else cmd=name ;
    call symput('cmd'||left(_n_),cmd);
	call symput('nobs',_n_);
run;

%AHGdatadelete(data=&varinfo);

proc sql noprint;
	create table &out(drop= AHGdrop) as
	select ' ' as AHGdrop 
    %do i=1 %to &nobs;
    %local zeroI;
    %if &zero %then %let zeroI=%AHGzero(&i,z&zero.);
    %else %let zeroI=&i;
	,&&cmd&i %if not %AHGblank(&rename) %then as &rename&zeroI;
	%end;
	from &dsn
	;quit;

%mend;




%macro AHGamp(myMAC);
&&&myMac
%mend;
%macro AHGanySlash(dir,toSlash,compress=1);    
%local fromslash;
%if %AHGblank(&toslash) %then 
%do;
%if %index(&dir,/) %then %do;%let fromSlash=%str(/); %let toSlash=\; %end;
%if %index(&dir,\) %then %do;%let fromSlash=\; %let toSlash=%str(/); %end;

%end;


%if &toSlash=\ %then %let fromSlash=%str(/);
%else %let fromSlash=%str(\);
%if not &compress %then %sysfunc(tranwrd(&dir,&fromSlash,&toSlash));
%else %sysfunc(compress(%sysfunc(tranwrd(&dir,&fromSlash,&toSlash))));

%mend;

%macro ahgarr(id,dlm,Arr=ahgarr);
	%if %AHGblank(&dlm) %then %let dlm=@;
	%scan(&&&Arr,&id, &dlm)
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
﻿%macro AHGbody(dsn,html,open=0);
/*%local spl=;*/
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

%macro AHGcatch(dsn,value,out=,strict=1,open=1,justopen=0,compact=0,lazy=1);
%local type;
%if %bquote(%substr(%bquote(&value),1,1))=%str(%')
or %bquote(%substr(%bquote(&value),1,1))=%str(%") %then %let type=char;
%else %let type=num;
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn)ct;
data &out(label="&dsn Temp dataset");
  set &dsn;
  %if &type=char %then
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
﻿%macro AHGcatchprx(dsn,prxptn,out=,open=1,compact=0,lazy=1);
 
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
%macro AHGcharToNum(dsn,vars,out=);
	%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
	%local rdm i;
	%let rdm=_;
	data &out;
		set &dsn;
		%do i=1 %to %AHGcount(&vars);
		%scan(&vars,&i)&rdm=input(%scan(&vars,&i),best.);
		%end;

		drop
		%do i=1 %to %AHGcount(&vars);
		%scan(&vars,&i) 
		%end;
		;
		rename 
		%do i=1 %to %AHGcount(&vars);
		%scan(&vars,&i)&rdm=%scan(&vars,&i) 
		%end;
		;
	run;
	
%mend;
%macro AHGclearglobalmac(begin=);
%local allmac len;
%if %AHGblank(&begin) %then %let len=0;
%else %let len=%length(&begin);
%AHGgettempname(allmac);

  data &allmac;
    set sashelp.vmacro(keep=name scope);
    where scope='GLOBAL' and (substr(upcase(name),1,&len)=upcase("&begin") or %AHGblank(&begin));
  run;  
  
  

    %local drvrmacs;    
    proc sql noprint;
    select '/* clear '||name||'*/'||' %symdel '|| name || '/NOWARN ;' into :drvrmacs separated by ' '
    from &allmac
    ;
    quit;
    %PUT %NRBQUOTE(&DRVRMACS);
    &drvrmacs;

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
	%local i ahuige456436;
	%let ahuige456436=sdksf4543534534;
 
	data _null_;
		format  &ahuige456436 $10000.;
		retain &ahuige456436 '';
		set &dsn end=end;
		%do i=1 %to %AHGcount(&vars);
		&ahuige456436=Trim(&ahuige456436)||"&dlm"||&quote(left(%scan(&vars,&i)));
		%end;

		if end then call symput("&mac",compbl(&ahuige456436));
	
	run;
 
%mend;
%macro AHGcopylib(inlib,tolib,exclude=,n=99999999999);
  %if %AHGblank(&tolib) %then %let tolib=work;
  %local i alldsn onedsn;
  %AHGdsnInLib(lib=&inlib,list=alldsn,lv=1);
  %do i=1 %to %AHGcount(&alldsn)
;
  %let onedsn= %scan(&alldsn,&i,%str( ));
  %if not %sysfunc(indexw(%upcase(&exclude),%upcase(&onedsn))  ) %then
  %do;
  data &tolib..&onedsn;
    set  &inlib..&onedsn(obs=&n);
  run;
  %end;
  %end;

%mend;%macro AHGcount(line,dlm=%str( ));
  %local i AHG66TheEnd;
  %let i=1;
  %do %until(&AHG66TheEnd=yes);
      %if  %qscan(%bquote(&line),&i,&dlm) eq %str() %then
      %do;
      %let AHG66TheEnd=yes;
      %eval(&i-1)
      %end;
    %else %let i=%eval(&i+1);
  %end;

%mend;
%macro 	AHGcreateHashex(HashID,Pairs,dlm=%str( ),dlm2=%str( ));
%AHGclearglobalmac(begin=&hashID);
%local i;
%global &hashid.list;
%let &hashid.list=;

%if &dlm ne %str( ) or &dlm2 ne %str( ) %then
	%do i= 1 %to %AHGcount(&pairs,dlm=&dlm);
	%let &hashid.list=&&&hashid.list %AHGscan2(&pairs,&i,1,dlm=&dlm,dlm2=&dlm2);
	%local id;
	%let id=&hashid&i;
	%global  &id;
	%let &id=%AHGscan2(&pairs,&i,2,dlm=&dlm,dlm2=&dlm2);
	%end;
%else
	%do;
		%local localpairs;
		%let localpairs=&pairs;
		%let i=0;
		%do %while(not %AHGblank(&localpairs));
		%AHGincr(i);
		%local id;
		%let &hashid.list=&&&hashid.list %AHGleft(localpairs);
		%let id= &hashID&i ;
		%global &id;
		%let &id=%AHGleft(localpairs);
		%end;
	%end;

%mend;
%macro ahgD(d=%str(,));
%*<spec> use ',' as dlm for loop i: skip 1, from 2 to n from select ... as , </spec>;
%if &i ne 1 %then &d; 
%MEND;

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
%macro AHGdel(mac,like=0,startwith=1);
%*<spec> del mac var exactly match or  looks like or start with </spec>;

%local i;
%if not &like %then
	%do i=1 %to %AHGcount(&mac);
		%symdel %scan(&mac,&i);
	%end;
%else
	%do ;
	   %local oneStr onetype  j;
	   %let mac=%upcase(&mac);
	   %do j=1 %to %AHGcount(&mac);
	       %let  oneStr=%scan(&mac,&j);  
	       %let oneType=;
	       proc sql noprint;
	        select name into :onetype separated by ' '
	        from sashelp.vmacro
	        where upcase(name) like %if &startwith %then "&oneStr%";%else "%"||"&oneStr%"; 
	        order by name
	        ;quit;

	      %local i onemac;
	      %do i=1 %to %AHGcount(&onetype);
	        %let Onemac=%scan(&onetype,&i);
	        %if not %AHGblank(&onemac) %then  %symdel &onemac;
	      %end;
	  %end;
	%end;
%mend;
%macro AHGdelimit;
%if %AHGpos(&sysscp,win)%then%str(\);
%else%str(/);
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
﻿%macro AHGdistinctvalue(dsn,var,sort=1,into=,dlm=@,quote=0);
%local item varIsNum ;
%let varIsnum=1;

%if   &quote %then %AHGvarisnum(&dsn,&var,into=varIsNum);

%let item=&var;
%if %eval(&quote and not &varIsNum )%then %let item=quote(&var);

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

    &item 
    into :&into separated by "&dlm"
    from &dsn
    ;quit;
    %end;
%let &into=%trim(%bquote(&&&into));

%mend;
%macro AHGdropvar(dsn,IDs,out=);
	%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
	%local i count varlist;
	%AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0);
	%let count=%ahgcount(&ids);
    data &out;
		set &dsn(drop=
		%do i=1 %to &count;
        %scan(&varlist,%scan(&IDs,&i)) 
		%end;
		);
	run;
	
%mend;

%macro AHGdsn(dsn,out=,where=1);
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
    data &out;
        set &dsn;
        if &where;
    run;
%mend;
%macro AHGdsninlib(lib=WORK,list=dsnlist,lv=2,mask=,global=0);
%local DsnInlibtemp;
%if %lowcase(&lib)=work %then %let lv=1;
%AHGgettempname(DsnInlibtemp);
%if &global %then %global &list;
  proc datasets lib=&lib nolist;

    contents data=_all_ memtype=data out=work.&DsnInlibtemp noprint;
  run;


/*  %AHGdatanodupkey(data =&DsnInlibtemp , out = , by =MEMNAME );*/
   %local outvalue;
   %if &lv=1 %then %let outvalue=MEMNAME;
   %else  %let outvalue="&lib.."||MEMNAME;
  proc sql noprint;
    select  &outvalue into :&list  separated by ' '
  from sashelp.vstable
  where upcase("&lib")=libname  and not %AHGeqv(memname, "&DsnInlibtemp")  %if not %AHGblank(&mask) %then %str(and upcase(memname) like %upcase(&mask));
      
  ;
  quit;
  
%AHGdatadelete(data=&DsnInlibtemp);  

%mend;


%macro AHGdsnOfFmt(fmt,lib=work,out=&fmt,var=&fmt);
/* create a dsn with all possible format values
with a variable name =fmt 
*/
proc format library=&lib CNTLOUT=&out(where=(fmtname=upcase("&fmt")) keep=fmtname start label type );
run;
%local type;
%AHGdistinctValue(&out,type,into=type,dlm=@);

data &out;
  set &out;
  %if &type=N %then &var=input(left(start),best.);
  %else &var=start;
  ;
  keep start &var label;
run;
%mend;
%macro AHGdsnwild(lib,dsn,into=_into_,last=0);
%*<spec> put wildcard dsname into :into </spec>;
  %local alldsn;
  %AHGdsninlib(lib=&lib,list=alldsn,lv=2,mask=,global=0);
  %local i;
  %let dsn=%sysfunc(tranwrd(&dsn,-,%str(\w*)));
  %do i=1 %to %AHGcount(%str(&alldsn));
  %if ((%AHGamp(&into)=) or &last) and %sysfunc(prxmatch(/&lib\.&dsn$/i,%scan(&alldsn,&i,%str( )))) %then %let &into=%scan(&alldsn,&i,%str( ));
  %end;
%mend;
/*%let _into_=;*/
/*%AHGdsnwild(rwork,t_uid-);*/
/*%AHGpm(_into_);*/

	
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
  %local date time datetime  thetime;
  %IF not %sysfunc(prxmatch(m/[\%str(,)\&\%str(;)\s]/,%bquote(&file)))%then
    %do;
    %if %sysfunc(fileexist(%bquote(&file))) and (not %AHGispath(%bquote(&file))) %then
      %do;
    	%local rc fid fidc onefile;                                                                                                                   
    	%local  ModifyDT;     
    	%let rc=%sysfunc(filename(onefile,%bquote(&file)));                                                                                       
    	%let fid=%sysfunc(fopen(&onefile));    
      %let thetime=%qsysfunc(finfo(&fid,Last Modified));
      
    /*  Fri Apr 10 14:54:10 2015*/
      %if (not %index(&SYSSCP,WIN)) %then
          %let datetime=%SYSFUNC(inputn(%SYSFUNC(PRXCHANGE(S/(^\w{3})\s*(\w+)\s*(\d+)\s*(\d+:\d+:\d+)\s*(\d+)/\3\2\5 \4/,1,&thetime)),datetime20.));
      %else %let datetime=%sysfunc(inputn(%bquote(&thetime) ,datetime20.));
      %LET DATE=%sysfunc(datepart(&datetime));
      %LET time=%sysfunc(timepart(&datetime));
      
      %let datetime=%sysfunc(putn(&date,yymmdd10.))&T%sysfunc(putn(&time,time10.));
      %let datetime=%sysfunc(prxchange(s/\s(\d:)/ 0\1/,-1,&datetime));
      %let fid=%sysfunc(fclose(&fid)); 
      %sysfunc(compress(&datetime))

      %end; 
    %end;
%mend;
%macro AHGfiledt(file,into=,dtfmt=mmddyy10.,tmfmt=time5.);
  %local date time datetime  thetime;
  %if %sysfunc(fileexist(&file)) %then
  %do;
  %AHGpipe(dir &file /tw,rcmac=thetime,start=6,end=6);
  %let date=%sysfunc(putn(%sysfunc(inputn(%substr(%bquote(&thetime),1,10),&dtfmt)),yymmdd10.));
  %let time=%sysfunc(putn(%sysfunc(inputn(%substr(%bquote(&thetime),11,6),&tmfmt)),time5.));

  %let &into=%sysfunc(translate(&date &time,___,:-%str( )));
  %AHGpm(thetime date time &into);
  %end; 
%mend;

%macro AHGfilelike(path,regex,exclude);
%local rcpipe dir big;
 %if  %AHGblank(&exclude) %then %let exclude='m/```/';
%AHGgettempname(dir);
%if %AHGonwin %then %AHGpipe(%bquote(dir  &path /O:D /b/s),rcmac=rcpipe,start=1,end=999999999,dsn=&dir,global=0);
%else %AHGpipe(%bquote(find  %cpipath(&path,unix) ),rcmac=rcpipe,start=1,end=999999999,dsn=&dir,global=0);

data new&dir;
  format str $500.;
  set &dir(  keep=line) ;
  where prxmatch(&regex,line) and  not prxmatch(&exclude,line) ;
  keep line  ;
  str=catx(' ','%let one= %cpipath(',line,',unix);%put &one;');
  call execute(str);
  str=catx(' ','%let one= %cpipath(',line,',win);%let big=&big@&one;');
  call execute(str);
  line=catx(' ','<a  href="',line,'">',line,'</a>');
  output;
run;

%AHGprt;

data new&dir;
  format line $500.;
  %local i;
  %do i=1 %to %AHGcount(&big,dlm=@);
  line=strip(scan("&big",&i,'@'));
  line=catx(' ','<a  href="',line,'">',line,'</a>');
  output;
  %end;
run;

%AHGprt;
 
%mend;
%macro AHGfilename(file);
  %local filename i;
  %let filename=%scan(&file,%AHGcount(&file,dlm=/),/);
  %let filename=%scan(&filename,%AHGcount(&filename,dlm=\),\);
  &filename
%mend;
%macro AHGfilesindir(dir,dlm=@,fullname=0,extension=,mask=,include=,except=,into=q,case=0,print=0);    
  %local DirID memcnt fileref rc i ahg0 name mydir;
  %let fileref=%substr(X%AHGrandom,1,8);
  %let rc=%sysfunc(filename(fileref,&dir)); 
  %let DirID=%sysfunc(dopen(&fileref));                                                                                                      
                                                                                                                                        
  /* Returns the number of members in the directory */                                                                   
  %let memcnt=%sysfunc(dnum(&DIRid)); 
  %local filedsn;
  %AHGgettempname(filedsn);
  data &filedsn;
  format file $200.;
  %do i = 1 %to &memcnt;                                                                                                                
     file="%qsysfunc(dread(&DIRid,&i))";
     output;
  %end;
  run;
  proc sql noprint;
    select  %if &fullname %then "&dir%AHGdelimit"||;file into :&into   separated by "&dlm"
  from &filedsn
  where 1=1 
  %if %AHGnonblank(&mask) %then 
  %if &case=0 %then %str(and upcase(file) like upcase(&mask));
  %else and file like &mask ;


  %local oneept;
  %if %AHGnonblank(&include) %then 
  %do;
    and ( 1
    %do ahg0=1 %to %AHGcount(&include); 
    %let oneEpt=%scan(&include,&ahg0,%str( ));
    %if &case=0 %then %str(or (index(upcase(file),upcase("&oneEpt")))        );
    %else or (index( file ,"&oneEpt")  );
    %end;
    )
  %end;

  %if %AHGnonblank(&except) %then 
  %do ahg0=1 %to %AHGcount(&except); 
  %let oneEpt=%scan(&except,&ahg0,%str( ));
  %if &case=0 %then %str(and not (index(upcase(file),upcase("&oneEpt")))        );
  %else and not (index( file ,"&oneEpt")  );
  %end;
  order by file
  ;
  quit;
  %let rc=%sysfunc(dclose(&DIRid)); 
  %let rc=%sysfunc(filename(filrf));
  %if &print %then %AHGpm(&into);
%mend;
  




%MACRO AHGfill(line,nums);

%let line=%sysfunc(prxchange(s/(\D*)\d+/$1```/, -1, &line));

%local i count one;
%do i=1 %to %AHGcount(&nums);
  %let one=%scan(&nums,&i,%str( ));
  %let one=s/([^`]*)```/$1 &one/;
  %let line=%sysfunc(prxchange(&one, 1, &line));
%end;
&line

%mend;

%macro AHGfmtmac(dsn,var=,url=);
%local varinfo;
%AHGgettempname(varinfo)
%AHGvarinfo(&dsn,out=&varinfo,info= name  type  length num fmt);

data _null_;
  set &varinfo;
  format command $200.;
  command=' %global &url.type'||name||';'||' %global &url.fmt'||name||';';
  call execute(command);
run;

data _null_;
  set &varinfo;
  call symput("&url.type"||name,type);
  call symput("&url.fmt"||name,fmt);
run;

%mend;

/*%AHGfmtmac(sashelp.class);*/
%macro AHGfmtValueLabel(fmt,ValueMac, LabelMac,dlm=@,out=);
%if %AHGblank(&out) %then %let out=&fmt.fmt;
proc format CNTLOUT=&out(where=(fmtname=upcase("&fmt")) keep=fmtname start label);
run;


proc sql noprint;
  select start,Label into :&valueMac  separated by "&dlm", :&labelMac separated by "&dlm"
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
1 New dsn: &url.by(1)  &url&outone.&i (N*O)
2  New Mac: &url.N

*/
%if %AHGblank(&url) %then %let url=_%substr(%AHGrandom,1,3);
%if %AHGblank(&cmd) %then %let cmd= put abc ;
%let cmd=%nrstr(%%)&cmd;
/*%AHGdatadelete(data=&url:);   */
%global &url.N;    
%let &url.N=0;


proc sql noprint;
  create table &bydsn as
  select distinct %AHGaddcomma(&byvars)
  from &dsn
  order by  %AHGaddcomma(&byvars)
  ;quit;
%local i byn;

%AHGnobs(&bydsn,into=&url.N);

data
%do i=1 %to &&&url.N;
&url&i
%end;
;
  set &bydsn;
  %do i=1 %to &&&url.N;
  if _n_=&i then output &url&i ;
  %end;
run;

%do i=1 %to &&&url.N;
  %local thebyvalue;

  data _null_;
    set &url&i;
    call symput('thebyvalue',compbl(left(%AHGaddcomma(&byvars,comma=%str(||)))));
  run;

%if &del %then
  %do;
  %AHGmergedsn(&url&i,&dsn,&in,by=&byvars,joinstyle=left/*left right full matched*/);
  %end;
%else
  %do;
  /* if not del then the temp &url&i dsn is there */
  %AHGmergedsn(&url&i,&dsn,&url&i,by=&byvars,joinstyle=left/*left right full matched*/);
  data &in ;
    set  &url&i;
  run;
  %end;


%AHGpm(cmd);
%if &execute=1 %then
  %do;
  %put ######################freeloopNo&i;
  %put &printstr;
  %if &title %then title "&thebyvalue";;
  %if %eval(&low<=&i) and %eval(&i<=&up) %then
    %do;
    %unquote(&cmd);
    %local j OneOut;
      %do j=1 %to %AHGcount(&out);
        %let OneOut=%scan(&out,&j);
        data &url&OneOut&i;
          set  &OneOut;
          %if &addLoopVar %then
          %do;
          point=&i;
          set &bydsn point=point;
          %end;
        run;
      %end;
    %end;
  
  
  %end;
  
  
%end;


/*%AHGdatadelete(data=&in &out  %if &del %then %do i=1 %to &&&url.N; &url&i %end;);*/

%mend;

%macro AHGfreesplit(dsn,byvars,outPref=,bydsn=);
%*<spec> split dataset by var like by age to age21 age14.... </spec>;

%AHGdatadelete(data=&outpref:);
%AHGdel(&outpref,like=1);
%global &outPref.N;


proc sql;
	create table &bydsn as
	select distinct %AHGaddcomma(&byvars)
	from &dsn
	group by  %AHGaddcomma(&byvars)
	;quit;
%local i byn;



%AHGnobs(&bydsn,into=&outPref.N);


data
%do i=1 %to &&&outPref.N;
&outpref&i
%end;
;
	set &bydsn;
	%do i=1 %to &&&outPref.N;
	if _n_=&i then output &outpref&i ;
	%end;
	run;

%do i=1 %to &&&outPref.N;
%AHGmergedsn(&outpref&i,&dsn,&outpref&i,by=&byvars,joinstyle=left/*left right full matched*/);
%end;


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
%if not %AHGblank(&TranBy) %then %AHGdatasort(data =&out , out = , by =&TranBy ) ;

proc transpose data=&out out=&out(drop=_name_);
  var 
  %if %AHGpos(&keep,cell)  %then cell;
  %else 
%AHGremoveWords(&keep,value &var,dlm=%str( )) ;
  ;
  id &tran;
  ;
  %if not %AHGblank(&TranBy) %then by &TranBy; ;
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
%macro AHGfreqCoreEX(dsn,var,out=,by=,print=0,rename=1
,keep=value cell frequency percent
);
%if %AHGblank(&out) %then %let out=&sysmacroname;
%local core;
%AHGgettempname(core);
data &core;
  set &dsn;
run;

proc freq data=&core(keep=&var %if %AHGnonblank(&by) %then  &by; rename=(&var=value));
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

%macro AHGfuncloop(func,loopvar=ahuige,loops=,dlm=%str( ),execute=yes,pct=1);
  %local i j cmd perccmd;
  %let j=%AHGcount(&loops,dlm=&dlm);
  %do i=1 %to &j;
  %let cmd=%sysfunc(tranwrd(&func,&loopvar,%scan(&loops,&i,&dlm)));
  %if &pct %then %let perccmd=%nrstr(%%)&cmd;
  %else %let perccmd=&cmd;
  %if &execute=yes or &execute=y %then %unquote(&perccmd);
  %else %put &perccmd;
  %end;
%mend;
%macro AHGgettempname(tempname,start=,useit=0);
  
  %if %AHGblank(&start) %then %let start=T_&tempname;
  %if %length(&start)>10 %then %let start=%substr(&start,1,10);
  %local  ahg9rdn  i;
  %do %until (not %sysfunc(exist(&&&tempName))  );
  %let ahg9rdn=;
  %do i=1 %to 7;
  %let ahg9rdn=&ahg9rdn%sysfunc(byte(%sysevalf(65+%substr(%sysevalf(%sysfunc(ranuni(0))*24),1,2))) ); 
  %end;
  %let &tempname=&start._&ahg9rdn;
  %end;
  %put &tempname=&&&tempname;
  %if &useit %then
  %do;
  data &&&tempname;
  run;
  %end;


%mend;
%macro ahggettempnameNeat(dsn,start=,useit=0);
%AHGincr(global__loop__for_temp_data);
%let &dsn=%sysfunc(translate(&dsn.__&global__loop__for_temp_data,_,.));
%mend;
%macro AHGgetwords(words,from,num,dlm1st=0,dlm=%str( )/*right*/);
%*<spec> like substrbyword(sentence,from, to ) </spec>;
	%local i outstr;
	%let outstr=;
	%do i=0 %to %eval(&num-1);
		%if &i gt &dlm1st %then %let outstr=&outstr&dlm;
		%let outstr=&outstr%scan(&words,%eval(&i+&from),&dlm);
	%end;
	&outstr
%mend;
%macro AHGgrep(all,ptn,r=0,dlm=%str( ));
%*<spec> grep: keep words contain ptn /or not : Mary Tom Mike Joe,M Jo  </spec>;
%local i  one;

%do i=1 %to %AHGcount(%str(&all),dlm=&dlm);
%let one=%scan(&all,&i,&dlm);
%let ptn=%sysfunc(tranwrd(%sysfunc(compbl(&ptn)),%str( ),|));
%if &r ne (%sysfunc(prxmatch(/(&ptn)/i,&one))>0) %then &one;
%end;

%mend;
%macro AHGhashvalue(hashid,handle);
	%local idx out;
	%let indx=%AHGindex(&&&hashid.list,&handle);
	%let  out=&&&hashid&indx;
	&out
%mend;
%macro AHGidx(str,sub,case=0);
  %AHGin(&sub,&str,case=&case)
%mend;
%macro AHGin(sub,str,case=0);
%*<spec> if one value contains another value auto-trim-ed </spec>;
  %if not &case %then index(left(trim(upcase(&str))),left(trim(upcase(&sub))));
  %else index(left(trim(&str)),left(trim(&sub)))
%mend;
%macro AHGincr(mac,by=1);
	%let &mac=%eval(&by+&&&mac);
%mend;
%macro AHGindex(full,sub,dlm=%str( ),case=0,lastone=0);
	%local index i;
	%if not &case %then
		%do;
		%let full=%upcase(&full);
		%let sub=%upcase(&sub);
		%end;
	%let  index=0;
	%do i=1 %to %AHGcount(&full,dlm=&dlm);
	%if %scan(&full,&i,&dlm)=&sub %then 
		%do;
		%let index=&i;
		%if not &lastone %then %goto indexExit;
		%end;
	%end;
	%indexExit:
	&index
%mend;
%macro AHGindex2(str,dlm);
%local i one result;
%let result=0;
%do i=1 %to %length(&str);
%if %qsubstr(&str,&i,1) = &dlm and (&result=0) and (&one=1) %then %let result=&i;
%if %qsubstr(&str,&i,1) eq &dlm %then %let one=1;
%end;
&result
%mend;
%macro AHGinterval(fromID,toid,pre=ahuigetimePoint,url=From &fromID To &toid );
%if %AHGblank(&fromid) %then %let fromID=0;
data _null_;
%IF %AHGblank(&toid) %then  diff=time()-input("&&ahuigetimepoint&fromID",time8.);
%ELSE diff=input("&&ahuigetimepoint&toID",time8.)-input("&&ahuigetimepoint&fromID",time8.);
;
put "######ahuige Interval:&url ########## time used :" diff time8.;
run;


%mend;
%macro AHGisPath(item);
%local pathid rc fname;
%let rc=%sysfunc(filename(fname,&item));
%let pathid = %sysfunc (dopen(&fname));
%if &pathid %then 
  %do;
  %let rc = %sysfunc (DCLOSE(&pathid));
  1
  %end;
%else 0;
%mend;%macro AHGits(str);
%local i n outstr from char;
%let n=%eval(%length(&str)/2);

%do i=1 %to &n;
%let from=%eval(&i*2-1);
%let outstr=&outstr%sysfunc(byte(%substr(&str,&from,2)));
%end;
&outstr
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
	%local i count varlist;
	%AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0);
	%let count=%ahgcount(&ids);
    data &out;
		set &dsn(keep=
		%do i=1 %to &count;
        %scan(&varlist,%scan(&IDs,&i)) 
		%end;
		);
	run;
	
%mend;


%macro AHGkill(dsns,
                dsetlist=_all_ ,   /* list of the SAS datasets to be deleted from the     */
                                   /*   LIBKILL library - if want all datasets deleted    */
                                   /*   from the WORK library then leave the default      */
                                   /*   value for DSETLIST                                */
               libkill= work      /* set the name of the library in which files are to   */
                                   /*   be killed                                         */
               )
               ;

     /* determine if the parameter conditions have been satisfied                         */
     /*   (DSETLIST eq _ALL_ only if LIBKILL eq WORK)                                     */


     %local killerror i;
     %if %AHGblank(&dsns) %then
           %do;

                proc datasets library=work kill memtype=data;

           %end;
     %else 
       %do i=1 %to %AHGcount(&dsns);
       %local dsn lib;
       %let dsn=%scan(&dsns,&i,%str( ));
       %if not %index(&dsn,.) %then %let dsn=work.&dsn;
       %let lib=%scan(&dsn,1);
       %let dsn=%scan(&dsn,2);
       proc datasets library=&lib memtype=data;
                     delete &dsn;
       run;
       quit;
       %end;

%mend  ;

%macro ahgkw(str,key);
%let &key=;
%if %sysfunc(prxmatch(/\b&key:/i,&str)) %then %let &key=%sysfunc(prxchange(s/(.*)(\b&key:)(\S+).*/\3/i,1,&str));
&&&key 
%mend;
﻿%macro AHGlatest(path,latest);
%local rcpipe dir;
%AHGgettempname(dir);
%AHGpipe(%bquote(dir  &path /O:D),rcmac=rcpipe,start=1,end=999999999,dsn=&dir,global=0);
data &dir;
  set &dir(where=(index(line,'.sas7bdat')) keep=line) end=end;
  if end then line=prxchange('s/.*?(\w+)\.sas7bdat.*/\1/',1,line);
  if end then call symput("&latest",trim(line));;
run;
%mend;
%macro AHGleft(arrname,mac,dlm=%str( ),global=0);
  %let &arrname=%sysfunc(left(%str(&&&arrname)));

  %local i count localmac;
  %let count=%AHGcount(&&&arrname,dlm=&dlm);
  %if &count<=1 %then 
    %do;
    %let localmac=&&&arrname;
    %let  &arrname=;
    %end;
  %else
    %do;
    %let localmac=%scan(&&&arrname,1,&dlm);
    %let &arrname=%substr(&&&arrname,%index(&&&arrname,&dlm)+1);
    %end;
  %if &global %then %global &mac;   
  %if %AHGblank(&mac) %then &localmac;
  %else %let &mac=&localmac;
%mend;
%macro AHGlibinfo(lib,out=,info=dsn name  label);
%local dsnlist;
%AHGdsnInLib(lib=&lib,list=dsnlist);
%AHGpm(dsnlist);
%local all;
%macro dosomething(dsn);
%local infodsn;
%AHGgettempname(infodsn);
%let all=&all &infodsn;
%AHGvarinfo(&dsn,out=&infodsn,info=&info  );
%mend;
%AHGfuncloop(%nrbquote( dosomething(ahuige) ) ,loopvar=ahuige,loops=&dsnlist);

data &out;
  set &all;
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
	%local finalstr i;
	%let finalstr=;
	%do i=1 %to %AHGcount(&string);
	%if  %AHGequalmactext(%sysfunc(compress(%scan(&string,&i),0123456789)),&word) %then %let finalstr=&finalstr %scan(&string,&i);
	%end;
	&finalstr
%mend;

%macro AHGlookslike(dsn);
%local lib ds libpath these into;
%let lib=%scan(&dsn,-2,.);
%let ds=%scan(&dsn,-1,.);
%IF &lib EQ %THEN %LET LIB=work;
%let libpath=%sysfunc(pathname(&lib));
%let these=%str( )%AHGls(&libpath,ext=sas7bdat);
 &lib..%sysfunc(prxchange(s/.*?\s(\w*&ds\w*)\.sas7bdat.*/\1/i,1,%bquote(&these))) ;

%mend;
%macro AHGls(dir,ext=%str(),dlm=%str( ),dt=0);
%local into onedir into rc wholepath;
%let rc=%sysfunc(filename(onedir,%bquote(&dir)));
%local ID i count name dummy;
%let id=%sysfunc(dopen(&onedir));
%let count=%sysfunc(dnum(&id));

%do i=1 %to &count;
  %let name=%trim(%sysfunc(dread(&id,&i)));
 
  %IF not %sysfunc(prxmatch(m/[\%str(~)\%str($)\%str(,)\&\%str(;)\s]/,%bquote(&name))) %then
    %do;
    %if %index(%upcase(%superq(name)),%upcase(&ext)) or %AHGblank(%bquote(&ext)) %then 
      %do;
      
      %let wholepath=&dir%AHGdelimit;
      %let wholepath=&wholepath.%superq(name);
      %if not %AHGispath(&wholepath) %then
        %do;
/*        %put ^^^ %superq(name);*/
        %if %AHGblank(&into) %then %let into=%superq(name);
        %else %let into=&into&dlm%superq(name);

        %if &dt and not %AHGispath(&wholepath) %then %let into=&into %AHGfiledatetime(&wholepath);
        %end;
      %end;
    %end;
%end;
%let dummy=%sysfunc(dclose(&id));
&into

%mend;
%macro AHGlsd(dir, dlm=%str( ) );
%local into onedir into rc wholepath;
%let rc=%sysfunc(filename(onedir,%bquote(&dir)));
%local ID i count name dummy;
%let id=%sysfunc(dopen(&onedir));
%let count=%sysfunc(dnum(&id));

%do i=1 %to &count;
  %let name=%trim(%sysfunc(dread(&id,&i)));
/*  %put ### &name;*/
  %IF not %sysfunc(prxmatch(m/[\%str(,)\&\%str(;)\s]/,%bquote(&name))) %then
    %do;
      %let wholepath=&dir%AHGdelimit;
      %let wholepath=&wholepath.%superq(name);
      %if %AHGispath(&wholepath) %then
        %do;
        %if %AHGblank(&into) %then %let into=%superq(name);
        %else %let into=&into&dlm%superq(name);
        %end;
    %end;
%end;
%let dummy=%sysfunc(dclose(&id));
&into

%mend;
%macro AHGmacAndvalue(pairs,global=1,dlm=%str( ),dlm2=|);
%local one i;
%do i=1 %to %AHGcount(&pairs,dlm=&dlm);
  %let one=%scan(&pairs,&i,&dlm);
  %if &global %then %global %scan(&one,1,&dlm2);;
  %let %scan(&one,1,&dlm2)=%scan(&one,2,&dlm2);
%end;
%mend;
﻿%macro ahgmacroot;
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
%local  i j itemN loopN	LastDsn
item1	dsn1	vars1	by1	tempdsn1
item2	dsn2	vars2	by2	tempdsn2
item3	dsn3	vars3	by3	tempdsn3
item4	dsn4	vars4	by4	tempdsn4
item5	dsn5	vars5	by5	tempdsn5
item6	dsn6	vars6	by6	tempdsn6
item7	dsn7	vars7	by7	tempdsn7
item8	dsn8	vars8	by8	tempdsn8
item9	dsn9	vars9	by9	tempdsn9

;
%do j =1  %to 9;
	%let i=%eval(10-&j);
	%if %AHGblank(&&item&i)  %then   %let itemN=%eval(&i-1);
%end;

%let loopN=%eval(&itemN-1);

%do i=1 %to &ItemN;
	%let dsn&i=%scan(&&item&i,1,@);
	%let vars&i=%scan(&&item&i,2,@);
	%if &i <= &loopN %then
	%do;
	%let by&i=%scan(&&item&i,3,@);
	%*pm(dsn&i  vars&i by&i);
	%end;
%end;

%AHGdatanodupkey(data =&dsn1(keep=&vars1) , out =&out , by =&by1 );

%do i=1 %to &loopN;
	%let j=%eval(&i+1);
	%AHGgettempname(tempdsn&j,start=%sysfunc(tranwrd(&&dsn&j,.,_))_);
	%AHGdatanodupkey(data =&&dsn&j(keep=&&by&i /*it is i not j*/ &&vars&j),
		out =&&tempdsn&j , by =&&by&i /*it is i not j*/ );

	%AHGmergedsn(&out,&&tempdsn&j,&out,rename=1,by=&&by&i,joinstyle=full/*left right full matched*/);
%end;
	

%mend;
%macro AHGmergedsn(dsn1,dsn2,outdsn,by=,rename=1,joinstyle=full,check=1/*left right full matched*/);
%if &check %then
%do;
%local varlist1 varlist2 uniqVar sameNonByvar;
%let varlist1=;
%let varlist2=;
%AHGvarlist(&dsn1,Into=varlist1,dlm=%str( ),global=0,print=1);
%AHGvarlist(&dsn2,Into=varlist2,dlm=%str( ),global=0,print=1);
%AHGpm(varlist1 varlist2);
%let UniqVar=%AHGremoveWords(&varlist1,&varlist2 &by );
%AHGpm(uniqvar);
%AHGpm(varlist1 varlist2);
%if not %AHGblank(&uniqvar) %then %let sameNonbyvar=%AHGremoveWords(&varlist1,&UniqVar &BY,dlm=%str( ));
%else %let sameNonbyvar=%AHGremoveWords(&varlist1,&by,dlm=%str( ));
%AHGpm(sameNonbyVar);
%if %AHGnonblank(&sameNonByVar) %then %put WARNING: &dsn1 has same variable names with &dsn2, They are &sameNonByvar;
%end;


%local mergedsn1 mergedsn2;
%if &rename %then
%do;
%AHGGetTempName(mergedsn1,start=%sysfunc(tranwrd(%scan(&dsn1,1,%str(%()),.,_))_);
%AHGGetTempName(mergedsn2,start=%sysfunc(tranwrd(%scan(&dsn2,1,%str(%()),.,_))_);
%end;
%else
%do;
%let mergedsn1=&dsn1;
%let mergedsn2=&dsn2;
%end;
%AHGdatasort(data =&dsn1 , out =&mergedsn1 , by =&by );
%AHGdatasort(data =&dsn2 , out =&mergedsn2 , by =&by );
%local ifstr;
%if %lowcase(&joinstyle)=full %then %let ifstr=%str(ind1 or ind2);
%if %lowcase(&joinstyle)=matched %then %let ifstr=%str(ind1 and ind2);
%if %lowcase(&joinstyle)=left %then %let ifstr=%str(ind1 );
%if %lowcase(&joinstyle)=right %then %let ifstr=%str(ind2 );
data &outdsn;
    merge  &mergedsn1(in=ind1) &mergedsn2(in=ind2) ;
    by &by;
    if &ifstr;
run;
%AHGdatadelete(data=&mergedsn1 &mergedsn2);



/*
%local i;
%if %lowcase(&joinstyle)=matched %then %let joinstyle=;
proc sql noprint;
    create table &outdsn as
    select *
    from &dsn1 as l &joinstyle join &dsn2 as r
    on 1 %do i=1 %to %AHGcount(&by);
       %bquote( and L.%scan(&by,&i)=r.%scan(&by,&i)   )
       %end;
       ;quit;
 */
%mend;


%macro AHGmergelike(dsn1,dsn2,out,by=,by1=,by2=,j=matched);
%if %AHGnonblank(&by) %then %let by1=&by;
%if %AHGnonblank(&by) %then %let by2=&by;
%local one two drop;
%AHGgettempname(one);
%AHGgettempname(two);

%if %AHGeqm(&by1,&by2) %then %let drop=&by1;


data &one(drop=&drop);
  set &dsn1;
  SDFALJPIJDASDFKJDFJKE=compbl(upcase(&by1));
run;

data &two;
  set &dsn2;
  SDFALJPIJDASDFKJDFJKE=compbl(upcase(&by2));
run;

%AHGmergedsn(&one,&two,&out(drop=SDFALJPIJDASDFKJDFJKE),by=SDFALJPIJDASDFKJDFJKE,joinstyle=&j/*left right full matched*/);

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
%local i dsnN   ;
%let dsnN=%AHGcount(&dsns);
%local %AHGwords(Printing,&dsnN);


%do i=1 %to &dsnN;
%let printing&i=;
%AHGgettempName(printing&i);
%end;

%local allvar;
%do i=1 %to &dsnN;
%local varlist charlist;
%let varlist=;
%let charlist=;
%AHGvarlist(%scan(&dsns,&i,%str( )),Into=varlist);
%let allvar=&allvar &varlist;

%AHGallchar(%scan(&dsns,&i,%str( )),into=charlist);
%AHGpm(printing&i);
data &&printing&i;

%do j=1 %to %AHGcount(&varlist);
%if %sysfunc(indexw(&charlist,%scan(&varlist,&j))) %then format &prefix._%sysfunc(putn(&i,z&z..))_%sysfunc(putn(&j,z&z..))   $&length.. ;
%else length &prefix._%sysfunc(putn(&i,z&z..))_%sysfunc(putn(&j,z&z..))   8;
;
%end;    

    set %scan(&dsns,&i,%str( ))
(
%do j=1 %to %AHGcount(&varlist);
%if not %sysfunc(indexw(%upcase(&by),%upcase(%scan(&varlist,&j))  )  )
   and  %lowcase(%scan(&varlist,&j)) ne ahuigebylabel
  %then rename=(%scan(&varlist,&j)=&prefix._%sysfunc(putn(&i,z&z..))_%sysfunc(putn(&j,z&z..))    ) ;
%end;
);


run;

%end;


data &out;
    merge  %do i=1 %to &dsnN; &&printing&i  %end;   ;
run;

%AHGuniq(&allvar,allvar);

%local dropstat;
%if not %AHGblank(&drop) %then %let dropstat=( drop= &drop) ;

%if &keep %then %AHGrenamekeep(&out,out=&out&dropstat ,names=&allvar,keepall=0);

%if &clean %then %AHGdatadelete(data=%do i=1 %to &dsnN; &&printing&i  %end;);

%if &print %then
%do;
proc print &label noobs width=min
;
run;
%end;


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

%local i dsnN  J ;
%let dsnN=%AHGcount(&dsns);
%local %AHGwords(Printing,&dsnN);


%do i=1 %to &dsnN;
%let printing&i=;
%AHGgettempName(printing&i);
%end;
%do i=1 %to &dsnN;
%local varlist;
%let varlist=;
%AHGvarlist(%scan(&dsns,&i,%str( )),Into=varlist);
%AHGpm(printing&i);
data &&printing&i;
    set %scan(&dsns,&i,%str( ))
( 
drop=&drop 
%do j=1 %to %AHGcount(&varlist);
%if not %sysfunc(indexw(%upcase(&by &keep &drop),%upcase(%scan(&varlist,&j))  )  )
	 and  %lowcase(%scan(&varlist,&j)) ne ahuigebylabel
	%then rename=(%scan(&varlist,&j)=&prefix._%sysfunc(putn(&i,z2.))_%sysfunc(putn(&j,z2.))    ) ;
%end;
);
/*%do j=1 %to %AHGcount(&varlist);*/
/*label &prefix._%sysfunc(putn(&i,z2.))_%sysfunc(putn(&j,z2.))="%scan(&varlist,&j)";*/
/*%end;*/

run;

%end;
data &out;
	set &printing1;
run;
 %do i=2 %to &dsnN;
%AHGmergedsn(&out,&&printing&i  ,&out,by=&by,joinstyle=full/*left right full matched*/);
%end;   ;



%if &clean %then 
%do;
%AHGdatadelete(data=
%do i=1 %to &dsnN;
 &&printing&i  
 %end;
 );
%end;

%if &print %then
%do;
proc print &label noobs;
run;
%end;


%mend;

%macro AHGmergevar(dsn,out=,mode=/*3:5@7:9*/,space=" ");
	%if %AHGblank(&out) %then %let out=%AHGbasename(&dsn);
	%local varlist ;
	%AHGalltocharnew(&dsn,out=&out,rename=AHGmv);
	%AHGvarlist(&out,Into=varlist,dlm=%str( ),global=0);
	%AHGpm(varlist);


	%local i j bigvarN dropstr lengthstr;
	%let bigvarN=%AHGcount(&mode,dlm=@); 
	%do  i=1 %to &bigvarN;
	 	%AHGsetvarLen(&out,AHGmv%AHGscan2(&mode,&i,1,dlm=@,dlm2=:),$80);
		%do j=%eval(1+%AHGscan2(&mode,&i,1,dlm=@,dlm2=:)) %to %AHGscan2(&mode,&i,2,dlm=@,dlm2=:);
		%let dropstr=&dropstr AHGmv&j  ;
		%end;
	%end;
	%let dropstr=drop %str( &dropstr;);
	%AHGpm(dropstr);

	data &out;
		set &out;
		&dropstr;
		%do  i=1 %to &bigvarN;
			AHGmv%AHGscan2(&mode,&i,1,dlm=@,dlm2=:) =''
			%do j=%AHGscan2(&mode,&i,1,dlm=@,dlm2=:) %to %AHGscan2(&mode,&i,2,dlm=@,dlm2=:);
			||trim(left(AHGmv&j)) ||&space
			%end;
			;
		%end;
	run;
	%AHGprt;
%mend;
option nomlogic;

%macro AHGname(stats,but=);
%local out final;
%let out=%sysfunc(translate(%bquote(&stats),__,%str(%')%str(%")));
%let out=%sysfunc(compress(&out));
%local i one rank;
%do i=1 %to %length(&out);
%let one=%bquote(%substr(&out,&i,1));
%if %SYSFUNC(NOTALNUM(%bquote(&one))) and not %index(&but,%bquote(&one)) %then %let final=&final._;
%else %let final=&final.%bquote(&one);
%end;
&final
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
%index(&SYSTCPIPHOSTNAME,sddsrv)
%mend;
%macro AHGonWIN;
	%if %UPCASE(%substr(&sysscp,1,3)) =WIN  %then 1;
	%else 0;
%mend;
%macro AHGopendsn(dsn,relabel=1,justopen=0);
%if %AHGblank(&dsn) %then %let dsn=&SYSLAST;

%if &justopen %then dm  "vt &dsn COLHEADING=NAMES " viewtable:&dsn view=form %str(;)  ;
%else 
  %do;
  %local tempdsn99;
  %AHGgettempname(tempdsn99,start=%AHGbarename(&dsn));
  data &tempdsn99 %if &relabel %then (label="&dsn  Temp dataset ");;
    set &dsn;
  run;

  %local mynobs;
  %let mynobs=0;
  %AHGnobs(sashelp.class,into=mynobs);

  %if &mynobs=0 %then 
  %do;
  %AHGdelta(msg=&dsn is  empty!);
  %end;
  %else dm  "vt &tempdsn99 COLHEADING=labels " viewtable:&tempdsn99 view=form   ;;
  %end;



%mend;
%macro AHGordvar(dsn,vars,out=,keepall=0);
%local sql;
%AHGgettempname(sql);
%if &keepall %then
  %do;
  %local restvardsn ;
  %let restvardsn=;
  %AHGgettempname(restvardsn);  
  
  data &restvardsn;
    set &dsn(drop=&vars);
  run;
  %end;
%if %AHGblank(&out) %then %let out=%AHGbasename(&dsn);
proc sql;
  create table &sql as
  select %AHGaddcomma(&vars)
  from  &dsn(keep=&vars)
;quit;



%if &keepall %then
%do;

data &sql ;
  merge &sql &restvardsn;
run;
%end;
%else 
%do;
data &out;
  set &sql;
run;
%end;


%mend;
%macro AHGpm(Ms);
  %local Pmloop2342314314 mac;
  %do Pmloop2342314314=1 %to %AHGcount(&Ms);
    %let mac=%scan(&Ms,&Pmloop2342314314,%str( ));
    %put &mac=&&&mac;
  %end;
%mend;

﻿%macro AHGpmByline(mac,dlm=`);
%local cnt i;
%let cnt=%AHGcount(%superq(&mac),dlm=&dlm);
%do i=1 %to &cnt;
%put %scan(%superq(&mac),&i,&dlm);
%end;
%mend;
/*Print Macro variables;*/
%macro AHGpmlike(Ms,start=1);
   %local oneStr onetype  j;
   %let ms=%upcase(&ms);
   %do j=1 %to %AHGcount(&ms);
       %let  oneStr=%scan(&ms,&j,%str( ));  
       %let oneType=;
       proc sql noprint;
        select name into :onetype separated by ' '
        from sashelp.vmacro
        %if &start eq 1 %then where upcase(name) like  "&oneStr%";
        %else where upcase(name) like "%"||"&oneStr%";

        order by name
        ;quit;
      %if &start=1 %then %AHGsortnum(&onetype,into=onetype);
      %local i mac;
      %do i=1 %to %AHGcount(&onetype);
        %let mac=%scan(&onetype,&i,%str( ));
        %put &mac=&&&mac;
      %end;
  %end;
%mend;

%macro AHGpop(arrname,mac,dlm=%str( ),global=0);
  %if &global %then %global &mac;
  %local stack;
  %let stack=%sysfunc(reverse(%str(&&&arrname)));
  %AHGleft(stack,&mac,dlm=&dlm);
  %let  &arrname=%sysfunc(reverse(%str(&stack)));
  %let  &Mac=%sysfunc(reverse(%str(&&&Mac)));


%mend;

%macro AHGpos(string,word);
	%let string=%upcase(&string);
	%let word=%upcase(&word);
	%index(&string,&word)
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

/*if no explicit definition of orientation then use @ as criteria*/
%if   %AHGblank(&orie)  %then 	%if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local localstats;
%let localstats=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local statN single %AHGwords(mystat,20)
	%AHGwords(myformat,20) %AHGwords(IsStat,20);
%local i sortdsn mystats;

%let sortdsn=&dsn;


%do i =1 %to %AHGcount(&stats);
	%let single=%scan(&stats,&i,%str( ));
	%let isStat&i=0;
	%if not  (%index(&single,%str(%")) or %index(&single,%str(%'))) %then
	%do;
	%let isStat&i=1;
	%let mystats=&mystats &single ;	/*mystats are real stats*/
	%end;
%end;


%AHGsetstatfmt(statfmt=&mystats);
%let statN=%AHGcount(&stats);

%do i=1 %to &statN;
	%let single=%scan(&stats,&i,%str( ));
	%let mystat&i=%scan(&single,1,&split);
	%let myformat&i=%scan(&single,2,&split);
	%if %AHGblank(&&myformat&i) and %str(&&isStat&i) %then %let myformat&i=&&&&formatof&&mystat&i;
	%if &&isStat&i %then %AHGpm(mystat&i myformat&i);
%end;


	proc means data=&sortdsn noprint %if not %AHGblank(&alpha) %then alpha=&alpha;;
		var	&var;
		output out=&out.s	
		%do i=1 %to  &statN;
		%if &&isStat&i %then &&mystat&i%str(=)&&mystat&i;
		%end;
		;
	run;


	


	proc sql;
		create table &out as
		select
        ' '
		%do i=1 %to  %AHGcount(&stats);
			%if &&isStat&i %then ,&left(put(&&mystat&i, &&myformat&i)) as  &&mystat&i ;
			%else  ,&&mystat&i;
		%end;
		from &out.s
		;quit;


%AHGrelabel(&out,out=&out,pos=,labels=Label@&labels);

%if &orie=vert %then	
%do;

%local varlist varN bigsingle statement;
%AHGvarlist(&out,Into=varlist,dlm=%str( ),global=0);
%local  num indx	;
%let indx=1;
%let varN=%AHGcount(&localstats,dlm=@);
%AHGpm(varN);
%do i=1 %to &varN;
	%let bigsingle=%scan(&localstats,&i,@);
	%do num=1 %to %AHGcount(&bigsingle);
	%let indx=%eval(&indx+1);
	%if &num=1 %then %let statement= &statement 	%str(theVerticalvar&i=) %scan(&varlist,&indx);
	%else  %let statement= &statement ||'  '|| %scan(&varlist,&indx);
	%if &num=%AHGcount(&bigsingle) %then  %let  statement= &statement %str(;);
	%end;
%end;

%local vertdsn;
%AHGgettempname(vertdsn);

data &vertdsn;
	set &out;
	keep  %do i=1 %to  &varN; theVerticalvar&i  %end;  ;
    %unquote(&statement);
run;

data hori&out;
	set &out;
run;

data &out;
	set &vertdsn;
	keep 
	%if not %AHGblank(&labels) %then label; 
	%if not %AHGblank(&statord) %then &statord;
	stat;
	array allvar(1:&varN) theVerticalvar1-theVerticalvar&varN;
	do i=1 to dim(allvar);
	%if not %AHGblank(&labels) %then label=scan("&labels",i,'@');;
	%if not %AHGblank(&statord) %then &statord=i; ;
	stat=input(allvar(i),$50.);
	output;
	end;

	
run;

%AHGdatadelete(data=&vertdsn);




%end;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;

%macro AHGprocMeansby(dsn,var,by,out=stats,print=0,alpha=0.05
,stats=n mean median  min max
 /* min\4. median\5.1 max\4. */
 /*n @ min '-' max*/
,split=\
,orie=
,labels=
,left=left
,statord=
);


%if &orie=vert and %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);
%if %AHGblank(&labels) %then %let labels=%sysfunc(translate(&stats,__,\.));
%if not %AHGblank(&labels) and %index(&labels,@)=0 %then %let labels=%AHGaddcomma(&labels,comma=@);

/*if no explicit definition of orientation then use @ as criteria*/
%if   %AHGblank(&orie)  %then 	%if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local localstats;
%let localstats=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local statN single %AHGwords(mystat,20)
	%AHGwords(myformat,20) %AHGwords(IsStat,20);
%local i sortdsn mystats;

%let sortdsn=&dsn;


%do i =1 %to %AHGcount(&stats);
	%let single=%scan(&stats,&i,%str( ));
	%let isStat&i=0;
	%if not  (%index(&single,%str(%")) or %index(&single,%str(%'))) %then
	%do;
	%let isStat&i=1;
	%let mystats=&mystats &single ;	/*mystats are real stats*/
	%end;
%end;


%AHGsetstatfmt(statfmt=&mystats);
%let statN=%AHGcount(&stats);

%do i=1 %to &statN;
	%let single=%scan(&stats,&i,%str( ));
	%let mystat&i=%scan(&single,1,&split);
	%let myformat&i=%scan(&single,2,&split);
	%if %AHGblank(&&myformat&i) and %str(&&isStat&i) %then %let myformat&i=&&&&formatof&&mystat&i;
	%if &&isStat&i %then %AHGpm(mystat&i myformat&i);
%end;


	proc means data=&sortdsn noprint alpha=&alpha;;
		var	&var;
    by &by;
		output out=&out	
		%do i=1 %to  &statN;
		%if &&isStat&i %then &&mystat&i%str(=)&&mystat&i;
		%end;
		;
	run;


	


	proc sql noprint;
		create table old&out as
		select
       ' '
		%do i=1 %to  %AHGcount(&stats);
			%if &&isStat&i %then ,&left(put(&&mystat&i, &&myformat&i)) as  &&mystat&i ;
			%else  ,&&mystat&i;
		%end;
    ,%AHGaddcomma(&by)
		from &out
		;quit;


%AHGrelabel(old&out,out=&out,pos=,labels=blank@&labels@%AHGaddcomma(&by,comma=@));

%if &orie=vert %then	
%do;

%local varlist varN bigsingle statement;
%AHGvarlist(&out,Into=varlist,dlm=%str( ),global=0);
%local  num indx	;
%let indx=1;
%let varN=%AHGcount(&localstats,dlm=@);
%AHGpm(varN);
%do i=1 %to &varN;
	%let bigsingle=%scan(&localstats,&i,@);
	%do num=1 %to %AHGcount(&bigsingle);
	%let indx=%eval(&indx+1);
	%if &num=1 %then %let statement= &statement 	%str(theVerticalvar&i=) %scan(&varlist,&indx);
	%else  %let statement= &statement ||'  '|| %scan(&varlist,&indx);
	%if &num=%AHGcount(&bigsingle) %then  %let  statement= &statement %str(;);
	%end;
%end;

%local vertdsn;
%AHGgettempname(vertdsn);

data &vertdsn;
	set &out;
	keep &BY %do i=1 %to  &varN; theVerticalvar&i  %end;  ;
    %unquote(&statement);
run;

data hori&out;
	set &out;
run;

data &out;
	set &vertdsn;
	keep &BY  
	%if not %AHGblank(&labels) %then label; 
	%if not %AHGblank(&statord) %then &statord;
	stat;
	array allvar(1:&varN) theVerticalvar1-theVerticalvar&varN;
	do i=1 to dim(allvar);
	%if not %AHGblank(&labels) %then label=scan("&labels",i,'@');;
	%if not %AHGblank(&statord) %then &statord=i; ;
	stat=input(allvar(i),$50.);

	output;
	end;

	
run;

%AHGdatadelete(data=&vertdsn);




%end;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;
%macro AHGprop(dsn,var,prop);
%local i tableid propvalue rc;

%if %sysfunc(exist(&dsn)) %then
%do;


%let tableid=%sysfunc(open(&dsn,i));

%do i=1 %to  %sysfunc(attrn(&tableid,nvars));
   %if %AHGeqm(&var,%sysfunc(varname(&tableid,&i))) %then 
     %do;
     %if %AHGeqm(&prop,format) %then  %let propvalue=%sysfunc(varfmt(&tableid,&i));
     %else %if %AHGeqm(&prop,label) %then  %let propvalue=%sysfunc(varlabel(&tableid,&i));
     %else %if %AHGeqm(&prop,length) %then  
        %do;
        %let propvalue=%sysfunc(varlength(&tableid,&i));
        %if %sysfunc(vartype(&tableid,&i))=C %then %let propvalue=$&propvalue;
        %end;
     %end;
      
%end;
%let rc=%sysfunc(close(&tableid));
%end;
&propvalue
%mend;
%macro AHGprt(dsn=_last_,label=label);
proc print data=&dsn noobs &label;run;
%mend;
%macro AHGpureName(dsn);
	%if %index(&dsn,%str(%()) %then %scan(&dsn,1,%str(%());
	%else &dsn;
%mend;
%macro AHGpush(arrname,value,dlm=%str( ));
	%if %AHGcount(&&&arrname,dlm=&dlm) >0 %then 	%let &arrname=&&&arrname&dlm&value;
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
%local i cnt word start end inQuote;
%let cnt=0;   
%let str=&str%str( );
%do i=1 %to %length(&str);
  %if &start=b and %qsubstr(&str,&i,1) eq %then %let end=1;
  %if &start=q and %qsubstr(&str,&i,1) eq &dlm %then %let end=1;
  %if &end=1 %then 
    %do;
    %let start=;
    %let word=&word%qsubstr(&str,&i,1);
    %AHGincr(cnt)
    %let end=0;
    %end;
  %else 
    %do;
    %if %AHGblank(&start) and %qsubstr(&str,&i,1) eq &dlm %then %let start=q  ;
    %else %if %AHGblank(&start) and %qsubstr(&str,&i,1) gt %then %let start=b  ;
    %let word=&word%qsubstr(&str,&i,1) ;
    %end;
%end;
&cnt
%mend;






%macro AHGqscan(str,n,dlm=%str(%'));
%local i cnt word start end inQuote;
%let cnt=0;   
%let str=&str%str( );
%do i=1 %to %length(&str);
  %if &start=b and %qsubstr(&str,&i,1) eq %then %let end=1;
  %if &start=q and %qsubstr(&str,&i,1) eq &dlm %then %let end=1;
  %if &end=1 %then 
    %do;
    %let start=;
    %let word=&word%qsubstr(&str,&i,1);
    %AHGincr(cnt)
    %if &cnt=&n %then &word;
    %let word=;
    %let end=0;
    %end;
  %else 
    %do;
    %if %AHGblank(&start) and %qsubstr(&str,&i,1) eq &dlm %then %let start=q  ;
    %else %if %AHGblank(&start) and %qsubstr(&str,&i,1) gt %then %let start=b  ;
    %let word=&word%qsubstr(&str,&i,1) ;
    %end;
%end;
%mend;






%macro AHGrandom;
  %local  rdn ;
  %let rdn=%sysfunc(normal(0));
	%let rdn=%sysfunc(translate(&rdn,00,.-));
  &rdn
  %put random=&rdn;
%mend;
﻿%macro ahgrank(dsn,var,_n_=);
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
%local i rdn;
%if %AHGblank(&length) %then %let length=6;
%do i=1 %to &length;
  %let rdn=&rdn%sysfunc(byte(%sysevalf(65+%substr(%sysevalf(%sysfunc(ranuni(&seed))*24),1,2))) ); 
%end;
&rdn
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
	%local varlist;
	%if %AHGblank(&pos) %then %let pos=%AHGwords(,%AHGcount(&labels,dlm=&dlm));
	%AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0);
	%AHGpm(varlist);
	data &out;
		set &dsn;
		%local i;
		label
		%do i=1 %to %AHGcount(&pos);
		%scan(&varlist, %scan(&pos,&i))="%scan( &labels,&i,&dlm)"
		%end;
		;
	run;
%mend;

%macro AHGremovewords(sentence,words,dlm=%str( ));
	%local i j CountS CountW final found itemS ;
	%let sentence=%bquote(&sentence);
	%let words=%bquote(&words);
	%let  CountS=%AHGcount(&sentence,dlm=&dlm);
	%let  CountW=%AHGcount(&words,dlm=&dlm);

	%let final=&dlm;
	%do i=1 %to &Counts;
		%let found=0;
		%let itemS=%scan(&sentence, &i,&dlm);
		%let j=0;
		%do %until (&j=&countW or &found) ;
		    %AHGincr(j)
	
			%if %upcase(&itemS)= %upcase(%scan(&words, &j,&dlm)) %then %let found=1;
		%end;
		%if &found=0 %then %let final=&final&dlm&itemS;
	%end;
	%let final=%sysfunc(tranwrd(&final,&dlm&dlm,));
	 &final
%mend;
%macro AHGrenamedsn(dsn,out);
%if not %sysfunc(exist(&out)) %then
  %do;
  %if not %index(&dsn,.) %then %let dsn=work.&dsn;
  %local lib ds dsout;
  %let lib=%scan(&dsn,1);
  %let ds=%scan(&dsn,2);
  proc datasets library=&lib;
     change &ds=%scan(&out,%AHGcount(&out,dlm=.));
  run;
  %end;
%else 
  %do; data %scan(&out,%AHGcount(&out,dlm=.));set &dsn;run;  %end; 


%mend;
%macro AHGrenamekeep(dsn,out=,pos=,names=,prefix=col,keepall=0);
  %if %AHGblank(&names) %then %let names=%AHGwords(&prefix,400);
  %if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
  %local varlist count;
  %AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0);
  %let count=%sysfunc(min(%AHGcount(&varlist),%AHGcount(&names)));
  
  %if %AHGblank(&pos) %then %let pos=%AHGwords(%str( ),&count);
  %AHGpm(pos);
 
  data &out;
    set &dsn;
    %local i;
    %if not &keepall %then
      %do;
      keep
      %do i=1 %to &count;
        %scan(&varlist, %scan(&pos,&i))
      %end;
      ;
      %end;
    rename
    %do i=1 %to &count;
    %scan(&varlist, %scan(&pos,&i))=%scan( &names,&i)
    %end;
    ;
  run;
%mend;
%macro AHGrenameVar(dsn,out=,names=,prefix=AHG	,zero=0);
	%if %AHGblank(&out) %then %let out=%AHGbasename(&dsn);
	%local i zeroI varlist;
	
	%AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0);
	proc sql noprint;
	create table &out as
	select 
	%do i=1 %to %AHGcount(&varlist); 
		
		%if &zero>0 %then %let zeroI=%AHGzero(&i,z&zero..);
		%else %let zeroI=&i;

		%if &i ne 1 %then ,;

		%if %AHGblank(&names) %then %scan(&varlist,&i) as &prefix._&zeroI ;
		%else %if not %AHGblank(%scan(&names,&i)) %then %scan(&varlist,&i) as  %scan(&names,&i)  ;
		%else   %scan(&varlist,&i) ;
	%end;

	from &dsn
	;
	quit;
%mend;%macro AHGreportby(dsn,by,ls=123,ps=45,flow=flow,widthmax=50,which=,
whichlength=,sort=0,groupby=0,groupto=0,topline=,showby=0,
option=nowd nocenter headline,labelopt=%str(option label;));
  %local rptdsn;
  %if %AHGblank(&by) %then %let by=0; 
  %AHGgettempname(rptdsn);
  data &rptdsn;
  %if &by=0 %then
  %do;
  ahuige34xbege5435='_';
  %let by=ahuige34xbege5435;
  %let showby=0;
  %end;
  set &dsn;
run;


  %local i varlist showlist;
  &labelopt;
/*  %if not &showby %then %let showlist=%AHGremoveWords(&varlist,&by,dlm=%str( ));*/
/*  %else %let showlist=&varlist;*/
  %if &sort %then
  %do;
  proc sort data=&rptdsn ; by &by;run;
  %end;
  %AHGvarlist(&rptdsn,Into=varlist,dlm=%str( ),global=0);

  %AHGvarinfo(&rptdsn,out=varinfo34589,info= name  length);


  %local infostr;
  %AHGcolumn2mac(varinfo34589,infostr,name length);
  %local rdm;
  %let rdm=%AHGrandom;
  %AHGcreatehashex(my&rdm.hash,&infostr);
  %put #####################;
  %let showlist=%AHGremoveWords(&varlist,&by,dlm=%str( ));
  &labelopt;
  
  proc report data=&rptdsn &option ;
    column
    %if %AHGblank(&topline) %then  &by &showlist;
    %else %if %index( %bquote(&topline) , %str( %( )    ) %then &topline;
    %else ( &topline &by &showlist );
    ;
    %do i=1 %to  %AHGcount(&by);
    %if &showby %then
    %do;
    define %scan(&by,&i)/order
    %if not &groupby %then display &flow;
    %else group;
    %end;
    %else  define %scan(&by,&i)/order noprint;

    
    ;
    %end;
    %local loop;
    %let loop=0;
    %do i=1 %to %AHGcount(&showlist);
    %local mylength;
    %local handle thePos;
    %let handle=%scan(&showlist,&i);
    %let mylength=%AHGhashvalue(my&rdm.hash,&handle);
/*    %if &mylength>&widthmax %then %let  mylength=*/
    %let mylength=%sysfunc(min(&widthmax,%sysfunc(max(&mylength,%length(&handle)))));
    define  %scan(&showlist,&i)  /
        %if %sysfunc(indexw(&which,&i))  %then %do;%let loop=%eval(&loop+1);width=%scan(&whichlength,%AHGindex(&which,&i))   %end;
    %else %str(width=)&mylength;
        %if &i<=&groupto %then group;
        %else display &flow;
          ;
    %end;
    by &by;


/*  compute before _page_ ;*/
/*        line @1 &ls.*"_";*/
/*    line @1 " ";*/
/*    endcomp;*/
/**/
/*  compute after _page_;*/
/*        line @1 &ls.*"_";*/
/*    endcomp;    */
  run;
  
%mend;
%macro AHGreportwithformat(dsn,fmts=,groupvar=,split=#,order=data);
	%local varlist info nobs i;
	%local %AHGwords(defstr,50);
	%AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0);
	%AHGgettempname(info);
	%AHGvarinfo(&dsn,out=&info,info=name type length num);
	data _null_;
		set &info;
		format defstr $500.;
		group='       ';
		%do i=1 %to %AHGcount(&fmts);
		if _n_=scan(scan("&fmts",&i,' '),1,'\') then length=scan(scan("&fmts",&i,' '),2,'\') ;
		%end;
		%do i=1 %to %AHGcount(&groupvar);
		if %AHGequaltext("%scan(&groupvar,&i)",name) then group=' group ';
		%end;
		defstr='define '||name||' /display width='||left(length)||group||" flow order=&order;";
		put defstr;
		call symput('defstr'||left(_n_),defstr);
		call symput('nobs',_n_);
	run;

	proc report data=&dsn nowindows headline split="&split" ;
		column &varlist;
		%do i=1 %to &nobs;
		&&defstr&i
		%end;
	run;
%mend;
%macro AHGRTFtotxt(rtf,out,txt,ptn=\b\f2\fs16,tailor=1);
  %if %AHGblank(&txt) %then %let txt=%tempdir\____SDD_OUTPUT.txt;
  %local theout ___  leadlen;
  %let ___='_____________________________________________';
  %if not %AHGblank(&out) %then %let theout=&out;
  %else %AHGgettempname(theout);
  %AHGreadline(file=&rtf,out=&theout);
data &theout;
    set &theout;
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
    set &theout;
    if index(line,'\page') then newpage=1;
    retain newpage 0;
    if index(line,&___) then linecount+1;
    if linecount<3 then output tailored;
    else if mod(linecount,3)=2 and not missing(line) and not index(line,&___) then output tailored;
    keep line;
    if linecount=3 and not newpage then output foot;
  run;

  data &theout;
    set tailored foot;
    file "&txt";
    put line;
  run;
  %end;



%mend;



%macro AHGscan2(mac,i,j,dlm=\,dlm2=#);
	%scan(%scan(&mac,&i,&dlm),&j,&dlm2)
%mend;
%macro AHGscanDim(str,dimNum,by=2,dlm=%str( ));
	%scan(&str,%eval(( &dimNum-1)*&by +1)) %scan(&str,%eval(( &dimNum-1)*&by +2)) %scan(&str,%eval(( &dimNum-1)*&by +3))
%mend;
%macro AHGscansub(line,Num,dlm=%str( ));
    %put cnt=%AHGcount(&line,dlm=&dlm);
    %do i=1 %to &num;
        %scan(%bquote(&line),&i,&dlm)&dlm
    %end;
%mend;


%macro AHGscanSubstr(words,from,num,dlm1st=0,dlm=%str( ),compress=0/*right*/);
	%local i outstr;
	%let outstr=;
	%do i=0 %to %eval(&num-1);
		%if &i gt &dlm1st %then %let outstr=&outstr&dlm;
		%let outstr=&outstr%scan(&words,%eval(&i+&from),&dlm);
	%end;
	%if &compress %then %let outstr=%sysfunc(compress(&outstr));
	&outstr
%mend;
%macro AHGsethashvalue(hashid,handle,value);
	%local idx out;
	%let indx=%AHGindex(&&&hashid.list,&handle);
	%let &hashid&indx=&value;
	&out
%mend;
﻿%macro AHGsetprint(dsns,out=setprint,print=0,prefix=,by=,ord=,keep=0,length=500);
option mprint;
%local i dsnN item onedsn alldsn; 
%if %AHGblank(&prefix) %then %let prefix=%AHGrdm(9);
%let dsnN=%AHGcount(&dsns);
%local allvar varlist;
%do i=1 %to  &dsnN;
  %let item=%scan(&dsns,&i,%str( ));
  %AHGgettempname(onedsn);
  %let alldsn=&alldsn &onedsn;
  option mprint;
  data &onedsn;
    %if %AHGnonblank(&ord) %then  &ord=&i;;
    %if %AHGnonblank(&by) %then 
    %do;
    format &by $40.; &by="&item";
    %end;
    set &item;
  run;
  
  %AHGalltocharnew(&onedsn,out=&onedsn);
  option mprint;
  %AHGvarlist(%scan(&dsns,&i,%str( )),Into=varlist);
  %if %AHGcount(&varlist)> %AHGcount(&allvar) %then 
        %let allvar=&allvar %AHGscansubstr( &varlist,%eval(%AHGcount(&allvar)+1),%eval(%AHGcount(&varlist)-%AHGcount(&allvar)));
  option mprint;
  %AHGmergeprint(&onedsn,out=&onedsn,print=0,prefix=&prefix,length=&length);
  option mprint;

%end;

data &out;
  set &alldsn;
run;
option mprint;
%if &keep %then %AHGrenamekeep(&out,names=&allvar,keepall=0);
%else  %AHGrenamekeep(&out,keepall=0);
%AHGtrimdsn(&out);

%if  &print %then %AHGprt;
option mprint;
%mend;
%macro AHGsetstatfmt(statfmt=);
%local i statement allstatfmt;
%let allstatfmt=n\5. std\6.2 mean\6.1 median\6.1 min\6.1 max\6.1 lclm\6.2 uclm\6.2 p25\6.2 p50\6.2 p75\6.2;
%do i=1 %to %AHGcount(&statfmt);
  %if %index(%scan(&statfmt,&i,%str( )),\) %then %let allstatfmt=&allstatfmt %scan(&statfmt,&i,%str( ));
%end;
%do i=1 %to %AHGcount(&allstatfmt);
%let statement=%nrstr(%global) formatof%scan(%scan(&allstatfmt,&i,%str( )),1,\);
%unquote(&statement);
%if %AHGblank(%scan(%scan(&allstatfmt,&i,%str( )),2,\)) %then %let formatof%scan(%scan(&allstatfmt,&i,%str( )),1,\)=6.2;
%else %let formatof%scan(%scan(&allstatfmt,&i,%str( )),1,\)=%scan(%scan(&allstatfmt,&i,%str( )),2,\);

%end;

%mend;
%macro AHGsetvarLen(dsn,var,len,out=);
	%if %AHGblank(&out) %then %let out=%AHGbasename(&dsn);
	%local empty varlist;
	%AHGgettempname(empty);
	%AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0);
	data &empty;
		length &var &len;
	run;
	data &out;
		merge &empty &dsn;
	run;
	%AHGordvar(&out,&varlist,keepall=0);
%mend;
%macro AHGshrink(dsn,vars,lengths,out=,pre=z487);
%if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
%local i;
data &out;
  format
  %do i=1 %to %AHGcount(&vars);
  %scan(&vars,&i) 
  %scan(&lengths,&i,%str( ))  
  %end;
  ;
  set &dsn
  (
  rename=(
  %do i=1 %to %AHGcount(&vars);
  %scan(&vars,&i)=&pre%scan(&vars,&i)
  %end;
  )
  );
  %do i=1 %to %AHGcount(&vars);
  %scan(&vars,&i)=put(&pre%scan(&vars,&i),%scan(&lengths,&i,%str( )));
  %end;

  drop
  %do i=1 %to %AHGcount(&vars);
   &pre%scan(&vars,&i) 
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
    %local onetemp i;
	%if %AHGblank(&start) %then %let start=T_&tempname;
	%local  rdn condition ;


	%do %until ( %unquote(&condition)    );
		%let rdn=%sysfunc(normal(0));
		%let rdn=%sysfunc(translate(&rdn,00,.-));
		%let onetemp=&start._%substr(&rdn,1,5);
		%let &tempname=%AHGwords(&onetemp,&n);
		%let condition=	1 ;
	    %do i=1 %to &n;
	    %let condition=%str(&condition and not %sysfunc(exist(&onetemp.&i)) )  ;
		%AHGpm(condition);
		%end;
	%end;
	%put &tempname=&&&tempname;



%mend;

%macro AHGsort(mac,into=sortoutmac,dlm=%str( ),nodup=0);

  %local i cnt dsn;

  %AHGgettempname(dsn);

  data &dsn;

  format item $200.;

  %do i=1 %to %AHGcount(&mac,dlm=&dlm);

  item=left("%scan(&mac,&i,&dlm)");

  output;

  %end;

  run;

  proc sql noprint;

  select %if &nodup %then distinct; item into :&into separated by "&dlm"

  from &dsn

  order by item

  ;quit;

%mend;
%macro AHGsortnum(mac,into=sortoutmac,dlm=%str( ),nodup=0);

  %local i cnt dsn;

  %AHGgettempname(dsn);

  data &dsn;

  format item $200.;

  %do i=1 %to %AHGcount(&mac,dlm=&dlm);

  item=left("%scan(&mac,&i,&dlm)");
  length=length(compress(item));

  output;

  %end;

  run;

  proc sql noprint;

  select %if &nodup %then distinct; item into :&into separated by "&dlm"

  from &dsn

  order by length,item

  ;quit;

%mend;

%macro AHGsortWords(words,into=,dlm=%str( ),length=100,nodup=1);
  %local i sortdsn;
  %AHGgettempname(sortdsn);
  option nomprint;
  data &sortdsn;
    length word $&length.;
    %do i=1 %to %AHGcount(&words,dlm=&dlm);
    word=scan("&words",&i,"&dlm");
    output;
    %end;
  run;

  proc sql noprint;
  select %if &nodup %then distinct; trim(word) as word into :&into separated by "&dlm"
  from &sortdsn
  order by word
  ;
  quit;

  %AHGdatadelete(data=&sortdsn);
  option mprint;

%mend;


%macro AHGsplitdsn(dsn,by,prefix=,into=,intoby=,nofmt=0,intobyvalues=);
%if %AHGblank(&prefix) %then %let prefix=splitdsn;
%local byN i dsns mydsn;
%AHGgettempname(mydsn);

%if &nofmt %then
%do;
data &mydsn;
	format &by best.;
	set &dsn;
run;	
%end;
%else
%do;
data &mydsn;
	set &dsn;
run;	
%end;


proc sql noprint;
	select count(distinct &by) into :byN
	from &mydsn
	;
	%if not %AHGblank(&intobyvalues) %then %let byN=%AHGcount(&intobyvalues,dlm=@);;
	%if not %AHGblank(&intoby) %then
	%do;
	proc sql noprint;
	select distinct &by into :&intoby separated by '@'
	from &mydsn
	order by &by
	;
	%end;
	%if not %AHGblank(&intobyvalues) %then %let intoby= &intobyvalues ;

	quit;


proc sort data=&mydsn ;
	by &by;
run;

%AHGsomeTempName(dsns,&byN,start=&prefix);
%if not %AHGblank(&into) %then %let &into=&dsns;

data &dsns;
	set &mydsn;
	by &by;
	if first.&by then ahuigebycount4352+1;
	drop ahuigebycount4352;
	%do i=1 %to &byN;
	if 	ahuigebycount4352=&i then output %scan(&dsns,&i);
	%end;
run;

%mend;

%macro AHGsplitVar(dsn,inVar,toVars,out=,dlm=@,drop=1);
  %if %AHGblank(&out) %then %let out=%AHGbarename(&dsn);
  data &out;
    set &dsn;
    %local i;
    %do i=1 %to %AHGcount(&toVars);
    %scan(&ToVars,&i)=scan(&inVar,&i,"&dlm");
    %end;
    %if &drop %then drop &invar;;
  run; 
                    
%mend;

%macro chn_ut_status(showall=0);

%local namelist sdtmall sdtmInDir SDTMstatus rdm;
%let rdm=_%AHGrdm(6);



%if %sysfunc(exist(specs.trackingsheet)) %then
  %do;
  data sdtmAll&rdm;
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
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&sdtm,out=sdtmInDir&rdm);

data sdtmInDir&rdm;
  set sdtmInDir&rdm; drop line;
  WHERE index(line,'.sas7bdat');
  format   name $100. FileDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  FileDate=left(substr(line,index(line,' ')));
  sdtm=1;
run;

%local nobs;

%AHGnobs(SDTMAll&rdm,into=nobs);

%AHGmergedsn(sdtmAll&rdm,sdtmInDir&rdm,sdtmstatus&rdm,by=name,joinstyle=full);

%macro dummy;
%if &nobs>0 %then %AHGmergedsn(sdtmAll&rdm,sdtmInDir&rdm,sdtmstatus&rdm,by=name,joinstyle=left/*left right full matched*/);
%else %AHGmergedsn(sdtmAll&rdm,sdtmInDir&rdm,sdtmstatus&rdm,by=name,joinstyle=right/*left right full matched*/);
%mend;
/* 
#####################################3
*/ 

%if %sysfunc(exist(specs.trackingsheet)) %then
  %do;
data adamAll&rdm;
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
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&adam,out=adamInDir&rdm);

data adamInDir&rdm;
  set adamInDir&rdm; drop line;
  WHERE index(line,'.sas7bdat');
  format name $100. FileDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  FileDate=left(substr(line,index(line,' ')));
  adam=1;
run;


%local nobs;

%AHGnobs(SDTMAll&rdm,into=nobs);

%AHGmergedsn(adamAll&rdm,adamInDir&rdm,adamstatus&rdm,by=name,joinstyle=full);


%macro dummy;
%if &nobs>0 %then %AHGmergedsn(adamAll&rdm,adamInDir&rdm,adamstatus&rdm,by=name,joinstyle=left/*left right full matched*/);
%else %AHGmergedsn(adamAll&rdm,adamInDir&rdm,adamstatus&rdm,by=name,joinstyle=right/*left right full matched*/);
%mend;

/* 
#####################################3
*/ 


%if %sysfunc(exist(specs.trackingsheet)) %then
  %do;
data tflAll&rdm;
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
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&tfl_output,out=TFLInDir&rdm);

data TFLInDir&rdm;
  set TFLInDir&rdm; drop line;
  WHERE index(line,'.rtf');
  format  name $100. FileDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  FileDate=left(substr(line,index(line,' ')));
run;



%local nobs;

%AHGnobs(TFLAll&rdm,into=nobs);

%AHGmergedsn(TFLAll&rdm,TFLInDir&rdm,TFLstatus&rdm,by=name,joinstyle=full);


%macro dummy;
%if &nobs>0 %then %AHGmergedsn(TFLAll&rdm,TFLInDir&rdm,TFLstatus&rdm,by=name,joinstyle=left/*left right full matched*/);
%else  %AHGmergedsn(TFLAll&rdm,TFLInDir&rdm,TFLstatus&rdm,by=name,joinstyle=right/*left right full matched*/);
%mend;


data AllFile&rdm one;
  set adamstatus&rdm sdtmstatus&rdm tflstatus&rdm;;
  if missing(FileDate) then FileDate='-NA-';
  if not missing(program_name) then name=program_name;
run;

%AHGpspipe(%str(ls *.lst |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.programs_stat\sdtm\system_files,out=sdtmlog&rdm);

data sdtmlog&rdm;
  set sdtmlog&rdm; drop line;
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
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.programs_stat\adam\system_files,out=adamlog&rdm);

data adamlog&rdm;
  set adamlog&rdm; drop line;
  format  name $100. LstDate $500.;
  name=scan(scan(line,1,' '),1,'.');
  LstDate=left(substr(line,index(line,' ')));
  if substr(name,1,1)='_' or name='' OR LstDate='LastWriteTime' then delete;
  
run;

%AHGpspipe(%str(ls *.lst |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.programs_stat\tfl\system_files,out=tfllog&rdm);
 
data tfllog&rdm;
  set tfllog&rdm; drop line;
  format   name $100. LstDate $500.;
  if 'lst' ne scan(scan(line,1,' '),2,'.') then delete;
  name=scan(scan(line,1,' '),1,'.');
  LstDate=left(substr(line,index(line,' ')));
  if substr(name,1,1)='_' or name='' OR LstDate='LastWriteTime' then delete;
run;




data allLog&rdm;
  set adamlog&rdm sdtmlog&rdm tfllog&rdm ;;
run;



%AHGmergedsn( AllFile&rdm ,allLog&rdm, AllFile&rdm, by =name,joinstyle=left);


%AHGpspipe(%str(ls |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.replica_programs\sdtm\system_files,out=sdtm2nd&rdm);

data sdtm2nd&rdm;
  set sdtm2nd&rdm; drop line;
  WHERE index(line,'.lst');
  format   name $100. QCdate $500.;
  name=substr(scan(scan(line,1,' '),1,'.'),4);
  QCdate=left(substr(line,index(line,' ')));
  OUTPUT;
  name='supp'||name;
  output;
run;



%AHGpspipe(%str(ls |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.replica_programs\adam\system_files,out=adam2nd&rdm);

data adam2nd&rdm;
  set adam2nd&rdm; drop line;
  WHERE index(line,'.lst');
  format   name $100. QCdate $500.;
  name=substr(scan(scan(line,1,' '),1,'.'),4);
  QCdate=left(substr(line,index(line,' ')));
run;




%AHGpspipe(%str(ls |select  Name, @{Name='LastWriteTime';
Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&__snapshot.replica_programs\tfl\system_files,out=tfl2nd&rdm);

data tfl2nd&rdm;
  set tfl2nd&rdm; drop line;
  WHERE index(line,'.lst');
  format   name $100. QCdate $500.;
  name=substr(scan(scan(line,1,' '),1,'.'),4);
  QCdate=left(substr(line,index(line,' ')));
run;


data allval&rdm;
  set  sdtm2nd&rdm  adam2nd&rdm  tfl2nd&rdm;
run;
  



%AHGmergedsn(AllFile&rdm , allval&rdm,AllFile&rdm ,by=name,joinstyle=left/*left right full matched*/);

%local edslatest sdtmlatest adamlatest ;

/*%AHGpspipe(%str(ls *.sas7bdat|select  Name, @{Name='LastWriteTime';*/
/*Expression={$_.LastWriteTime.ToString('yyyy-MM-dd HH:mm:ss')}}),path=&eds,out=eds&rdm);*/
/**/
/*data eds&rdm;*/
/*  set eds&rdm; drop line;*/
/*  array maxdate $15. _temporary_;*/
/*  WHERE index(line,'.sas7bdat');*/
/*  format Category  name $100. QCdate $100.;*/
/*  name=substr(scan(scan(line,1,' '),1,'.'),4);*/
/*  QCdate=left(substr(line,index(line,' ')));*/
/*run;*/



proc sql noprint;
  select distinct max(filedate) into :sdtmlatest
  from allfile&rdm
  where %AHGeqv(category,'sdtm') and not %AHGeqv(filedate,'-NA-');
  ;

  select distinct max(filedate) into :adamlatest
  from allfile&rdm
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

data AllFile&rdm;
  set AllFile&rdm;
  array alldate filedate lstdate qcdate;

  /*File is created not by batch run*/
  do over alldate;
  if missing(alldate) then alldate='-NA-';
  end;



  do over alldate;
  alldate=trim(ALLDATE)||'``';
  end;


  if LstDate<FileDate then flag=1;

  /*QC is out-of-date*/
  if lstdate>QCdate and not (prxmatch('/\d{5}.*\d{5}/',name))then flag=2;

  /*adam is earlier than sdtm*/
  if (%AHGeqv(category,'adam') and Filedate<"&sdtmlatest")  or 
( folder='tfl' and (Filedate<"&adamlatest") )then flag=3;

  /*    File is not created*/
  if missing(FileDate) or FileDate='-NA-' then flag=9;

  FLAG=max(flag,0);
  format comment $50.;
  comment=put(flag,colorcd.);

  format str $200. folder;

  drop program_name str  INSHEET SDTM ADAM loc ext folder ;
  if category='SDTM' THEN  category='  SDTM' ;
  if category='ADAM' THEN  category=' ADAM' ;
  if prxmatch('/relrec\w+/',name) then delete;
/*  label filedate='Created' lstdate='Log Created by Batch run' qcdate='Validation Date'; */
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


/*  comment=trim(comment)||'`'||%AHGputn(flag)||'`';*/

/*  if flag=2 then QCdate=prxchange('s/([^`]*)`([^`]*)`([^`]*)/\1`2`\3/',1,QCdate);*/
/*  ELSE if flag=3 then filedate=prxchange('s/([^`]*)`([^`]*)`([^`]*)/\1`3`\3/',1,filedate);*/
/*  ELSE if flag=9 then filedate=prxchange('s/([^`]*)`([^`]*)`([^`]*)/\1`9`\3/',1,filedate);*/

  %if not &showall %then if (not insheet)  then delete;;
  if   missing(name) then delete;
  order=category;
  if not ( index(lowcase(category),'sdtm') or index(lowcase(category),'adam')) then order='TFL';
run;

%AHGalltocharNew(AllFile&rdm);

data sdtmlabel&rdm;
  format label $500.;
  set specs.meta_table:;
  keep name label  ;
  name=lowcase(dataset);
  where not missing(dataset);
run;

%AHGmergedsn(AllFile&rdm,sdtmlabel&rdm,AllFile&rdm,by=name,joinstyle=left/*left right full matched*/);


data adamlabel&rdm;
  set specs.meta_adam_table:;
  keep name label  ;
  name=lowcase(dataset);
  where not missing(dataset);
run;

%AHGmergedsn(AllFile&rdm,adamlabel&rdm,AllFile&rdm,by=name,joinstyle=left/*left right full matched*/);

data tfllabel&rdm;
  set specs.meta_tfl;
  keep display_id  DESCRIPTION  ;
  display_id= DISPLAY_IDENTIFIER;
  rename DESCRIPTION=label;
  where not missing(DISPLAY_IDENTIFIER);
run;

%AHGmergedsn(AllFile&rdm,tfllabel&rdm,AllFile&rdm,by=display_id,joinstyle=left/*left right full matched*/);



%AHGdatasort(data = AllFile&rdm, by =order category name);

data AllFile&rdm;
  retain id 0;
  set AllFile&rdm;
  by order category name;
  array cols  LABEL  NAME  CATEGORY  COMMENT  ;
  do over cols;
  cols=trim(cols)||'`'||trim(flag)||'`';
  end;
  
  if first.order then id=1;
  else id+1;
run;

%AHGordvar(AllFile&rdm,id label name category filedate lstdate qcdate comment,out=,keepall=0);



option formdlim=' ' mprint;

/*%ahgcolorex(AllFile&rdm,flag=flag,file=,label=%str(  filedate='File Date' lstdate='Log Date' qcdate='Validation Date'  )) ; */

%ahgcolorex(AllFile&rdm,flag=flag,file=,label=%str(  filedate='File Date' lstdate='Log Date' qcdate='Validation Date'  )) ; 
 
%mend;


%macro AHGsum(dsn,var,by,out=stats,print=0,alpha=
,stats=n mean median  min max
,orie=
,labels=
,left=left
,statord=
);


%if %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);

/*%if &orie=vert and %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);*/

%macro dosomething;
%local i j one;
%do i=1 %to %AHGcount(&stats,dlm=@);
  %let one=%scan(&stats,&i,@);
  %let labels=&labels@;
  %do j=1 %to %AHGcount(&one);
    %let labels=&labels %AHGscan2(&one,&j,1,dlm=%str( ),dlm2=\);
  %end;
%end;
%let labels=%substr(&labels,2);
%mend;
%if %AHGblank(&labels) %then %doSomething ;
%if not %AHGblank(&labels) and not %index(&labels,@) %then %let labels=%AHGaddcomma(&labels,comma=@);

/*if no explicit definition of orientation then use @ as criteria*/
%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local localstats;
%let localstats=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local statN single %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(IsStat,20);
%local i sortdsn mystats;

%let sortdsn=&dsn;


%do i =1 %to %AHGcount(&stats);
  %let single=%scan(&stats,&i,%str( ));
  %let isStat&i=0;
  %if not  (%index(&single,%str(%")) or %index(&single,%str(%'))) %then
  %do;
  %let isStat&i=1;
  %let mystats=&mystats &single ; /*mystats are real stats*/
  %end;
%end;


%AHGsetstatfmt(statfmt=&mystats);
%let statN=%AHGcount(&stats);

%do i=1 %to &statN;
  %let single=%scan(&stats,&i,%str( ));
  %let mystat&i=%scan(&single,1,\);
  %let myformat&i=%scan(&single,2,\);

  %if %AHGblank(&&myformat&i) and %str(&&isStat&i) %then 
  %do;
  %global formatof&&mystat&i;
  %let myformat&i=&&&&formatof&&mystat&i;
  %if %AHGblank(&&myformat&i) %then %let myformat&i=7.2;
  %end;
  %if &&isStat&i %then %AHGpm(mystat&i myformat&i);
%end;


  proc means data=&sortdsn noprint %AHG1(&alpha,%str(alpha=&alpha));  ;
    var &var;
    by &by;
    output out=&out 
    %do i=1 %to  &statN;
    %if &&isStat&i %then &&mystat&i%str(=)&&mystat&i;
    %end;
    ;
  run;




  proc sql noprint;
    create table old&out as
    select
    
    %do i=1 %to  %AHGcount(&stats);
      %if &&isStat&i %then %AHGd &left(put(&&mystat&i, &&myformat&i)) as  &&mystat&i ;
      %else  %AHGd &&mystat&i;
    %end;
    %if not %AHGblank(&by) %then ,%AHGaddcomma(&by);
    
    from &out
    ;quit;

%local labeln labelmodi labelfinal;
%let labelmodi=%sysfunc(tranwrd(&labels,@,%str( )));
%let labeln=%AHGqcount(&labelmodi);
%do i=1 %to &labeln;
%let labelfinal=&labelfinal@%AHGqscan(&labelmodi,&i);
%end;
%AHGrelabel(old&out,out=&out,pos=,labels=&labelfinal %if not %AHGblank(&by) %then @%AHGaddcomma(&by,comma=@););
%if %AHGequalmactext(&orie,hori)=1 and (not %AHGblank(&by)) %then 
%do;
%local varlist;
%AHGvarlist(&out,Into=varlist);
%let varlist=&by %AHGremoveWords(&varlist,&by,dlm=%str( ));
%AHGordvar(&out,&varlist,out=,keepall=0);
%end;

%if %AHGequalmactext(&orie,vert) %then  
%do;

%local varlist varN bigsingle statement;
%AHGvarlist(&out,Into=varlist,dlm=%str( ),global=0);
%local  num indx  ;
%let indx=0;
%let varN=%AHGcount(&localstats,dlm=@);
%AHGpm(varN);
%do i=1 %to &varN;
  %let bigsingle=%scan(&localstats,&i,@);
  %do num=1 %to %AHGcount(&bigsingle);
  %let indx=%eval(&indx+1);
  %if &num=1 %then %let statement= &statement   %str(theVerticalvar&i=) %scan(&varlist,&indx);
  %else  %let statement= &statement ||'  '|| %scan(&varlist,&indx);
  %if &num=%AHGcount(&bigsingle) %then  %let  statement= &statement %str(;);
  %end;
%end;

%local vertdsn;
%AHGgettempname(vertdsn);

data &vertdsn;
  set &out;
  keep &BY %do i=1 %to  &varN; theVerticalvar&i  %end;  ;
    %unquote(&statement);
run;

data hori&out;
  set &out;
run;

data &out;
  set &vertdsn;
  keep &BY  
  %if not %AHGblank(&labels) %then label; 
  %if not %AHGblank(&statord) %then &statord;
  stat;
  array allvar(1:&varN) theVerticalvar1-theVerticalvar&varN;
  do i=1 to dim(allvar);
  %if not %AHGblank(&labels) %then label=left(scan("%sysfunc(compress(&labels,%str(%'%")))",i,'@'));;
  %if not %AHGblank(&statord) %then &statord=i; ;
  stat=input(allvar(i),$50.);

  output;
  end;

  
run;

/*%AHGdatadelete(data=&vertdsn);*/




%end;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;
%macro AHGsumex(dsn,var,by=,out=stats,print=0,alpha=0.05
,stats=n mean median  min max
 /* min\4. median\5.1 max\4. */
 /*n @ min '-' max*/
,orie=
,labels=
,left=left
,statord=
);

%local thedsn byflag;
%if %AHGblank(&statord) %then %let statord=ahgdummy%AHGrdm(10);
%AHGgettempname(thedsn);
%if %AHGblank(&by) %then 
%do;
%let byflag=missing;
%let by=%AHGrdm(10);
%end;
data &thedsn;
  set &dsn;
 %if &byflag=missing %then  &by=1; ;
run;
%local fn;
%let fn=ahgxxxyyyzzz;
%macro ahgxxxyyyzzz(one);
  %IF not  (%index(&one,%str(%")) or %index(&one,%str(%'))) %THEN 1;
  %ELSE 0;
%mend;
%local finallabel;
%let finallabel=%AHGname(&stats,but=@);
%if %AHGblank(&statord) %then %let statord=ahgdummy%AHGrdm(10);
%if %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);
%macro dosomething;
%local i j one;
%do i=1 %to %AHGcount(&stats,dlm=@);
  %let one=%scan(&stats,&i,@);
  %let labels=&labels@;
  %do j=1 %to %AHGcount(&one);
    %let labels=&labels %AHGscan2(&one,&j,1,dlm=%str( ),dlm2=\);
  %end;
%end;
%let labels=%substr(&labels,2);
%mend;
%if %AHGblank(&labels) %then %doSomething ;
%if not %AHGblank(&labels) and not %index(&labels,@) %then %let labels=%AHGaddcomma(&labels,comma=@);
%local finallabel;
%let finallabel=%AHGname(&labels,but=@);

/*if no explicit definition of orientation then use @ as criteria*/
%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local localstats;
%let localstats=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local statN single %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(ISstat,20);
%local i sortdsn mystats;
%AHGgettempname(sortdsn);
%AHGdatasort(data =&thedsn , out =&sortdsn , by = &by  );


%do i =1 %to %AHGcount(&stats);
  %let single=%scan(&stats,&i,%str( ));
  %if %&fn(&single) %then %let mystats=&mystats &single ; /*mystats are real stats*/
%end;

%AHGsetstatfmt(statfmt=&mystats);
%let statN=%AHGcount(&stats);

%do i=1 %to &statN;
  %let single=%scan(&stats,&i,%str( ));
  %let mystat&i=%scan(&single,1,\);
  %let myformat&i=%scan(&single,2,\);
  %if %AHGblank(&&myformat&i) and %&fn(&&mystat&i) %then 
  %do;
  %global formatof&&mystat&i;
  %let myformat&i=&&&&formatof&&mystat&i;
  %if %AHGblank(&&myformat&i) %then %let myformat&i=7.2;
  %end;
  %if %&fn(&&mystat&i)  %then %AHGpm(mystat&i myformat&i);
%end;

  proc means data=&sortdsn noprint alpha=&alpha;;
    var &var;
    by &by;
    output out=&out 
    %do i=1 %to  &statN;
    %if %&fn(&&mystat&i) %then  &&mystat&i%str(=)&&mystat&i;
    %end;
    ;
  run;

%macro ahgD(d=%str(,));
%if &i ne 1 %then &d; 
%MEND;

  proc sql noprint;
    create table &out as
    select
    %do i=1 %to  %AHGcount(&stats);
      %if %&fn(&&mystat&i) %then %AHGd &left(put(&&mystat&i, &&myformat&i)) as  &&mystat&i ;
      %else  %AHGd &&mystat&i;
    %end;
    %if not %AHGblank(&by) %then ,%AHGaddcomma(&by);
    from &out
    ;quit;

%if %substr(&sysmacroname,1,3)=AHG %then  
%do;

%local varlist varN bigsingle statement;
%AHGvarlist(&out,Into=varlist,dlm=%str( ),global=0);
%local  num indx  ;
%let indx=0;
%let varN=%AHGcount(&localstats,dlm=@);
%AHGpm(varN);
%do i=1 %to &varN;
  %let bigsingle=%scan(&localstats,&i,@);
  %do num=1 %to %AHGcount(&bigsingle);
  %let indx=%eval(&indx+1);
  %if &num=1 %then %let statement= &statement   %str(theVerticalvar&i=compbl%() %scan(&varlist,&indx);
  %else  %let statement= &statement ||'  '|| %scan(&varlist,&indx);
  %if &num=%AHGcount(&bigsingle) %then  %let  statement= &statement %str(%););
  %end;
%end;

%local vertdsn;
%AHGgettempname(vertdsn);

data &vertdsn;
  set &out;
  keep &BY %do i=1 %to  &varN; theVerticalvar&i  %end;  ;
    %unquote(&statement);
run;

data hori&out;
  set &out;
run;

data &out;
  set &vertdsn;
  keep &BY  
  %if not %AHGblank(&labels) %then label; 
  %if not %AHGblank(&statord) %then &statord;
  stat;
  array allvar(1:&varN) theVerticalvar1-theVerticalvar&varN;
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
data &out;set &vertdsn;run;

%AHGrelabel(&out,out=&out,labels=&by@&labels);
%end; 
data &out;
  set &out(drop=%if &byflag=missing %then &by;  
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
 /* min\4. median\5.1 max\4. */
 /*n @ min '-' max*/
,orie=vert
,labels=
,left=left
,statord=
);
%local thedsn byflag;
%if %AHGblank(&statord) %then %let statord=ahgdummy%AHGrdm(10);
%AHGgettempname(thedsn);
%if %AHGblank(&by) %then 
%do;
%let byflag=missing;
%let by=%AHGrdm(10);
%end;
data &thedsn;
  set &dsn;
 %if &byflag=missing %then  &by=1; ;
run;



%local fn;
%let fn=ahgxxxyyyzzz;
%macro ahgxxxyyyzzz(one);
  %IF not  (%index(&one,%str(%")) or %index(&one,%str(%'))) %THEN 1;
  %ELSE 0;
%mend;
/*%local finallabel;*/
/*%let finallabel=%AHGname(&stats,but=@);*/

%if %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);
%macro dosomething;
%local i j one;
%do i=1 %to %AHGcount(&stats,dlm=@);
  %let one=%scan(&stats,&i,@);
  %let labels=&labels@;
  %do j=1 %to %AHGcount(&one);
    %let labels=&labels %AHGscan2(&one,&j,1,dlm=%str( ),dlm2=\);
  %end;
%end;
%let labels=%substr(&labels,2);
%mend;
%if %AHGblank(&labels) %then %doSomething ;
%if not %AHGblank(&labels) and not %index(&labels,@) %then %let labels=%AHGaddcomma(&labels,comma=@);
%local finallabel;
%let finallabel=%AHGname(&labels,but=@);
%AHGpm(finallabel);
%let finallabel=%sysfunc(tranwrd(&finallabel,@,%str( )));
%AHGpm(finallabel);

/*if no explicit definition of orientation then use @ as criteria*/
%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%local localstats;
%let localstats=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local statN single %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(ISstat,20);
%local i sortdsn mystats;
%AHGgettempname(sortdsn);


%do i =1 %to %AHGcount(&stats);
  %let single=%scan(&stats,&i,%str( ));
  %if %&fn(&single) %then %let mystats=&mystats &single ; /*mystats are real stats*/
%end;

%AHGsetstatfmt(statfmt=&mystats);
%let statN=%AHGcount(&stats);

%do i=1 %to &statN;
  %let single=%scan(&stats,&i,%str( ));
  %let mystat&i=%scan(&single,1,\);
  %let myformat&i=%scan(&single,2,\);
  %if %AHGblank(&&myformat&i) and %&fn(&&mystat&i) %then 
  %do;
  %global formatof&&mystat&i;
  %let myformat&i=&&&&formatof&&mystat&i;
  %if %AHGblank(&&myformat&i) %then %let myformat&i=7.2;
  %end;
  %if %&fn(&&mystat&i)  %then %AHGpm(mystat&i myformat&i);
%end;

%AHGdatasort(data =&thedsn , out = , by = &by &trt);

  proc means data=&thedsn noprint %if %AHGnonblank(&alpha) %then alpha=&alpha;;
    var &var;
    by &by &trt;
    output 
    /**/
    %local everyone;
    %let everyone= %str(out=&out ) ;
    %do i=1 %to  &statN;
    %if %&fn(&&mystat&i) %then  %let everyone=&everyone &&mystat&i  %bquote(=)  &&mystat&i;
    %end;
    &everyone
    ;
  run;

%macro ahgD(d=%str(,));
%if &i ne 1 %then &d; 
%MEND;

  proc sql noprint;
    create table temp&out as
    select
    %do i=1 %to  %AHGcount(&stats);
      %if %&fn(&&mystat&i) %then %AHGd &left(put(&&mystat&i, &&myformat&i)) as  &&mystat&i ;
      %else  %AHGd &&mystat&i;
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

%local varlist varN bigsingle statement;
%AHGvarlist(&out,Into=varlist,dlm=%str( ),global=0);
%local  num indx  ;
%let indx=0;
%let varN=%AHGcount(&localstats,dlm=@);
%AHGpm(varN);
%do i=1 %to &varN;
  %let bigsingle=%scan(&localstats,&i,@);
  %do num=1 %to %AHGcount(&bigsingle);
  %let indx=%eval(&indx+1);
  %if &num=1 %then %let statement= &statement   %str(theVerticalvar&i=compbl%() %scan(&varlist,&indx);
  %else  %let statement= &statement ||'  '|| %scan(&varlist,&indx);
  %if &num=%AHGcount(&bigsingle) %then  %let  statement= &statement %str(%););
  %end;
%end;

%local vertdsn;
%AHGgettempname(vertdsn);

data &vertdsn;
  set &out;
  keep   %AHG1(&trt,&trt) &BY %do i=1 %to  &varN; theVerticalvar&i  %end;  ;
    %unquote(&statement);
run;

data hori&out;
  set &out;
run;

data &out;
  set &vertdsn;
  keep &BY  
  %AHG1(&labels,label) 
  %AHG1(&statord,&statord) 
  %AHG1(&trt,&trt)
  stat;
  array allvar(1:&varN) theVerticalvar1-theVerticalvar&varN;
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
    by &BY  
    &statord
    %if not %AHGblank(&labels) %then label;   
    ;
    id &trt;
  run;

  %local myvars  entrys IDs;
  %AHGvarlist(&out,Into=myvars );
  %let IDs=%AHGremoveWords(&myvars,&BY &statord label );
  %let entrys=%AHGremoveWords(&myvars,&ids);
  %AHGsortwords(&IDS,into=ids);
  %AHGordvar(&out,&entrys &ids,out=,keepall=0);
  %end;
%end;

%IF %AHGequalmactext(&orie,hori) %then
%do;
data &out;set &vertdsn;run;
%local rdm;
%let rdm=%AHGrdm()_;
%if %AHGblank(&trt) %then %let &rdm.n=1;
%else 
  %do;
  %AHGfreeloop(&out,&trt
  ,cmd=put
  ,in=ahuige
  ,out=ahuige
  ,url=&rdm
  ,addloopvar=0);


  %macro dosomething(dsn);
    data &dsn;
      set &dsn(drop=&trt);
    run;
  %mend;

  %AHGfuncloop(%nrbquote(dosomething(ahuige) ) ,loopvar=ahuige,loops=%AHGwords(&rdm.AHUIGE,&&&rdm.n));


  %AHGmergePrintex(%AHGwords(&rdm.AHUIGE,&&&rdm.n)
  ,by=&by,drop=,out=&out,print=0,prefix=ahuigecol);

  %local varlist;
  %AHGvarlist(&out,Into=varlist);
  %let varlist=&by %AHGremoveWords(&varlist,&by,dlm=%str( ));
  %AHGordvar(&out,&varlist,out=,keepall=0);
  %end;


  %local longlabel;
  %AHGuniq(%do i=1 %to &&&rdm.n;  &finallabel   %end;,longlabel);
    
  %AHGrenamekeep(&out,names=&by &longlabel,out=&out);
%end; 

data &out;
  set &out(drop=%if &byflag=missing %then &by;  
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
%local varinfo varlb trtlb bylb;

%AHGgettempname(varinfo)
%AHGvarinfo(%AHGpurename(&dsn),out=&varinfo,info= name label);
%AHGcolumn2mac(&varinfo(where=(upcase(name)=upcase("&var"))),varlb,label)
%AHGcolumn2mac(&varinfo(where=(upcase(name)=upcase("&trt"))),trtlb,label)
%AHGcolumn2mac(&varinfo(where=(upcase(name)=upcase("&by"))),bylb,label)

%AHGpm(varlb trtlb bylb);

title;
title1 "Dataset:  &dsn   ";
title2 "Variable:  &var %AHG1(&varlb,[&varlb])";
title3 "Treatment: %AHG1(&trt,&trt) %AHG1(&trtlb,[&trtlb]) ";
Title4 "By: %AHG1(&by,&by)  %AHG1(&bylb,[&bylb])";
%if &print %then %AHGreportby(&out,0); 
%local sepdsn;
%AHGgettempname(sepdsn);
data &sepdsn;
  format line $200.;
  line=repeat('#',200);output;
  line="End of  Dataset:%AHGpurename(&dsn)    Variable:&var   Treatment:&trt  By:&by";output;
  line=repeat('#',200);output;
run;
%if &print %then %AHGprt;
%mend;
%macro AHGsumtrt(dsn,var,by,trt,out=stats,print=0,alpha=0.05
,stats=n mean median  min max
 /* min\4. median\5.1 max\4. */
 /*n @ min '-' max*/
,orie=
,labels=
,left=left
,statord=
);
%macro ahgD(d=%str(,));
%if &i ne 1 %then &d; 
%MEND;
%if %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);

/*%if &orie=vert and %index(&stats,@)=0 %then %let stats=%AHGaddcomma(&stats,comma=@);*/

%macro dosomething;
%local i j one;
%do i=1 %to %AHGcount(&stats,dlm=@);
  %let one=%scan(&stats,&i,@);
  %let labels=&labels@;
  %do j=1 %to %AHGcount(&one);
    %let labels=&labels %AHGscan2(&one,&j,1,dlm=%str( ),dlm2=\);
  %end;
%end;
%let labels=%substr(&labels,2);
%mend;
%if %AHGblank(&labels) %then %doSomething ;

/*if no explicit definition of orientation then use @ as criteria*/
%if   %AHGblank(&orie)  %then   %if %index(&stats,@) %then %let orie=vert ;%else  %let orie=hori;
%if %AHGequalmactext(&orie,hori) and  %AHGblank(&statord) %then %let statord=statord34325435;


%local localstats;
%let localstats=&stats;
%let stats=%sysfunc(tranwrd(&stats,@,%str( )));
%local statN single %AHGwords(mystat,20)
  %AHGwords(myformat,20) %AHGwords(IsStat,20);
%local i sortdsn mystats;

%AHGgettempname(sortdsn);

%if not %AHGblank(&by) %then %AHGdatasort(data =&dsn , out =&sortdsn , by = &by &trt );
%else %let sortout=&dsn;


%do i =1 %to %AHGcount(&stats);
  %let single=%scan(&stats,&i,%str( ));
  %let isStat&i=0;
  %if not  (%index(&single,%str(%")) or %index(&single,%str(%'))) %then
  %do;
  %let isStat&i=1;
  %let mystats=&mystats &single ; /*mystats are real stats*/
  %end;
%end;

%AHGsetstatfmt(statfmt=&mystats);
%let statN=%AHGcount(&stats);

%do i=1 %to &statN;
  %let single=%scan(&stats,&i,%str( ));
  %let mystat&i=%scan(&single,1,\);
  %let myformat&i=%scan(&single,2,\);
  %if %AHGblank(&&myformat&i) and %str(&&isStat&i) %then 
  %do;
  %global formatof&&mystat&i;
  %let myformat&i=&&&&formatof&&mystat&i;
  %if %AHGblank(&&myformat&i) %then %let myformat&i=7.2;
  %end;
  %if &&isStat&i %then %AHGpm(mystat&i myformat&i);
%end;

  proc means data=&sortdsn noprint alpha=&alpha;;
    var &var;
    by &by &trt;
    output out=&out 
    %do i=1 %to  &statN;
    %if &&isStat&i %then &&mystat&i%str(=)&&mystat&i;
    %end;
    ;
  run;

  proc sql noprint;
    create table old&out as
    select
    %do i=1 %to  %AHGcount(&stats);
      %if &&isStat&i %then %ahgd &left(put(&&mystat&i, &&myformat&i)) as  &&mystat&i ;
      %else  %ahgd &&mystat&i as mystat&i;
    %end;
    ,%AHGaddcomma(&by &trt)
    from &out
    ;quit;

%local labeln labelmodi labelfinal;
%let labelmodi=%sysfunc(tranwrd(&labels,@,%str( )));
%let labeln=%AHGqcount(&labelmodi);
%do i=1 %to &labeln;
%let labelfinal=&labelfinal%ahgd(d=@)%AHGqscan(&labelmodi,&i);
%end;
%AHGrelabel(old&out,out=&out,pos=,labels=&labelfinal@%AHGaddcomma(&by &trt,comma=@));

%if &orie=hori %then  
%do;
/*%local someVars;*/
/*%do i=1 %to %AHGcount(&stats);*/
/*    %if &&isStat&i %then %let someVars=&somevars  &&mystat&i ;*/
/*    %else %let someVars=&somevars mystat&i;*/
/*%end;*/
/**/
/*data &out test;*/
/*  set &out;*/
/*  format ahgid3457843 $100.;*/
/*  ahgid3457843=catx('_',&trt,&statord);*/
/*run;*/
/**/
/*proc transpose data=&out out=&out;*/
/*  var &someVars;*/
/*  by &by;*/
/*  id ahgid3457843;*/
/*run;*/

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



/*%AHGpm(stat_n);*/
/*%AHGdsnInLib(lib=work,list=dsnlist,mask='stat_%%');*/
/*%put %AHGwords(stat_,&stat_n);*/

%AHGmergePrintex(%AHGwords(stat_AHUIGE,&stat_n)
,by=&by,drop=,out=&out,print=0,prefix=ahuigecol);

%local varlist;
%AHGvarlist(&out,Into=varlist);
%let varlist=&by %AHGremoveWords(&varlist,&by,dlm=%str( ));
%AHGordvar(&out,&varlist,out=,keepall=0);
%end;

%if &orie=vert %then  
%do;

%local varlist varN bigsingle statement;
%AHGvarlist(&out,Into=varlist,dlm=%str( ),global=0);
%local  num indx  ;
%let indx=0;
%let varN=%AHGcount(&localstats,dlm=@);
%AHGpm(varN);
%do i=1 %to &varN;
  %let bigsingle=%scan(&localstats,&i,@);
  %do num=1 %to %AHGcount(&bigsingle);
  %let indx=%eval(&indx+1);
  %if &num=1 %then %let statement= &statement   %str(theVerticalvar&i=) %scan(&varlist,&indx);
  %else  %let statement= &statement ||'  '|| %scan(&varlist,&indx);
  %if &num=%AHGcount(&bigsingle) %then  %let  statement= &statement %str(;);
  %end;
%end;

%local vertdsn;
%AHGgettempname(vertdsn);

data &vertdsn;
  set &out;
  keep &BY &trt %do i=1 %to  &varN; theVerticalvar&i  %end;  ;
    %unquote(&statement);
run;

data hori&out;
  set &out;
run;

data new&out;
  set &vertdsn;
  keep &BY &trt  
  %if not %AHGblank(&labels) %then label; 
  %if not %AHGblank(&statord) %then &statord;
  stat;
  array allvar(1:&varN) theVerticalvar1-theVerticalvar&varN;
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
  by &BY  
  &statord
  %if not %AHGblank(&labels) %then label;   
  ;
  id &trt;
run;

%local myvars  entrys IDs;
%AHGvarlist(&out,Into=myvars );
%let IDs=%AHGremoveWords(&myvars,&BY &statord label );
%let entrys=%AHGremoveWords(&myvars,&ids);
%AHGsortwords(&IDS,into=ids);
%AHGordvar(&out,&entrys &ids,out=,keepall=0);

/*%AHGdatadelete(data=&vertdsn);*/

%end;

%if &print %then
%do;
%AHGprt;
%end;
%theexit:
%mend;
%macro AHGtime(id,pre=ahuigetimePoint);
%if %AHGblank(&id) %then %let ID=0;
%global &pre&id;
data _null_;
  call symput("&pre&id",put(time(),time8.));
run;
%mend;
%macro AHGtitleft(prefix,ls=223);
%if %AHGblank(&prefix) %then %let prefix=AHG;
%local i;
%do i=1 %to 10;
%global &prefix.title&i &prefix.ft&i;
%if not %AHGblank(%bquote(&&&prefix.title&i)) %then title&i justify=left "%AHGapplyls(%bquote(&&&prefix.title&i),&ls)";
%else title&i;
;
%if not %AHGblank(%bquote(&&&prefix.ft&i)) %then footnote&i justify=left "%AHGapplyls(%bquote(&&&prefix.ft&i),&ls)";
%else footnote&i ;
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
    set &out(obs=&N);
  run;
%mend;
%macro ahgtran(dsn,out=);
%local outdsn tran varlist;
%AHGgettempname(outdsn);
%AHGgettempname(tran);
 
%AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0,print=1);

data &outdsn;
  set &dsn;
  theid=_n_;
  dlm='############################';  
;
run;


proc transpose data=&outdsn out=&tran;
  var   &VARLIST dlm;
  by theid;
run;

data &out(where=(not missing(value)));
  set &tran;
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
%local theN;
%AHGnobs(&dsn,into=theN);
%if &theN>0 %then
    %do;
    %local max charlist i count rdn len varlist;

    %AHGvarlist(&dsn,Into=varlist,dlm=%str( ),global=0);
    /*%AHGgettempname(max);*/

    %AHGallchar(&dsn,into=charlist);

    %let count=%AHGcount(&charlist);
    %let rdn=%AHGrdm(20);

    data test _null_;
      retain 
      %do i=1 %to &count;
      &rdn.&i 
      %end;
      &min
      ;
      set &dsn end=end;
      %do i=1 %to &count;
      if length(%scan(&charlist,&i))> &rdn.&i then &rdn.&i=length(%scan(&charlist,&i));
      %end;

      keep &rdn:;
      if end then  call symput('len',compbl(''
      %do i=1 %to &count;
       ||trim(put(&rdn.&i,best.))||' '
      %end;
      ))

      ;
    run;
    %local rdm;
    %let rdm=%AHGrdm(25);
    data &out(rename=(
    %do i=1 %to &count;
      &rdm&i=%scan(&charlist,&i)  
    %end;
    ));
      format
      %do i=1 %to &count;
      &rdm&i $%scan(&len,&i). 
      %end;
      ;
      drop 
      %do i=1 %to &count;
      %scan(&charlist,&i)  
      %end;
      ;
      set &dsn;
      %do i=1 %to &count;
      %if &left %then &rdm&i=left(%scan(&charlist,&i));
      %else &rdm&i=%scan(&charlist,&i);
      ;
      %end;
        
    run;

    %AHGordvar(&out,&varlist,out=&out,keepall=0);
    %end;
%mend;

%macro AHGtype(dsn,var);
%local did;
%let did=  %sysfunc(open(&dsn,in));
%sysfunc(vartype(&did,%sysfunc(varnum(&did,&var))))
%mend;
%MACRO AHGuniq(mac,into);
%local i uniq;
%AHGgettempname(uniq);
data &uniq;
  format word $100.;
  %do i=1 %to %AHGcount(&mac);
  word="%lowcase(%scan(&mac,&i))";
  i=&i;
  output;
  %end;
run;


%AHGdatasort(data = &uniq, out = , by =word );

data &uniq;
  set &uniq;
  format ord $3.;
  retain ord;
  by word;
  if first.word then ord='1';
  else ord=%AHGputn(input(ord,best.)+1);
run;

%AHGdatasort(data = &uniq, out = , by =i);

data &uniq;
  set &uniq;
  if ord ne '1' then word=compress(word||'_'||ord);
run;

proc sql noprint;
  select trim(word) into :&into separated by ' '
  from &uniq
  ;
  quit;

%mend;
%macro AHGup(dir,level );
  %if %AHGblank(&level) %then %let level=1;
  %local slash  i count updir;
  %let dir=%sysfunc(compress(&dir));
  %let slash=%bquote(/);
  %if %index(&dir,\) %then %let slash=\;
  %if %bquote(%substr(&dir,1,1))=&slash %then %let updir=&slash%scan(&dir,1,&slash);
  %else %let updir=%scan(&dir,1,&slash);
  %let count=%AHGcount(&dir,dlm=&slash);
  %do i=2 %to %eval(&count-&level);
  %let updir=&updir&slash%scan(&dir,&i,&slash);
  %end;
  &updir
%mend;
%macro AHGuseLabel(dsn,out=,dlm=%str( ),remove=,to=);
%local rename i;
%if %sysfunc(exist(&dsn)) %then
%do;


data _null_;
length varlist $ 32000;

tableid=open("&dsn",'i');
varlist=' ';
do i=1 to  attrn(tableid,'nvars');
   label=put(varlabel(tableid,i),$100.);
   %do i=1 %to %AHGcount(&remove);
   label=tranwrd(upcase(label),upcase("%scan(&remove,&i)"),"%scan(&to,&i)");
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
call symput("rename", varlist);
rc=close(tableid);
run;
%AHGpm(rename);
%if not %AHGblank(&out) %then
%do;
data &out;
  set &dsn;
  &rename;
run;
%end;


%end;

%mend;


%macro AHGuserDir;
\\gh3nas01\gh3nas_sales.grp\LCDDMAC\STATS\SA\Macro library\users\&sysuserid
%mend;
%macro AHGvarinfo(dsn,out=varinfoout,info= name  type  length num fmt);
%local i ahg3allinfo;
%let ahg3allinfo=name  type   length  format  informat label ;     

data &out(keep=&info);
length dsn $40 name $32  type $4  length 8 format $12  informat $10 label $50  num 8 superfmt fmt $12;
tableid=open("&dsn",'i');
varlist=' ';
dsn="&dsn";
do i=1 to  attrn(tableid,'nvars');
   %do i=1 %to %AHGcount(&ahg3allinfo);
   %if %scan(&ahg3allinfo,&i) ne num 
    and %scan(&ahg3allinfo,&i) ne fmt 
    and %scan(&ahg3allinfo,&i) ne superfmt 
    %then  %scan(&ahg3allinfo,&i)= var%scan(&ahg3allinfo,&i)(tableid,i);;
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
%macro AHGvarisnum(dsn,var,into=varIsNum);
%local varinfo;
%AHGgettempname(varinfo);
%AHGvarinfo(&dsn,out=&varinfo,info= name type);
data _null_;
  set &varinfo(where=(%AHGequaltext(name,"&var")  ) );
  if type='N' then call symput("&into",'1');
  else call symput("&into",'0');
run;
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
%if &print %then %AHGpm(&into);

%mend;
%macro AHGvarlistm(dsn,dlm=%str( ));
%local i tableid varlist rc;

%if %sysfunc(exist(&dsn)) %then
%do;


%let tableid=%sysfunc(open(&dsn,i));

%do i=1 %to  %sysfunc(attrn(&tableid,nvars));
   %let varlist=&varlist&dlm.%sysfunc(varname(&tableid,&i));
%end;

%let rc=%sysfunc(close(&tableid));
%end;
&varlist

%mend;
%macro AHGwhere(subjid);
 1
%mend;
%macro AHGwild(string,word);
  %local i wordN;
  %let wordN=%AHGcount(&word,dlm=@);

  %local finalstr pos item notFound;
    %let finalstr=&string;
    %let i=0;
    %let notfound=0;
    %do %until( (&i=&wordN) or &NotFound) ;
      %AHGincr(i)
      %let item=%scan(&word,&i,@);
      %let pos=%AHGpos(&finalstr,&item);
      %if &pos>0 %then   %let finalstr=%substr(&finalstr,%eval(&pos+%length(&item)));
      %else %let notFound=1;

    %end;
    %if (not &notFound) %then &string;
%mend;  
%macro AHGwildall(string,theword);
%local i item final;
%do i=1 %to %AHGcount(&string);
%let item=%scan(&string,&i,%str( ));
%AHGwild(&item,&theword)
%end;

%mend;

%macro AHGwinorunix;
    %if %UPCASE(%substr(&sysscp,1,3)) =WIN  %then WIN;
    %else UNIX;
%mend;
%macro AHGwintemp;
C:\Users\AHG\AppData\Local\Temp 
%mend;
%macro AHGwords(word,n,base=1);
%local AHG4I;
%if not %index(&word,@) %then %let word=&word@;
%if %AHGcount(&n)=1 %then
  %do AHG4I=%eval(&base) %to %eval(&n+&base-1);
  %sysfunc(tranwrd(&word,@,&AHG4i))
  %end;
%else 
  %do AHG4i=1 %to %AHGcount(&n) ;
  %sysfunc(tranwrd(&word,@,%scan(&n,&AHG4i))) 
  %end;

%mend;





%macro AHGworkout(fromlib,dsns,tolib=work,pre=,where=%str(where 1));
  %if %AHGblank(&dsns) %then %AHGdsnInLib(lib=&fromlib,list=dsns,lv=1);;
/*  data &tolib..*/
  %local i;
  %do i=1 %to %AHGcount(&dsns);
    data &tolib..&pre%scan(&dsns,&i);
      set &fromlib..%scan(&dsns,&i);
      &where ;
    run;
  %end;

%mend;
%macro AHGzero(n,length);
	%sysfunc(putn(&n,&length))
%mend;
