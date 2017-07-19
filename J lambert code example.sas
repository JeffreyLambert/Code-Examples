/***************************************************************************************************************
PROGRAM NAME           :     Macro for creating report for each site.sas
PROGRAMMER             :     Jeff Lambert
DATE OF CREATION :    		 7/20/2014
PROJECT                :     (redacted)
PURPOSE                :     this program is designed to
								1) Extract all necessary information for the reporting process
								2) Create data sets associated with retention, enrollment and event information
							
                                    
 INPUT FILE(S)          :     \output\REDCap_&month_&year
							\output\&month_&year\dates_&month_&year
							\output\&month_&year\enrollment_&month_&year						
							\output\&month_&year\&month_&year
							\output\&month_&year\retention_&month_&year
							\output\&month_&year\temp_&month_&year
							\output\&month_&year\temp
							
 OUTPUT FILE(S)         :    
**************************************************************************;
Date		Author		Notes
03/11/2015	JRL			Added site 35 "Costilla County Public Health"
07/09/2015	JRL			Added a count of the number of people who completed the study



/****************************************************************************************************************************************

									Warning! Warning! Warning! Warning! Warning! Warning! 

********************************************************************************************************************************************/

/* Before proceeding any further:
		1) Create a sub folder under the report folder for the month and date you wish to generate reports for

		2) Make sure the library name has been changed to the appropriate month and year for the report you wish to generate

		3) Double check that the library name is for the appropriate month and  year

*/

*libname out 'C:\Users\lambejef\Dropbox\Baby and Me (1)\Program code for reporting\Output\June_2015'; /* insert month_year for the month and year that the report is
																											being generated for */

libname out 'C:\Users\jNelson\Dropbox\Baby and Me (1)\Program code for reporting\Output\October_2015'; /* insert month_year for the month and year that the report is
																											being generated for */
/***********************
	Global Macros
***********************/
%let input = enrolled; /* specify the data set to be used in macros*/
%let round = 0.01;
%let wgtdata = out.redcap_october_2015; *ADJUST TO APPROPRIATE MONTH AND YEAR;


/*************************
	Fill in Site numbers
*************************/


proc sort data=&wgtdata;;
by mom_id;
run;

data weight_data;
	set &wgtdata;
	by mom_id;
	retain site;
	if first.mom_id then do;
		site = site_crf;
	end;
	else do;
		site = site;
	end;
run;

/*************
	Formatting involved in the reporting process
************/


proc format;
	value unanswered
		. = "Unanswered";
	value white
		1 = "White";
	value black
		1 = "Black, African American";
	value indian	
		1 = "American Indian, Alaska Native or Native Hawaiian";
	value asian 
		1 = "Asian (Indian, Chinese, Japanese, Korean, Filipino, Vietnamese, Guamanian, Samoan, Pacific Islander or Other)"; 
	value other
		1 = "Other";
	value refused
		1 = "Refused"; 
	value unknown
		1 = "Don't Know" ;
	value latino
		. = "Unanswered"
		1 = "No" 
		2 = "Yes" 
		3 = "Refused" 
		4 = "Don't Know";
	value job
		1 = "Health insurance from your job or the job of your husband, parents, partner"; 
	value notjob
		1 = "Health insurance that you or someone else paid for (not from a job)" ;
	value medicaid
		1 = "Medicaid";
	value tricare 
		1 = "TRICARE or military health care";
	value CICP 
		1 = "Colorado Indigent Care Program (CICP)";
	value CHP 
		1 = "Child Health Plan Plus (CHP+)"; 
	value none 
		1 = "I do not have health insurance to pay for my prenatal care";
	value applying 
		1 = "Not currently insured but in the process of applying";
	value income
		. = "Unanswered"
		1 = "Less than $10,000"
		2 = "$10,000 to $14,999"
		3 = "$15,000 to $19,999" 
		4 = "$20,000 to $24,999" 
		5 = "$25,000 to $34,999" 
		6 = "$35,000 to $49,999"
		7 = "$50,000 or more" 
		8 = "Refused" 
		9 = "Don't Know" ;
	value education
		. = "Unanswered"
		1 = "Less than high school"
		2 = "High school graduate (high school diploma or GED)" 
		3 = "Business or technical school"
		4 = "1 or more years of college but no degree" 
		5 = "Associate's degree" 
		6 = "Bachelor's degree" 
		7 = "Master's, Doctorate, or professional degree" 
		8 = "Refused" 
		9 = "Don't Know" ;
	value access_group
		
		1 = "Alamosa County Public Health Department" 
		2 = "Baca County Public Health Agency"
		3 = "Bent County Public Health Agency" 
		4 = "Chaffee County Health and Human Services" 
		5 = "Cheyenne County Public Health" 
		6 = "Clear Creek County Public and Environmental Health" 
		7 = "Colorado Adolescent Maternity Program" 
		8 = "Delta County Health and Human Services Department" 
		9 = "El Paso County Public Health" 
		10 = "Elbert County Health and Environment" 
		11 = "Garfield County Public Health Service" 
		12 = "Gunnison County Health and Human Services" 
		13 = "Hilltop Community Resources - Grand Junction" 
		14 = "Hilltop Community Resources - Montrose" 
		15 = "Jefferson County Public Health" 
		16 = "Kit Carson County Health and Human Services"
		17 = "Las Animas-Huerfano Counties District Health Department - Huerfano" 
		18 = "Las Animas-Huerfano Counties District Health Department - Las Animas" 
		19 = "Lincoln County Public Health" 
		20 = "Montezuma County Public Health Agency" 
		21 = "Montrose County Health & Human Services" 
		22 = "Northeast Colorado Health Department" 
		23 = "Northwest Colorado Visiting Nurse Association, Inc. - Moffat County"
		24 = "Northwest Colorado Visiting Nurse Association, Inc. - Routt County" 
		25 = "Otero County Health Department" 
		26 = "Park County Public Health Agency" 
		27 = "Peak Vista Community Health Center" 
		28 = "Planned Parenthood Prenatal Plus at Rose Medical Center" 
		29 = "Prowers County Public Health and Environment" 
		30 = "Rio Grande County Public Health Agency" 
		31 = "Summit Public Health Department" 
		32 = "Sunrise Community Health" 
		33 = "Teller County Public Health" 
		34 = "Tri-County Public Health"
		35 = "Costilla County Public Health";
	value Age
		1 = "Median"
		2 = "Mean"
		3 = "Standard deviation"
		4 = "Minimum"
		5 = "Maximum";
	value $event
		'client_enrollment_arm_1' = 'Registration'
		'prenatal_class_ses_arm_1' = 'Prenatal session 1'
		'prenatal_class_ses_arm_1b' = 'Prenatal session 2'
		'prenatal_class_ses_arm_1c' = 'Prenatal session 3'
		'prenatal_class_ses_arm_1d' = 'Prenatal session 4'
		'postpartum_visit_m_arm_1' = 'Postpartum session 1'
		'postpartum_visit_m_arm_1b' = 'Postpartum session 2'
		'postpartum_visit_m_arm_1c' = 'Postpartum session 3'
		'postpartum_visit_m_arm_1d' = 'Postpartum session 4'
		'postpartum_visit_m_arm_1e' = 'Postpartum session 5'
		'postpartum_visit_m_arm_1f' = 'Postpartum session 6'
		'postpartum_visit_m_arm_1g' = 'Postpartum session 7'
		'postpartum_visit_m_arm_1h' = 'Postpartum session 8'
		'postpartum_visit_m_arm_1i' = 'Postpartum session 9'
		'postpartum_visit_m_arm_1j' = 'Postpartum session 10'
		'postpartum_visit_m_arm_1k' = 'Postpartum session 11'
		'postpartum_visit_m_arm_1l' = 'Postpartum session 12'
		'postpartum_intervi_arm_1' = 'Postpartum interview'
		'client_exit_drop_arm_1' = 'Drop / Exit';

		value hsmoke
	    . = "Unanswered"
	 	0 = "No"
		1 = "Yes"
		2 = "Refused"
		3 = "Don't know";
run;




/*****************************
	Template for the report
****************************/

options nodate; *remove date from top right corner;
proc template;
	define style styles.journalS;
	parent=styles.journal;
		class body / 
			backgroundcolor = white
			color = black
			fontfamily = "Times Roman, Arial";
			
		class systemtitle / 
			fontfamily = "Helvetica"
			fontsize = 20 pt
			fontweight = bold;

		class table /
			backgroundcolor = grey
			bordercolor = black
			borderstyle = solid
			borderwidth = 1pt
			cellpadding = 5 pt
			cellspacing = 0pt
			frame = void
			rules = group
			fontfamily = "Arial";

		class header, footer /
			backgroundcolor = grey
			fontfamily = "Times Roman"
			fontweight = bold
			fontsize = 12 pt;

		class data /
			bordercolor = black
			borderstyle = solid
			borderwidth = 2pt
			rules = group;
		
	end;
run;


proc template;
	define style styles.journalR; *Define the template style Journal R ;
	parent=styles.journal;
	end;
run;


/***********************************
	Run these macros before the reporting macro
************************************/

%macro tab(tabvar=, name=, fmt=);
	proc sort data=&input;
		by redcap_data_access_group;
	run;
	proc freq data=&input noprint;	
			table &tabvar / missing out=&tabvar.1;
		
	run;
	data &tabvar.2;
		set &tabvar.1 /* (where=(&tabvar ne .)) */;
		drop PERCENT;
		rename COUNT = Enrolled; 
		varname = "&name";
		subcat = put(&tabvar,&fmt);
		drop &tabvar;
		rename sub = subcat; 
	run;
%mend tab;

%macro race(racevar=, num= , fmt=);
	data &racevar.1;
		set &input;
			one =1;
			keep one;
			keep &racevar;
	run;

	ods output summary = &racevar.2;
		proc means data = &racevar.1 n completetypes;
			freq &racevar;
			var one;
		run;
	data &racevar.3;
		set &racevar.2;
			one = 1;
			rename one_N = Enrolled;
			subcat = put(one,&fmt);
			varname = 'Race';
			drop one;
	run;
%mend race;


%macro insurance(var=, num= , fmt=);
	data &var.1;
		set &input;
			one =1;
			keep one;
			keep &var;
	run;

	ods output summary = &var.2;
		proc means data = &var.1 n completetypes;
			freq &var;
			var one;
		run;
	data &var.3;
		set &var.2;
			one = 1;
			rename one_N = Enrolled;
			subcat = put(one,&fmt);
			varname = 'Insurance';
			drop one;
	run;
%mend insurance;


%macro means(meanvar= , fmt=);
	proc means data= &input noprint;
		var &meanvar;
		output out = &meanvar.1 n=N p50=MED mean=MEAN stddev=SD min=MIN max=MAX;
	run;
	data &meanvar.2;
		set &meanvar.1;
		if mean ne . then mean = round(mean, &round);
		if med ne . then med = round(med, &round);
		if sd ne . then sd = round(sd, &round);
		drop _TYPE_;
		drop _FREQ_;
		drop N;
	run;	
	proc transpose data=&meanvar.2 out=&meanvar.3;
		var MED MEAN SD MIN MAX;
	run;
	data &meanvar.4;
		set &meanvar.3;
			varname = "Age";
			if _NAME_ = 'MED' then cat = 1; 
			if _NAME_ = 'MEAN'  then cat = 2; 
			if _NAME_ = 'SD'   then cat = 3; 
			if _NAME_ = 'MIN'  then cat = 4; 
			if _NAME_ = 'MAX'  then cat = 5; 
			rename COL1 = Enrolled;
			drop _NAME_;
			subcat = put(cat, &fmt);
			drop cat;
			
	%mend means;


%macro wts(meanvar= , fmt=);
	

data wgts;
	set weights;
	where redcap_event_name = 'postpartum_visit_m_arm_1';
	keep baby1 baby2;
	array babywt1 {2} bw_wtlbs1 bw_wtoz1; *english system weights baby1;
	array babywt2 {2} bw_wtlbs2 bw_wtoz2; *English system weights baby2;
	array babywtG {2} bw_grams1 bw_grams2; *Metric system weights for baby1 and baby2;
	array weights {2} baby1 baby2;
	
	if babywt1{1} ne . then do;
			weights{1} = babywt1{1} + babywt1{2}*(1/16);
		output;
	end;
	if babywt1{1} =  . then do;
			weights{1} = babywtG{1} * (1/453.592);
		output;
	end;	

	if babywt2{1} ne . then do;
			weights{2} = babywt2{1} + babywt2{2}*(1/16);
		output;
	end;
	if babywt2{1} =  . then do;
			weights{2} = babywtG{2} * (1/453.592);
		output;
	end;	 
run;

data b1;
	set wgts;
	where baby1 ne .;
	keep wt b;
	wt = baby1;
	b = 1;
run;

data b2;
	set wgts;
	where baby2 ne .;
	keep wt b;
	wt = baby2;
	b = 2;
run;

data wgts2;
merge b1 b2;
by b;
run;

proc means data= wgts2 noprint;
		var wt;
		output out = wt1 n=N p50=MED mean=MEAN stddev=SD min=MIN max=MAX;
	run;
		*Output frequency, median, mean, standard deviation, minimum, maximum to temporary data set;

	data wt2;
		set wt1;
		if mean ne . then mean = round(mean, &round); *Round to two decimal places;
		if med ne . then med = round(med, &round); *Round to two decimal places;
		if sd ne . then sd = round(sd, &round); *Round to two decimal places;
		if min ne . then min = round(min, &round);
		if max ne . then max = round(max, &round);
		drop _TYPE_;
		drop _FREQ_;
		drop N;
	run;	
	proc transpose data=wt2 out=wt3;
		var MED MEAN SD MIN MAX;
	run;
		*Transpose the varaibles into a column that can be used with the rest of the output;

	data wt4;
		set wt3;
			varname = "Birth Weight";
			if _NAME_ = 'MED' then cat = 1; * Denotes a category level that will be used in conjuction with format statement
												to create subcategories;
			if _NAME_ = 'MEAN'  then cat = 2; 
			if _NAME_ = 'SD'   then cat = 3; 
			if _NAME_ = 'MIN'  then cat = 4; 
			if _NAME_ = 'MAX'  then cat = 5; 
			rename COL1 = Enrolled;
			drop _NAME_;
			subcat = put(cat, &fmt); * Formats the levels of the input variable and puts them into a variable named subcategory;
			drop cat;
			
	run;

			
	%mend wts;
	*Manipulated and summarize birthweight data;




/******************
	Reporting Macro
************************/
%macro site_summary(month=, year=, site=); *Input the month and year you are reporting for.  Site=, input the number of sites in the study;

%do i=1 %to &site; * Repeat the macro for each of the 34 sites;

data site_specific;
	set out.dates_&month._&year. (where=(site=&i));
run;


data enrolled;
	set out.enrollment_&month._&year. (where=(site_crf=&i));
run;


data retention;
	set out.retention_&month._&year. (where=(site_crf=&i));
run;

data weights;
	set weight_data (where=(site=&i));
run;


proc freq data=site_specific noprint;
	table redcap_event_name / out = total;
run;
data total2;
	set total;
	where redcap_event_name ne 'Postpartum interview';
	drop PERCENT;
	
	subcat = put(redcap_event_name, $event.);
	varname = 'Forms filled out';
	rename COUNT = Enrolled;
run;


proc freq data=enrolled noprint;
	table site_crf / out = per_site;
	
run;
data per_site2;
	length subcat $1000;
	set per_site (where=(site_crf ne .));
		drop PERCENT;
		varname = 'Enrolled per site';
		rename COUNT = Enrolled;
		subcat = put(site_crf, access_group.);
		drop site_crf;
run;

proc freq data=site_specific noprint;
	where (redcap_event_name = 'client_exit_drop_arm_1');
	table redcap_event_name / out=drop;
run;

data drop2;
	set drop;
		drop PERcENT;
		varname = 'Dropped from Study';
		rename COUNT = Enrolled;
		subcat = put(redcap_event_name, $event.);
	drop redcap_event_name;
run;
	

	
data comp;
	set retention;
	where redcap_event_name = 'client_exit_drop_arm_1';
	keep mom_id drop;
	drop = 1;
run;

data comp2;
	merge retention comp;
		by mom_id;
run;

data comp2;
	set comp2;
	if redcap_event_name = 'postpartum_visit_m_arm_1l' and drop ne 1 then do;
		complete = 1;
		output;
	end;
run;

proc freq data=comp2;
	table complete / out=comp3;
run;
 	
data comp3;
	set comp3;
	drop percent complete;
	varname = 'Completed study';
	rename count=Enrolled;
	subcat = "Complete";
run;


*data retention;
*	set out.retention (where=(site_crf=&site));
*run;

proc sql;
	create table vouchers as
	select sum(num_vouchers)
		from retention;
	quit;
	
data vouchers2;
	set vouchers;
		rename _TEMG001 = Enrolled;
		Subcat = 'Total vouchers given out';
		varname = 'Vouchers';
run;



/*****************
Generate frequency counts for the following:
1) are you latino?
2) What is your household income?
3) What is your education level?
*****************/


%tab(tabvar=q7_crf, name=Latino, fmt=latino.); /* latino? */
%tab(tabvar=q9_crf, name=Income, fmt=income.); /* income */
%tab(tabvar=q10_crf, name=Education, fmt=education.); /* mother's education level */

/*******************
Generate frequency counts for race
********************/

%race(racevar=q6_crf___1,  num=1, fmt=white.); /* # of whites */
%race(racevar=q6_crf___2,  num=2, fmt=black.); /* # of blacks */
%race(racevar=q6_crf___3,  num=3, fmt=indian.); /* # of native americans */
%race(racevar=q6_crf___4,  num=4, fmt=asian.); /* # of asians */
%race(racevar=q6_crf___5,  num=5, fmt=other.); /* # of other */
%race(racevar=q6_crf___6,  num=6, fmt=refused.); /* # refusing to answer */
%race(racevar=q6_crf___7,  num=7, fmt=unknown.); /* # unknown */

/***********
Merge the race variables into a single set
************/

data race;
	length subcat varname $1000;
	set q6_crf___13 q6_crf___23 q6_crf___33 q6_crf___43 q6_crf___53 q6_crf___63 q6_crf___73;
	
run; 

proc print data=race;
	run;

/**********************
Generate frequency counts for 
insurance categories
*****************/

%insurance(var=q8_crf___1, num=1, fmt=job.);
%insurance(var=q8_crf___2, num=2, fmt=notjob.);
%insurance(var=q8_crf___3, num=3, fmt=medicaid.);
%insurance(var=q8_crf___4, num=4, fmt=tricare.);
%insurance(var=q8_crf___5, num=5, fmt=CICP.);
%insurance(var=q8_crf___6, num=6, fmt=CHP.);
%insurance(var=q8_crf___7, num=7, fmt=none.);
%insurance(var=q8_crf___8, num=8, fmt = applying.);
%insurance(var=q8_crf___9, num=9, fmt = other.);
%insurance(var=q8_crf___10, num=10, fmt = refused.);
%insurance(var=q8_crf___11, num=11, fmt=unknown.);


/*************
Merge all the insurances into a single
data set for insurance frequencies
*************/

data insurance;
	merge q8_crf___13 q8_crf___23 q8_crf___33 q8_crf___43 q8_crf___53 q8_crf___63 
			q8_crf___73 q8_crf___83 q8_crf___93 q8_crf___103 q8_crf___113;
	by subcat;
	varname = 'Insurance';
run;


/*************************
	Generate means, std deviations, min / max for
	continuous variables age and birthweight
************************/


%means(meanvar=age_crf, fmt=age.);
%wts(meanvar=wt,fmt=age.);



/****************
Put all the data together
*****************/

data Site_&i.;
	length varname $100;
	set 
		per_site2	 (in=inper_site2) /* enrollment by site based on registration forms filled out*/
		
		total2		 (in=intotal2) /* total who've gone through which session based on forms filled out */

		drop2		(in=indrop2)  /* number of drop forms filled out */
		
		comp3		(in=incomp3)	/* number of people who completed study */

		vouchers2 	(in=invouchers2) /* Number of vouchers handed out */

		insurance	 (in=ininsurance) /* Insurance demographics */

		race 		 (in=inrace) /* number of people identifying as this race */

		q7_crf2	     (in=inq7_crf2) /* Latino demographic */

		q9_crf2		 (in=inq9_crf2) /* Income level */

		q10_crf2	 (in=inq10_crf2) /* Education level of mother */

		age_crf4	 (in=inage_crf4) /* Age information of participants */

		wt4			 (in=inwt4); 	/* Birthweight information */


	label subcat = 'Subcategory';
 	format Enrolled comma10.0;

run;


/*******************************
	Create data sets by site, with detailed event data
********************************/

data temp_date_&i.;
	set out.dates_&month._&year. (where=(site=&i));
run;

/**********************
Report generation
**********************/

/******************************************
Report generation all sites with all dates and voucher numbers
**************************************/

/*****************************************
Generate and output data to RTF file
			1) Creates RTF file using the information from the summary data set
			2) Uses the template journalS
			3) Summary statistics by site

*****************************************/

%let s = %sysfunc(putn(&i, access_group.));
*%let outpath = C:\Users\lambejef\Desktop\temp\&s._summary.rtf;

%let outpath = C:\Users\jNelson\Dropbox\Baby and Me (1)\Program code for reporting\Reports\&month._&year.\by site\&s._summary.rtf;

%let template = journalS;
%let title1 = Summary Data: Baby and Me Tobacco Free;
%let title2 = The following report is a summary of drops, enrollments, forms filled out and the number of vouchers handed out at &s; 
%let datefoot = December 2013 - &month. &year.;




/** Set ODS escape character */
options orientation=portrait;
ods escapechar="^";

title1 color=darkblue "&title1";
title2 justify=left "&title2";
footnote1 "&datefoot";
ods rtf style=journals file = "&outpath";

proc report data=site_&i. nowd center split="|"
			style(report) = [outputwidth=100% cellpadding = 2 pt] style(header)=[vjust=middle borderbottomewidth=2 pt cellheight=.8 in];

			column varname subcat Enrolled;
				define varname / order "Category" style(column) = [cellwidth=1.4 in font_weight=bold];
				define subcat / "Subcategory" order order=internal style(header) = [just=l];
				define Enrolled / " " style(column) = [cellwidth=1 in just=center] style(header)= [just=c];
				break after varname /  summarize suppress style(summary)=[font_weight=bold foreground=blue];
				compute before varname;
				line " ";
				endcomp;
run;

ods rtf close;
ods html;
title;
footnote;


/***********************************************
Report generated is a detailed list of the following for each mother ID:
			1) Voucher serial numbers
			2) Due date
			3) Site of enrollment
			4) Events (session)
			5) Dates the sessions were attended
***********************************************/


*%let outpath_detailed = C:\Users\lambejef\Desktop\temp\&s._Detailed_&month._&year..rtf;
%let outpath_detailed = C:\Users\jNelson\Dropbox\Baby and Me (1)\Program code for reporting\Reports\&month._&year.\by site\&s._Detailed_&month._&year..rtf;
%let template = journalR;
%let title1 = Detailed Session Data: Baby and Me Tobacco Free;
/*****************************
	Create date automatically for the day
	the report has been generated
*****************************/



/** Set ODS escape character */

ods escapechar="^";
options orientation=landscape;
title "&title1";
footnote1 "Report for BMTF participants with an enrollment date after  12/30/13–%sysfunc(date(),mmddyy10.)"; *print date at foot, sysfunc generates todays date;
ods rtf style=&template file = "&outpath_detailed";

proc report data=temp_date_&i. nowd center split="|"
			style(report) = [outputwidth=100% cellpadding = 2 pt] style(header)=[vjust=middle borderbottomewidth=2 pt cellheight=.8 in];

			column site site_crf ID race_identified due_date redcap_event_name date mo1_vouchnum1_mpvl mo1_vouchnum2_mpvl;
				define site / order noprint;
				define site_crf /  style(column) = [cellwidth=1 in font_weight=bold just=left];
				define ID / style(column) = [cellwidth=.5 in font_weight=bold just=center];
				define race_identified /  style(column) = [cellwidth=.70 in font_weight=bold just=left];
				define due_date / style(column) = [cellwidth=.55 in font_weight=bold just=c];
				*define delivery_date1 / style(column) = [cellwidth=.55 in font_weight=bold just=c];
				define redcap_event_name / style(column) = [cellwidth=1.2 in font_weight=bold just=center] ;
				define date / style(column) = [cellwidth=.6 in font_weight=bold just=center] style(header)= [just=c];
				define mo1_vouchnum1_mpvl / style(column) = [cellwidth=.4 in font_weight=bold] ;
				define mo1_vouchnum2_mpvl / style(column) = [cellwidth=.4 in font_weight=bold];

				break before site / skip;
				
run;
				

ods rtf close;
ods html;
title;
footnote;
*NOTE: delivery date removed 11/12 per request by Martha;




%end;

%mend site_summary;


/****************************************************************************************************************************************

									Warning! Warning! Warning! Warning! Warning! Warning! 

********************************************************************************************************************************************/


/* Before proceeding any further:
		1) Create a sub folder under the report folder for the month and date you wish to generate reports for

		2) Make sure the macro has been updated for the appropriate month and year of the report you wish to generate

		3) Double check 

*/


%site_summary(month=October, year=2015, site=35); /* Is this the appropriate month and year that you wish to generate a report for? */


*%let outpath = C:\Users\jNelson\Dropbox\Baby and Me (1)\Program code for reporting\Reports\&month._&year.\by site\&s._Summary_&month._&year..rtf; 
*%let outpath_detailed = C:\Users\jNelson\Dropbox\Baby and Me (1)\Program code for reporting\Reports\&month._&year.\by site\&s._detailed_&month._&year..rtf;
