/*log using mcdben.txt, text append*/

use mc_fmr
keep state year mcdben fymcdben
reshape wide mcdben fymcdben, i(state) j(year)

gen rat96=mcdben1996/fymcdben1996
gen rat97=mcdben1997/fymcdben1997
gen rat98=mcdben1998/fymcdben1998
gen rat99=mcdben1999/fymcdben1999
gen rat00=mcdben2000/fymcdben2000
gen rat01=mcdben2001/fymcdben2001
gen rat02=mcdben2002/fymcdben2002

gen m96b=fymcdben1996*rat97
gen m96c=fymcdben1996*rat98
gen m96d=fymcdben1996*rat99
gen m96e=fymcdben1996*rat00
gen m96f=fymcdben1996*rat01
gen m96g=fymcdben1996*rat02

reg m96b mcdben1996
reg m96c mcdben1996
reg m96d mcdben1996
reg m96e mcdben1996
reg m96f mcdben1996
reg m96g mcdben1996


gen m97a=fymcdben1997*rat96
gen m97c=fymcdben1997*rat98
gen m97d=fymcdben1997*rat99
gen m97e=fymcdben1997*rat00
gen m97f=fymcdben1997*rat01
gen m97g=fymcdben1997*rat02

reg m97a mcdben1997
reg m97c mcdben1997
reg m97d mcdben1997
reg m97e mcdben1997
reg m97f mcdben1997
reg m97g mcdben1997

gen m98a=fymcdben1998*rat96
gen m98b=fymcdben1998*rat97
gen m98d=fymcdben1998*rat99
gen m98e=fymcdben1998*rat00
gen m98f=fymcdben1998*rat01
gen m98g=fymcdben1998*rat02

reg m98a mcdben1998
reg m98b mcdben1998
reg m98d mcdben1998
reg m98e mcdben1998
reg m98f mcdben1998
reg m98g mcdben1998

gen m99a=fymcdben1999*rat96
gen m99b=fymcdben1999*rat97
gen m99c=fymcdben1999*rat98
gen m99e=fymcdben1999*rat00
gen m99f=fymcdben1999*rat01
gen m99g=fymcdben1999*rat02

reg m99a mcdben1999
reg m99b mcdben1999
reg m99c mcdben1999
reg m99e mcdben1999
reg m99f mcdben1999
reg m99g mcdben1999

gen m00a=fymcdben2000*rat96
gen m00b=fymcdben2000*rat97
gen m00c=fymcdben2000*rat98
gen m00d=fymcdben2000*rat99
gen m00f=fymcdben2000*rat01
gen m00g=fymcdben2000*rat02

reg m00a mcdben2000
reg m00b mcdben2000
reg m00c mcdben2000
reg m00d mcdben2000
reg m00f mcdben2000
reg m00g mcdben2000

gen m01a=fymcdben2001*rat96
gen m01b=fymcdben2001*rat97
gen m01c=fymcdben2001*rat98
gen m01d=fymcdben2001*rat99
gen m01e=fymcdben2001*rat00
gen m01g=fymcdben2001*rat02

reg m01a mcdben2001
reg m01b mcdben2001
reg m01c mcdben2001
reg m01d mcdben2001
reg m01e mcdben2001
reg m01g mcdben2001

