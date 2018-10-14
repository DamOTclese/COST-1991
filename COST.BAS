
	dim discount.data(20, 3), cost.data$(20)
	dim free.areas(20), free.areas.exchanges$(20)
	dim zone.areas(20), zone.codes(20), zone.exchanges$(20)

rem	*****************************************************************
rem	* Remove leading spaces from the record.			*
rem	* Remove trailing spaces from the record.			*
rem	* Remove multiple spaces between data elements from record.	*
rem	*								*
rem	*****************************************************************

def process.raw.data.line
	record$ = ucase$(record$)

	while " " = left$(record$, 1)
	   record$ = right$(record$, len(record$) - 1)
	wend

	while " " = right$(record$, 1)
	   record$ = left$(record$, len(record$) - 1)
	wend

process.loop:
	at.point.one% = match("  ", record$, 1)
	if at.point.one% = 0 then return

	record$ = left$(record$, at.point.one% - 1) + \
	   right$(record$, len(record$) - at.point.one%)

	goto process.loop
fend

rem	*****************************************************************
rem	* We have the area code and exchange, see what zone its in.	*
rem	* A.The.Area and The.Exchange are known.			*
rem	*								*
rem	*****************************************************************

def	see.what.zone.its.in
	for b.loop% = 1 to zone.count%

	   if zone.areas(b.loop%) = val(a.the.area$) then \
	      goto we.have.this.area.found

test.another.zone:
	next b.loop%
	return

we.have.this.area.found:
	at.point.two% = match(the.exchange$, zone.exchanges$(b.loop%), 1)
	if at.point.two% = 0 then goto test.another.zone
	the.zone.is = zone.codes(b.loop%)

	for c.loop% = 1 to cost.count%
	   at.point.3% = match(" ", cost.data$(c.loop%), 1)

	   if at.point.3% = 0 then \
	      print "Error in Cost line within file: Local.Dat": \
	      stop

	   if val(left$(cost.data$(c.loop%), at.point.3% - 1)) = \
	      the.zone.is then goto process.the.cost.information

	next c.loop%
	first.min$ = "": return

process.the.cost.information:
	at.point.one% = match(" AND ", ucase$(cost.data$(c.loop%)), 1)

	if at.point.one% = 0 then \
	   print "Invalid cost information in Local.Dat": \
	   stop

	first.min$ = left$(cost.data$(c.loop%), at.point.one% - 1)

	after.that$ = right$(cost.data$(c.loop%), \
	   len(cost.data$(c.loop%)) - (at.point.one% + 4))

	return
fend

rem	*****************************************************************
rem	* Go through the nodelist line and see if there is a phone	*
rem	* number in it by searching for a 1-XXX that matches the area	*
rem	* codes or zones. If so, see if te exchange is listed as free	*
rem	* or cheap.							*
rem	*								*
rem	*****************************************************************

def	check.for.a.phone.number
	for loop% = 1 to local.count%
	   test.string$ = "1-" + str$(free.areas(loop%)) + "-"
	   if 0 <> match(test.string$, record$, 1) then goto test.this.one

not.this.area.code:
	next loop%

	for loop% = 1 to zone.count%
	   test.string$ = "1-" + str$(zone.areas(loop%)) + "-"
	   a.the.area$ = str$(zone.areas(loop%))
	   if 0 <> match(test.string$, record$, 1) then goto test.this.two

not.this.area.code.either:
	next loop%
	return

test.this.one:
	the.point% = match(test.string$, record$, 1)
	the.exchange$ = mid$(record$, the.point% + 6, 3)

	if 0 = match(the.exchange$, free.areas.exchanges$(loop%), 1) then \
	   goto not.this.area.code

	call process.raw.data.line
	print record$
	print " --- FREE ---"

	if answer$ = "F" then print using "&"; #2; record$: \
	   print using "&"; #2; " --- FREE ---"

	return

test.this.two:
	the.point% = match(test.string$, record$, 1)
	the.exchange$ = mid$(record$, the.point% + 6, 3)

	if 0 = match(the.exchange$, zone.exchanges$(loop%), 1) then \
	   goto not.this.area.code.either

	call process.raw.data.line
	print record$
	call see.what.zone.its.in

	if first.min$ = "" then \
	   the.ap$ = " No cost information" else \
	   the.ap$ = " " + first.min$ + " the first minute, " + after.that$

	print " --- Zone:"; the.ap$ + " every minute after"
	print

	if answer$ = "F" then print using "&"; #2; record$: \
	   print using "&"; #2; " --- Zone:" + the.ap$ + \
	   " every minute after": print using "&"; #2; ""

	return
fend

	on error goto we.have.error
	cls$ = chr$(27) + chr$(91) + chr$(50) + chr$(74)
	phone.number$ = command$
	if ucase$(phone.number$) = "SEARCH" then goto search.node.list

	if len(phone.number$) <> 12 then \
	   print "Syntax is: XXX-XXX-XXXX": \
	   print "Or:        Cost SEARCH": \
	   stop

	if mid$(phone.number$, 4, 1) <> "-" or \
	   mid$(phone.number$, 8,1 ) <> "-" then \
	   print "Syntax is: Cost XXX-XXX-XXXX": \
	   print "Or:        Cost SEARCH": \
	   stop

	print cls$;
	area.code$ = left$(phone.number$, 3)
	exchange$ = mid$(phone.number$, 5, 3)
	the.number$ = right$(phone.number$, 4)

	if end #1 then cant.find.phone.file
	open "LOCAL.DAT" as 1
	goto phone.file.found

cant.find.phone.file:
	print "Unable to locate file: LOCAL.DAT"
	stop

phone.file.found:
	if end #1 then end.of.file.found
	read #1; line record$
	if len(record$) < 1 then goto phone.file.found
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto phone.file.found

bounce.back.into.it:
	if left$(record$, 5) = "LOCAL" then goto process.local.information
	if left$(record$, 4) = "ZONE"  then goto process.zone.information
	if left$(record$, 4) = "COST"  then goto process.cost.information
	if left$(record$, 4) = "FROM"  then goto process.from.information
	print: print "Unknown command in LOCAL.DAT file:"
	print record$: close 1: stop

rem	*****************************************************************
rem	* A local command was found. If the local does not point to the	*
rem	* area code we want to look up, read data until another command	*
rem	* is found while throwing the data away.			*
rem	*								*
rem	*****************************************************************

process.local.information:
	if right$(record$, 3) = area.code$ then goto we.have.area.code

throw.away.local.information:
	read #1; line record$
	if len(record$) < 1 then goto throw.away.local.information
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto throw.away.local.information

	if left$(record$, 5) = "LOCAL" or \
	   left$(record$, 4) = "ZONE" or \
	   left$(record$, 4) = "COST" or \
	   left$(record$, 4) = "FROM" then goto bounce.back.into.it

	goto throw.away.local.information

rem	*****************************************************************
rem	* Here we have found a ZONE XXX command that matches the area	*
rem	* code being looked for. Now read in data and try to match a	*
rem	* listed exchange. If a command comes up, bounce back and       *
rem	* branch back to command.					*
rem	*								*
rem	*****************************************************************

we.have.area.code:
	read #1; line record$
	if len(record$) < 1 then goto we.have.area.code
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto we.have.area.code

	if left$(record$, 5) = "LOCAL" or \
	   left$(record$, 4) = "ZONE" or \
	   left$(record$, 4) = "COST" or \
	   left$(record$, 4) = "FROM" then goto bounce.back.into.it

	if match(exchange$, record$, 1) = 0 then \
	   goto we.have.area.code

	print "Area code: "; area.code$
	print "Exchange:  "; exchange$
	print "Number:    "; the.number$
	print: print "This is a free call": stop

rem	*****************************************************************
rem	* It wasn't a free call, so its a Zone or long distance. At     *
rem	* this point, we have encountered a Zone. See if the area code  *
rem	* encountered is what we are looking up. If not, throw data     *
rem	* away until a command is found then bounce to command.         *
rem	*								*
rem	*****************************************************************

process.zone.information:
	if zone.flag% = 1 then goto throw.zone.away
	if right$(record$, 3) = area.code$ then goto we.have.a.zone

throw.zone.away:
	read #1; line record$
	if len(record$) < 1 then goto throw.zone.away
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto throw.zone.away

	if left$(record$, 5) = "LOCAL" or \
	   left$(record$, 4) = "ZONE" or \
	   left$(record$, 4) = "COST" or \
	   left$(record$, 4) = "FROM" then goto bounce.back.into.it

	goto throw.zone.away

rem	*****************************************************************
rem	* This is the zone we are interested in. Look and see if any	*
rem	* of the listed exchanges match the one we are looking for. If	*
rem	* not, bounce out, else, set a zone flag and bounce out.	*
rem	*								*
rem	*****************************************************************

we.have.a.zone:
	the.zone$ = right$(record$, len(record$) - 5)
	the.zone$ = left$(the.zone$, len(the.zone$) - 4)

	while " " = left$(the.zone$, 1)
	   the.zone$ = right$(the.zone$, len(the.zone$) - 1)
	wend

	while " " = right$(the.zone$, 1)
	   the.zone$ = left$(the.zone$, len(the.zone$) - 1)
	wend

loop.on.zone:
	read #1; line record$
	if len(record$) < 1 then goto loop.on.zone
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto loop.on.zone

	if left$(record$, 5) = "LOCAL" or \
	   left$(record$, 4) = "ZONE"  or \
	   left$(record$, 4) = "COST"  or \
	   left$(record$, 4) = "FROM"  then goto bounce.back.into.it

	if 0 <> match(exchange$, record$, 1) and zone.flag% = 0 then \
	   zone.flag% = 1

	goto loop.on.zone

rem	*****************************************************************
rem	* We have found a cost line for a zone. See if the zone we have *
rem	* found is describing the cost of the zone we found, (if zone	*
rem	* flag is set that is).						*
rem	*								*
rem	*****************************************************************

process.cost.information:
	record$ = right$(record$, len(record$) - 10)
	at.point.one% = match(" ", record$, 1)

	if at.point.one% = 0 then \
	   print "Error in cost zone information.": \
	   close 1: stop

	cost.zone$ = left$(record$, at.point.one% - 1)

	if zone.flag% <> 1 then goto phone.file.found
	if the.zone$ <> cost.zone$ then goto phone.file.found

rem	*****************************************************************
rem	* We found the zone we wanted to dial into and the cost of that	*
rem	* zones access. Extract the cost of the first minute and the	*
rem	* minutes after that.						*
rem	*								*
rem	*****************************************************************

	record$ = right$(record$, len(record$) - at.point.one%)
	at.point.one% = match(" AND ", record$, 1)
	first.min$ = left$(record$, at.point.one% - 1)
	after.that$ = right$(record$, len(record$) - (at.point.one% + 4))
	goto phone.file.found

rem	*****************************************************************
rem	* From information is found. Display long distance if the zone	*
rem	* was not listed, else simply display the discounts 		*
rem	* information.							*
rem	*****************************************************************

process.from.information:
	discount.number% = discount.number% + 1
	discount.data(discount.number%, 1) = val(mid$(record$, 6, 2))
	discount.data(discount.number%, 2) = val(mid$(record$, 12, 2))
	discount.data(discount.number%, 3) = val(mid$(record$, 15, 3))

check.for.more.from:
	read #1; line record$
	if len(record$) < 1 then goto check.for.more.from
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto check.for.more.from
	if left$(record$, 4) <> "FROM" then goto check.for.more.from
	goto process.from.information

end.of.file.found:
	print "Long Distance Call": \
	print "Area Code: "; area.code$
	print "Exchange:  "; exchange$
	print "Number:    "; the.number$

	if zone.flag% = 0 then \
	   print " --- Long Distance Call ---": \
	   close 1: stop: \
	   else print "In Zone:   "; the.zone$

	if first.min$ = "" then \
	   print "No cost information provided."

	print " --- First minute: "; first.min$; ", after that: ";
	print after.that$; " ---": print

	for loop% = 1 to discount.number%
	   the.from% = discount.data(loop%, 1)
	   the.to% = discount.data(loop%, 2)
	   the.percent% = discount.data(loop%, 3)
	   print "From";

	   if the.from% > 12 then \
	      print the.from% - 12; "pm";: \
	      else print the.from%; "am";

	   print " to";

	   if the.to% > 12 then \
	      print the.to% - 12; "pm";: \
	      else print the.to%; "am";

	   print " at";

	   if the.percent% = 100 then \
	      print " full cost.": \
	      else print the.percent%; "%"

	next loop%

	close 1: stop

rem	*****************************************************************
rem	* Go through the local.dat file and pull all area codes that    *
rem	* are designated as locals. Plug that information into the	*
rem	* matrix in RAM. Then search through the nodelist for any of	*
rem	* the exchanged listed.						*
rem	*								*
rem	*****************************************************************

search.node.list:
	if end #1 then cant.find.phone.file
	open "LOCAL.DAT" as 1
	if end #1 then end.pass.one

go.through.file:
	read #1; line record$
	if len(record$) < 1 then goto go.through.file
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto go.through.file
	if left$(record$, 5) = "LOCAL" then goto plug.local.data
	if left$(record$, 4) = "ZONE"  then goto plug.zone.data
	if left$(record$, 4) = "COST"  then goto plug.cost.data
	goto go.through.file

plug.local.data:
	local.count% = local.count% + 1
	free.areas(local.count%) = val(right$(record$, 3))

rem	*****************************************************************
rem	* Now find all of the exchanges					*
rem	*								*
rem	*****************************************************************

acquire.local.exchanges:
	read #1; line record$
	if len(record$) < 1 then goto acquire.local.exchanges
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto acquire.local.exchanges

	if left$(record$, 5) = "LOCAL" then goto plug.local.data
	if left$(record$, 4) = "ZONE"  then goto plug.zone.data
	if left$(record$, 4) = "COST"  then goto plug.cost.data
	if left$(record$, 4) = "FROM"  then goto go.through.file

	free.areas.exchanges$(local.count%) = \
	   free.areas.exchanges$(local.count%) + " " + record$

	goto acquire.local.exchanges

plug.zone.data:
	zone.count% = zone.count% + 1
	zone.areas(zone.count%) = val(right$(record$, 3))
	record$ = left$(record$, len(record$) - 4)
	the.zone$ = right$(record$, len(record$) - 5)

	while " " = left$(the.zone$, 1)
	   the.zone$ = right$(the.zone$, len(the.zone$) - 1)
	wend

	while " " = right$(the.zone$, 1)
	   the.zone$ = left$(the.zone$, len(the.zone$) - 1)
	wend

	zone.codes(zone.count%) = val(the.zone$)

rem	*****************************************************************
rem	* Stuff any exchanges into the array.				*
rem	*								*
rem	*****************************************************************

acquire.zone.exchanges:
	read #1; line record$
	if len(record$) < 1 then goto acquire.zone.exchanges
	call process.raw.data.line
	if left$(record$, 1) = ";" then goto acquire.zone.exchanges

	if left$(record$, 5) = "LOCAL" then goto plug.local.data
	if left$(record$, 4) = "ZONE"  then goto plug.zone.data
	if left$(record$, 4) = "COST"  then goto plug.cost.data
	if left$(record$, 4) = "FROM"  then goto go.through.file

	zone.exchanges$(zone.count%) = \
	   zone.exchanges$(zone.count%) + " " + record$

	goto acquire.zone.exchanges

plug.cost.data:
	cost.count% = cost.count% + 1
	cost.data$(cost.count%) = right$(record$, len(record$) - 10)
	goto go.through.file

end.pass.one:
	close 1
	if end #1 then try.to.open.text.file
	open "NODELIST.PRN" as 1
	goto nodelist.is.opened

try.to.open.text.file:
	if end #1 then cant.open.nodelist.file
	open "NODELIST.TXT" as 1
	goto nodelist.is.opened

cant.open.nodelist.file:
	print "Unable to locate either NODLIST.PRN or NODELIST.TXT"
	stop

nodelist.is.opened:
	print cls$

ask.again.for.file.name:
	print "There are"; local.count%; "local area codes"
	print "There are"; zone.count%; "cheap zone codes"
	PRINT "COST COUNT: "; COST.COUNT%

	print
	input "Do you want file output or screen (F or S): "; line answer$
	answer$ = ucase$(left$(answer$, 1))
	if answer$ = "F" then goto create.an.output.file
	if answer$ = "S" then goto produce.screen.output
	goto nodelist.is.opened

create.an.output.file:
	input "Enter file name: [COST.OUT] "; line file.name$
	if len(file.name$) = 0 then file.name$ = "COST.OUT"
	if end #2 then cant.create.output.file
	create file.name$ as 2
	goto produce.screen.output

cant.create.output.file:
	print "Unable to create file: "; file.name$
	goto ask.again.for.file.name

produce.screen.output:
	if end #1 then end.of.nodelist.file
	read #1; line record$
	call check.for.a.phone.number
	goto produce.screen.output

end.of.nodelist.file:
	close 1
	if answer$ = "Y" then close 2
	stop

we.have.error:
	console: print "Error "; err; " in line "; errl: stop: stop: stop


