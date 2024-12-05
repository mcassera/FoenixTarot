5     ' Foenix Tarot
10    ' 2024, Michael Cassera
15    '
20    ' set some screen and MMU parameters
25    poke 1,0
30    pokel $d00d,$000000
35    cursor off
40    
45    ' load the ML routine for memory copy
50    bload "tarot/mlcopy2$7800.bin",$7800
55    
60    ' turn on the bitmap and clear
65    bitmap on
70    bitmap clear 0
75    cls
80    
85    ' load font and palette info
90    loadfont()
95    loadpal()
100   
105   ' load the title screen live
110   bload "tarot/tarot_title.bin",$10000
115   
120   ' load and display the welcome text 
125   bload "tarot/welcome.txt",$6000
130   printtext(0,0)
135   
140   ' load some assets - backs of cards etc.
145   bload "tarot/tarot_3Card.bin",$30000
150   bload "tarot/f79.bin",$40000
155   bload "tarot/r79.bin",$42bc0
160   
165   ' Set some arrays
170   dim deck$(78),spread$(11),pos(3),cmem(3)
175   pos(1)=22:pos(2)=120:pos(3)=218
180   cmem(1)=$45780:cmem(2)=$48340:cmem(3)=$4af00
185   cards=alloc(78)
190   user=alloc(20)
195   
200   ' Get the 4 types of spreads we can do
205   for n=0 to 11
210     read spread$(n)
215   next
220   
225   ' load user data if there is any
230   bload "tarot/user.bin",user
235   
240   ' Main routine for the program
245   repeat
250     clearspread()
255     user$=""
260     
265   ' Get the user name and spread from previous session  
270     for n=0 to 9
275       if peek(user+n)<>asc(".") then user$=user$+chr$(peek(user+n))
280     next
285     usersize=len(user$)
290     sp=val(chr$(peek(user+10)))
295   
300     if user$="new" 
305       bload "tarot/newuser.txt",$6000
310       printtext(0,0)
315       newuser=1
320     else
325       bload "tarot/olduser.txt",$6000
330       printtext(usersize,user)
335       repeat
340       until peek($680)<>0
345       poke $680,0
350       k=peek($678)
355       if (k=89) | (k=121)
360         msg=peek(user+10)
365         newuser = 0
370       else
375         newuser = 2
380       endif
385     endif
390     
395     if newuser=2
400       bload"tarot/changeuser.txt",$6000
405       printtext(usersize,user)
410       newuser=1
415     endif
420     
425     ' Show old spread
430     if newuser=0
435       bload "tarot/viewoldspread.txt",$6000
440       printtext(usersize,user)
445       repeat
450         repeat
455         until peek($680)<>0
460         keypress=peek($678):poke $680,0
465       until (keypress=asc("y")) | (keypress=asc("n"))
470       if keypress=asc("y")
475         oldcards(user, sp)
480         viewdes(1)
485         clearspread()
490         cls
495       endif  
500     endif
505       
510     ' enter new user name here
515     if newuser=1
520       ix=35:iy=57
525       inputpos=$c000+(iy*80)+ix
530       poke 1,2
535       for n=0 to 9
540         poke inputpos+n,46
545         poke user+n,46
550       next
555     
560   
565       keypress=0:n=0
570       while keypress<>13
575         repeat
580         until peek($680)<>0
585         keypress=peek($678):poke $680,0
590         if keypress=8
595           n=n-1
600           if n<0 then n=0      
605           poke inputpos+n,46
610           poke user+n,46
615         endif
620         if ((keypress>64)&(keypress<91))|((keypress>96)&(keypress<123))
625           poke inputpos+n,keypress
630           poke user+n,keypress
635           n=n+1:if n>9 then n=9
640         endif
645       wend
650       poke 1,0    
655       user$=""
660       for n=0 to 9
665         if peek(user+n)<>asc(".") then user$=user$+chr$(peek(user+n))
670       next
675       if user$="" 
680         user$="BOB"
685         poke user,66
690         poke user+1,79
695         poke user+2,66
700       endif
705       usersize=len(user$)
710     endif
715     
720     ' start the reading by selecting type of spread
725     clearspread()  
730     if newuser=0 then bload"tarot/welcomeold.txt",$6000
735     if newuser=1 then bload"tarot/welcomenew.txt",$6000
740     printtext(usersize,user)
745     
750     cursorpos(59,20)
755     print chr$(128+7);"LEFT/RIGHT to navigate, RETURN when done";
760     
765     keypress=0:sp=0
770     print chr$(12+128);
775     while keypress<>13
780       cursorpos(43,9)
785       print spread$(0+sp)
790       cursorpos(43,33)
795       print spread$(1+sp)
800       cursorpos(43,57)
805       print spread$(2+sp)
810       repeat
815       until peek($680)<>0
820       keypress=peek($678):poke $680,0
825       if keypress=6
830         sp=sp+3
835         if sp>9 then sp=0
840       endif
845       if keypress=2
850         sp=sp-3
855         if sp<0 then sp=9
860       endif
865     wend
870     
875     
880     cursorpos(59,20)
885     print chr$(128+7);"                                        ";
890     
895     
900     ' Now tell the user the think of a question before the draw
905     ' The computer is constantly picking random numbers
910     ' while waiting - to make it more random.
915     if newuser=0 then bload"tarot/questionold.txt",$6000
920     if newuser=1 then bload"tarot/questionnew.txt",$6000
925     printtext(usersize,user)
930     spacebar()
935     call $78f2
940     
945     ' clear the cards and shuffle the deck    
950     clearcards(cards)
955     shufflecards(cards)
960     drawcards()
965     
970     ' weird reset of name in memory - required to keep the name 
975     ' from corruption
980     for n=0 to usersize
985       poke user+n,asc(mid$(user$,n+1,1))
990     next  
995     
1000    if newuser=0 then bload"tarot/readytoviewold.txt",$6000
1005    if newuser=1 then bload"tarot/readytoviewnew.txt",$6000
1010    
1015    printtext(usersize,user)
1020    spacebar()
1025  
1030  
1035  
1040    ' reveal cards
1045    for p=1 to 3
1050      card$="tarot/"+deck$(p)
1055      loadtxt(card$)
1060      displaycard(cmem(p),$10000+(320*23)+pos(p),80,139)
1065      if p<3
1070        printtext(0,0)
1075        spacebar()
1080      endif     
1085    next
1090    
1095    viewdes(3)
1100  
1105  
1110  
1115  bload"tarot/another.txt",$6000
1120  printtext(usersize,user)
1125  
1130  ' save user data
1135  us$=user$+left$("..........",10-len(user$))
1140  us$=us$+str$(sp)+deck$(1)+deck$(2)+deck$(3)
1145  for g=0 to 19
1150    poke user+g,asc(mid$(us$,g+1,1))
1155  next
1160  bsave"tarot/user.bin",user,20
1165  
1170  cursorpos(59,18)
1175  print chr$(128+7);"'Q' to quit, anything else for new reading";  
1180  
1185  repeat
1190  until peek($680)<>0
1195  keypress=peek($678):poke $680,0
1200  
1205  cls
1210  until keypress=asc("q")
1215  
1220  bitmap off:poke $d000,1
1225  cursor on
1230  print chr$(128+9)
1235  print "    Thanks for using Foenix Tarot, goodbye ";user$;"."
1240  print
1245  end
1250  
1255  ' Wait for Spacebar
1260  proc spacebar()
1265    cursorpos(59,28)
1270    print chr$(128+7);"Press space to continue";
1275    repeat
1280      repeat
1285      q=random(78)
1290      until peek($680)<>0
1295      poke $680,0
1300    until peek($678)=32
1305    cursorpos(59,28)
1310    print "                        ";
1315    cursorpos(0,0)
1320  endproc
1325  
1330  ' Load the color palette
1335  proc loadpal()
1340    bload "tarot/tarot.pal",$4000
1345    poke 1,1
1350    for n=0 to 256*4    
1355      poke $D000+n,peek($4000+n)
1360    next
1365    poke 1,0
1370  endproc
1375  
1380  ' load image
1385  proc loadpic(filename$,loc)
1390    picname$=filename$+".bin"
1395    bload picname$,loc
1400  endproc
1405  
1410  ' load text
1415  proc loadtxt(filename$)
1420    textname$=filename$+".txt"
1425    bload textname$,$6000
1430  endproc
1435  
1440  ' clear the deck to shuffle
1445  proc clearcards(cards)
1450    for n=0 to 78
1455      poke cards+n,0
1460    next
1465  endproc
1470  
1475  ' clear the screen display of cards
1480  proc clearspread()
1485    displaycard($30000,$10000,320,167)
1490  endproc
1495  
1500  ' draw the cards for the spread
1505  proc drawcards()  
1510    ' draw cards and display backs
1515    for n=1 to 3
1520      card$="tarot/"+deck$(n)
1525      loadpic(card$,cmem(n))
1530      if left$(deck$(n),1)="f"
1535        displaycard($40000,$10000+(320*23)+pos(n),80,139) 
1540       else
1545        displaycard($42bc0,$10000+(320*23)+pos(n),80,139)
1550      endif
1555    next
1560  endproc
1565  
1570  ' display a previous spread
1575  proc oldcards(user,sp)  
1580    ' draw cards and display
1585    deck$(1)=chr$(peek(user+11))+chr$(peek(user+12))+chr$(peek(user+13))
1590    deck$(2)=chr$(peek(user+14))+chr$(peek(user+15))+chr$(peek(user+16))
1595    deck$(3)=chr$(peek(user+17))+chr$(peek(user+18))+chr$(peek(user+19))
1600  
1605    print chr$(128+4);
1610    cursorpos(43,9)
1615    print spread$(0+sp)
1620    cursorpos(43,33)
1625    print spread$(1+sp)
1630    cursorpos(43,57)
1635    print spread$(2+sp)    
1640          
1645    for n=1 to 3
1650      card$="tarot/"+deck$(n)
1655      loadpic(card$,cmem(n))
1660      displaycard(cmem(n),$10000+(320*23)+pos(n),80,139) 
1665    next
1670  endproc
1675  
1680  ' display a card using ml routine and DMA
1685  proc displaycard(s,d,sx,sy)
1690    pokel $7803,s
1695    pokel $7806,d
1700    pokel $7809,sx
1705    poke $780C,sy
1710    call $7800
1715  endproc
1720  
1725  ' load the font
1730  proc loadfont()
1735    bload "tarot/tarot_letters2.bin",$4000
1740    poke 1,1
1745    for n=32*8 to 123*8
1750      poke $C000+n,peek($4000+n-256)
1755    next
1760    poke 1,0
1765  endproc
1770  
1775  ' Shuffle the card for the draw
1780  proc shufflecards(cards)
1785    for n=1 to 78
1790      repeat
1795        c=random(78)+1
1800      until peek(cards+c)=0
1805      poke cards+c,1
1810      o=random(2)+1
1815      deck$(n)=mid$("fr",o,1)
1820      if c<10 then deck$(n)=deck$(n)+"0"
1825      deck$(n)=deck$(n)+str$(c)
1830    next
1835  endproc
1840  
1845  ' print the block text at the lower portion of the screen
1850  proc printtext(usersize,user)
1855    call $78f2
1860    j=0:y=0:n=0
1865    ul=0
1870    repeat
1875      a=peek($6000+n)
1880      
1885      if a=37
1890        for k=0 to usersize-1
1895          a=peek(user+k)
1900          poke 1,2
1905          poke ($C00C+(48*80))+(y*80)+j,a
1910          poke 1,3
1915          poke ($C00C+(48*80))+(y*80)+j,$E2
1920          poke 1,0
1925          j=j+1
1930        next
1935        n=n+1
1940        a=peek($6000+n)
1945      endif  
1950      
1955      if (a<>94)&(a<>13)
1960        poke 1,2
1965        poke ($C00C+(48*80))+(y*80)+j,a
1970        poke 1,3
1975        poke ($C00C+(48*80))+(y*80)+j,$E2
1980        poke 1,0
1985        n=n+1:j=j+1
1990      endif
1995      if a=(13)&(y=0)
2000        j=0
2005        y=y+2:n=n+2
2010      endif
2015      if (j>50)&(a=32)
2020        j=0
2025        y=y+1
2030      endif
2035    until a=94
2040  endproc
2045  
2050  ' set the cursor position for printing
2055  proc cursorpos(y,x)
2060    for n=0 to 60
2065      print chr$(16);
2070    next
2075    print chr$(1);
2080    for n=1 to y
2085      print chr$(14);
2090    next
2095    for n=1 to x
2100      print chr$(6);
2105    next
2110  endproc
2115  
2120  ' view the descriptions of the cards
2125  proc viewdes(cd)
2130    ' press through descriptions
2135   
2140    cursorpos(59,20)
2145    print chr$(128+7);"LEFT/RIGHT to navigate, RETURN when done";
2150    repeat
2155  
2160      
2165      if cd=1 
2170        cursorpos(43,9):print chr$(128+15);spread$(sp)
2175        cursorpos(43,33):print chr$(128+4);spread$(sp+1)
2180        cursorpos(43,57):print chr$(128+4);spread$(sp+2)
2185      endif
2190      if cd=2
2195        cursorpos(43,9):print chr$(128+4);spread$(sp)
2200        cursorpos(43,33):print chr$(128+15);spread$(sp+1)
2205        cursorpos(43,57):print chr$(128+4);spread$(sp+2)
2210      endif
2215      if cd=3
2220        cursorpos(43,9):print chr$(128+4);spread$(sp)
2225        cursorpos(43,33):print chr$(128+4);spread$(sp+1)
2230        cursorpos(43,57):print chr$(128+15);spread$(sp+2)
2235      endif
2240    
2245      card$="tarot/"+deck$(cd)
2250      loadtxt(card$)
2255      printtext(0,0)
2260      
2265      repeat
2270      until peek($680)<>0
2275      keypress=peek($678):poke $680,0
2280      if keypress=6
2285        cd=cd+1:if cd>3 then cd=1
2290      endif
2295      if keypress=2
2300        cd=cd-1:if cd<1 then cd=3
2305      endif
2310      
2315    until keypress=13
2320  
2325    cursorpos(59,20)
2330    print chr$(128+7);"                                        ";
2335  endproc
2340  
2345  
2350  ' data for the spreads
2355  data "     Past    ","   Present   ","    Future   "
2360  data "  Situation  ","   Problem   ","    Action   "
2365  data "     You     ","     Me      ","      Us     "
2370  data "  Option A   ","   Option B  ","How to decide"
