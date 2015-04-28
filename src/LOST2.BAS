DECLARE SUB talk7 ()
DECLARE SUB erasemaps ()
DECLARE SUB playervars ()
DECLARE SUB fadeout ()
DECLARE SUB systemsetup ()
'$DYNAMIC
DECLARE SUB click ()
DECLARE SUB playwav (wavefile$)
DECLARE SUB points ()
DECLARE SUB sbplay (notes$)
DECLARE SUB sbsetup ()
DECLARE SUB mousectrl (set!)
DECLARE SUB scroll (text$, ca!, cb!, col!, text2$, cc!, cd!, col2!)
DECLARE SUB mouseinit ()
DECLARE SUB mouseupdate ()
DECLARE SUB person (num%)
DECLARE SUB endofdemo ()
DECLARE SUB darksequence ()
DECLARE SUB highlight2 (menuselect!)
DECLARE SUB options ()
DECLARE SUB opts ()
DECLARE SUB credits ()
DECLARE SUB talk6 ()
DECLARE SUB youarehere (num%)
DECLARE SUB movie (num%)
DECLARE SUB talk5 ()
DECLARE SUB store ()
DECLARE SUB talk4 ()
DECLARE SUB loadgif (a$)
DECLARE SUB delay (D!)
DECLARE SUB logo ()
DECLARE SUB task (quest$, PLA!, quest2$, pla2)
DECLARE SUB highlight (num)
DECLARE SUB updateinven ()
DECLARE SUB itemvars ()
DECLARE SUB clearmenu (clea, cleb, clec)
DECLARE SUB examine (selected$)
DECLARE SUB opopn (selected$)
DECLARE SUB combine (selected$)
DECLARE SUB use (selected$)
DECLARE SUB loadtiles ()
DECLARE SUB invenselect (select$)
DECLARE SUB inventory ()
DECLARE SUB getitem ()
DECLARE SUB gettheitem (item$)
DECLARE SUB intro ()
DECLARE SUB status ()
DECLARE SUB talk3 ()
DECLARE SUB heal ()
DECLARE SUB talk2 ()
DECLARE SUB reply (text$)
DECLARE SUB loadmap (al!)
DECLARE SUB loadmaps (num%)
DECLARE SUB box ()
DECLARE SUB ask (quest$, PLA!)
DECLARE SUB selecty ()
DECLARE SUB waitforkey ()
DECLARE SUB talkselect ()
DECLARE SUB talk1 ()
DECLARE SUB look ()
DECLARE SUB tileback ()
DECLARE SUB selectmove ()
DECLARE SUB drawit ()
DECLARE SUB refresh (maptype$)
DECLARE SUB evars ()
DECLARE SUB dead ()
DECLARE SUB vars ()
DECLARE SUB fightmon ()
DECLARE SUB game ()
DECLARE SUB waitasec ()

TYPE PaletteType
Red AS INTEGER
Green AS INTEGER
Blue AS INTEGER
END TYPE
DIM SHARED mouse$
COMMON SHARED lb%, rb%, ex%, mouseax%, mousebx%, mousecx%, mousedx%, mouse%, length!
CLEAR , , 29000
'$INCLUDE: 'c:\qb45\lost2\data.bi'
DIM SHARED map(25, 0 TO 21, 0 TO 40)
DIM SHARED databox$(10), statsbox$(9), storebox$(14), invenbox$(22), objectbox$(8)
DIM SHARED enemyname$(3)
DIM SHARED enemywep$(3)
DIM SHARED enemyarm$(3)
DIM SHARED enemyhp(3)
DIM SHARED enemyexp(3)
DIM SHARED enemymon(3)
DIM SHARED enemywp(3)
DIM SHARED enemyap(3)
DIM SHARED enhpcnt(3)
DIM SHARED Pal AS PaletteType
DIM SHARED col, row, xx, yy, hp, healcount, expe
DIM SHARED weppower
DIM SHARED armpower
DIM SHARED money
DIM SHARED posa, posb
DIM SHARED weapon$, armor$
DIM SHARED enemycount
DIM SHARED placement
DIM SHARED maptype$
DIM SHARED selects
DIM SHARED temp
DIM SHARED inven$(20)
DIM SHARED invennum
DIM SHARED ply1(400), ply2(400), ply3(400), ply4(400)
DIM SHARED dynam(400), mug(400), light(400)
DIM SHARED bottle(400)
DIM SHARED magnet(400)
DIM SHARED rag(400)
DIM SHARED gas(400)
DIM SHARED en1(400), en2(400), en3(400), en4(400), en5(400)
DIM SHARED chr1(400), chr2(400), obj1(400), obj6(400), flr1(400), flr2(400), flr3(400), obj2(400), obj3(400), obj4(400), obj5(400), chr3(400), obj7(400), obj8(400), obj9(400), obj10(400), obj11(400)
DIM SHARED opopen, opexam, opuse, opusewith, melt, boom, hands, magnet, bottle, dyn, mug, gas, well, ikey, enem, curmap, mapnums
DIM SHARED menuselect, al, row2, col2
DIM SHARED selected$, location$, island, personon, persontype$
CONST down = 1
CONST left = 2
CONST up = 3
CONST right = 4
RANDOMIZE TIMER
placement = right
vars
game




REM $STATIC
SUB ask (quest$, PLA)
COLOR 3
FOR temp = 1 TO LEN(quest$)
letr$ = MID$(quest$, temp, 1)
LOCATE PLA, 1 + temp: PRINT letr$
FOR temp2 = 1 TO 3000: NEXT
NEXT
COLOR 7
END SUB

SUB box
CLS
COLOR 47
LOCATE 1, 1: PRINT STRING$(40, "-")
LOCATE 22, 1: PRINT STRING$(40, "-")
FOR a = 1 TO 40 STEP 39
FOR tem = 1 TO 22
LOCATE tem, a: PRINT "|"
NEXT
NEXT
COLOR 25
LOCATE 4, 20: PRINT CHR$(1)
END SUB

SUB clearmenu (clea, cleb, clec)
LOCATE clea, cleb: PRINT STRING$(clec, " ")
END SUB

SUB click
sbplay "1$aaa"
END SUB

SUB combine (selected$)
clearmenu 19, 2, 38
clearmenu 20, 2, 38
ask "(C)ombine, (S)eperate, (F)orget it", 19
DO
select$ = INKEY$
IF select$ = "c" OR select$ = "C" THEN GOSUB combineit
IF select$ = "s" OR select$ = "S" THEN GOSUB seperateit
IF select$ = "f" OR select$ = "F" THEN clearmenu 19, 2, 38: clearmenu 20, 2, 38: EXIT SUB
LOOP

combineit:
clearmenu 19, 2, 38
clearmenu 20, 2, 38
ask "Select an item to combine it with.", 19
DO
select$ = INKEY$
IF select$ = "d" OR select$ = "D" THEN inventory
IF select$ = CHR$(27) THEN drawit: game
IF select$ <> "" THEN GOSUB combinechoice
LOOP

combinechoice:
IF selected$ = "bottle" AND inven$(VAL(select$)) = "Gas" THEN GOSUB addbottletogas
IF selected$ = "oily rag" AND inven$(VAL(select$)) = "Bottle" THEN GOSUB addragtobottlevice
IF selected$ = "oily rag" AND inven$(VAL(select$)) = "Bottle with Gas" THEN GOSUB addmolotov
IF selected$ = "bottle" AND inven$(VAL(select$)) = "Oily Rag" THEN GOSUB addragtobottlevice
IF selected$ = "bottlerag" AND inven$(VAL(select$)) = "Gas" THEN GOSUB addmolotov
IF selected$ = "bottlegas" AND inven$(VAL(select$)) = "Oily Rag" THEN GOSUB addragtogas
IF selected$ = "gas" AND inven$(VAL(select$)) = "Bottle" THEN GOSUB addbottletogas
IF selected$ = "gas" AND inven$(VAL(select$)) = "Bottle with Rag" THEN GOSUB addmolotov
clearmenu 19, 2, 38
clearmenu 20, 2, 38
ask "That doesn't work.", 19
EXIT SUB

addbottletogas:
points
FOR temp = 0 TO invennum
IF inven$(temp) = "Bottle" THEN temp2 = temp
IF inven$(temp) = "Gas" THEN inven$(temp) = "Bottle with Gas"
NEXT
FOR temp = temp2 TO invennum
inven$(temp) = inven$(temp + 1)
NEXT
GOSUB endit

addragtobottlevice:
points
FOR temp = 0 TO invennum
IF inven$(temp) = "Oily Rag" THEN temp2 = temp
IF inven$(temp) = "Bottle" THEN inven$(temp) = "Bottle with Rag"
NEXT
FOR temp = temp2 TO invennum
inven$(temp) = inven$(temp + 1)
NEXT
GOSUB endit


addragtogas:
points
FOR temp = 0 TO invennum
IF inven$(temp) = "Oily Rag" THEN temp2 = temp
IF inven$(temp) = "Bottle with Gas" THEN inven$(temp) = "Molotov Cocktail"
NEXT
FOR temp = temp2 TO invennum
inven$(temp) = inven$(temp + 1)
NEXT
GOSUB endit


addmolotov:
points
FOR temp = 0 TO invennum
IF inven$(temp) = "Bottle with Rag" THEN temp2 = temp
IF inven$(temp) = "Gas" THEN inven$(temp) = "Molotov Cocktail"
NEXT
FOR temp = temp2 TO invennum
inven$(temp) = inven$(temp + 1)
NEXT
GOSUB endit

endit:
invennum = invennum - 1
inventory

seperateit:
IF selected$ = "bottlerag" THEN
invennum = invennum + 1
inven$(invennum) = "Oily Rag"
FOR temp = 1 TO invennum
IF inven$(temp) = "Bottle with Rag" THEN inven$(temp) = "Bottle"
NEXT
GOSUB endit2
END IF


IF selected$ = "bottlegas" THEN
invennum = invennum + 1
inven$(invennum) = "Gas"
FOR temp = 1 TO invennum
IF inven$(temp) = "Bottle with Gas" THEN inven$(temp) = "Bottle"
NEXT
GOSUB endit2
END IF



IF selected$ = "Molotov" THEN
invennum = invennum + 1
inven$(invennum) = "Gas"
invennum = invennum + 1
inven$(invennum) = "Oily Rag"
FOR temp = 1 TO invennum
IF inven$(temp) = "Molotov Cocktail" THEN inven$(temp) = "Bottle"
NEXT
GOSUB endit2
END IF

endit2:
updateinven
clearmenu 19, 2, 38: clearmenu 20, 2, 38:
inventory

END SUB

SUB darksequence
CLS
ask "Wow.  Sure is dark down here.", 19
delay 1
CLS
ask "I can't see a thing.", 19
delay .2
ask "How am I supposed to find the key?", 20
delay 2
CLS
ask "Hmmm....I think it's time I use", 19
ask "the magnet.", 20
delay 2
CLS
ask "*Swish*...*Swish*...*Swish*...*CLANK!*", 19
delay 1
CLS
ask "FOUND IT!  Wait a sec!  This is a fish!", 19
delay 1
CLS
ask "Ew...thats disgusting.  But where's", 19
ask "the key???", 20
delay 2
CLS
ask "I'm gettin' outta here.  This place", 19
ask "is too dark for my liking.", 20
delay 2
CLS
row = 11
col = 31
well = 1
invennum = invennum + 1
inven$(invennum) = "Fish"
loadmap 17
game

END SUB

SUB drawit
COLOR 9
LOCATE 22, 12: PRINT "LOST 2 V1.1 DEMO"
FOR a = 1 TO xx
FOR b = 1 TO yy

IF island = 1 THEN
IF map(curmap, a, b) = 5 THEN COLOR 10: LOCATE a, b: PRINT CHR$(6)
IF map(curmap, a, b) = 6 THEN COLOR 54: LOCATE a, b: PRINT CHR$(178)
IF map(curmap, a, b) <= 0 THEN COLOR 44: LOCATE a, b: PRINT CHR$(219)
IF map(curmap, a, b) = 3 THEN COLOR 7: LOCATE a, b: PRINT CHR$(177)
IF map(curmap, a, b) = 4 THEN COLOR 6: LOCATE a, b: PRINT CHR$(219)
IF map(curmap, a, b) = 7 THEN COLOR 25: LOCATE a, b: PRINT CHR$(1)
IF map(curmap, a, b) = 8 THEN COLOR 11: LOCATE a, b: PRINT CHR$(177)
IF map(curmap, a, b) = 9 THEN COLOR 8: LOCATE a, b: PRINT CHR$(219)
IF map(curmap, a, b) = 10 THEN COLOR 6: LOCATE a, b: PRINT CHR$(147)
IF map(curmap, a, b) = 11 THEN COLOR 6: LOCATE a, b: PRINT CHR$(254)
END IF
IF island = 2 THEN
IF map(curmap, a, b) = 5 THEN COLOR 2: LOCATE a, b: PRINT CHR$(5)
IF map(curmap, a, b) = 6 THEN COLOR 54: LOCATE a, b: PRINT CHR$(178)
IF map(curmap, a, b) <= 0 THEN COLOR 2: LOCATE a, b: PRINT CHR$(219)
IF map(curmap, a, b) = 3 THEN COLOR 7: LOCATE a, b: PRINT CHR$(177)
IF map(curmap, a, b) = 4 THEN COLOR 6: LOCATE a, b: PRINT CHR$(219)
IF map(curmap, a, b) = 7 THEN COLOR 25: LOCATE a, b: PRINT CHR$(1)
IF map(curmap, a, b) = 8 THEN COLOR 11: LOCATE a, b: PRINT CHR$(177)
IF map(curmap, a, b) = 9 THEN COLOR 8: LOCATE a, b: PRINT CHR$(219)
IF map(curmap, a, b) = 10 THEN COLOR 6: LOCATE a, b: PRINT CHR$(147)
IF map(curmap, a, b) = 11 THEN COLOR 6: LOCATE a, b: PRINT CHR$(254)
END IF

IF map(curmap, a, b) >= 30 AND map(curmap, a, b) <= 60 THEN COLOR 25: LOCATE a, b: PRINT CHR$(1)
IF map(curmap, a, b) >= 70 AND map(curmap, a, b) <= 90 THEN COLOR 11: LOCATE a, b: PRINT "?"

NEXT
NEXT
COLOR 7
LOCATE row, col: PRINT CHR$(2)
END SUB

SUB erasemaps
FOR a = 1 TO 18
FOR b = 1 TO xx
FOR c = 1 TO yy
map(a, b, c) = 0
NEXT
NEXT
NEXT
END SUB

SUB fightmon
FOR a = 0 TO 400
LINE (0, 0)-(a, a - 40), 195, B
FOR b = 1 TO 100: NEXT
NEXT
fadeout
CLS
PALETTE
COLOR 3
line$ = STRING$(40, "-")
FOR temp = 1 TO LEN(line$)
LOCATE 1, temp: PRINT MID$(line$, temp, 1)
FOR temp2 = 1 TO 100: NEXT
NEXT

COLOR 40

FOR temp = 1 TO 10
LOCATE temp + 13, 1: PRINT databox$(temp)
NEXT
FOR temp = 1 TO 50
PUT (temp, 50), ply3, PSET
FOR temp2 = 1 TO 500: NEXT
NEXT

COLOR 3: LOCATE 14, 7: PRINT "Gerfy"

enemychoice = INT(RND * 5) + 1
SELECT CASE enemychoice

CASE 1
PUT (220, 50), en1, PSET
enemytype$ = "Scorpion"
ewep$ = "Sting"
earm$ = "Shell"
speed = 6
power = 4
emoney = 2
ehp = 5
LOCATE 1, 13: PRINT "Oh No! A Scorpion"

CASE 2
PUT (220, 50), en2, PSET
enemytype$ = "Centipede"
ewep$ = "Feet"
earm$ = "Skin"
speed = 2
power = 5
emoney = 3
ehp = 10
LOCATE 1, 13: PRINT "Uh Oh! A Centipede"

CASE 3
PUT (220, 50), en3, PSET
enemytype$ = "Red Ant"
ewep$ = "Firesting"
earm$ = "ExoShield"
power = 7
speed = 4
emoney = 4
ehp = 6
LOCATE 1, 13: PRINT "Ahh! A Red ant!"

CASE 4
PUT (220, 50), en4, PSET
enemytype$ = "Muskrat"
ewep$ = "Rabies"
earm$ = "Fur"
power = 5
speed = 8
emoney = 5
ehp = 7
LOCATE 1, 13: PRINT "Yikes! A Muskrat!"

CASE 5
PUT (220, 50), en5, PSET
enemytype$ = "Spider"
ewep$ = "Sting"
earm$ = "Web"
power = 5
speed = 5
emoney = 4
ehp = 4
LOCATE 1, 13: PRINT "Holy Cow! A Spider!"
END SELECT
COLOR 40: LOCATE 14, 26: PRINT enemytype$
COLOR 40: LOCATE 15, 2: PRINT "HP:"
ehealcount = ehp
GOSUB updatehp

COLOR 40
LOCATE 16, 2: PRINT "Exp:": COLOR 2: LOCATE 16, 6: PRINT expe: COLOR 40
LOCATE 17, 2: PRINT "Cash:$": COLOR 2: LOCATE 17, 8: PRINT money: COLOR 40
LOCATE 18, 2: PRINT "Weapon:": COLOR 2: LOCATE 18, 10: PRINT weapon$: COLOR 40
LOCATE 19, 2: PRINT "Armor:": COLOR 2: LOCATE 19, 9: PRINT armor$: COLOR 40
COLOR 34
LOCATE 21, 2: PRINT "Attack"
LOCATE 22, 2: PRINT "Run"


menu2:
menuselect = 1
COLOR 34
LOCATE 20 + menuselect, 19: PRINT CHR$(17)
DO
select$ = INKEY$
IF RIGHT$(select$, 1) = "H" THEN
LOCATE 20 + menuselect, 19: PRINT " "
menuselect = menuselect - 1
IF menuselect = 0 THEN menuselect = 2
LOCATE 20 + menuselect, 19: PRINT CHR$(17)
END IF

IF RIGHT$(select$, 1) = "P" THEN
LOCATE 20 + menuselect, 19: PRINT " "
menuselect = menuselect + 1
IF menuselect = 3 THEN menuselect = 1
LOCATE 20 + menuselect, 19: PRINT CHR$(17)
END IF

IF RIGHT$(select$, 1) = CHR$(13) THEN EXIT DO

LOOP

IF menuselect = 1 THEN GOSUB attackmode
IF menuselect = 2 THEN GOSUB runchance
GOSUB enemyshot
GOSUB menu2

runchance:
chance = INT(RND * speed) + 1
IF chance = 1 THEN CLS : drawit: game
LOCATE 12, 2: PRINT "I can't get away!"
DO: LOOP UNTIL INKEY$ <> ""
LOCATE 22, 19: PRINT " "
menuselect = 1
LOCATE 12, 1: PRINT STRING$(40, " ")
RETURN

enemyshot:
ehit = INT(RND * power) + 1
ehit = ehit - armpower
IF ehit <= 0 THEN
COLOR 3
LOCATE 12, 2: PRINT "It's attack failed!"
DO: LOOP UNTIL INKEY$ <> ""
LOCATE 12, 1: PRINT STRING$(40, " ")
RETURN
END IF

hp = hp - ehit: GOSUB updatehp
COLOR 40
LOCATE 12, 2: PRINT "It hit me for"; ehit; "!"
waitforkey
LOCATE 12, 1: PRINT STRING$(40, " ")
IF hp <= 0 THEN dead
RETURN

attackmode:
hit = INT(RND * armpower) + 1
expepower = INT(RND * expe) + 1
hit = hit + expepower
IF hit <= 0 THEN
COLOR 44
LOCATE 12, 2: PRINT "My attack failed!"
DO: LOOP UNTIL INKEY$ <> ""
LOCATE 12, 1: PRINT STRING$(40, " ")
RETURN
END IF

ehp = ehp - hit: GOSUB updatehp
COLOR 2
LOCATE 12, 2: PRINT "I hit it for"; hit; "!"
waitforkey
LOCATE 12, 1: PRINT STRING$(40, " ")
IF ehp <= 0 THEN GOSUB edead
RETURN

edead:
LOCATE 12, 2: PRINT "I killed it!"
waitforkey
LOCATE 12, 1: PRINT STRING$(40, " ")
LOCATE 11, 2: PRINT "I got "; emoney; "and 1 experience"
LOCATE 12, 2: PRINT "for winning!"
waitforkey
money = money + emoney
expe = expe + 1
CLS
drawit
game


updatehp:
COLOR 1
SELECT CASE hp
CASE healcount
COLOR 1
CASE IS < healcount - (healcount / 2)
COLOR 4
CASE IS < healcount - (healcount / 4)
COLOR 44
END SELECT
LOCATE 15, 5: PRINT hp; "/"; healcount

COLOR 4
LOCATE 15, 22: PRINT "HP"; ehp; "/"; ehealcount
LOCATE 16, 22: PRINT "Money:"; emoney
LOCATE 17, 22: PRINT "Weapon:"; ewep$
LOCATE 18, 22: PRINT "Armor:"; earm$

RETURN
END SUB

SUB game
sweet:
enemy = 4
COLOR 7
persondelay = TIMER
LOCATE row, col: PRINT CHR$(2)
DO
DO
LOCATE 23, 1: PRINT row; col; map(curmap, row, col)
move$ = INKEY$
IF personon = 1 THEN
IF persondelay + .01 <= TIMER THEN
GOSUB wanks
COLOR 7: LOCATE row2, col2: PRINT CHR$(1)
persondelay = TIMER
END IF
END IF

LOOP UNTIL move$ <> ""


SELECT CASE RIGHT$(move$, 1)
CASE "H"
placement = up
IF map(curmap, row - 1, col) < 5 THEN
GOSUB detect
tileback
LET row = row - 1
IF row = 0 THEN LET row = row + 1
LOCATE row, col: PRINT CHR$(2)
END IF


CASE "M"
placement = right
IF map(curmap, row, col + 1) < 5 THEN
GOSUB detect
tileback
LET col = col + 1
IF col > 40 THEN LET col = col - 1
LOCATE row, col: PRINT CHR$(2)
END IF


CASE "P"
placement = down
IF map(curmap, row + 1, col) < 5 THEN
GOSUB detect
tileback
LET row = row + 1
IF row > 21 THEN LET row = row - 1
LOCATE row, col: PRINT CHR$(2)
END IF


CASE "K"
placement = left
IF map(curmap, row, col - 1) < 5 THEN
GOSUB detect
tileback
LET col = col - 1
IF col = 0 THEN LET col = col + 1
LOCATE row, col: PRINT CHR$(2)
END IF
END SELECT

IF move$ = "q" OR move$ = "Q" THEN
CLS
PRINT "Are you sure?"
DO
select$ = UCASE$(INKEY$)
SELECT CASE select$
CASE "Y"
EXIT DO
CASE "N"
drawit
GOSUB sweet
END SELECT
LOOP

CLOSE : credits
END IF
IF move$ = "l" OR move$ = "L" THEN look
IF move$ = "t" OR move$ = "T" THEN talkselect
IF move$ = "s" OR move$ = "S" THEN status
IF move$ = "g" OR move$ = "G" THEN getitem
IF move$ = "i" OR move$ = "I" THEN inventory
selectmove
LOOP

wanks:

SELECT CASE movement

CASE 1
IF map(curmap, row2, col2 - 1) = 0 THEN
IF row2 <> row AND col2 - 1 <> col THEN
map(curmap, row2, col2) = 0
IF island = 1 THEN COLOR 44
IF island = 2 THEN COLOR 2
LOCATE row2, col2: PRINT CHR$(219)
col2 = col2 - 1
END IF
END IF

CASE 2
IF map(curmap, row2, col2 + 1) = 0 THEN
IF row2 <> row AND col2 + 1 <> col THEN
map(curmap, row2, col2) = 0
IF island = 1 THEN COLOR 44
IF island = 2 THEN COLOR 2
LOCATE row2, col2: PRINT CHR$(219)
col2 = col2 + 1
END IF
END IF

CASE 3
IF map(curmap, row2 - 1, col2) = 0 THEN
IF row2 - 1 <> row AND col2 <> col THEN
map(curmap, row2, col2) = 0
IF island = 1 THEN COLOR 44
IF island = 2 THEN COLOR 2
LOCATE row2, col2: PRINT CHR$(219)
row2 = row2 - 1
END IF
END IF

CASE 4
IF map(curmap, row2 + 1, col2) = 0 THEN
IF row2 + 1 <> row AND col2 <> col THEN
map(curmap, row2, col2) = 0
IF island = 1 THEN COLOR 44
IF island = 2 THEN COLOR 2
LOCATE row2, col2: PRINT CHR$(219)
row2 = row2 + 1
END IF
END IF

END SELECT

SELECT CASE persontype$
CASE "guy"
map(curmap, row2, col2) = 30
CASE "healer"
map(curmap, row2, col2) = 32
CASE "elf1"
map(curmap, row2, col2) = 30
END SELECT
movefactor = movefactor + 1
IF movefactor = 10 THEN GOSUB changedir
RETURN

changedir:
movefactor = 0
move = INT(RND * 4) + 1
SELECT CASE move
CASE 1
movement = 1
IF map(curmap, row2, col2 - 1) > 0 THEN GOSUB changedir
CASE 2
movement = 2
IF map(curmap, row2, col2 + 1) > 0 THEN GOSUB changedir
CASE 3
movement = 3
IF map(curmap, row2 - 1, col2) > 0 THEN GOSUB changedir
CASE 4
movement = 4
IF map(curmap, row2 + 1, col2) > 0 THEN GOSUB changedir
END SELECT
RETURN

detect:
IF enem = 1 THEN
enemchance = INT(RND * 100) + 1
IF enemchance = 69 THEN fightmon
END IF
RETURN

END SUB

SUB getitem
IF placement = left THEN
IF map(curmap, row, col - 1) = 70 THEN gettheitem "bottle"
IF map(curmap, row, col - 1) = 71 THEN gettheitem "rag"
END IF
IF placement = up THEN
IF map(curmap, row - 1, col) = 70 THEN gettheitem "bottle"
IF map(curmap, row - 1, col) = 71 THEN gettheitem "rag"
END IF
IF placement = right THEN
IF map(curmap, row, col + 1) = 70 THEN gettheitem "bottle"
IF map(curmap, row, col + 1) = 71 THEN gettheitem "rag"
END IF
IF placement = down THEN
IF map(curmap, row + 1, col) = 70 THEN gettheitem "bottle"
IF map(curmap, row + 1, col) = 71 THEN gettheitem "rag"
END IF

scroll "You can't pick that up!", 23, 1, 4, "", 0, 0, 0
END SUB

SUB gettheitem (item$)

IF item$ = "bottle" THEN
points
scroll "You picked up a bottle!", 23, 1, 3, "", 0, 0, 0
invennum = invennum + 1
inven$(invennum) = "Bottle"
map(curmap, 6, 21) = 0
waitforkey
clearmenu 23, 1, 38
drawit
game
END IF

IF item$ = "rag" THEN
points
scroll "You picked up a rag!", 23, 1, 3, "", 0, 0, 0
invennum = invennum + 1
inven$(invennum) = "Rag"
map(curmap, 8, 37) = 0
waitforkey
clearmenu 23, 1, 38
drawit
game
END IF



END SUB

SUB heal
DO
select$ = INKEY$
IF select$ = "Y" OR select$ = "y" THEN GOSUB healme
IF select$ = "N" OR select$ = "n" THEN clearmenu 19, 2, 38: clearmenu 20, 2, 38: clearmenu 21, 2, 38: EXIT SUB
LOOP

healme:
LOCATE 21, 2: PRINT STRING$(38, " ")
IF money < 10 THEN GOSUB notenoughcash
IF hp = healcount THEN GOSUB max
money = money - 10
hp = healcount
ask "Ok,  there ya go.", 21
delay 1
LOCATE 21, 2: PRINT STRING$(38, " ")
LOCATE 20, 2: PRINT STRING$(38, " ")
EXIT SUB

notenoughcash:
LOCATE 21, 2: PRINT STRING$(38, " ")
reply "You don't have enough $$$!": delay 2
LOCATE 21, 2: PRINT STRING$(38, " ")
LOCATE 20, 2: PRINT STRING$(38, " ")
EXIT SUB

max:
LOCATE 21, 2: PRINT STRING$(38, " ")
reply "Your hitpoints are at maximum!": delay 2
LOCATE 21, 2: PRINT STRING$(38, " ")
LOCATE 20, 2: PRINT STRING$(38, " ")
EXIT SUB

END SUB

SUB highlight (num)
GOSUB clearlines
COLOR 11
IF num = 1 THEN LOCATE 14, 39: PRINT CHR$(17)
IF num = 2 THEN LOCATE 15, 39: PRINT CHR$(17)
IF num = 3 THEN LOCATE 16, 39: PRINT CHR$(17)
IF num = 4 THEN LOCATE 17, 39: PRINT CHR$(17)
selects = num
EXIT SUB
clearlines:
FOR temp = 14 TO 17
IF num <> (temp - 15) THEN LOCATE temp, 39: PRINT " "
NEXT
RETURN

END SUB

SUB intro
loadtiles
logo
DIM blank(400)
CLS
SCREEN 13
GET (0, 0)-(20, 20), blank
COLOR 15
LOCATE 5, 17: PRINT "LOST 1"
LOCATE 16, 9: PRINT "By Epsilon Entertainment"
delay 2
FOR a = 1 TO 100
PUT (a, 100), ply3, PSET
FOR b = 1 TO 100: NEXT b
NEXT
PUT (110, 100), dynam, PSET
FOR a = 100 TO 1 STEP -1
PUT (a, 100), ply2, PSET
FOR b = 1 TO 100: NEXT b
NEXT
PUT (1, 100), blank, PSET
delay 1
PSET (123, 105), 44
delay 1
PSET (123, 105), 0
PSET (122, 106), 44
delay 1
PSET (122, 106), 0
PSET (122, 107), 44
delay 1
CLS
PAINT (1, 1), 15
delay 1
fadeout
CLS
PALETTE
loadgif "lost.gif"
delay 2
END SUB

SUB invenselect (select$)
FOR a = 1 TO invennum
IF VAL(select$) = a THEN GOSUB selectinvenoption
NEXT
EXIT SUB

selectinvenoption:
IF inven$(VAL(select$)) = "Bottle" THEN
PUT (262, 70), bottle, PSET
selected$ = "bottle"
opoen = 1
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Magnet" THEN
PUT (262, 70), magnet, PSET
selected$ = "magnet"
opoen = 0
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Rag" THEN
PUT (262, 70), rag, PSET
selected$ = "rag"
opoen = 0
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Bottle with Rag" THEN
PUT (262, 70), bottle, PSET
selected$ = "bottlerag"
opoen = 1
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Bottle with Gas" THEN
PUT (262, 70), bottle, PSET
selected$ = "bottlegas"
opoen = 1
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF


IF inven$(VAL(select$)) = "Gas" THEN
PUT (262, 70), gas, PSET
selected$ = "gas"
opoen = 1
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Dynamite" THEN
PUT (262, 70), dynam, PSET
selected$ = "dynam"
opoen = 0
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Oily Rag" THEN
PUT (262, 70), rag, PSET
selected$ = "oily rag"
opoen = 0
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Molotov Cocktail" THEN
PUT (262, 70), bottle, PSET
selected$ = "Molotov"
opoen = 1
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Mug" THEN
PUT (262, 70), mug, PSET
selected$ = "mug"
opoen = 0
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Lighter" THEN
PUT (262, 70), light, PSET
selected$ = "lighter"
opoen = 0
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Mug of Water" THEN
PUT (262, 70), mug, PSET
selected$ = "mugowater"
opoen = 0
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Key" THEN
PUT (262, 70), obj5, PSET
selected$ = "key"
opoen = 0
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF

IF inven$(VAL(select$)) = "Fish" THEN
PUT (262, 70), obj11, PSET
selected$ = "fish"
opoen = 1
opexam = 1
opuse = 1
opusewith = 1
GOSUB menu
END IF


DO
select$ = INKEY$
IF select$ = "e" OR select$ = "E" AND opexam = 1 THEN examine selected$
IF select$ = "o" OR select$ = "O" AND opoen = 1 THEN opopn selected$
IF select$ = "c" OR select$ = "C" AND opusewith = 1 THEN combine selected$
IF select$ = "u" OR select$ = "U" AND opuse = 1 THEN use selected$
IF select$ = "d" OR select$ = "D" THEN inventory
IF select$ = CHR$(27) THEN drawit: game
LOOP

menu:
IF opexam = 0 THEN COLOR 8 ELSE COLOR 10
LOCATE 15, 2: PRINT "(E)xamine"
IF opoen = 0 THEN COLOR 8 ELSE COLOR 10
LOCATE 15, 14: PRINT "(O)pen"
IF opusewith = 0 THEN COLOR 8 ELSE COLOR 10
LOCATE 16, 2: PRINT "(C)ombine/Seperate"
IF opuse = 0 THEN COLOR 8 ELSE COLOR 10
LOCATE 17, 2: PRINT "(U)se item"
LOCATE 17, 13: PRINT "(D)ifferent item"
RETURN

END SUB

SUB inventory
COLOR 2
FOR a = 1 TO 22
LOCATE 0 + a, 1: PRINT invenbox$(a)
NEXT
updateinven
DO
select$ = INKEY$
IF select$ = CHR$(27) THEN drawit: game
IF select$ <> "" THEN invenselect select$
LOOP
END SUB

SUB loadmap (a2)
map(curmap, row2, col2) = 0
CLS
curmap = a2
al = a2
drawit
END SUB

SUB loadmaps (num%)
SELECT CASE num%
CASE 1
OPEN "island1.map" FOR INPUT AS #1
FOR temp = 1 TO 18
LINE INPUT #1, garbage$
FOR a = 1 TO xx
FOR b = 1 TO yy
INPUT #1, temp$: map(temp, a, b) = VAL(temp$)
NEXT
NEXT
LOCATE 7, 16 + temp: PRINT CHR$(15)
NEXT
CLOSE #1
CASE 2
erasemaps
OPEN "island2.map" FOR INPUT AS #1
FOR temp = 1 TO 4
LINE INPUT #1, garbage$
FOR a = 1 TO xx
FOR b = 1 TO yy
INPUT #1, temp$: map(temp, a, b) = VAL(temp$)
NEXT
NEXT
NEXT
CLOSE #1
END SELECT
END SUB

SUB loadtiles

'Items
DEF SEG = VARSEG(bottle(0)): BLOAD "im1.bsv", VARPTR(bottle(0))

DEF SEG = VARSEG(magnet(0)): BLOAD "im2.bsv", VARPTR(magnet(0))
DEF SEG = VARSEG(rag(0)): BLOAD "im3.bsv", VARPTR(rag(0))
DEF SEG = VARSEG(gas(0)): BLOAD "im4.bsv", VARPTR(gas(0))
DEF SEG = VARSEG(mug(0)): BLOAD "im5.bsv", VARPTR(mug(0))
DEF SEG = VARSEG(light(0)): BLOAD "im6.bsv", VARPTR(light(0))

'Gerfy
DEF SEG = VARSEG(ply1(0)): BLOAD "gph1.bsv", VARPTR(ply1(0))
DEF SEG = VARSEG(ply2(0)): BLOAD "gph2.bsv", VARPTR(ply2(0))
DEF SEG = VARSEG(ply3(0)): BLOAD "gph3.bsv", VARPTR(ply3(0))
DEF SEG = VARSEG(ply4(0)): BLOAD "gph4.bsv", VARPTR(ply4(0))

'Dynamite
DEF SEG = VARSEG(dynam(0)): BLOAD "gph6.bsv", VARPTR(dynam(0))

'Characters
DEF SEG = VARSEG(chr1(0)): BLOAD "chr1.bsv", VARPTR(chr1(0))
DEF SEG = VARSEG(chr2(0)): BLOAD "chr2.bsv", VARPTR(chr2(0))
DEF SEG = VARSEG(chr3(0)): BLOAD "chr3.bsv", VARPTR(chr3(0))

'Objects
DEF SEG = VARSEG(obj1(0)): BLOAD "obj1.bsv", VARPTR(obj1(0))
DEF SEG = VARSEG(obj2(0)): BLOAD "obj2.bsv", VARPTR(obj2(0))
DEF SEG = VARSEG(obj3(0)): BLOAD "obj3.bsv", VARPTR(obj3(0))
DEF SEG = VARSEG(obj4(0)): BLOAD "obj4.bsv", VARPTR(obj4(0))
DEF SEG = VARSEG(obj5(0)): BLOAD "obj5.bsv", VARPTR(obj5(0))
DEF SEG = VARSEG(obj6(0)): BLOAD "obj6.bsv", VARPTR(obj6(0))
DEF SEG = VARSEG(obj7(0)): BLOAD "obj7.bsv", VARPTR(obj7(0))
DEF SEG = VARSEG(obj8(0)): BLOAD "obj8.bsv", VARPTR(obj8(0))
DEF SEG = VARSEG(obj9(0)): BLOAD "obj9.bsv", VARPTR(obj9(0))
DEF SEG = VARSEG(obj10(0)): BLOAD "obj10.bsv", VARPTR(obj10(0))
DEF SEG = VARSEG(obj11(0)): BLOAD "obj11.bsv", VARPTR(obj11(0))

'Floors
DEF SEG = VARSEG(flr1(0)): BLOAD "flr1.bsv", VARPTR(flr1(0))
DEF SEG = VARSEG(flr2(0)): BLOAD "flr2.bsv", VARPTR(flr2(0))
DEF SEG = VARSEG(flr3(0)): BLOAD "flr3.bsv", VARPTR(flr3(0))

'Enemies
DEF SEG = VARSEG(en1(0)): BLOAD "en1.bsv", VARPTR(en1(0))
DEF SEG = VARSEG(en2(0)): BLOAD "en2.bsv", VARPTR(en2(0))
DEF SEG = VARSEG(en3(0)): BLOAD "en3.bsv", VARPTR(en3(0))
DEF SEG = VARSEG(en4(0)): BLOAD "en4.bsv", VARPTR(en4(0))
DEF SEG = VARSEG(en5(0)): BLOAD "en5.bsv", VARPTR(en5(0))



END SUB

SUB logo
loadgif "2.gif"
COLOR 5
LOCATE 21, 16: PRINT "Presents..."
delay (2)
CLS
PALETTE
END SUB

SUB look
SELECT CASE island
CASE 1
IF placement = left THEN
IF map(curmap, row, col - 1) <= 0 THEN text$ = "It's sand."
IF map(curmap, row, col - 1) = 3 THEN text$ = "It's a floor."
IF map(curmap, row, col - 1) = 4 THEN text$ = "It's a bridge."
IF map(curmap, row, col - 1) = 5 THEN text$ = "It's a cactus."
IF map(curmap, row, col - 1) = 6 THEN text$ = "It's water...in the desert??"
IF map(curmap, row, col - 1) >= 30 AND map(curmap, row, col - 1) <= 60 THEN text$ = "It's a person."
IF map(curmap, row, col - 1) = 8 THEN IF melt = 0 THEN text$ = "It's a bag of ice." ELSE text$ = "It's a bag of water."
IF map(curmap, row, col - 1) = 9 THEN text$ = "It's a wall."
IF map(curmap, row, col - 1) = 10 THEN text$ = "It's a well."
END IF
IF placement = up THEN
IF map(curmap, row - 1, col) <= 0 THEN text$ = "It's sand."
IF map(curmap, row - 1, col) = 3 THEN text$ = "It's a floor."
IF map(curmap, row - 1, col) = 4 THEN text$ = "It's a bridge."
IF map(curmap, row - 1, col) = 5 THEN text$ = "It's a cactus."
IF map(curmap, row - 1, col) = 6 THEN text$ = "It's water...in the desert??"
IF map(curmap, row - 1, col) >= 30 AND map(curmap, row - 1, col) <= 60 THEN text$ = "It's a person."
IF map(curmap, row - 1, col) = 8 THEN IF melt = 0 THEN text$ = "It's a bag of ice." ELSE text$ = "It's a bag of water."
IF map(curmap, row - 1, col) = 9 THEN text$ = "It's a wall."
IF map(curmap, row - 1, col) = 10 THEN text$ = "It's a well."
END IF
IF placement = right THEN
IF map(curmap, row, col + 1) <= 0 THEN text$ = "It's sand."
IF map(curmap, row, col + 1) = 3 THEN text$ = "It's a floor."
IF map(curmap, row, col + 1) = 4 THEN text$ = "It's a bridge."
IF map(curmap, row, col + 1) = 5 THEN text$ = "It's a cactus."
IF map(curmap, row, col + 1) = 6 THEN text$ = "It's water...in the desert??"
IF map(curmap, row, col + 1) >= 30 AND map(curmap, row, col + 1) <= 60 THEN text$ = "It's a person."
IF map(curmap, row, col + 1) = 8 THEN IF melt = 0 THEN text$ = "It's a bag of ice." ELSE text$ = "It's a bag of water."
IF map(curmap, row, col + 1) = 9 THEN text$ = "It's a wall."
IF map(curmap, row, col + 1) = 10 THEN text$ = "It's a well."
END IF
IF placement = down THEN
IF map(curmap, row + 1, col) <= 0 THEN text$ = "It's sand."
IF map(curmap, row + 1, col) = 3 THEN text$ = "It's a floor."
IF map(curmap, row + 1, col) = 4 THEN text$ = "It's a bridge."
IF map(curmap, row + 1, col) = 5 THEN text$ = "It's a cactus."
IF map(curmap, row + 1, col) = 6 THEN text$ = "It's water...in the desert??"
IF map(curmap, row + 1, col) >= 30 AND map(curmap, row + 1, col) <= 60 THEN text$ = "It's a person."
IF map(curmap, row + 1, col) = 8 THEN IF melt = 0 THEN text$ = "It's a bag of ice." ELSE text$ = "It's a bag of water."
IF map(curmap, row + 1, col) = 9 THEN text$ = "It's a wall."
IF map(curmap, row + 1, col) = 10 THEN text$ = "It's a well."
END IF

CASE 2
IF placement = left THEN
IF map(curmap, row, col - 1) <= 0 THEN text$ = "It's your basic grass."
IF map(curmap, row, col - 1) = 3 THEN text$ = "It's a floor."
IF map(curmap, row, col - 1) = 4 THEN text$ = "It's a bridge."
IF map(curmap, row, col - 1) = 5 THEN text$ = "It's a tree.  Yeehaa."
IF map(curmap, row, col - 1) = 6 THEN text$ = "Yep.  It's water."
IF map(curmap, row, col - 1) >= 30 AND map(curmap, row, col - 1) <= 60 THEN text$ = "It's a person."
IF map(curmap, row, col - 1) = 8 THEN IF melt = 0 THEN text$ = "It's a bag of ice." ELSE text$ = "It's a bag of water."
IF map(curmap, row, col - 1) = 9 THEN text$ = "It's a wall."
IF map(curmap, row, col - 1) = 10 THEN text$ = "It's a well."
END IF
IF placement = up THEN
IF map(curmap, row - 1, col) <= 0 THEN text$ = "It's your basic grass."
IF map(curmap, row - 1, col) = 3 THEN text$ = "It's a floor."
IF map(curmap, row - 1, col) = 4 THEN text$ = "It's a bridge."
IF map(curmap, row - 1, col) = 5 THEN text$ = "It's a tree.  Yeehaa."
IF map(curmap, row - 1, col) = 6 THEN text$ = "Yep.  It's water."
IF map(curmap, row - 1, col) >= 30 AND map(curmap, row - 1, col) <= 60 THEN text$ = "It's a person."
IF map(curmap, row - 1, col) = 8 THEN IF melt = 0 THEN text$ = "It's a bag of ice." ELSE text$ = "It's a bag of water."
IF map(curmap, row - 1, col) = 9 THEN text$ = "It's a wall."
IF map(curmap, row - 1, col) = 10 THEN text$ = "It's a well."
END IF
IF placement = right THEN
IF map(curmap, row, col + 1) <= 0 THEN text$ = "It's your basic grass."
IF map(curmap, row, col + 1) = 3 THEN text$ = "It's a floor."
IF map(curmap, row, col + 1) = 4 THEN text$ = "It's a bridge."
IF map(curmap, row, col + 1) = 5 THEN text$ = "It's a tree.  Yeehaa."
IF map(curmap, row, col + 1) = 6 THEN text$ = "Yep.  It's water."
IF map(curmap, row, col + 1) >= 30 AND map(curmap, row, col + 1) <= 60 THEN text$ = "It's a person."
IF map(curmap, row, col + 1) = 8 THEN IF melt = 0 THEN text$ = "It's a bag of ice." ELSE text$ = "It's a bag of water."
IF map(curmap, row, col + 1) = 9 THEN text$ = "It's a wall."
IF map(curmap, row, col + 1) = 10 THEN text$ = "It's a well."
END IF
IF placement = down THEN
IF map(curmap, row + 1, col) <= 0 THEN text$ = "It's your basic grass."
IF map(curmap, row + 1, col) = 3 THEN text$ = "It's a floor."
IF map(curmap, row + 1, col) = 4 THEN text$ = "It's a bridge."
IF map(curmap, row + 1, col) = 5 THEN text$ = "It's a tree.  Yeehaa."
IF map(curmap, row + 1, col) = 6 THEN text$ = "Yep.  It's water."
IF map(curmap, row + 1, col) >= 30 AND map(curmap, row + 1, col) <= 60 THEN text$ = "It's a person."
IF map(curmap, row + 1, col) = 8 THEN IF melt = 0 THEN text$ = "It's a bag of ice." ELSE text$ = "It's a bag of water."
IF map(curmap, row + 1, col) = 9 THEN text$ = "It's a wall."
IF map(curmap, row + 1, col) = 10 THEN text$ = "It's a well."
END IF
END SELECT
scroll text$, 23, 1, 3, "", 0, 0, 0
waitforkey
clearmenu 23, 1, 38

                                                      
END SUB

SUB movie (num%)
DIM blank(400)
CLS
GET (0, 0)-(19, 19), blank
IF num% = 1 THEN
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr1, PSET
NEXT
PUT (20, 130), chr1, PSET
PUT (70, 130), obj1, PSET
delay 1
FOR a = 300 TO 90 STEP -2
PUT (a, 130), ply2, PSET
FOR temp = 1 TO 500: NEXT
NEXT
text$ = "Hey there.  Want this rag"
loca = 1
GOSUB gerfsword
text$ = "to clean your hands?"
loca = 2
GOSUB gerfsword
delay 1
GOSUB cleartext
text$ = "Sure!  Thanks!"
locb = 1
GOSUB otherguys
delay 1
GOSUB cleartext
text$ = "Here.  You can have this old"
locb = 1
GOSUB otherguys
text$ = "oily rag."
locb = 2
GOSUB otherguys
FOR temp = 0 TO invennum
IF inven$(temp) = "Rag" THEN inven$(temp) = "Oily Rag"
NEXT
hands = 1
delay 1
CLS
drawit
game
END IF

IF num% = 2 THEN
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr2, PSET
NEXT
PUT (20, 130), chr1, PSET
delay 1
FOR a = 300 TO 90 STEP -2
PUT (a, 130), ply2, PSET
FOR temp = 1 TO 500: NEXT
NEXT
text$ = "Hey there Pete.  Interested in"
loca = 1
GOSUB gerfsword
text$ = "this stick of dynamite?"
loca = 2
GOSUB gerfsword
delay 1
GOSUB cleartext
text$ = "Wow!  Cool!"
locb = 1
GOSUB otherguys
delay 1
GOSUB cleartext
text$ = "Here.  Take this lighter."
locb = 1
GOSUB otherguys
text$ = "I couldn't get it to work."
locb = 2
GOSUB otherguys

FOR temp = 1 TO invennum
IF inven$(temp) = "Dynamite" THEN inven$(temp) = "Lighter"
NEXT

delay 1

FOR a = 90 TO 200 STEP 2
PUT (a, 130), ply3, PSET
FOR temp = 1 TO 500: NEXT
NEXT
GOSUB cleartext
PUT (40, 130), dynam, PSET
text$ = "YEEEEEEEHAAAA!!!"
GOSUB otherguys
delay 1
FOR temp = 1 TO 100
CIRCLE (50, 150), temp, 44
FOR temp2 = 1 TO 500: NEXT
NEXT
FOR temp = 1 TO 100
CIRCLE (50, 150), temp, 0
FOR temp2 = 1 TO 500: NEXT
NEXT
FOR a = 200 TO 300 STEP 2
PUT (a, 130), ply3, PSET
FOR temp = 1 TO 500: NEXT
NEXT
boom = 1
CLS
drawit
game
END IF

IF num% = 3 THEN
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr3, PSET
NEXT
PUT (40, 130), chr1, PSET
PUT (20, 130), obj2, PSET
delay 1
FOR a = 300 TO 220 STEP -2
PUT (a, 130), ply2, PSET
FOR temp = 1 TO 500: NEXT
NEXT
text$ = "Take this!"
loca = 1
GOSUB gerfsword
delay 1
GOSUB cleartext
xtemp = 200
ytemp = 130
value = 1
DO UNTIL ytemp = 140
PUT (xtemp, ytemp), bottle, PSET
IF value = 1 THEN ytemp = ytemp - 1
IF value = 2 THEN ytemp = ytemp + 1
xtemp = xtemp - 1
FOR temp = 1 TO 500: NEXT
IF ytemp = 40 THEN value = 2
LOOP

FOR temp = 1 TO 100
CIRCLE (50, 150), temp, 44
FOR temp2 = 1 TO 500: NEXT
NEXT
FOR temp = 1 TO 100
CIRCLE (50, 150), temp, 0
FOR temp2 = 1 TO 500: NEXT
NEXT

delay 1
CLS
COLOR 3
LOCATE 15, 15: PRINT "Minutes Later..."
delay 2
CLS
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr3, PSET
NEXT

PUT (40, 130), chr1, PSET
PUT (20, 130), obj3, PSET
PUT (220, 130), ply2, PSET
delay 1
text$ = "You idiot!  You melted my ice!"
locb = 1
GOSUB otherguys
text$ = "It's worthless now!"
locb = 2
GOSUB otherguys
delay 1
GOSUB cleartext
text$ = "Sorry,  it was an accident."
loca = 1
GOSUB gerfsword
delay 1
GOSUB cleartext
text$ = "Oh,  well then it's ok."
locb = 1
GOSUB otherguys
text$ = "As long as it wasn't on purpose."
locb = 2
GOSUB otherguys
delay 1
CLS
FOR temp = 0 TO invennum
IF inven$(temp) = "Molotov Cocktail" THEN temp2 = temp
NEXT
FOR temp = temp2 TO invennum
inven$(temp) = inven$(temp + 1)
NEXT
invennum = invennum - 1
melt = 1
drawit
game
END IF

IF num% = 4 THEN
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr3, PSET
NEXT
PUT (20, 130), chr1, PSET
PUT (70, 130), obj3, PSET
delay 1
FOR a = 300 TO 90 STEP -2
PUT (a, 130), ply2, PSET
FOR temp = 1 TO 500: NEXT
NEXT
text$ = "Can I take some water?"
loca = 1
GOSUB gerfsword
delay 1
GOSUB cleartext
text$ = "Sure.  I don't want it."
locb = 1
GOSUB otherguys
delay 1
GOSUB cleartext
text$ = "But be warned.  I fear it might"
locb = 1
GOSUB otherguys
text$ = "have magical properties."
locb = 2
GOSUB otherguys
delay 1
GOSUB cleartext
FOR temp = 0 TO invennum
IF inven$(temp) = "Mug" THEN inven$(temp) = "Mug of Water"
NEXT
CLS
drawit
game
END IF

IF num% = 5 THEN
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr3, PSET
NEXT
PUT (20, 130), obj6, PSET
PUT (290, 130), chr2, PSET
delay 1
FOR a = 10 TO 240 STEP 2
PUT (a, 130), ply3, PSET
PUT (20, 130), obj6, PSET
FOR temp = 1 TO 500: NEXT
NEXT
text$ = "Hey there.  Thirsty?"
locb = 1
GOSUB otherguys
delay 1
GOSUB cleartext
text$ = "Am I ever!  Thanks a lot!"
loca = 1
GOSUB gerfsword
delay 1
GOSUB cleartext
text$ = "Here ya go."
locb = 1
GOSUB otherguys
delay 1
PUT (270, 130), mug, PSET
delay 1
PUT (270, 130), blank, PSET
GOSUB cleartext
text$ = "Whoa...what was in that?..."
loca = 1
GOSUB gerfsword
delay 1
GOSUB cleartext
PUT (290, 130), chr3, PSET
delay 1
text$ = "Muah hahaha! The magic water worked!"
locb = 1
GOSUB otherguys
delay 1
GOSUB cleartext
FOR a = 240 TO 270 STEP 2
PUT (a, 130), ply3, PSET
FOR temp = 1 TO 500: NEXT
NEXT
PUT (290, 130), obj4, PSET
PUT (290, 120), obj5, PSET
LOCATE 12, 1: PRINT "               *CRUNCH*"
delay 1
FOR a = 270 TO 240 STEP -2
PUT (a, 130), ply3, PSET
FOR temp = 1 TO 500: NEXT
NEXT
GOSUB cleartext
delay 1
text$ = "Hey!  A key!"
locb = 1
GOSUB otherguys
delay 1
FOR a = 240 TO 270 STEP 2
PUT (a, 130), ply3, PSET
FOR temp = 1 TO 500: NEXT
NEXT
PUT (290, 120), blank, PSET
delay 1
invennum = invennum + 1
inven$(invennum) = "Key"
map(17, 11, 38) = 0
FOR temp = 0 TO invennum
IF inven$(temp) = "Mug of Water" THEN temp2 = temp
NEXT
FOR temp = temp2 TO invennum
inven$(temp) = inven$(temp + 1)
NEXT
invennum = invennum - 1
CLS
loadmap 17
drawit
game
END IF

IF num% = 6 THEN
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr3, PSET
NEXT
FOR a = 0 TO 120 STEP 20
PUT (300, a), obj7, PSET
NEXT
PUT (300, 130), obj8, PSET
FOR a = 10 TO 280 STEP 2
PUT (a, 130), ply3, PSET
PUT (20, 130), obj6, PSET
FOR temp = 1 TO 500: NEXT
NEXT
text$ = "Time to use the key."
locb = 1
GOSUB otherguys
delay 1
PUT (300, 130), blank, PSET
delay 1
FOR temp = 12 TO 14
LOCATE temp, 1: PRINT STRING$(30, " ")
NEXT
text$ = "HEY!!  It was unlocked!"
locb = 1
GOSUB otherguys
delay 1
text$ = "All that hard work for nothing!"
locb = 2
GOSUB otherguys
delay 1
FOR temp = 12 TO 14
LOCATE temp, 1: PRINT STRING$(31, " ")
NEXT
PUT (280, 130), ply2, PSET
text$ = "STUPID KEY!"
locb = 1
GOSUB otherguys
delay 1
FOR temp = 12 TO 14
LOCATE temp, 1: PRINT STRING$(31, " ")
NEXT

xtemp = 260
ytemp = 100
value = 1
DO UNTIL ytemp = 110
PUT (xtemp, ytemp), obj5, PSET
IF value = 1 THEN ytemp = ytemp - 1
IF value = 2 THEN ytemp = ytemp + 1
xtemp = xtemp - 1.7
FOR temp = 1 TO 500: NEXT
IF ytemp = 40 THEN value = 2
LOOP
PUT (xtemp, ytemp), blank, PSET
COLOR 1
LOCATE 12, 1: PRINT "CLANK!"
delay 1
LOCATE 12, 1: PRINT "       "
PUT (280, 130), ply3, PSET
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr3, PSET
NEXT
c = 280
FOR b = 280 TO 0 STEP -20
WAIT &H3DA, 8
FOR a = 0 TO 120 STEP 20
PUT (b, a), obj7, PSET
PUT (b + 20, a), blank, PSET
PUT (b, 130), blank, PSET
PUT (c, 130), ply3, PSET
FOR temp2 = 1 TO 900: NEXT
NEXT
c = c - 3
NEXT
FOR a = 0 TO 120 STEP 20
PUT (0, a), blank, PSET
NEXT
PUT (280, 130), obj9, PSET
text$ = "It's a locked treasure chest..."
locb = 1
GOSUB otherguys
delay 1
PUT (c, 130), ply1, PSET
GOSUB cleartext
text$ = "NOOOOOO!!!!!!!!!"
locb = 1
GOSUB otherguys
delay 1
GOSUB cleartext
text$ = "I better go get the key...*grumble*..."
locb = 1
GOSUB otherguys
delay 1
CLS
LOCATE 12, 10: PRINT "Minutes later..."
delay 2
CLS
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr3, PSET
NEXT
PUT (220, 130), ply2, PSET
PUT (200, 130), obj6, PSET
text$ = "Ok,  here we go."
loca = 1
GOSUB gerfsword
delay 1
GOSUB cleartext
FOR a = 130 TO 90 STEP -2
PUT (220, a), ply2, PSET
delay .001
NEXT
b = 220: a = 90
DO UNTIL a = 110
a = a + 1
b = b - 1
PUT (b, a), ply2, PSET
delay .001
LOOP
PUT (b, a), blank, PSET
text$ = "YEEHAAAA!"
loca = 1
GOSUB gerfsword
delay 1
CLS
FOR temp = 0 TO invennum
IF inven$(temp) = "Key" THEN temp2 = temp
NEXT
FOR temp = temp2 TO invennum
inven$(temp) = inven$(temp + 1)
NEXT
invennum = invennum - 1
darksequence
END IF

IF num% = 7 THEN
FOR a = 0 TO 300 STEP 20
PUT (a, 150), flr1, PSET
NEXT
PUT (160, 130), ply3, PSET
PUT (180, 130), obj9, PSET
delay 1
text$ = "Here goes nothing."
locb = 1
GOSUB otherguys
delay 1
GOSUB cleartext
delay 1
PUT (180, 130), obj10, PSET
text$ = "Hey!  Another magic rock!"
locb = 1
GOSUB otherguys
delay 1
GOSUB cleartext
text$ = "*RUB RUB*"
locb = 1
GOSUB otherguys
delay 1
endofdemo
END
END IF




gerfsword:
COLOR 3
FOR temp = 1 TO LEN(text$)
LOCATE 12 + loca, temp + 10: PRINT MID$(text$, temp, 1)
delay .001
NEXT
RETURN

otherguys:
COLOR 2
FOR temp = 1 TO LEN(text$)
LOCATE 12 + locb, temp: PRINT MID$(text$, temp, 1)
delay .001
NEXT
RETURN

cleartext:
FOR temp = 12 TO 14
LOCATE temp, 1: PRINT STRING$(40, " ")
NEXT
RETURN

END SUB

SUB opopn (selected$)
clearmenu 19, 2, 38
clearmenu 20, 2, 38
IF selected$ = "bottle" OR selected$ = "bottlerag" OR selected$ = "bottlegas" OR selected$ = "Molotov" THEN
IF magnet = 0 THEN
ask "Hey!  There's a magnet inside!", 19
invennum = invennum + 1
inven$(invennum) = "Magnet"
magnet = 1
updateinven
EXIT SUB
END IF
END IF
IF selected$ = "gas" THEN
ask "No...I don't want to empty the glass.", 19
END IF


IF selected$ = "fish" AND ikey = 0 THEN
ask "Hey!  It must have swallowed the key!", 19
ask "*GRAB*,  *PULL*,  *POP!*", 20
invennum = invennum + 1
inven$(invennum) = "Key"
ikey = 1
updateinven
EXIT SUB
END IF

ask "There isn't anything inside.", 19

END SUB

SUB person (num%)
SELECT CASE num%

CASE 1
persontype$ = "healer"
personon = 1
row2 = 12
col2 = 12
CASE 2
persontype$ = "guy"
personon = 1
row2 = 5
col2 = 5
CASE 3
persontype$ = "elf1"
personon = 1
row2 = 2
col2 = 3
END SELECT

END SUB

SUB playervars
hp = 10
healcount = 10
weapon$ = "Dagger"
armor$ = "Rags"
expe = 0
money = 25
weppower = 1
armpower = 1
col = 29
row = 10
invennum = 0
OPEN "data.dat" FOR INPUT AS #1: INPUT #1, temp$: enem = VAL(temp$): CLOSE #1
menuselect = 1
RESTORE starts
READ xx, yy

END SUB

SUB points
sbplay "$1AaBC%cDdEFfGgA$aa"
END SUB

SUB refresh (maptype$)

IF maptype$ = "map1" THEN RESTORE map1: curmap = 0
IF maptype$ = "map2" THEN RESTORE map2: curmap = 0
IF maptype$ = "map3" THEN RESTORE map3: curmap = 0
IF maptype$ = "map4" THEN RESTORE map4: curmap = 0
IF maptype$ = "map5" THEN RESTORE map5: curmap = 0
al = 0
CLS
FOR a = 1 TO xx
FOR b = 1 TO yy
READ map(curmap, a, b)
NEXT
NEXT
drawit
END SUB

SUB reply (text$)
COLOR 2
FOR temp = 1 TO LEN(text$)
letr$ = MID$(text$, temp, 1)
LOCATE 21, 1 + temp: PRINT letr$
delay .001
NEXT
COLOR 7
END SUB

SUB selectmove
SELECT CASE island
CASE 1
IF map(curmap, row, col) = -1 THEN col = 39: personon = 0: location$ = "": refresh "map2"
IF map(curmap, row, col) = -2 THEN col = 2: personon = 0: location$ = "": refresh "map1"
IF map(curmap, row, col) = -3 THEN col = 39: personon = 0: location$ = "": refresh "map3"
IF map(curmap, row, col) = -4 THEN col = 2: personon = 0: location$ = "": refresh "map2"
IF map(curmap, row, col) = -5 THEN col = 39: person 2: location$ = "": refresh "map4"
IF map(curmap, row, col) = -6 THEN col = 2: personon = 0: location$ = "": refresh "map3"
IF map(curmap, row, col) = -7 THEN col = 39: personon = 0: location$ = "": refresh "map5"
IF map(curmap, row, col) = -8 THEN
location$ = ""
row = 20
loadmap 7
END IF
IF map(curmap, row, col) = -9 THEN col = 2: person 2: refresh "map4"

IF map(curmap, row, col) = -10 THEN
location$ = ""
row = 2
personon = 0
loadmap 1
END IF

IF map(curmap, row, col) = -11 THEN row = 20: location$ = "": refresh "map5"

IF map(curmap, row, col) = -12 THEN
location$ = ""
col = 39
row = row - 1
personon = 0
loadmap 2
END IF

IF map(curmap, row, col) = -13 THEN
location$ = ""
col = 2
row = row + 1
personon = 0
loadmap 1
END IF

IF map(curmap, row, col) = -14 THEN
location$ = ""
col = 39
location$ = "ice"
personon = 0
loadmap 3
END IF

IF map(curmap, row, col) = -15 THEN
location$ = ""
col = 2
personon = 0
loadmap 2
END IF

IF map(curmap, row, col) = -16 THEN
location$ = ""
row = 2
person 1
loadmap 4
END IF

IF map(curmap, row, col) = -17 THEN
location$ = ""
col = 39
personon = 0
loadmap 5
END IF

IF map(curmap, row, col) = -18 THEN
location$ = ""
row = 20
location$ = "ice"
personon = 0
loadmap 3
END IF

IF map(curmap, row, col) = -19 THEN
location$ = ""
col = 2
location$ = "ice"
personon = 0
loadmap 3
END IF

IF map(curmap, row, col) = -20 THEN
location$ = ""
col = 39
personon = 0
loadmap 6
END IF

IF map(curmap, row, col) = -21 THEN
location$ = ""
col = 2
personon = 0
loadmap 5
END IF

IF map(curmap, row, col) = -22 THEN
location$ = ""
row = 2
personon = 0
refresh "map5"
END IF

IF map(curmap, row, col) = -23 THEN
location$ = ""
row = 20
person 1
loadmap 8
END IF

IF map(curmap, row, col) = -24 THEN
location$ = ""
row = 2
personon = 0
loadmap 7
END IF

IF map(curmap, row, col) = -25 THEN
location$ = ""
col = 2
personon = 0
loadmap 9
END IF

IF map(curmap, row, col) = -26 THEN
location$ = ""
col = 39
person 1
loadmap 8
END IF

IF map(curmap, row, col) = -27 THEN
location$ = ""
col = 2
personon = 0
loadmap 10
END IF

IF map(curmap, row, col) = -28 THEN
location$ = ""
col = 39
personon = 0
loadmap 9
END IF

IF map(curmap, row, col) = -29 THEN
location$ = ""
row = 20
personon = 0
loadmap 11
END IF

IF map(curmap, row, col) = -30 THEN
col = 2
personon = 0
location$ = "store"
loadmap 15
END IF

IF map(curmap, row, col) = -31 THEN
location$ = ""
row = 2
personon = 0
loadmap 10
END IF

IF map(curmap, row, col) = -32 THEN
location$ = ""
row = 20
personon = 0
loadmap 12
END IF

IF map(curmap, row, col) = -33 THEN
location$ = ""
row = 2
personon = 0
loadmap 11
END IF

IF map(curmap, row, col) = -34 THEN
location$ = ""
row = 20
personon = 0
loadmap 13
END IF

IF map(curmap, row, col) = -35 THEN
location$ = ""
row = 2
personon = 0
loadmap 12
END IF

IF map(curmap, row, col) = -36 THEN
location$ = "pyro"
row = 20: col = 21
personon = 0
loadmap 14
END IF

IF map(curmap, row, col) = -37 THEN
location$ = ""
row = 7: col = 18
personon = 0
loadmap 13
END IF

IF map(curmap, row, col) = -38 THEN
location$ = ""
col = 39
personon = 0
loadmap 10
END IF

IF map(curmap, row, col) = -39 THEN
location$ = ""
col = 2
personon = 0
loadmap 16
END IF

IF map(curmap, row, col) = -40 THEN
location$ = ""
col = 39
personon = 0
loadmap 15
END IF

IF map(curmap, row, col) = -41 THEN
location$ = "guard"
col = 2
personon = 0
loadmap 17
END IF

IF map(curmap, row, col) = -42 THEN
location$ = ""
col = 39
personon = 0
loadmap 16
END IF

IF map(curmap, row, col) = -43 THEN
location$ = ""
IF well = 0 THEN
movie 6
END IF
IF well = 1 THEN location$ = "chest": personon = 0: col = 2: loadmap 18
END IF

IF map(curmap, row, col) = -44 THEN
personon = 0
location$ = ""
col = 39
loadmap 17
END IF

CASE 2

IF map(curmap, row, col) = -1 THEN
personon = 0
location$ = ""
col = 2
loadmap 2
END IF

IF map(curmap, row, col) = -2 THEN
personon = 0
location$ = ""
col = 39
loadmap 1
END IF

IF map(curmap, row, col) = -3 THEN
location$ = ""
person 3
col = 2
loadmap 3
END IF

IF map(curmap, row, col) = -4 THEN
personon = 0
location$ = ""
col = 39
loadmap 2
END IF

IF map(curmap, row, col) = -5 THEN
personon = 0
location$ = ""
col = 2
loadmap 4
END IF

IF map(curmap, row, col) = -6 THEN
person 3
location$ = ""
col = 39
loadmap 3
END IF


END SELECT
END SUB

SUB status
COLOR 55
FOR temp = 1 TO 9
LOCATE 3 + temp, 6: PRINT statsbox$(temp)
NEXT
COLOR 10
LOCATE 7, 7: PRINT "Weapon:", weapon$
LOCATE 8, 7: PRINT "Armor:", armor$
LOCATE 9, 7: PRINT "Money:$", money
LOCATE 10, 7: PRINT "Experience:"; expe
LOCATE 11, 7: PRINT "Hitpoints:"; hp; "/"; healcount; ""
COLOR 7
DO: LOOP UNTIL INKEY$ <> ""
drawit
END SUB

SUB store
mouseinit
mousectrl 1
retrn:
COLOR 3
FOR temp = 1 TO 14
LOCATE temp, 1: PRINT storebox$(temp)
NEXT
COLOR 2
LOCATE 10, 2: PRINT "You have: $"; money
LOCATE 4, 2: PRINT "Gasoline:  $10"
LOCATE 5, 2: PRINT "Souvenir Mug:  $5"
LOCATE 6, 2: PRINT "Dynamite:  $10"
LOCATE 7, 2: PRINT "Exit"
GOSUB hl
mousectrl 2
DO
mouseupdate
select$ = INKEY$
FOR temp = 24 TO 30
IF used <> 1 THEN
IF mousedx% = temp THEN menuselect = 1: used = 1: GOSUB hl
END IF
NEXT
FOR temp = 32 TO 38
IF used <> 2 THEN
IF mousedx% = temp THEN menuselect = 2: used = 2: GOSUB hl
END IF
NEXT
FOR temp = 40 TO 46
IF used <> 3 THEN
IF mousedx% = temp THEN menuselect = 3: used = 3: GOSUB hl
END IF
NEXT
FOR temp = 48 TO 54
IF used <> 4 THEN
IF mousedx% = temp THEN menuselect = 4: used = 4: GOSUB hl
END IF
NEXT

IF lb% = -1 THEN EXIT DO
LOOP
mousectrl 1
ask "Are you sure?(Y/N)", 12
DO
select$ = UCASE$(INKEY$)
IF select$ = "Y" THEN EXIT DO
IF select$ = "N" THEN GOSUB retrn
LOOP
GOSUB cleartheline
IF menuselect = 1 THEN

        IF money < 10 THEN
        ask "You don't have enough", 12
        ask "money!", 13
        delay 2
        GOSUB retrn
        END IF

        IF gas = 1 THEN
        ask "You already got all the", 12
        ask "Gas you need.", 13
        delay 3
        GOSUB retrn
        END IF
       
GOSUB cleartheline
ask "Nice doin' business", 12
ask "with ya.", 13
gas = 1
money = money - 10
invennum = invennum + 1
inven$(invennum) = "Gas"
delay 2
GOSUB retrn
END IF

IF menuselect = 2 THEN

        IF money < 5 THEN
        ask "You don't have enough", 12
        ask "money!", 13
        delay 2
        GOSUB retrn
        END IF

        IF mug = 1 THEN
        ask "You already got the", 12
        ask "mug.", 13
        delay 3
        GOSUB retrn
        END IF
      
GOSUB cleartheline
ask "Nice doin' business", 12
ask "with ya.", 13
mug = 1
money = money - 5
invennum = invennum + 1
inven$(invennum) = "Mug"
delay 2
GOSUB retrn
END IF

IF menuselect = 3 THEN

        IF money < 10 THEN
        ask "You don't have enough", 12
        ask "money!", 13
        delay 2
        GOSUB retrn
        END IF

        IF dyn = 1 THEN
        ask "You already got all the", 12
        ask "Dynamite you need.", 13
        delay 3
        GOSUB retrn
        END IF
      
GOSUB cleartheline
ask "Nice doin' business", 12
ask "with ya.", 13
dyn = 1
money = money - 10
invennum = invennum + 1
inven$(invennum) = "Dynamite"
delay 2
GOSUB retrn
END IF

IF menuselect = 4 THEN talk5


hl:
COLOR 4
FOR temp = 1 TO 4
LOCATE temp + 3, 27: PRINT " "
NEXT
LOCATE menuselect + 3, 27: PRINT CHR$(17)
RETURN

cleartheline:
LOCATE 12, 2: PRINT STRING$(18, " ")
LOCATE 13, 2: PRINT STRING$(18, " ")
RETURN

END SUB

SUB systemsetup
CLS
COLOR 1: LOCATE 1, 1: PRINT STRING$(80, CHR$(219))
COLOR 14, 1: LOCATE 1, 16: PRINT "Lost 2 Demo V1.1 by Groovy Concepts LTD Setup"
COLOR 7, 0
LOCATE 2, 1
PRINT "Setting up system..."
PRINT
PRINT "Loading Player Variables..."; : playervars
PRINT
PRINT "Loading tiles...": loadtiles:
PRINT "Setting up soundcard...":  sbsetup
PRINT "Loading maps...["; SPC(mapnums); "]": loadmaps 1
PRINT
COLOR 3
PRINT "Your system is ready.  Let's go!"
delay 1
fadeout
END SUB

SUB talk1
box
COLOR 4
task "BILL BAXTER:  Desert Dweller", 6, "", 0
task "Hey,  whats up?", 12, "", 0
task "What are you doing out here?", 14, "", 0
task "Where am I?", 15, "", 0
task "Do you know The Ancient?", 16, "", 0
task "See ya.", 17, "", 0
COLOR 4
LOCATE 18, 2: PRINT STRING$(38, "-")
talka:
selecty
click
mousectrl 1
IF selects = 1 THEN task "I'm trying to get a tan.", 19, "", 0
IF selects = 2 THEN task "This is Vanderin Island.", 19, "", 0
IF selects = 3 THEN task "Oh,  the old guy?  Yeah,  I saw the", 19, "Enforcer dragging him westward.", 20
IF selects = 4 THEN task "Catch ya later.", 19, "", 0: delay 2: CLS : drawit: game
mousectrl 2
GOSUB talka
END SUB

SUB talk2
box
task "FLIPPY GIPPER:  Desert Salesman", 6, "", 0
IF melt = 0 THEN
task "Howdy friend.  I sell ice!", 12, "", 0
task "What's new?", 14, "", 0
task "Can I buy some ice?", 15, "", 0
task "How do I get out of here?", 16, "", 0
task "See ya.", 17, "", 0
END IF
IF melt = 1 THEN
task "Hey there.  Want some water?", 12, "", 0
task "What's up?", 14, "", 0
task "Whats with the water?", 15, "", 0
task "How do I get out of here?", 16, "", 0
task "See ya.", 17, "", 0
END IF

COLOR 4
LOCATE 18, 2: PRINT STRING$(38, "-")
talkb:
selecty
click
mousectrl 1
IF melt = 0 THEN
IF selects = 1 THEN task "Nothing.  It's all the same.", 19, "", 0
IF selects = 2 THEN task "It'll cost you $600,000,000.", 19, "", 0: reply "Hmmm...I don't think so."
IF selects = 3 THEN task "If I knew,  would I be out here?", 19, "", 0
IF selects = 4 THEN task "Ok.", 19, "", 0: waitforkey: CLS : drawit: game
END IF
IF melt = 1 THEN
IF selects = 1 THEN task "Nothing anymore.  My business", 19, "is ruined.", 20
IF selects = 2 THEN task "I don't know.  Looks magical, ", 19, "does it not?", 20
IF selects = 3 THEN task "If I knew,  would I be out here?", 19, "", 0
IF selects = 4 THEN task "Ok.", 19, "", 0: waitforkey: CLS : drawit: game
END IF
mousectrl 2
GOSUB talkb
END SUB

SUB talk3
box
task "MYSTIC HEALER:  What more can I say?", 6, "", 0
task "I am a Mystic Healer.  Howdy.", 12, "", 0
task "Can you heal me?", 14, "", 0
task "How can I get out of here?", 15, "", 0
task "How come healing costs so much?", 16, "", 0
task "Bye.", 17, "", 0
COLOR 4
LOCATE 18, 2: PRINT STRING$(38, "-")
talkc:
selecty
click
mousectrl 1
IF selects = 1 THEN task "Sure.  It'll cost ya $10, though.", 19, "", 0: reply "Hmmm...(Y/N)": heal
IF selects = 2 THEN task "Why do you want to leave?", 19, "Is the game that bad?  =)", 20
IF selects = 3 THEN task "I'm putting my kids through college.", 19, "", 0
IF selects = 4 THEN task "Bye!", 19, "", 0: waitforkey: CLS : drawit: game
mousectrl 2
GOSUB talkc
END SUB

SUB talk4
box
task "Pete the Pyro:  Local Arsonist.", 6, "", 0
task "Hey,  buddy.  Got any matches?", 12, "", 0

task "Why do you play with fire?", 14, "", 0
task "What are you doing?", 15, "", 0
task "Why are you in this airplane?", 16, "", 0
task "See ya.", 17, "", 0
COLOR 4
LOCATE 18, 2: PRINT STRING$(38, "-")
talkd:
selecty
click
mousectrl 1
IF selects = 1 THEN task "Because I like watching stuff go boom.", 19, "", 0
IF selects = 2 THEN
IF boom = 0 THEN
task "I'm trying to get this cigarette", 19, "lighter to work.", 20
END IF
IF boom = 1 THEN
task "I'm trying to clean off all this", 19, "soot.", 20
END IF
END IF

IF selects = 3 THEN task "For the shade,  man.  It's hot out", 19, "there!", 20
IF selects = 4 THEN task "Ok.", 19, "", 0: waitforkey: CLS : drawit: game
mousectrl 2
GOSUB talkd

END SUB

SUB talk5
box
task "Salesman Bob:  Store Owner", 6, "", 0
task "Hi.  Can I help you?", 12, "", 0
task "Yes,  I'd like to buy something.", 14, "", 0
task "Why do you keep wringing your hands?", 15, "", 0
task "How did you start a business here?", 16, "", 0
task "Catch ya later.", 17, "", 0
COLOR 4
LOCATE 18, 2: PRINT STRING$(38, "-")
talke:
selecty
click
mousectrl 1
IF selects = 1 THEN task "Sure...see anything you like?", 19, "", 0: delay 1: store
IF selects = 2 THEN
IF hands = 0 THEN
task "They're dirty and this stupid rag", 19, "is too greasy to clean them!", 20
END IF
IF hands = 1 THEN
task "I'm not,  thanks to you!", 19, "", 0
END IF
END IF
IF selects = 3 THEN task "I found this empty store in my", 19, "travels and decided to stay.", 20
IF selects = 4 THEN task "Sure.", 19, "", 0: waitforkey: CLS : drawit: game
mousectrl 2
GOSUB talke
END SUB

SUB talk6
box
task "Guardian Fred:  Guard", 6, "", 0
task "Who goes there?", 12, "", 0
task "Er..whatcha guardin'?", 14, "", 0
task "You look tired...", 15, "", 0
task "Can I go by?", 16, "", 0
task "Er...see ya.", 17, "", 0
COLOR 4
LOCATE 18, 2: PRINT STRING$(38, "-")
talkf:
selecty
click
IF selects = 1 THEN task "I am guarding the treasure of", 19, "Vanderin Island.", 20
IF selects = 2 THEN task "What I really am is thirsty,  and", 19, "that stupid well is bone dry.", 20
IF selects = 3 THEN task "Nope.  I have my orders.  No one", 19, "is to go by.", 20
IF selects = 4 THEN task "Sir.", 19, "", 0: waitforkey: CLS : drawit: game
GOSUB talkf

END SUB

SUB talk7
box
COLOR 4
task "Phil the Elf", 6, "", 0
task "Hello,  friend.", 12, "", 0
task "Who are you?", 14, "", 0
task "Where am I?", 15, "", 0
task "Do you know The Ancient?", 16, "", 0
task "Later.", 17, "", 0
COLOR 4
LOCATE 18, 2: PRINT STRING$(38, "-")
talkg:

selecty
click
mousectrl 1
IF selects = 1 THEN task "I'm Phil the Elf.  I'm an Elf.", 19, "", 0
IF selects = 2 THEN task "This is Ghan'dor Island,  man.", 19, "", 0
IF selects = 3 THEN task "The old man?  Some big guy was", 19, "carrying him northward.", 20
IF selects = 4 THEN task "Go in peace,  dude.", 19, "", 0: delay 2: CLS : drawit: game
mousectrl 2
GOSUB talkg

END SUB

SUB talkselect
SELECT CASE island
CASE 1
IF placement = left THEN
IF map(curmap, row, col - 1) = 30 THEN talk1
IF map(curmap, row, col - 1) = 31 THEN talk2
IF map(curmap, row, col - 1) = 32 THEN talk3
IF map(curmap, row, col - 1) = 33 THEN talk3
IF map(curmap, row, col - 1) = 34 THEN talk4
IF map(curmap, row, col - 1) = 35 THEN talk5
IF map(curmap, row, col - 1) = 36 THEN talk6
END IF
IF placement = up THEN
IF map(curmap, row - 1, col) = 30 THEN talk1
IF map(curmap, row - 1, col) = 31 THEN talk2
IF map(curmap, row - 1, col) = 32 THEN talk3
IF map(curmap, row - 1, col) = 33 THEN talk3
IF map(curmap, row - 1, col) = 34 THEN talk4
IF map(curmap, row - 1, col) = 35 THEN talk5
IF map(curmap, row - 1, col) = 36 THEN talk6
END IF
IF placement = right THEN
IF map(curmap, row, col + 1) = 30 THEN talk1
IF map(curmap, row, col + 1) = 31 THEN talk2
IF map(curmap, row, col + 1) = 32 THEN talk3
IF map(curmap, row, col + 1) = 33 THEN talk3
IF map(curmap, row, col + 1) = 34 THEN talk4
IF map(curmap, row, col + 1) = 35 THEN talk5
IF map(curmap, row, col + 1) = 36 THEN talk6
END IF
IF placement = down THEN
IF map(curmap, row + 1, col) = 30 THEN talk1
IF map(curmap, row + 1, col) = 31 THEN talk2
IF map(curmap, row + 1, col) = 32 THEN talk3
IF map(curmap, row + 1, col) = 33 THEN talk3
IF map(curmap, row + 1, col) = 34 THEN talk4
IF map(curmap, row + 1, col) = 35 THEN talk5
IF map(curmap, row + 1, col) = 36 THEN talk6
END IF

CASE 2
IF placement = left THEN
IF map(curmap, row, col - 1) = 30 THEN talk7
END IF
IF placement = up THEN
IF map(curmap, row - 1, col) = 30 THEN talk7
END IF
IF placement = right THEN
IF map(curmap, row, col + 1) = 30 THEN talk7
END IF
IF placement = down THEN
IF map(curmap, row + 1, col) = 30 THEN talk7
END IF
END SELECT
scroll "You can't talk to that!", 23, 1, 4, "", 0, 0, 0
waitforkey
clearmenu 23, 1, 38

END SUB

SUB task (quest$, PLA, quest2$, pla2)
LOCATE 19, 2: PRINT STRING$(38, " ")
LOCATE 20, 2: PRINT STRING$(38, " ")
LOCATE 21, 2: PRINT STRING$(38, " ")
COLOR 34
FOR temp = 1 TO LEN(quest$)
letr$ = MID$(quest$, temp, 1)
LOCATE PLA, 1 + temp: PRINT letr$
FOR temp2 = 1 TO 1000: NEXT
NEXT
IF quest2$ <> "" THEN
FOR temp = 1 TO LEN(quest2$)
letr$ = MID$(quest2$, temp, 1)
LOCATE pla2, 1 + temp: PRINT letr$
FOR temp2 = 1 TO 1000: NEXT
NEXT
END IF
COLOR 7
END SUB

SUB tileback
IF island = 1 THEN
IF map(curmap, row, col) = 0 THEN LOCATE row, col: COLOR 44: PRINT CHR$(219)
IF map(curmap, row, col) = 4 THEN LOCATE row, col: COLOR 6: PRINT CHR$(219)
IF map(curmap, row, col) = 3 THEN LOCATE row, col: COLOR 7: PRINT CHR$(177)
END IF
IF island = 2 THEN
IF map(curmap, row, col) = 0 THEN LOCATE row, col: COLOR 2: PRINT CHR$(219)
IF map(curmap, row, col) = 4 THEN LOCATE row, col: COLOR 6: PRINT CHR$(219)
IF map(curmap, row, col) = 3 THEN LOCATE row, col: COLOR 7: PRINT CHR$(177)
END IF

COLOR 7
END SUB

SUB updateinven
COLOR 1
FOR temp = 1 TO invennum
LOCATE 3 + temp, 2: PRINT "("; temp; ")"; inven$(temp)
NEXT

END SUB

SUB use (selected$)
IF selected$ = "rag" AND location$ = "store" THEN points:  movie 1
IF selected$ = "dynam" AND location$ = "pyro" THEN points: movie 2
IF selected$ = "key" AND location$ = "chest" THEN points: movie 7

IF selected$ = "Molotov" THEN
IF location$ = "ice" THEN
IF magnet = 0 THEN
ask "I'd better take out the magnet.", 19
invennum = invennum + 1
inven$(invennum) = "Magnet"
magnet = 1
delay 1
END IF
FOR temp = 0 TO invennum
IF inven$(temp) = "Lighter" THEN points: movie 3
NEXT
clearmenu 19, 2, 38: clearmenu 20, 2, 38
ask "I can't light it.", 19
END IF
END IF

IF selected$ = "mug" THEN
IF location$ = "ice" THEN
IF melt = 1 THEN
points
movie 4
END IF
END IF
END IF

IF selected$ = "mugowater" THEN
IF location$ = "guard" THEN
points
movie 5
END IF
END IF

clearmenu 19, 2, 38: clearmenu 20, 2, 38
ask "You can't use that here!", 19

END SUB

SUB vars
databox$(1) = "|--------------------------------------|"
databox$(2) = "|                  |                   |"
databox$(3) = "|                  |                   |"
databox$(4) = "|                  |                   |"
databox$(5) = "|                  |                   |"
databox$(6) = "|                  |                   |"
databox$(7) = "|==================|                   |"
databox$(8) = "|                  |                   |"
databox$(9) = "|                  |                   |"
databox$(10) = "|--------------------------------------|"

statsbox$(1) = "|---------------------------|"
statsbox$(2) = "|          Status:          |"
statsbox$(3) = "|---------------------------|"
statsbox$(4) = "|                           |"
statsbox$(5) = "|                           |"
statsbox$(6) = "|                           |"
statsbox$(7) = "|                           |"
statsbox$(8) = "|                           |"
statsbox$(9) = "|---------------------------|"

invenbox$(1) = "|---------------------------|----------|"
invenbox$(2) = "| Press ESC to leave menu   |          |"
invenbox$(3) = "|---------------------------|          |"
invenbox$(4) = "|                           |          |"
invenbox$(5) = "|                           |          |"
invenbox$(6) = "|                           |          |"
invenbox$(7) = "|                           |          |"
invenbox$(8) = "|                           |          |"
invenbox$(9) = "|                           |          |"
invenbox$(10) = "|                           |          |"
invenbox$(11) = "|                           |          |"
invenbox$(12) = "|                           |          |"
invenbox$(13) = "|                           |          |"
invenbox$(14) = "|---------------------------|          |"
invenbox$(15) = "|                           |          |"
invenbox$(16) = "|                           |          |"
invenbox$(17) = "|                           |          |"
invenbox$(18) = "|---------------------------|----------|"
invenbox$(19) = "|                                      |"
invenbox$(20) = "|                                      |"
invenbox$(21) = "|--------------------------------------|"

storebox$(1) = "|---------------------------|"
storebox$(2) = "|What would you like to buy?|"
storebox$(3) = "|---------------------------|"
storebox$(4) = "|                        |  |"
storebox$(5) = "|                        |  |"
storebox$(6) = "|                        |  |"
storebox$(7) = "|                        |  |"
storebox$(8) = "|                        |  |"
storebox$(9) = "|---------------------------|"
storebox$(10) = "|                           |"
storebox$(11) = "|---------------------------|"
storebox$(12) = "|                           |"
storebox$(13) = "|                           |"
storebox$(14) = "|---------------------------|"

mapnums = 18
CHDIR "c:\qb45\lost2\"
systemsetup
'delay 1
SCREEN 13
playervars
youarehere 2
loadmap 1
game
END
intro
options
youarehere 1
refresh "map1"
END SUB

SUB waitforkey
DO: LOOP UNTIL INKEY$ <> ""
END SUB

SUB youarehere (num%)
loadgif "isles.gif"
SELECT CASE num%
CASE 1
LOCATE 17, 10: PRINT "You Are Here"
LINE (150, 135)-(90, 180), 4
island = 1
CASE 2
LOCATE 17, 10: PRINT "You Are Here"
LINE (150, 135)-(145, 175), 4
row = 6: col = 5
island = 2
loadmaps 2
END SELECT
delay 3
fadeout
CLS
PALETTE
EXIT SUB

END SUB

