:- dynamic(person/7).
:- dynamic(married/2).




person('Murat Aslan',1940,none,m,none,none,children([])).
person('Sedanur Aslan',1942,none,f,none,none,children([])).
married('Murat Aslan','Sedanur Aslan').




% Simetrik ilişki tanımı yapılır
marriage(X,Y) :- married(X,Y).
marriage(X,Y) :- married(Y,X).
%person(isim,birth,death,gender,father,mother,children)
anne(X,Y):-person(Y,_,_,_,_,X,_),person(X,_,_,f,_,_,_).
baba(X,Y):-person(Y,_,_,_,X,_,_),person(X,_,_,m,_,_,_).
ogul(X,Y):-
    (anne(Y,X);
    baba(Y,X)),
    person(X,_,_,m,_,_,_).
kiz(X,Y):-
    (anne(Y,X);
    baba(Y,X)),
    person(X,_,_,f,_,_,_).
erkek_kardes(X,Y):-
    person(X,YR1,_,m,F,M,_),
    person(Y,YR2,_,_,F,M,_),
    (F\=none ,M\=none),
    YR1>YR2,
    X\=Y.
kiz_kardes(X,Y):-
    person(X,YR1,_,f,F,M,_),
    person(Y,YR2,_,_,F,M,_),
    (F\=none ,M\=none),
    YR1>YR2,
    X\=Y.
abi(X,Y):-
    person(X,YR1,_,m,F,M,_),
    person(Y,YR2,_,_,F,M,_),
    (F\=none ,M\=none),
    YR1<YR2,
    X\=Y.
abla(X,Y):-
    person(X,YR1,_,f,F,M,_),
    person(Y,YR2,_,_,F,M,_),
    (F\=none ,M\=none),
    YR1<YR2,
    X\=Y.
amca(X,Y):-
    person(Y,_,_,_,F,_,_),
    (erkek_kardes(F,X); abi(F,X)).
hala(X,Y):-
    person(Y,_,_,_,F,_,_),
    (kiz_kardes(F,X); abla(F,X)).
dayi(X,Y):-
    person(Y,_,_,_,_,M,_),
    (erkek_kardes(M,X); abi(M,X)).
teyze(X,Y):-
    person(Y,_,_,_,_,M,_),
    (kiz_kardes(M,X); abla(M,X)).
eniste(X,Y):-
    (kiz_kardes(Z,Y);abla(Z,Y);teyze(Z,Y);hala(Z,Y)),
    marriage(X,Z).
yenge(X,Y):-
    (erkek_kardes(Z,Y);abi(Z,Y);amca(Z,Y);dayi(Z,Y)),
    marriage(Z,X).
yegen(X,Y):-    %x y'nin yeğeni
    (erkek_kardes(Y,Z);kiz_kardes(Y,Z);abi(Y,Z);abla(Y,Z)),
    (ogul(X,Z);kiz(X,Z)).
kuzen(X,Y):-
    (amca(Z,Y);dayi(Z,Y);hala(Z,Y);teyze(Z,Y)),
    (ogul(X,Z);kiz(X,Z)).
kayinvalide(X,Y):- %X YNİN KAYINVALİDESİ
    anne(X,Z),
    marriage(Z,Y).
kayinpeder(X,Y):- %X YNİN KAYINVALİDESİ
    baba(X,Z),
    marriage(Z,Y).
gelin(X,Y):-
    marriage(X,Z),
    ogul(Z,Y).
damat(X,Y):-
    kiz(Z,Y),
    marriage(X,Z).
bacanak(X,Y):-
     X\=Y,
     marriage(X,Z),
     (kiz_kardes(Z,K);abla(Z,K)),
     person(K,_,_,f,_,_,_),
     marriage(Y,K).
baldiz(X,Y):-  %x y nin baldızı
     (kiz_kardes(K,X);abla(K,X)),
     marriage(Y,K),
     person(X,_,_,f,_,_,_).
elti(X,Y):-
     X\=Y,
     (abi(K,L);erkek_kardes(K,L)),
     (abi(L,K);erkek_kardes(L,K)),
     marriage(K,X),
     marriage(L,Y).
kayinbirader(X,Y):-
     marriage(Y,K),
     (kiz_kardes(K,X);abla(K,X)),
     person(X,_,_,m,_,_,_).
anneanne(X,Y):-
     anne(K,Y),
     anne(X,K).
babaanne(X,Y):-
     baba(K,Y),
     anne(X,K).
dede(X,Y):-
     (anne(K,Y);baba(K,Y)),
     baba(X,K).
ata(X,Y):-
     (anneanne(K,Y);babaanne(K,Y);dede(K,X)),
     (anne(X,K);baba(X,K)).








start :-
    main_menu.


main_menu :-
    nl,
    writeln('Main Menu:'),
    writeln('1. Ask relation'),
    writeln('2. Add/update person'),
    writeln('3. Get information of any person'),
    writeln('4. Print the family tree'),
    writeln('5. Add marriage'),
    writeln('6. Terminate program'),
    nl,
    write('Please choose an operation (1-6): '),
    read(Choice),
    process_choice(Choice).




process_choice(1) :- ask_relation, main_menu.
process_choice(2) :- add_update_person, main_menu.
process_choice(3) :- get_information, main_menu.
process_choice(4) :- print_tree, main_menu.
process_choice(5) :- add_marriage.
process_choice(6) :- writeln('Exiting FTWA. Goodbye!').
process_choice(_) :- writeln('Invalid choice. Please try again.'),main_menu.


ask_relation:-
    writeln('Please type the first person s name and surname'),
    read(Name1),
    writeln('Please type the second person s name and surname'),
    read(Name2),
    relation(Name1,Name2,Relation),
    writeln(Relation).


relation(Name1, Name2, Relation) :-
    ( anne(Name1, Name2)        -> Relation = 'anne'
    ; baba(Name1, Name2)        -> Relation = 'baba'
    ; ogul(Name1, Name2)        -> Relation = 'ogul'
    ; kiz(Name1, Name2)         -> Relation = 'kiz'
    ; abi(Name1, Name2)         -> Relation = 'abi'
    ; abla(Name1, Name2)        -> Relation = 'abla'
    ; erkek_kardes(Name1, Name2)-> Relation = 'erkek kardes'
    ; kiz_kardes(Name1, Name2)  -> Relation = 'kiz kardes'
    ; amca(Name1, Name2)        -> Relation = 'amca'
    ; hala(Name1, Name2)        -> Relation = 'hala'
    ; dayi(Name1, Name2)        -> Relation = 'dayi'
    ; teyze(Name1, Name2)       -> Relation = 'teyze'
    ; yegen(Name1, Name2)       -> Relation = 'yegen'
    ; kuzen(Name1, Name2)       -> Relation = 'kuzen'
    ; gelin(Name1, Name2)       -> Relation = 'gelin'
    ; damat(Name1, Name2)       -> Relation = 'damat'
    ; eniste(Name1, Name2)      -> Relation = 'eniste'
    ; yenge(Name1, Name2)       -> Relation = 'yenge'
    ; bacanak(Name1, Name2)     -> Relation = 'bacanak'
    ; baldiz(Name1, Name2)      -> Relation = 'baldiz'
    ; kayinvalide(Name1, Name2) -> Relation = 'kayinvalide'
    ; kayinpeder(Name1, Name2)  -> Relation = 'kayinpeder'
    ; marriage(Name1, Name2)    -> Relation = 'kari-koca'
    ; Relation = 'No known relationship found between the given names.'
    ).








add_update_person:-
    writeln('1-) Add person'),
    writeln('2-) Update person'),
    writeln('Please choose an option:'),
    read(Choice),
    (Choice = 1 -> add_person;
     Choice = 2 -> update_person).
add_person :-
    takeFatherName(Father),
    takeMotherName(Mother),








    % Anne ve babanın evli olup olmadığını kontrol eder
    ( marriage(Father, Mother) ->
        true
    ;
        writeln('Error: Mother and father are not married.'),
        !, fail
    ),


    writeln('Please enter the name and surname of the person:'),
    read(Name),
    takeBirthYear(Mother, Father, BirthYear),
    writeln('Please enter the death year (or none if alive):'),
    read(DeathYear),
    checkForDeathYear(DeathYear, BirthYear, ValidDeathYear),
    writeln('Please enter the gender (male/female):'),
    read(Gender),
    assertz(person(Name, BirthYear, ValidDeathYear, Gender, Father, Mother, children([]))),
    
    % Baba varsa, çocuk listesi güncellenir
    ( person(Father, BY, DY, G, Ff, Fm, children(OldChildren)) ->
        retract(person(Father, BY, DY, G, Ff, Fm, children(OldChildren))),
        append(OldChildren, [Name], NewChildren),
        assertz(person(Father, BY, DY, G, Ff, Fm, children(NewChildren)))
    ; true ),


    % Anne varsa, çocuk listesi güncellenir
    ( person(Mother, BY2, DY2, G2, Mf, Mm, children(OldChildren2)) ->
        retract(person(Mother, BY2, DY2, G2, Mf, Mm, children(OldChildren2))),
        append(OldChildren2, [Name], NewChildren2),
        assertz(person(Mother, BY2, DY2, G2, Mf, Mm, children(NewChildren2)))
    ; true ),


    writeln('Person added successfully.').




takeFatherName(Father) :-
    writeln('Please enter the father name and surname:'),
    read(Father),
    (   person(Father, _, _, m, _, _, _) -> true
    ;   writeln('Father not found. Please add the father first.'),
        main_menu, fail).


takeMotherName(Mother) :-
    writeln('Please enter the mother name and surname:'),
    read(Mother),
    (   person(Mother, _, _, f, _, _, _) -> true
    ;   writeln('Mother not found. Please add the mother first.'),
        main_menu, fail).


takeBirthYear(Mother, Father, BirthYear) :-
    writeln('Please enter the birth year:'),
    read(TempBirthYear),
    checkForMother(Mother, Father, TempBirthYear, BirthYear).
%The checkForFather function is called from within the checkForMother function. So we only called checkForMother here




% checkForMother and checkForFather check if the birth year is valid with respect to the mother's and father's birth and death years.
checkForMother(Mother, Father, TempBirthYear, ValidBirthYear) :-
    ( integer(TempBirthYear) ->
        ( person(Mother, MotherBirthYear, MotherDeathYear, f, _, _, _) ->
            ( integer(MotherDeathYear),
              TempBirthYear > MotherDeathYear
            -> writeln('Birth year cannot be after the mother\'s death year. Try again.'),
               takeBirthYear(Mother, Father, ValidBirthYear)
            ; integer(MotherBirthYear),
              TempBirthYear < MotherBirthYear
            -> writeln('Birth year cannot be before the mother\'s birth year. Try again.'),
               takeBirthYear(Mother, Father, ValidBirthYear)
            ; checkForFather(Mother, Father, TempBirthYear, ValidBirthYear)
            )
        ; writeln('Mother not found in records.'),
          fail
        )
    ; writeln('Invalid birth year. Please enter a number.'),
      takeBirthYear(Mother, Father, ValidBirthYear)
    ).


checkForFather(Mother, Father, TempBirthYear, ValidBirthYear) :-
    ( integer(TempBirthYear) ->
        ( person(Father, FatherBirthYear, FatherDeathYear, m, _, _, _) ->
            ( integer(FatherDeathYear),
              TempBirthYear > FatherDeathYear
            -> writeln('Birth year cannot be after the father\'s death year. Try again.'),
               takeBirthYear(Mother, Father, ValidBirthYear)
            ; integer(FatherBirthYear),
              TempBirthYear < FatherBirthYear
            -> writeln('Birth year cannot be before the father\'s birth year. Try again.'),
               takeBirthYear(Mother, Father, ValidBirthYear)
            ; ValidBirthYear = TempBirthYear
            )
        ; writeln('Father not found in records.'),
          fail
        )
    ; writeln('Invalid birth year. Please enter a number.'),
      takeBirthYear(Mother, Father, ValidBirthYear)
    ).




checkForDeathYear(DeathYear, BirthYear, ValidDeathYear) :-
    ( DeathYear = none -> ValidDeathYear = none
    ; integer(DeathYear) ->
        ( integer(BirthYear),
          DeathYear < BirthYear
        -> writeln('Death year cannot be before the birth year. Try again.'),
           read(NewDeathYear),
           checkForDeathYear(NewDeathYear, BirthYear, ValidDeathYear)
        ; ValidDeathYear = DeathYear
        )
    ; writeln('Invalid death year. Please enter a number or "none".'),
      read(NewDeathYear),
      checkForDeathYear(NewDeathYear, BirthYear, ValidDeathYear)
    ).


update_person :-
writeln('1- Update the birth year'),
writeln('2- Update the death year'),
writeln('3- Cancel'),
writeln('Please choose an option:'),
read(Choice),
(Choice = 1 -> update_birth_year;
 Choice = 2 -> update_death_year;
 Choice = 3 -> writeln('Update cancelled.')).




update_birth_year :-
    writeln('Please enter the name and surname of the person:'),
    read(Name),
    ( person(Name, BirthYear, DeathYear, Gender, Father, Mother, children(Children)) ->
        writeln('Please enter the new birth year:'),
        read(NewBirthYear),
        ( integer(NewBirthYear) ->
            ( DeathYear = none ; NewBirthYear < DeathYear ) ->
                ( checkParents(Mother, Father, NewBirthYear, ValidBirthYear) ->
                    ( checkChildren(Name, ValidBirthYear) ->
                        retract(person(Name, BirthYear, DeathYear, Gender, Father, Mother, children(Children))),
                        assertz(person(Name, ValidBirthYear, DeathYear, Gender, Father, Mother, children(Children))),
                        format("Birth year for ~w has been updated to ~w.~n", [Name, ValidBirthYear])
                    ;
                        writeln('Error: New birth year is not valid with respect to children.')
                    )
                ;
                    writeln('Birth year update is invalid, cancelled because it does not match parent information.')
                )
            ;
                writeln('Error: New birth year must be less than death year.')
        ;
            writeln('Error: Please enter a valid integer for birth year.')
        )
    ;
        writeln('Error: Person not found, cannot update.')
    ).




% Anne ve baba varsa:
%   - Çocuk doğumu, ebeveynin doğumundan sonra olmalı
%   - Ebeveynin ölümünden önce olmalı (ölüm yılı varsa)
checkParents(Mother, Father, NewBirthYear, NewBirthYear) :-
    check_parent(Mother, NewBirthYear),
    check_parent(Father, NewBirthYear), !.


checkParents(_, _, _, _) :-
    writeln('Error: New birth year is not valid with respect to parent(s).'),
    fail.


% Parent none ise geçerli kabul edilir. Değilse kurallar kontrol edilir.
check_parent(none, _) :- !.
check_parent(Parent, ChildBirthYear) :-
    person(Parent, ParentBirth, ParentDeath, _, _, _, _),
    ChildBirthYear >= ParentBirth,
    ( ParentDeath == none ; ChildBirthYear =< ParentDeath ).


% Kişinin çocuklarının doğum tarihlerini kontrol eder.
checkChildren(Person, NewBirthYear) :-
    person(Person, _, _, _, _, _, children(Children)),
    check_children_birth_years(Children, NewBirthYear).


check_children_birth_years([], _).
check_children_birth_years([Child|Rest], NewBirthYear) :-
    person(Child, ChildBirth, _, _, _, _, _),
    NewBirthYear =< ChildBirth,
    check_children_birth_years(Rest, NewBirthYear).




update_death_year :-
    writeln('Please enter the name and surname of the person:'),
    read(Name),
    (person(Name, BirthYear, DeathYear, Gender, Father, Mother, children(Children)) ->
        writeln('Please enter the new death year (or none if alive):'),
        read(NewDeathYear),
        (NewDeathYear = none ->
            retract(person(Name, BirthYear, DeathYear, Gender, Father, Mother, children(Children))),
            assertz(person(Name, BirthYear, none, Gender, Father, Mother, children(Children))),
            writeln('Death year updated to "none" (alive).')
        ;
        integer(NewDeathYear) ->
            (NewDeathYear > BirthYear ->
                (checkDeathAfterChildren(Name, NewDeathYear) ->
                    retract(person(Name, BirthYear, DeathYear, Gender, Father, Mother, children(Children))),
                    assertz(person(Name, BirthYear, NewDeathYear, Gender, Father, Mother, children(Children))),
                    format('Death year for ~w has been updated to ~w.~n', [Name, NewDeathYear])
                ;
                    writeln('Error: Death year must be after all children\'s birth years.')
                )
            ;
                writeln('Error: Death year must be after birth year.')
            )
        ;
            writeln('Invalid death year. Please enter a number or "none".')
        )
    ;
        writeln('Error: Person not found, cannot update.')
    ).




% Ölüm yılı tüm çocukların doğum yılından sonra olmalı
checkDeathAfterChildren(Person, NewDeathYear) :-
    person(Person, _, _, _, _, _, children(Children)),
    check_death(Children, NewDeathYear).


check_death([], _).
check_death([Child|Rest], NewDeathYear) :-
    person(Child, ChildBirth, _, _, _, _, _),
    NewDeathYear >= ChildBirth,
    check_death(Rest, NewDeathYear).




% Helper to check if a person is alive
is_alive(Name) :-
    person(Name, _, Death, _, _, _, _),
    (Death = 'none' ; (number(Death), Death > 2025)).


add_marriage:-
    writeln('Name of first person'),
    read(First),
    writeln('Name of second person'),
    read(Second),
    (person(First,_,_,_,_,_,_), person(Second,_,_,_,_,_,_) -> true ;
        (person(First,_,_,_,_,_,_) ->
            % Second sistemde yok, onun bilgilerini al
            writeln('Please type the birth date of SECOND person.'),
            read(BD2),
            writeln('Please type the death date of SECOND person.'),
            read(DeathInput2),
            ((DeathInput2 = 'none' ; DeathInput2 = none ; DeathInput2 = "none") -> DD2 = none ; DD2 = DeathInput2),
            writeln('Please type the gender of SECOND person.'),
            read(G2),
            assertz(person(Second, BD2, DD2, G2, none, none, children([])))
        ; person(Second,_,_,_,_,_,_) ->
            % First sistemde yok, onun bilgilerini al
            writeln('Please type the birth date of FIRST person.'),
            read(BD1),
            writeln('Please type the death date of FIRST person.'),
            read(DeathInput1),
            ((DeathInput1 = 'none' ; DeathInput1 = none ; DeathInput1 = "none") -> DD1 = none ; DD1 = DeathInput1),
            writeln('Please type the gender of FIRST person.'),
            read(G1),
            assertz(person(First, BD1, DD1, G1, none, none, children([])))
        ;
            writeln('Error: At least one person must already exist in the system.'),
            main_menu,
            !
        )
    ),
    % Evli olup olmadıklarını kontrol et
    ((marriage(First,_); marriage(Second,_)) ->
        writeln('Married persons cannot marry again.'),
        main_menu,
        !
    ; true),




    % Yaş kontrolü
    get_age(BD1, DD1, AGE1),
    get_age(BD2, DD2, AGE2),
    (
        integer(AGE1), AGE1 < 18 ->
            format('Error: ~w is under 18 (Age: ~w)! Marriage not allowed.~n', [First, AGE1]),
            main_menu,
            !
        ;   integer(AGE2), AGE2 < 18 ->
            format('Error: ~w is under 18 (Age: ~w)! Marriage not allowed.~n', [Second, AGE2]),
            main_menu,
            !
        ;   AGE1 == 'Unknown' ; AGE2 == 'Unknown' ->
            writeln('Error: Age information is incomplete or invalid. Marriage not allowed.'),
            main_menu,
            !
        ;   true
    ),
    (is_alive(First) -> true ; writeln('Error: First person is dead. Marriage not allowed.'), main_menu),
    (is_alive(Second) -> true ; writeln('Error: Second person is dead. Marriage not allowed.'), main_menu),
    (
     (person(First,_,_,m,_,_,_),person(Second,_,_,f,_,_,_));
     (person(First,_,_,f,_,_,_),person(Second,_,_,m,_,_,_))
    ),
    relation(First,Second,Relation),
    (Relation \= 'No known relationship found between the given names.' ->
        forbidden_marriage(Relation, First, Second)
    ;
        true
    ),
    assertz(married(First, Second)),
    writeln('Marriage added successfully.'),
    main_menu.








forbidden_marriage(Relation, Name1, Name2) :-
    (Relation = 'anne'         -> writeln('Invalid Marriage: anne-ogul'), main_menu;
     Relation = 'ogul'         -> writeln('Invalid Marriage: ogul-anne'), main_menu;
     Relation = 'kiz'          -> writeln('Invalid Marriage: kiz-baba'), main_menu;
     Relation = 'baba'         -> writeln('Invalid Marriage: baba-kiz'), main_menu;
     Relation = 'amca'         -> writeln('Invalid Marriage: amca-yegen'), main_menu;
     Relation = 'dayi'         -> writeln('Invalid Marriage: dayi-yegen'), main_menu;
     Relation = 'teyze'        -> writeln('Invalid Marriage: teyze-yegen'), main_menu;
     Relation = 'hala'         -> writeln('Invalid Marriage: hala-yegen'), main_menu;
     Relation = 'yegen'        ->
        (dayi(Name2, Name1) -> writeln('Invalid Marriage: yegen-dayi'), main_menu;
         amca(Name2, Name1) -> writeln('Invalid Marriage: yegen-amca'), main_menu;
         teyze(Name2, Name1) -> writeln('Invalid Marriage: yegen-teyze'), main_menu;
         hala(Name2, Name1) -> writeln('Invalid Marriage: yegen-hala'), main_menu;
         true
        );
     Relation = 'abi'          -> writeln('Invalid Marriage: abi-kiz kardes'), main_menu;
     Relation = 'abla'         -> writeln('Invalid Marriage: abla-erkek kardes'), main_menu;
     Relation = 'erkek kardes' -> writeln('Invalid Marriage: erkek kardes-abla'), main_menu;
     Relation = 'kiz kardes'   -> writeln('Invalid Marriage: kiz kardes-abi'), main_menu;
     true).








get_information :-
    writeln('Please type the person name and surname:'),
    read(Name),
    ( person(Name, Birth, Death, _, _, _, children(Children)) ->
        ( Death = none ->
            Status = 'Alive'
        ;
            Status = 'Dead'
        ),
        get_age(Birth, Death, Age),
        format(atom(AgeInfo), 'Age: ~w', [Age]),
        length(Children, TotalChild),
        ( level(Name, Level) ->
            format('Level: ~w~n', [Level])
        ;
            writeln('Level: Unknown')
        ),
        writeln(AgeInfo),
        format('Total child: ~w~n', [TotalChild]),
        writeln(Status)
    ;
        writeln('Person not found.')
    ).




get_age(Birth, Death, Age) :-
    integer(Birth),
    (Death = 'none' -> Age is 2025 - Birth ;
     integer(Death) -> Age is Death - Birth ;
     Age = 'Unknown').
get_age(_, _, 'Unknown').








% Kök kişiler (Murat ve Sedanur) level 0
level(Name, 0) :-
    member(Name, ['Murat Aslan', 'Sedanur Aslan']).




% Kan bağı olan çocuklar için level hesaplama
level(Name, L) :-
    % Baba tarafından
    (person(Name, _, _, _, Father, _, _),
    Father \= none,
    level(Father, L0),
    L is L0 + 1)
    ;
    % Anne tarafından
    (person(Name, _, _, _, _, Mother, _),
    Mother \= none,
    level(Mother, L0),
    L is L0 + 1).




% Evlilik yoluyla bağlantılı kişiler için (damat/gelin)
level(Name, L) :-
    % Eş üzerinden level bulma
    marriage(Name, Spouse),
    level(Spouse, L),
    % Eşle aynı seviyede say
    !.


% Group people by their level
level_list(Levels) :-
    findall(Level-Person, (person(Person,_,_,_,_,_,_), level(Person, Level)), Pairs),
    sort(Pairs, Sorted),
    group_levels(Sorted, Levels).




group_levels([], []).
group_levels([L-P|Rest], [L-[P|People]|Grouped]) :-
    same_level(L, Rest, People, Rest2),
    group_levels(Rest2, Grouped).




same_level(_, [], [], []).
same_level(L, [L-P|Rest], [P|People], Rest2) :-
    same_level(L, Rest, People, Rest2).
same_level(L, [L2-P|Rest], [], [L2-P|Rest]) :-
    L \= L2.




% Print married couples on the same line, singles alone
print_couples([], _).
print_couples([Person|Rest], Seen) :-
    (married(Person, Spouse), \+ member(Spouse, Seen) ->
        format('~w-~w~n', [Person, Spouse]),
        print_couples(Rest, [Person, Spouse|Seen])
    ;
        (\+ member(Person, Seen) ->
            writeln(Person)
        ;
            true
        ),
        print_couples(Rest, Seen)
    ).


print_levels([]).
print_levels([Level-People|Rest]) :-
    format('---LEVEL ~w---~n', [Level]),
    print_couples(People, []),
    nl,
    print_levels(Rest).


print_tree :-
    level_list(Levels),
    print_levels(Levels).





