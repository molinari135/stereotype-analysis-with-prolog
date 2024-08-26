:- module(stereotypes, [role/2, trait/2, hypernym/2, synonym/2]).

:- dynamic trait/2.
:- dynamic role/2.

% Initialization to confirm the file has been loaded
:- initialization(format('Loading stereotypes.pl~n')).

% Female roles
% role(female, FRole) means that FRole is a stereotypical female role
role(female, nurse).
role(female, teacher).
role(female, caregiver).
role(female, homemaker).
role(female, secretary).
role(female, psychologist).
role(female, journalist).
role(female, writer).
role(female, therapist).
role(female, babysitter).

% Male roles
% role(male, MRole) means that MRole is a stereotypical male role
role(male, engineer).
role(male, leader).
role(male, scientist).
role(male, lawyer).
role(male, doctor).
role(male, architect).
role(male, pilot).
role(male, programmer).
role(male, manager).
role(male, businessman).

% Female traits
% trait(female, FTrait) means that FTrait is a stereotypical female trait
trait(female, emotional).
trait(female, nurturing).
trait(female, patient).
trait(female, empathetic).
trait(female, delicate).
trait(female, gentle).
trait(female, supportive).
trait(female, caring).
trait(female, kind).
trait(female, understanding).

% Male traits
% trait(male, MTrait) means that MTrait is a stereotypical male trait
trait(male, strong).
trait(male, assertive).
trait(male, independent).
trait(male, confident).
trait(male, logical).
trait(male, ambitious).
trait(male, aggressive).
trait(male, tough).
trait(male, competitive).
trait(male, decisive).

% Female synonyms of hypernyms and hyponyms from WordNet
% synonym(A,B) means that A is a synonym for B and vice versa
synonym(female, female_person).
synonym(female, she).

synonym(female_child, girl).
synonym(female_child, little_girl).

synonym(woman, adult_female).
synonym(daughter, girl).
synonym(bridesmaid, maid_of_honor).

synonym(dame, madam).
synonym(dame, lady).
synonym(dame, gentlewoman).

synonym(enchantress, femme_fatale).
synonym(enchantress, temptress).

synonym(girl, miss).
synonym(girl, missy).
synonym(girl, young_lady).
synonym(girl, young_woman).

synonym(wife, married_woman).

synonym(maid, housemaid).
synonym(maid, maiden).

synonym(concubine, courtesan).

synonym(granny, grandma).
synonym(granny, nan).
synonym(granny, grandmother).

synonym(mother, female_parent).

synonym(ma, mama).
synonym(ma, mamma).
synonym(ma, mom).
synonym(ma, momma).
synonym(ma, mommy).
synonym(ma, mammy).
synonym(ma, mum).

synonym(signora, mrs).

synonym(caregiver, health_professional).
synonym(homemaker, housewife).
synonym(homemaker, lady_of_the_house).
synonym(homemaker, woman_of_the_house).

synonym(secretary, secretarial_assistant).

% Male synonyms of hypernyms and hyponyms from WordNet
% synonym(A,B) means that A is a synonym for B and vice versa
synonym(male, male_person).
synonym(male, he).
synonym(male_child, boy).
synonym(man, adult_male).
synonym(son, boy).
synonym(bachelor, unmarried_man).
synonym(dandy, dude).

synonym(fellow, dude).
synonym(fellow, boyfriend).

synonym(guy, hombre).
synonym(guy, bozo).

synonym(monsieur, mr).
synonym(old_boy, old_man).
synonym(patriarch, paterfamilias).
synonym(signore, sir).
synonym(widower, widowman).
synonym(wonder_boy, golden_boy).
synonym(junior, jr).
synonym(father, male_parent).

synonym(dad, dada).
synonym(dad, daddy).
synonym(dad, pa).
synonym(dad, papa).
synonym(dad, pappa).
synonym(dad, pop).

synonym(engineer, applied_scientist).
synonym(engineer, technologist).

synonym(lawyer, attorney).

synonym(doctor, doc).
synonym(doctor, dr).

synonym(architect, designer).

synonym(pilot, airplane_pilot).

synonym(programmer, computer_programmer).
synonym(programmer, coder).
synonym(programmer, software_engineer).

synonym(manager, director).
synonym(manager, manging_director).

% Female hypernyms and hyponyms from WordNet
% hypernym(A,B) means that A is a hypernym of B and B is a hyponym of A
hypernym(female, female_child).
hypernym(female, female_offspring).
hypernym(female, foster-sister).
hypernym(female, girl_wonder).
hypernym(female, woman).

hypernym(female_child, campfire_girl).
hypernym(female_child, farm_girl).
hypernym(female_child, flower_girl).
hypernym(female_child, moppet).
hypernym(female_child, schoolgirl).
hypernym(female_child, girl_scout).

hypernym(female_offspring, daughter).

hypernym(woman, bridesmaid).
hypernym(woman, cinderella).
hypernym(woman, coquette).
hypernym(woman, dame).
hypernym(woman, divorcee).
hypernym(woman, donna).
hypernym(woman, enchantress).
hypernym(woman, girl).
hypernym(woman, girlfriend).
hypernym(woman, lady).
hypernym(woman, mistress).
hypernym(woman, matron).
hypernym(woman, mother-figure).
hypernym(woman, nanny).
hypernym(woman, old_woman).
hypernym(woman, unmarried_woman).
hypernym(woman, widow).
hypernym(woman, wife).

hypernym(dame, madame).

hypernym(girl, baby_girl).
hypernym(girl, belle).
hypernym(girl, dame).
hypernym(girl, gal).
hypernym(girl, maid).
hypernym(girl, tomboy).

hypernym(mistress, concubine).

hypernym(old_woman, granny).
hypernym(old_woman, mother).
hypernym(old_woman, hag).

hypernym(unmarried_woman, signorina).

hypernym(wife, first_lady).
hypernym(wife, housewife).
hypernym(wife, signora).

hypernym(mother, ma).
hypernym(mother, mater).

hypernym(teacher, art_teacher).
hypernym(teacher, english_teacher).
hypernym(teacher, french_teacher).
hypernym(teacher, governess).
hypernym(teacher, instructress).
hypernym(teacher, music_teacher).
hypernym(teacher, reading_teacher).
hypernym(teacher, schoolteacher).

hypernym(caregiver, medical_assistent).
hypernym(caregiver, pharmacist).

hypernym(secretary, receptionist).

hypernym(journalist, broadcast_journalist).
hypernym(journalist, columnist).
hypernym(journalist, correspondent).

hypernym(writer, authoress).
hypernym(writer, biographer).
hypernym(writer, essayist).
hypernym(writer, ghostwriter).
hypernym(writer, novelist).
hypernym(writer, poet).

hypernym(terapist, herbalist).
hypernym(terapist, naturopath).
hypernym(terapist, osteopath).

% Male hypernyms and hyponyms from WordNet
% hypernym(A,B) means that A is a hypernym of B and B is a hyponym of A
hypernym(male, boy_wonder).
hypernym(male, foster-brother).
hypernym(male, macho).
hypernym(male, male_child).
hypernym(male, male_offspring).
hypernym(male, man).

hypernym(male_child, altar_boy).
hypernym(male_child, ball_boy).
hypernym(male_child, bat_boy).
hypernym(male_child, farm_boy).
hypernym(male_child, plowboy).
hypernym(male_child, schoolboy).
hypernym(male_child, boy_scout).

hypernym(male_offspring, son).

hypernym(man, bachelor).
hypernym(man, boy).
hypernym(man, boyfriend).
hypernym(man, dandy).
hypernym(man, father).
hypernym(man, father-figure).
hypernym(man, fellow).
hypernym(man, gentleman).
hypernym(man, guy).
hypernym(man, housefather).
hypernym(man, husband).
hypernym(man, middle-aged_man).
hypernym(man, monsieur).
hypernym(man, old_boy).
hypernym(man, old_man).
hypernym(man, patriarch).
hypernym(man, peter_pan).
hypernym(man, signore).
hypernym(man, widower).
hypernym(man, womanizer).
hypernym(man, wonder_boy).

hypernym(son, junior).

hypernym(father, dad).

hypernym(gentleman, don).
hypernym(gentleman, gent).

hypernym(middle-aged_man, dirty_old_man).

hypernym(womanizer, casanova).
hypernym(womanizer, don_juan).
hypernym(womanizer, wolf).

hypernym(engineer, aeroneutical_engineer).
hypernym(engineer, aerospace_engineer).
hypernym(engineer, army_engineer).
hypernym(engineer, automotive_engineer).
hypernym(engineer, civil_engineer).
hypernym(engineer, electrical_engineer).
hypernym(engineer, marine_engineer).
hypernym(engineer, mechanical_engineer).
hypernym(engineer, metallurgist).
hypernym(engineer, mining_engineer).
hypernym(engineer, programmer).
hypernym(engineer, rocket_engineer).

hypernym(leader, boss).
hypernym(leader, captain).
hypernym(leader, commander).
hypernym(leader, employer).
hypernym(leader, father).
hypernym(leader, guide).
hypernym(leader, headman).
hypernym(leader, politician).

hypernym(scientist, biologist).
hypernym(scientist, chemist).
hypernym(scientist, computer_scientist).
hypernym(scientist, mathematician).
hypernym(scientist, medical_scientist).
hypernym(scientist, physicist).

hypernym(lawyer, defense_attorney).
hypernym(lawyer, divorce_lawyer).
hypernym(lawyer, prosecutor).
hypernym(lawyer, trial_attorney).

hypernym(doctor, gastroenterologist).
hypernym(doctor, intern).
hypernym(doctor, specialist).
hypernym(doctor, surgeon).

hypernym(manager, bank_manager).
hypernym(businessman, owner).