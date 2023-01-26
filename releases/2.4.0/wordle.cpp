#include<bits/stdc++.h>
#include<conio.h>
#include<windows.h>
using namespace std;
string ver="2.4.0";
string date="20230126";
string config="\\wordle.conf";
string user_root;
string progname,cppname;
bool havecpp;
vector<string> words={"ABACK","ABAFT","ABASE","ABASH","ABATE","ABBEY","ABBOT","ABEAM","ABHOR","ABIDE","ABODE","ABORT","ABOUT","ABOVE","ABUSE","ABUZZ","ABYSS","ACORN","ACRES","ACRID","ACTOR","ACUTE","ADAGE","ADAPT","ADDER","ADDLE","ADEPT","ADIEU","ADIOS","ADMAN","ADMIT","ADMIX","ADOBE","ADOPT","ADORE","ADORN","ADULT","AEGIS","AERIE","AFFIX","AFIRE","AFOOT","AFOUL","AFTER","AGAIN","AGAPE","AGATE","AGAVE","AGENT","AGGRO","AGILE","AGING","AGLOW","AGONY","AGREE","AHEAD","AIDED","AIRED","AISLE","ALACK","ALARM","ALBUM","ALDER","ALERT","ALGAE","ALGAL","ALIAS","ALIBI","ALIEN","ALIGN","ALIKE","ALIVE","ALKYD","ALLAY","ALLEY","ALLOT","ALLOW","ALLOY","ALOES","ALOFT","ALOHA","ALONE","ALONG","ALOOF","ALOUD","ALPHA","ALTAR","ALTER","AMASS","AMAZE","AMBER","AMBIT","AMBLE","AMEND","AMIDE","AMIGO","AMINE","AMINO","AMISS","AMITY","AMONG","AMOUR","AMPLE","AMPLY","AMUSE","ANENT","ANGEL","ANGER","ANGLE","ANGRY","ANGST","ANION","ANISE","ANKLE","ANNEX","ANNOY","ANNUL","ANODE","ANTIC","ANTSY","ANVIL","AORTA","APACE","APART","APHID","APISH","APNEA","APPLE","APPLY","APRIL","APRON","APTLY","ARBOR","ARCED","ARDOR","AREAL","ARENA","ARGON","ARGOT","ARGUE","ARISE","ARMED","ARMOR","AROMA","ARRAS","ARRAY","ARROW","ARSON","ASCII","ASCOT","ASHEN","ASIDE","ASKEW","ASPEN","ASPIC","ASSAY","ASSET","ASTER","ASTIR","ATILT","ATLAS","ATOLL","ATONE","ATTAR","ATTIC","AUDIO","AUDIT","AUGER","AUGHT","AUGUR","AURAL","AURIC","AUXIN","AVAIL","AVAST","AVERT","AVIAN","AVOID","AWAIT","AWAKE","AWARD","AWARE","AWASH","AWFUL","AWING","AXIAL","AXIOM","AZURE","BABEL","BACCY","BACON","BADGE","BADLY","BAGEL","BAGGY","BAIRN","BAIZE","BAKED","BAKER","BALDY","BALKY","BALLY","BALMY","BALSA","BANAL","BANDY","BANJO","BANNS","BARED","BARGE","BARMY","BARON","BASAL","BASED","BASIC","BASIL","BASIN","BASIS","BASSO","BASTE","BATCH","BATED","BATHE","BATIK","BATON","BATTY","BAULK","BAWDY","BAYOU","BEACH","BEADS","BEADY","BEARD","BEAST","BEATS","BEAUT","BEBOP","BEDIM","BEECH","BEEFY","BEERY","BEFIT","BEFOG","BEGET","BEGIN","BEGUM","BEIGE","BEING","BELAY","BELCH","BELIE","BELLE","BELLY","BELOW","BENCH","BENDS","BERET","BERRY","BERTH","BERYL","BESET","BESOM","BETEL","BEVEL","BEZEL","BIBLE","BIDDY","BIDET","BIGHT","BIGOT","BIJOU","BILGE","BILLY","BINGE","BINGO","BIOTA","BIPED","BIRCH","BIRTH","BISON","BITER","BITTY","BLACK","BLADE","BLAHS","BLAME","BLAND","BLANK","BLARE","BLASE","BLAST","BLAZE","BLEAK","BLEAR","BLEAT","BLEED","BLEEP","BLEND","BLESS","BLIMP","BLIND","BLINI","BLINK","BLISS","BLITZ","BLOAT","BLOCK","BLOKE","BLOND","BLOOD","BLOOM","BLOWN","BLOWY","BLUES","BLUFF","BLUNT","BLURB","BLURT","BLUSH","BOARD","BOAST","BOBBY","BOFFO","BOGEY","BOGGY","BOGUS","BOLUS","BONCE","BONES","BONGO","BONNY","BONUS","BOOBY","BOOST","BOOTH","BOOTY","BOOZE","BOOZY","BORAX","BORED","BORER","BORON","BOSOM","BOSON","BOSSY","BOTCH","BOUGH","BOUND","BOWED","BOWEL","BOWER","BOWLS","BOXED","BOXER","BRACE","BRACT","BRAID","BRAIN","BRAKE","BRAND","BRASH","BRASS","BRAVE","BRAVO","BRAWL","BRAWN","BRAZE","BREAD","BREAK","BREAM","BREED","BREVE","BRIBE","BRICK","BRIDE","BRIEF","BRIER","BRILL","BRINE","BRING","BRINK","BRINY","BRISK","BROAD","BROIL","BROKE","BRONC","BROOD","BROOK","BROOM","BROTH","BROWN","BRUIN","BRUIT","BRUNT","BRUSH","BRUTE","BUDDY","BUDGE","BUGGY","BUGLE","BUILD","BUILT","BULGE","BULGY","BULKY","BULLY","BUMPY","BUNCH","BUNNY","BURGH","BURLY","BURNT","BURRO","BURSA","BURST","BUSBY","BUSHY","BUSTY","BUTTE","BUTTY","BUXOM","BUYER","BYLAW","BYWAY","CABAL","CABER","CABIN","CABLE","CACAO","CACHE","CADDY","CADET","CADGE","CADRE","CAGEY","CAIRN","CALLA","CALVE","CALYX","CAMEL","CAMEO","CAMPY","CANAL","CANDY","CANNY","CANOE","CANON","CANTO","CAPER","CAPON","CARAT","CARDS","CARET","CARGO","CAROB","CAROL","CAROM","CARRY","CARVE","CASED","CASTE","CATCH","CATER","CATTY","CAULK","CAUSE","CAVIL","CEASE","CECAL","CECUM","CEDAR","CELLO","CHAFE","CHAFF","CHAIN","CHAIR","CHALK","CHAMP","CHANT","CHAOS","CHARD","CHARM","CHART","CHARY","CHASE","CHASM","CHEAP","CHEAT","CHECK","CHEEK","CHEEP","CHEER","CHERT","CHESS","CHEST","CHEWY","CHICK","CHIDE","CHIEF","CHILD","CHILI","CHILL","CHIME","CHIMP","CHINA","CHINE","CHINO","CHIPS","CHIRP","CHIVE","CHOCK","CHOIR","CHOKE","CHOMP","CHORD","CHORE","CHUCK","CHUFF","CHUMP","CHUNK","CHURL","CHURN","CHUTE","CHYME","CIDER","CIGAR","CINCH","CIRCA","CISSY","CIVET","CIVIC","CIVIL","CLACK","CLAIM","CLAMP","CLAMS","CLANG","CLANK","CLASH","CLASP","CLASS","CLEAN","CLEAR","CLEAT","CLEFT","CLERK","CLEWS","CLICK","CLIFF","CLIMB","CLIME","CLING","CLINK","CLOAK","CLOCK","CLOMP","CLONE","CLOSE","CLOTH","CLOUD","CLOUT","CLOVE","CLOWN","CLUCK","CLUMP","CLUNK","COACH","COAST","COBOL","COBRA","COCCI","COCKY","COCOA","CODER","CODEX","COLIC","COLON","COLOR","COMBO","COMER","COMET","COMFY","COMIC","COMMA","CONCH","CONDO","CONGA","CONIC","COPRA","COPSE","CORAL","CORDS","CORER","CORGI","CORNY","CORPS","COSTS","COUCH","COUGH","COUNT","COUPE","COURT","COVEN","COVER","COVET","COVEY","COWER","COYLY","COYPU","COZEN","CRABS","CRACK","CRAFT","CRAMP","CRANE","CRANK","CRAPE","CRASH","CRASS","CRATE","CRAVE","CRAWL","CRAZE","CRAZY","CREAK","CREAM","CREDO","CREED","CREEK","CREEL","CREEP","CREPE","CRESS","CREST","CRICK","CRIER","CRIME","CRIMP","CRISP","CROAK","CROCK","CROFT","CRONE","CRONY","CROOK","CROON","CROSS","CROUP","CROWD","CROWN","CRUDE","CRUEL","CRUET","CRUMB","CRUSE","CRUSH","CRUST","CRYPT","CUBIC","CUBIT","CUMIN","CUPID","CUPPA","CURED","CURIE","CURIO","CURLY","CURRY","CURSE","CURVE","CURVY","CUSHY","CYCLE","CYDER","CYNIC","DACHA","DADDY","DAILY","DAIRY","DAISY","DALLY","DANCE","DANDY","DARTS","DATED","DATUM","DAUNT","DAVIT","DAZED","DEARY","DEATH","DEBAR","DEBIT","DEBUG","DEBUT","DECAF","DECAL","DECAY","DECOR","DECOY","DECRY","DEEDS","DEFER","DEGAS","DEIFY","DEIGN","DEISM","DEIST","DEITY","DELAY","DELFT","DELTA","DELVE","DEMOB","DEMON","DEMUR","DENIM","DENSE","DEPOT","DEPTH","DERBY","DETER","DETOX","DEUCE","DEVIL","DHOTI","DIARY","DICEY","DIGIT","DIMER","DIMLY","DINAR","DINER","DINGO","DINGY","DINKY","DIODE","DIRGE","DIRTY","DISCO","DISHY","DITCH","DITTO","DITTY","DIVAN","DIVER","DIVOT","DIVVY","DIZZY","DODGE","DODGY","DOGGY","DOGIE","DOGMA","DOILY","DOING","DOLLY","DOLOR","DOMED","DONOR","DOPED","DOPEY","DOSED","DOTTY","DOUBT","DOUGH","DOUSE","DOWDY","DOWEL","DOWER","DOWNY","DOWRY","DOWSE","DOYEN","DOZEN","DRAFT","DRAIN","DRAKE","DRAMA","DRAPE","DRAWL","DRAWN","DREAD","DREAM","DREAR","DREGS","DRESS","DRIED","DRIER","DRIFT","DRILL","DRINK","DRIVE","DROLL","DRONE","DROOL","DROOP","DROSS","DROVE","DROWN","DRUNK","DRUPE","DRYAD","DRYER","DRYLY","DUCAL","DUCAT","DUCHY","DUCKY","DULLY","DUMMY","DUMPS","DUMPY","DUPLE","DURUM","DUSKY","DUSTY","DUVET","DWARF","DWEEB","DWELL","DYING","EAGER","EAGLE","EARED","EARLY","EARTH","EASED","EASEL","EATER","EAVES","EBONY","ECLAT","EDEMA","EDGED","EDGER","EDICT","EDIFY","EDUCE","EERIE","EGRET","EIDER","EIGHT","EJECT","ELAND","ELATE","ELBOW","ELDER","ELECT","ELEGY","ELFIN","ELIDE","ELITE","ELOPE","ELUDE","ELVER","ELVES","EMAIL","EMBED","EMBER","EMCEE","EMEND","EMERY","EMOTE","EMPTY","ENACT","ENDED","ENDOW","ENDUE","ENEMA","ENEMY","ENJOY","ENNUI","ENSUE","ENTER","ENTRY","ENVOY","EPOCH","EPOXY","EQUAL","EQUIP","ERASE","ERECT","ERGOT","ERODE","ERROR","ERUPT","ESSAY","ESTER","ETHER","ETHIC","ETHOS","ETHYL","ETUDE","EVADE","EVENT","EVERY","EVICT","EVOKE","EXACT","EXALT","EXCEL","EXERT","EXILE","EXIST","EXPAT","EXPEL","EXTOL","EXTRA","EXUDE","EXULT","FABLE","FACED","FACET","FADDY","FADED","FAINT","FAIRY","FAITH","FAKER","FAKIR","FALLS","FALSE","FAMED","FANCY","FARAD","FARCE","FATAL","FATED","FATTY","FATWA","FAULT","FAUNA","FAVOR","FAZED","FEAST","FECAL","FECES","FEIGN","FEINT","FELLA","FELON","FEMUR","FENCE","FERAL","FERNY","FERRY","FETAL","FETCH","FETID","FETUS","FEVER","FEWER","FIBER","FICHU","FIELD","FIEND","FIERY","FIFTH","FIFTY","FIGHT","FILCH","FILER","FILLY","FILMY","FILTH","FINAL","FINCH","FINER","FINIS","FIRED","FIRST","FIRTH","FISHY","FITLY","FIVER","FIVES","FIXED","FIXER","FIZZY","FJORD","FLACK","FLAIL","FLAIR","FLAKE","FLAKY","FLAME","FLANK","FLAPS","FLARE","FLASH","FLASK","FLATS","FLECK","FLEET","FLESH","FLICK","FLIER","FLIES","FLING","FLINT","FLIRT","FLOAT","FLOCK","FLOOD","FLOOR","FLORA","FLOSS","FLOUR","FLOUT","FLUFF","FLUID","FLUKE","FLUKY","FLUME","FLUNK","FLUSH","FLUTE","FOAMY","FOCAL","FOCUS","FOGGY","FOIST","FOLIO","FOLKS","FOLLY","FORAY","FORCE","FORGE","FORGO","FORTE","FORTH","FORTY","FORUM","FOUND","FOUNT","FOYER","FRAIL","FRAME","FRANC","FRANK","FRAUD","FREAK","FRESH","FRIAR","FRIED","FRIES","FRILL","FRISK","FRIZZ","FROCK","FROND","FRONT","FROST","FROTH","FROWN","FRUIT","FRUMP","FRYER","FUDGE","FUGAL","FUGUE","FULLY","FUMED","FUMES","FUNDS","FUNKY","FUNNY","FUROR","FURRY","FURZE","FUSED","FUSEE","FUSSY","FUSTY","FUTON","FUZZY","GABBY","GABLE","GAFFE","GAILY","GAMIN","GAMMA","GAMMY","GAMUT","GASSY","GATOR","GAUDY","GAUNT","GAUZE","GAUZY","GAVEL","GAWKY","GECKO","GELID","GENIE","GENRE","GENUS","GEODE","GETUP","GHOST","GHOUL","GIANT","GIDDY","GIMPY","GIRTH","GIVEN","GIVER","GLACE","GLADE","GLAND","GLANS","GLARE","GLASS","GLAZE","GLEAM","GLEAN","GLIDE","GLINT","GLITZ","GLOAT","GLOBE","GLOOM","GLORY","GLOSS","GLOVE","GLUED","GLUEY","GLUON","GLYPH","GNARL","GNASH","GNOME","GODLY","GOFER","GOING","GOLLY","GONER","GOODY","GOOEY","GOOFY","GOOSE","GORGE","GORSE","GOUGE","GOURD","GOUTY","GRACE","GRADE","GRAFT","GRAIL","GRAIN","GRAND","GRANT","GRAPE","GRAPH","GRASP","GRASS","GRATE","GRAVE","GRAVY","GRAZE","GREAT","GREBE","GREED","GREEN","GREET","GRIEF","GRILL","GRIME","GRIMY","GRIND","GRIPE","GRIST","GRITS","GROAN","GROAT","GROIN","GROOM","GROPE","GROSS","GROUP","GROUT","GROVE","GROWL","GROWN","GRUEL","GRUFF","GRUMP","GRUNT","GUANO","GUARD","GUAVA","GUESS","GUEST","GUIDE","GUILD","GUILE","GUILT","GUISE","GULAG","GULCH","GULLY","GUMBO","GUMMY","GUNNY","GUPPY","GUSHY","GUSTO","GUSTY","GUTSY","GYPSY","HABIT","HAIKU","HAIRY","HALAL","HALLO","HALON","HALVE","HAMMY","HANDS","HANDY","HAPLY","HAPPY","HARDY","HAREM","HARPY","HARRY","HARSH","HASTE","HASTY","HATCH","HATED","HATER","HAUNT","HAVEN","HAVOC","HAZEL","HEADS","HEADY","HEAPS","HEARD","HEART","HEATH","HEAVE","HEAVY","HEDGE","HEFTY","HEIST","HELIX","HELLO","HELOT","HELVE","HENCE","HENNA","HERON","HERTZ","HEWER","HEXED","HIKER","HILLY","HINGE","HIPPO","HIRED","HIRER","HITCH","HIVES","HOARD","HOARY","HOBBY","HOGAN","HOIST","HOKEY","HOKUM","HOLEY","HOLLY","HOMER","HOMEY","HONEY","HONOR","HOOCH","HOOEY","HOOKS","HOOKY","HOOPS","HORDE","HORSE","HOTEL","HOTLY","HOUND","HOURI","HOURS","HOUSE","HOVEL","HOVER","HOWDY","HUBBY","HUFFY","HULLO","HUMAN","HUMID","HUMOR","HUMPH","HUMUS","HUNCH","HURRY","HUSKY","HUTCH","HYDRA","HYENA","HYMEN","ICILY","ICING","ICTUS","IDEAL","IDIOM","IDIOT","IDLER","IDYLL","IGLOO","ILEUM","ILIUM","IMAGE","IMAGO","IMBUE","IMPEL","IMPLY","INANE","INAPT","INCUR","INDEX","INDIE","INEPT","INERT","INFER","INFIX","INFRA","INGOT","INLAY","INLET","INNER","INPUT","INSET","INTER","INURE","IONIC","IRATE","IRONS","IRONY","ISLET","ISSUE","ITCHY","IVIED","IVORY","JABOT","JACKS","JADED","JAPAN","JAUNT","JAWED","JAZZY","JELLY","JEMMY","JENNY","JERKY","JETTY","JEWEL","JIFFY","JIHAD","JIMMY","JINGO","JINKS","JOINT","JOIST","JOKER","JOLLY","JOULE","JOUST","JOWLY","JUDGE","JUICE","JUICY","JULEP","JUMBO","JUMPY","JUNCO","JUNTA","JUROR","KABOB","KAPOK","KAPPA","KAPUT","KARAT","KARMA","KAYAK","KAZOO","KEBAB","KETCH","KEYED","KHAKI","KINDA","KIOSK","KITTY","KLUTZ","KNACK","KNAVE","KNEAD","KNEEL","KNELL","KNIFE","KNISH","KNOCK","KNOLL","KNOWN","KOALA","KOOKY","KRAAL","KRILL","KRONA","KRONE","KUDOS","KUDZU","LABEL","LABOR","LACED","LADEN","LADLE","LAGER","LAIRD","LAITY","LANAI","LANCE","LANKY","LAPEL","LAPIN","LAPSE","LARCH","LARGE","LARGO","LARVA","LASER","LASSO","LATCH","LATER","LATEX","LATHE","LATTE","LAUGH","LAXLY","LAYER","LAYUP","LEACH","LEAFY","LEAKY","LEARN","LEASE","LEASH","LEAST","LEAVE","LEDGE","LEECH","LEERY","LEFTY","LEGAL","LEGGY","LEMMA","LEMON","LEMUR","LENTO","LEPER","LETCH","LETUP","LEVEE","LEVEL","LEVER","LEXIS","LIBEL","LICIT","LIEGE","LIFER","LIGHT","LIKED","LIKEN","LILAC","LIMBO","LIMEY","LIMIT","LINED","LINEN","LINER","LINGO","LINKS","LIPID","LISLE","LITER","LITHE","LIVEN","LIVER","LIVID","LLAMA","LLANO","LOADS","LOAMY","LOATH","LOBAR","LOBBY","LOBED","LOCAL","LOCUM","LOCUS","LODGE","LOFTY","LOGIC","LOINS","LOLLY","LONER","LOONY","LOOPY","LOOSE","LORRY","LOSER","LOTTO","LOTUS","LOUGH","LOUSE","LOUSY","LOVED","LOVER","LOWER","LOWLY","LOYAL","LUCID","LUCKY","LUCRE","LUMEN","LUMPY","LUNAR","LUNCH","LUNGE","LUPUS","LURCH","LURID","LUSTY","LYING","LYMPH","LYNCH","LYRIC","MACAW","MACHO","MACRO","MADAM","MADLY","MAFIA","MAGIC","MAGMA","MAIZE","MAJOR","MAKER","MAMBA","MAMBO","MANGE","MANGO","MANGY","MANIA","MANIC","MANLY","MANNA","MANOR","MANSE","MANTA","MAPLE","MARCH","MARGE","MARIA","MARRY","MARSH","MASER","MASON","MATCH","MATED","MATER","MATES","MATEY","MATTE","MATZO","MAUVE","MAVEN","MAXIM","MAYBE","MAYOR","MEALY","MEANS","MEATY","MECCA","MEDAL","MEDIC","MELEE","MELON","MERCY","MERGE","MERIT","MERRY","MESON","MESSY","METAL","METER","METRO","MEZZO","MICRO","MIDDY","MIDGE","MIDST","MIGHT","MILCH","MILER","MILKY","MIMIC","MINCE","MINED","MINER","MINGY","MINIM","MINOR","MINTY","MINUS","MIRED","MIRTH","MISER","MISTY","MITER","MIXED","MIXER","MOCHA","MODAL","MODEL","MODEM","MOGUL","MOIRE","MOIST","MOLAR","MOLDY","MOLLY","MOMMA","MOMMY","MONEY","MONTH","MOOCH","MOODY","MOOSE","MOPED","MOPES","MORAL","MORAY","MOREL","MORES","MOSEY","MOSSY","MOTEL","MOTET","MOTIF","MOTOR","MOTTO","MOULT","MOUND","MOUNT","MOURN","MOUSE","MOUSY","MOUTH","MOVED","MOVER","MOVIE","MOWER","MOXIE","MUCKY","MUCUS","MUDDY","MUFTI","MUGGY","MULCH","MULCT","MUMMY","MUMPS","MUNCH","MURAL","MURKY","MUSHY","MUSIC","MUSKY","MUSSY","MUSTY","MUTED","MYRRH","NABOB","NACHO","NACRE","NADIR","NAIAD","NAIVE","NAKED","NAMES","NANNY","NAPPY","NASAL","NASTY","NATAL","NATTY","NAVAL","NAVEL","NAVVY","NEATH","NEEDS","NEEDY","NEIGH","NERVE","NERVY","NEVER","NEVUS","NEWEL","NEWLY","NEWSY","NEXUS","NICHE","NIECE","NIFTY","NIGHT","NINJA","NINNY","NINTH","NIPPY","NITER","NOBLE","NOBLY","NOHOW","NOISE","NOISY","NOMAD","NONCE","NOOSE","NORTH","NOSED","NOTCH","NOTED","NOVEL","NUBBY","NUDGE","NURSE","NUTTY","NYLON","NYMPH","OAKEN","OAKUM","OASIS","OATEN","OBESE","OCCUR","OCEAN","OCHER","OCTAL","OCTET","ODDLY","ODIUM","OFFAL","OFFER","OFTEN","OHMIC","OILED","OKAPI","OLDEN","OLDER","OLDIE","OLIVE","OMEGA","ONION","ONSET","OOMPH","OPERA","OPINE","OPIUM","OPTIC","ORATE","ORBIT","ORDER","ORGAN","ORIEL","ORRIS","OSIER","OTHER","OTTER","OUNCE","OUTDO","OUTER","OUTGO","OUTRE","OVARY","OVATE","OVERT","OVOID","OVULE","OWING","OWLET","OWNED","OWNER","OXBOW","OXIDE","OZONE","PACER","PADRE","PAEAN","PAGAN","PAGER","PAINS","PAINT","PALLY","PALMY","PALSY","PANDA","PANEL","PANIC","PANTO","PANTS","PAPAL","PAPAW","PAPER","PARCH","PARER","PARKA","PARRY","PARSE","PARTS","PARTY","PASHA","PASSE","PASTA","PASTE","PASTY","PATCH","PATER","PATIO","PATSY","PATTY","PAUSE","PAVED","PAYEE","PAYER","PEACE","PEACH","PEAKY","PEARL","PEATY","PECAN","PEDAL","PEEVE","PEKOE","PENAL","PENNY","PEONY","PEPPY","PERCH","PERIL","PERKY","PERRY","PESKY","PESTO","PETAL","PETER","PETTY","PEWEE","PHAGE","PHASE","PHIAL","PHLOX","PHONE","PHONY","PHOTO","PIANO","PICKY","PICOT","PIECE","PIETY","PIGGY","PILAF","PILES","PILOT","PINCH","PINKO","PINNY","PINON","PINTO","PIOUS","PIPER","PIPIT","PIQUE","PISTE","PITCH","PITHY","PITON","PITTA","PIVOT","PIXEL","PIXIE","PIZZA","PLACE","PLAID","PLAIN","PLAIT","PLANE","PLANK","PLANT","PLASH","PLATE","PLATY","PLAZA","PLEAD","PLEAT","PLEBE","PLONK","PLUCK","PLUMB","PLUME","PLUMP","PLUMY","PLUNK","PLUSH","POACH","POESY","POINT","POISE","POKER","POLAR","POLIO","POLKA","POLLS","POLYP","POOCH","POPPY","PORCH","PORGY","POSED","POSER","POSIT","POSSE","POTTY","POUCH","POUND","POWER","PRANG","PRANK","PRATE","PRAWN","PREEN","PRESS","PRICE","PRIDE","PRIME","PRIMP","PRINT","PRION","PRIOR","PRISM","PRIVY","PRIZE","PROBE","PROLE","PRONE","PRONG","PROOF","PROPS","PROSE","PROSY","PROUD","PROVE","PROWL","PROXY","PRUDE","PRUNE","PSALM","PSHAW","PUBES","PUBIC","PUBIS","PUDGY","PUFFY","PUKKA","PULPY","PULSE","PUNCH","PUNKS","PUPAL","PUPIL","PUPPY","PUREE","PURGE","PURSE","PUSHY","PUTTY","PYGMY","PYLON","QUACK","QUAFF","QUAIL","QUAKE","QUALM","QUARK","QUART","QUASH","QUASI","QUEEN","QUEER","QUELL","QUERN","QUERY","QUEST","QUEUE","QUICK","QUIET","QUIFF","QUILL","QUILT","QUINT","QUIRE","QUIRK","QUIRT","QUITE","QUITS","QUOIN","QUOIT","QUOTA","QUOTE","RABBI","RABID","RACER","RADAR","RADIO","RADIX","RADON","RAFTS","RAILS","RAINY","RAISE","RAJAH","RALLY","RAMIE","RANCH","RANEE","RANGE","RANGY","RAPID","RASPY","RATES","RATIO","RATTY","RAVEL","RAVEN","RAVER","RAYON","RAZED","RAZOR","REACH","REACT","READY","REALM","REARM","REBEL","REBUS","REBUT","RECAP","RECCE","RECTO","RECUR","REEDY","REEVE","REFER","REFIT","REGAL","REIGN","RELAX","RELAY","RELIC","REMIT","RENAL","RENEW","REPAY","REPEL","REPLY","RERUN","RESET","RESIN","RETCH","RETIE","RETRO","RETRY","REUSE","REVEL","REVUE","RHEUM","RHINO","RHYME","RICER","RIDER","RIDGE","RIFLE","RIGHT","RIGID","RIGOR","RILED","RIMED","RINGS","RINSE","RIPEN","RISEN","RISER","RISKY","RITZY","RIVAL","RIVER","RIVET","RIYAL","ROACH","ROADS","ROAST","ROBED","ROBIN","ROBOT","ROCKY","RODEO","ROGUE","ROMAN","RONDO","ROOMS","ROOMY","ROOST","ROOTS","ROPER","ROPEY","ROSIN","ROTOR","ROUGE","ROUGH","ROUND","ROUSE","ROUTE","ROVER","ROWAN","ROWDY","ROWEL","ROWER","ROYAL","RUBLE","RUDDY","RUGBY","RULED","RULER","RUMBA","RUMMY","RUMOR","RUNIC","RUNNY","RUNTY","RUPEE","RURAL","RUSHY","RUSTY","RUTTY","SABER","SABLE","SABOT","SABRA","SADHU","SADLY","SAHIB","SAINT","SALAD","SALES","SALLY","SALON","SALSA","SALTY","SALVE","SALVO","SAMBA","SANDS","SANDY","SAPPY","SARAN","SASSY","SATIN","SATYR","SAUCE","SAUCY","SAUNA","SAUTE","SAVED","SAVER","SAVOR","SAVOY","SAVVY","SCADS","SCALD","SCALE","SCALP","SCALY","SCAMP","SCANT","SCAPE","SCARE","SCARF","SCARP","SCARY","SCENE","SCENT","SCHWA","SCION","SCOFF","SCOLD","SCONE","SCOOP","SCOOT","SCOPE","SCORE","SCORN","SCOUR","SCOUT","SCOWL","SCRAG","SCRAM","SCRAP","SCREE","SCREW","SCRIM","SCRIP","SCROD","SCRUB","SCRUM","SCUBA","SCUFF","SCULL","SCURF","SEAMY","SEATS","SEBUM","SEDAN","SEDGE","SEDGY","SEEDY","SEEMS","SEGUE","SEINE","SEIZE","SENNA","SENSE","SEPAL","SEPIA","SERGE","SERIF","SERUM","SERVE","SERVO","SETUP","SEVEN","SEVER","SEWED","SEWER","SHACK","SHADE","SHADY","SHAFT","SHAKE","SHAKY","SHALE","SHAME","SHANK","SHAPE","SHARD","SHARE","SHARK","SHARP","SHAVE","SHAWL","SHEAF","SHEAR","SHEEN","SHEEP","SHEER","SHEET","SHEIK","SHELF","SHELL","SHIFT","SHILL","SHINE","SHINY","SHIRE","SHIRK","SHIRT","SHOAL","SHOAT","SHOCK","SHOES","SHOOK","SHOOT","SHORE","SHORT","SHOUT","SHOVE","SHOWY","SHRED","SHREW","SHRUB","SHRUG","SHUCK","SHUNT","SHUSH","SHYLY","SIBYL","SIDLE","SIEGE","SIEVE","SIGHT","SIGMA","SILKS","SILKY","SILLY","SILTY","SINCE","SINEW","SINGE","SINUS","SIREN","SISAL","SITAR","SIXTH","SIXTY","SIZED","SKATE","SKEET","SKEIN","SKIER","SKIFF","SKILL","SKIMP","SKINT","SKIRT","SKIVE","SKULK","SKULL","SKUNK","SLACK","SLAIN","SLAKE","SLANG","SLANT","SLASH","SLATE","SLAVE","SLEEK","SLEEP","SLEET","SLEWS","SLICE","SLICK","SLIDE","SLIME","SLIMY","SLING","SLINK","SLOOP","SLOPE","SLOPS","SLOSH","SLOTH","SLUMP","SLURP","SLUSH","SLYLY","SMACK","SMALL","SMART","SMASH","SMEAR","SMELL","SMELT","SMILE","SMIRK","SMITE","SMITH","SMOCK","SMOKE","SMOKY","SNACK","SNAFU","SNAIL","SNAKE","SNAKY","SNARE","SNARL","SNEAK","SNEER","SNICK","SNIDE","SNIFF","SNIPE","SNIPS","SNOOD","SNOOP","SNOOT","SNORE","SNORT","SNOUT","SNOWY","SNUFF","SOAPY","SOBER","SOFTY","SOGGY","SOLAR","SOLED","SOLID","SOLVE","SONAR","SONIC","SONNY","SOOTH","SOOTY","SOPPY","SORRY","SOUGH","SOUND","SOUPY","SOUSE","SOUTH","SOWER","SPACE","SPADE","SPANK","SPARE","SPARK","SPASM","SPATE","SPAWN","SPEAK","SPEAR","SPECK","SPECS","SPEED","SPELL","SPEND","SPENT","SPICE","SPICY","SPIEL","SPIFF","SPIKE","SPIKY","SPILL","SPINE","SPINY","SPIRE","SPITE","SPLAT","SPLAY","SPLIT","SPOIL","SPOKE","SPOOF","SPOOK","SPOOL","SPOON","SPOOR","SPORE","SPORT","SPOTS","SPOUT","SPRAT","SPRAY","SPREE","SPRIG","SPROG","SPUME","SPUNK","SPURN","SPURT","SQUAB","SQUAD","SQUAT","SQUIB","SQUID","STACK","STAFF","STAGE","STAGY","STAID","STAIN","STAIR","STAKE","STALE","STALK","STALL","STAMP","STAND","STAPH","STARE","STARK","START","STASH","STATE","STAVE","STAYS","STEAD","STEAK","STEAL","STEAM","STEED","STEEL","STEEP","STEER","STEIN","STEPS","STERN","STICK","STIFF","STILE","STILL","STILT","STING","STINK","STINT","STOAT","STOCK","STOGY","STOIC","STOKE","STOLE","STOMA","STOMP","STONE","STONY","STOOL","STOOP","STOPS","STORE","STORK","STORM","STORY","STOUP","STOUT","STOVE","STRAP","STRAW","STRAY","STREP","STREW","STRIA","STRIP","STROP","STRUM","STRUT","STUCK","STUDY","STUFF","STUMP","STUNG","STUNT","STYLE","SUAVE","SUCKS","SUDSY","SUEDE","SUGAR","SUITE","SULFA","SULKY","SULLY","SUMAC","SUNNY","SUNUP","SUPER","SUPRA","SURGE","SURLY","SUSHI","SWAIN","SWAMI","SWAMP","SWANK","SWARD","SWARM","SWASH","SWATH","SWEAR","SWEAT","SWEDE","SWEEP","SWEET","SWELL","SWEPT","SWIFT","SWILL","SWINE","SWING","SWIPE","SWIRL","SWISH","SWOON","SWOOP","SWORD","SWORN","SYLPH","SYNOD","SYRUP","TABBY","TABLE","TABOO","TABOR","TACIT","TACKY","TAFFY","TAILS","TAINT","TAKEN","TAKER","TALKS","TALKY","TALLY","TALON","TALUS","TAMED","TAMER","TANGO","TANGY","TANSY","TAPED","TAPER","TAPIR","TARDY","TAROT","TARRY","TASTE","TASTY","TATER","TATTY","TAUNT","TAUPE","TAWNY","TAXER","TAXIS","TEACH","TEARS","TEARY","TEASE","TEDDY","TEENS","TEENY","TEETH","TELEX","TELLY","TEMPO","TEMPT","TENCH","TENET","TENON","TENOR","TENSE","TENTH","TEPEE","TEPID","TERMS","TERRY","TERSE","TESTY","TETRA","THANE","THANK","THEFT","THEIR","THEME","THERE","THERM","THESE","THETA","THICK","THIEF","THIGH","THINE","THING","THINK","THIRD","THOLE","THONG","THORN","THOSE","THREE","THROB","THROE","THROW","THRUM","THUMB","THUMP","THYME","TIARA","TIBIA","TIDAL","TIGER","TIGHT","TILDE","TILED","TILER","TIMED","TIMER","TIMES","TIMID","TINGE","TINNY","TIPSY","TIRED","TITAN","TITER","TITHE","TITLE","TIZZY","TOADY","TOAST","TODAY","TODDY","TOKEN","TONAL","TONED","TONER","TONGS","TONIC","TONNE","TOOTH","TOPAZ","TOPEE","TOPIC","TOQUE","TORCH","TORSO","TORTE","TORUS","TOTAL","TOTEM","TOTER","TOUCH","TOUGH","TOWEL","TOWER","TOXIC","TOXIN","TRACE","TRACK","TRACT","TRADE","TRAIL","TRAIN","TRAIT","TRAMP","TRASH","TRAWL","TREAD","TREAT","TREED","TREND","TRESS","TREWS","TRIAD","TRIAL","TRIBE","TRICE","TRICK","TRIED","TRIER","TRIKE","TRILL","TRIPE","TRITE","TROLL","TROOP","TROPE","TROTH","TROUT","TROVE","TRUCE","TRUCK","TRULY","TRUMP","TRUNK","TRUSS","TRUST","TRUTH","TRYST","TUBAL","TUBBY","TUBED","TUBER","TULIP","TULLE","TUMID","TUMMY","TUMOR","TUNER","TUNIC","TUNNY","TURPS","TUTOR","TWAIN","TWANG","TWEAK","TWEED","TWEET","TWICE","TWILL","TWINE","TWINS","TWIRL","TWIST","TYING","UDDER","UKASE","ULCER","ULNAR","ULTRA","UMBEL","UMBER","UMBRA","UNBAR","UNCLE","UNCUT","UNDER","UNDUE","UNFED","UNFIT","UNIFY","UNION","UNITE","UNITY","UNLIT","UNMAN","UNPIN","UNSAY","UNTIE","UNTIL","UNWED","UNZIP","UPEND","UPPER","UPSET","URBAN","URINE","USAGE","USHER","USING","USUAL","USURP","USURY","UTTER","UVULA","VAGUE","VALET","VALID","VALOR","VALUE","VALVE","VAPID","VAPOR","VAULT","VAUNT","VEGAN","VELAR","VELUM","VENAL","VENOM","VENUE","VERGE","VERSE","VERSO","VERVE","VETCH","VEXED","VIAND","VIBES","VICAR","VIDEO","VIGIL","VIGOR","VILLA","VINYL","VIOLA","VIPER","VIRAL","VIREO","VIRUS","VISIT","VISOR","VISTA","VITAL","VIVID","VIXEN","VOCAL","VODKA","VOGUE","VOICE","VOILE","VOMIT","VOTER","VOUCH","VOWEL","WACKO","WACKY","WADER","WAFER","WAGER","WAGES","WAGON","WAIST","WAIVE","WAKEN","WALLY","WALTZ","WANLY","WARTY","WASHY","WASTE","WATCH","WATER","WAVER","WAXED","WAXEN","WEARY","WEAVE","WEDGE","WEEDS","WEEDY","WEENY","WEEPY","WEIGH","WEIRD","WELSH","WHACK","WHALE","WHARF","WHEAL","WHEAT","WHEEL","WHELK","WHELM","WHELP","WHERE","WHICH","WHIFF","WHILE","WHINE","WHINY","WHIRL","WHISK","WHIST","WHITE","WHOLE","WHOOP","WHORL","WHOSE","WHOSO","WIDEN","WIDOW","WIDTH","WIELD","WIGHT","WIMPY","WINCE","WINCH","WINDY","WINGS","WIPER","WIRED","WISPY","WITCH","WITHE","WITTY","WOMAN","WONKY","WOODS","WOODY","WOOER","WOOZY","WORDS","WORDY","WORKS","WORLD","WORMY","WORRY","WORSE","WORST","WORTH","WOUND","WOVEN","WRACK","WRATH","WREAK","WRECK","WREST","WRING","WRIST","WRITE","WRONG","WROTH","WRYLY","XENON","XEROX","XYLEM","YACHT","YAHOO","YEARN","YEARS","YEAST","YIELD","YODEL","YOGIC","YOKEL","YOUNG","YOURS","YOUTH","YUCCA","YUCKY","YUMMY","ZEBRA","ZESTY","ZILCH","ZIPPY","ZLOTY","ZONAL"};
string user;
int back=FOREGROUND_RED|FOREGROUND_BLUE|FOREGROUND_GREEN;
int red=FOREGROUND_INTENSITY|FOREGROUND_RED;
int green=FOREGROUND_INTENSITY|FOREGROUND_GREEN;
int blue=FOREGROUND_INTENSITY|FOREGROUND_BLUE;
int yellow=FOREGROUND_INTENSITY|FOREGROUND_GREEN|FOREGROUND_RED;
int purple=FOREGROUND_INTENSITY|FOREGROUND_BLUE|FOREGROUND_RED;
void scta(int clr){
	SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),clr);
}
vector<pair<char,int> > hist[10010];
int cnt[26];
int col[10010];
vector<string> keyboard={"Q W E R T Y U I O P",
                         " A S D F G H J K L",
						 "  Z X C V B N M"};
int bad[26];
string det;
map<string,int> vals;
set<string> inter;
map<string,string> desc,strv;
pair<int,int> rat;
void writeconf();
void clr(int R){
	if(R<800) scta(back);
	else if(R<1000) scta(green);
	else if(R<1300) scta(blue);
	else if(R<1700) scta(purple);
	else if(R<2000) scta(yellow);
	else scta(red);
}
string level(int R){
	if(R<800) return "newbie";
	else if(R<1000) return "pupil";
	else if(R<1300) return "expert";
	else if(R<1700) return "master";
	else if(R<2000) return "grandmaster";
	else return "legendary";
}
string encode(int rating,int rounds){
	string ret="";
	rounds=rounds*3000+rating;
	rounds*=97;
	rounds+=rand()%96;
	rounds++;
	while(rounds){
		ret+=((rounds%26)+'A');
		rounds/=26;
	}
	while(ret.size()<=10) ret+="A";
	reverse(ret.begin(),ret.end());
	return ret;
}
pair<int,int> decode(string rating){
	int val=0;
	for(int i=0;i<(int)(rating.size());i++){
		val=(val*26+rating[i]-'A');
	}
	val/=97;
	return make_pair(val%3000,val/3000);
}
void prep(){
	desc["TURNS"]="Number of turns you have of guessing the word.\n"; 
}
void printstate(int rounds,int wid,int rounds_disp){
	system("cls");
	scta(back);
	cout<<"Guess Word"<<endl;
	printf("Try #%4d\n",rounds_disp);
	for(int i=0;i<3;i++){
		for(int j=0;j<(int)(keyboard[i].size());j++){
			if(keyboard[i][j]!=' '){
				if(bad[keyboard[i][j]-'A']==0) scta(back);
				if(bad[keyboard[i][j]-'A']==1) scta(red);
				if(bad[keyboard[i][j]-'A']==2) scta(yellow);
				if(bad[keyboard[i][j]-'A']==3) scta(back);
			}
			cout<<keyboard[i][j];
			scta(back);
		}
		cout<<endl;
	}
	cout<<(int)(words[wid].size())<<" letters."<<endl;
	cout<<det<<endl;
	hist[rounds+1].clear();
	for(int i=1;i<=rounds;i++){
		for(int j=0;j<hist[i].size();j++){
			scta(hist[i][j].second);
			cout<<hist[i][j].first;
		}
		cout<<endl;
	}
	scta(back);
}
void wordle(){
	int id=rand()%(int)(words.size());
	scta(back);
	int round=0;
	memset(bad,0,sizeof(bad));
	det="";
	for(int i=0;i<(int)(words[id].size());i++) det+="_";
	while(1){
		round++;
		if(round==vals["TURNS"]+1){
			printstate(round-1,id,round-1);
			scta(red);
			cout<<"GAME OVER!"<<endl;
			scta(green);
			cout<<"ANSWER: "<<words[id]<<endl;
			scta(back);
			int score=0;
			for(int i=0;i<(int)(words[id].size());i++)
				if(det[i]!='_') score+=100;
			for(int i=0;i<3;i++)
				for(int j=0;j<(int)(keyboard[i].size());j++)
					if(keyboard[i][j]!=' '){
						if(bad[keyboard[i][j]-'A']==2)  score+=50;
					}
			cout<<"Score:"<<score<<endl;
			rat.second=min(rat.second+1,1000);
			rat.first=(rat.first*(rat.second-1)+score)/rat.second;
			strv["RATING_"+strv["ACCOUNT"]]=encode(rat.first,rat.second);
			writeconf();
			system("pause");
			return;
		}
		printstate(round-1,id,round);
		string ssss;
		cin>>ssss;
		if(ssss=="help!"){
			printstate(round-1,id,round-1);
			scta(red);
			cout<<"YOU GAVE UP!"<<endl;
			scta(green);
			cout<<"ANSWER: "<<words[id]<<endl;
			scta(back);
			system("pause");
			return;
		}
		bool flag=true;
    	for(int i=0;i<(int)(ssss.size());i++){
    		if(!isalpha(ssss[i])){
    			flag=false;
    			break;
			}
		}
		if(!flag){
    		for(int i=0;i<(int)(ssss.size());i++)
				hist[round].push_back(make_pair(ssss[i],blue));
			continue;
		}
    	for(int i=0;i<(int)(ssss.size());i++) if(ssss[i]<='Z')
    		ssss[i]=ssss[i]-'A'+'a';
		if((int)(ssss.size())!=(int)(words[id].size())){
    		for(int i=0;i<(int)(ssss.size());i++)
				hist[round].push_back(make_pair(ssss[i],purple));
			continue;
		}
		flag=false;
		for(int i=0;i<words.size();i++) if(words[i]==ssss) flag=true;
		if(!flag){
    		for(int i=0;i<(int)(ssss.size());i++)
				hist[round].push_back(make_pair(ssss[i],blue));
			continue;
		}
		memset(cnt,0,sizeof(cnt));
		for(int i=0;i<(int)(words[id].size());i++)
			cnt[words[id][i]-'a']++;
		int GR=0;
		for(int i=0;i<(int)(words[id].size());i++){
			if(ssss[i]==words[id][i]){
				cnt[ssss[i]-'a']--;
				col[i]=green;
				det[i]=ssss[i];
				bad[ssss[i]-'a']=3;
				ssss[i]=ssss[i]-'a'+'A';
				GR++;
			}
		}
		for(int i=0;i<(int)(words[id].size());i++){
			if(ssss[i]>='a'&&cnt[ssss[i]-'a']){
				cnt[ssss[i]-'a']--;
				col[i]=yellow;
				if(!bad[ssss[i]-'a']) bad[ssss[i]-'a']=2;
				ssss[i]=ssss[i]-'a'+'A';
			}
		}
		for(int i=0;i<(int)(words[id].size());i++){
			if(ssss[i]>='a'){
				col[i]=red;
				if(!bad[ssss[i]-'a']) bad[ssss[i]-'a']=1;
				ssss[i]=ssss[i]-'a'+'A';
			}
		}
    	for(int i=0;i<(int)(words[id].size());i++)
			hist[round].push_back(make_pair(ssss[i]-'A'+'a',col[i]));
		if(GR==(int)(words[id].size())){
			printstate(round,id,round);
			printf("Guessed correctly after %d tries.\n",round);
			cout<<"Score:"<<flush;
			clr(max(25-round,5)*100);
			cout<<max(25-round,5)*100<<endl;
			scta(back);
			rat.second=min(rat.second+1,1000);
			rat.first=(rat.first*(rat.second-1)+max(25-round,5)*100)/rat.second;
			strv["RATING_"+strv["ACCOUNT"]]=encode(rat.first,rat.second);
			writeconf();
			scta(back);
			system("pause");
			return;
		}
	}
}
void about(){
	system("cls");
	scta(back);
	cout<<"About"<<endl;
	cout<<"WORDLE (C++) Version "<<ver<<" ("<<date<<") by David X"<<endl;
	cout<<"Copyright (C) DavidX 2022-2023.All rights reserved."<<endl;
	system("pause");
}
void rules(){
	system("cls");
	scta(back);
	cout<<"Help > Rules"<<endl;
	cout<<"You have "<<vals["TURNS"]<<" chances to guess a secret word."<<endl;
	cout<<"If you got a letter right,and in the same position,it will turn "<<flush;
	scta(green);cout<<"green"<<flush;scta(back);
	cout<<"."<<endl;
	cout<<"If you got a letter right,but in the wrong position,it will turn "<<flush;
	scta(yellow);cout<<"yellow"<<flush;scta(back);
	cout<<"."<<endl;
	cout<<"If you got a letter wrong,it will turn "<<flush;
	scta(red);cout<<"red"<<flush;scta(back);
	cout<<"."<<endl;
	cout<<"If the word isn't in the dictionary,it will be shown in "<<flush;
	scta(blue);cout<<"blue"<<flush;scta(back);
	cout<<"."<<endl;
	cout<<"If the word is too long/short,it will be shown in "<<flush;
	scta(purple);cout<<"purple"<<flush;scta(back);
	cout<<"."<<endl;
	cout<<"When you want to give up,type \"help!\" to see the answer."<<endl;
	system("pause");
}
void rating(){
	system("cls");
	scta(back);
	cout<<"Help > Rating"<<endl;
	cout<<"In every round of the game,you will be given a score."<<endl;
	cout<<"It depends on how quickly you guessed the word correctly."<<endl;
	cout<<"Your rating is the average score of all your games."<<endl;
	cout<<"Ratings are shown in different colors:"<<endl;
	clr(0);cout<<"0000"<<flush;scta(back);cout<<" to "<<flush;
	clr(799);cout<<"0799"<<flush;scta(back);cout<<" : ";
	clr(799);cout<<"newbie"<<endl;scta(back);
	clr(800);cout<<"0800"<<flush;scta(back);cout<<" to "<<flush;
	clr(999);cout<<"0999"<<flush;scta(back);cout<<" : ";
	clr(999);cout<<"pupil"<<endl;scta(back);
	clr(1000);cout<<"1000"<<flush;scta(back);cout<<" to "<<flush;
	clr(1299);cout<<"1299"<<flush;scta(back);cout<<" : ";
	clr(1299);cout<<"expert"<<endl;scta(back);
	clr(1300);cout<<"1300"<<flush;scta(back);cout<<" to "<<flush;
	clr(1699);cout<<"1699"<<flush;scta(back);cout<<" : ";
	clr(1699);cout<<"master"<<endl;scta(back);
	clr(1700);cout<<"1700"<<flush;scta(back);cout<<" to "<<flush;
	clr(1999);cout<<"1999"<<flush;scta(back);cout<<" : ";
	clr(1999);cout<<"grandmaster"<<endl;scta(back);
	clr(2000);cout<<"2000"<<flush;scta(back);cout<<" to "<<flush;
	clr(2400);cout<<"2400"<<flush;scta(back);cout<<" : ";
	clr(2400);cout<<"lengendary"<<endl;scta(back);
	system("pause");
}
void help(){
	while(1){
		system("cls");
		cout<<"Help"<<endl;
		cout<<"1)   rules"<<endl;
		cout<<"2)   rating system"<<endl;
		cout<<"0)   exit help"<<endl;
		cout<<"Please choose one (index):"<<flush;
		int t=getch();
		cout<<(char)(t)<<endl;
		if(t=='1') rules();
		if(t=='2') rating();
		if(t=='0') return;
	}
}
void readconf(){
	vals.clear();
	ifstream conf(config.data());
	string field,val_string;
	int val_int;
	while(conf>>field){
		if(field[0]=='['){
			conf>>val_int;
			vals[field.substr(1,field.size()-2)]=val_int;
		}
		else{
			conf>>val_string;
			strv[field.substr(1,field.size()-2)]=val_string;
		}
	}
	conf.close();
}
void writeconf(){
	ofstream conf(config.data());
	for(pair<string,int> conff:vals){
		conf<<"["<<conff.first<<"]"<<endl;
		conf<<conff.second<<endl;
	}
	for(pair<string,string> conff:strv){
		conf<<"<"<<conff.first<<">"<<endl;
		conf<<conff.second<<endl;
	}
	conf.close();
}
void settings(){
	while(1){
		system("cls");
		cout<<"Settings"<<endl;
		for(pair<string,int> conff:vals) if(desc.count(conff.first)){
			cout<<conff.first<<"="<<conff.second<<endl;
		}
		for(pair<string,string> conff:strv) if(desc.count(conff.first)){
			cout<<conff.first<<"="<<conff.second<<endl;
		}
		cout<<"0)   exit settings"<<endl;
		cout<<"1)   edit configuration"<<endl;
		cout<<"Please choose one (index):"<<flush;
		int t=getch();
		cout<<(char)(t)<<endl;
		if(t=='0'){
			writeconf();
			return;
		}
		if(t=='1'){
			cout<<"Name:"<<flush;
			string S;
			cin>>S;
			if(!desc.count(S)){
				cout<<"Value doesn't exist!"<<endl;
				system("pause");
				continue;
			}
			if(vals.count(S)){
				cout<<"Description:"<<desc[S]<<endl;
				cout<<"Old value:"<<vals[S]<<endl;
				cout<<"New value(leave blank to keep old value):"<<flush;
				string tmp;
				getline(cin,tmp);
				getline(cin,tmp);
				if(tmp=="") continue;
				int T=0;
				for(int i=0;i<(int)(tmp.size());i++) T=(T*10+tmp[i]-'0');
				vals[S]=T;
			}
			if(strv.count(S)){
				cout<<"Description:"<<desc[S]<<endl;
				cout<<"Old value:"<<strv[S]<<endl;
				cout<<"New value(leave blank to keep old value):"<<flush;
				string tmp;
				getline(cin,tmp);
				getline(cin,tmp);
				if(tmp=="") continue;
				strv[S]=tmp;
			}
		}
	}
}
int download_file(string pth,string out){
	string url="http://gh.api.99988866.xyz/https://raw.githubusercontent.com/Davidasx/wordle-cpp/main/"+pth;
	string cmd="curl \""+url+"\" --silent -o "+out;
	return system(cmd.data());
}
int download_release(string rls,string fil,string out){
	string url="http://gh.api.99988866.xyz/https://github.com/Davidasx/wordle-cpp/releases/download/"+rls+"/"+fil;
	string cmd="curl \""+url+"\" --silent -o "+out;
	return system(cmd.data());
}
bool later(string a,string b){
	int cur=0;
	vector<int> va,vb;
	cur=0;
	for(int i=0;i<(int)(a.size());i++){
		if(a[i]=='.') va.push_back(cur),cur=0;
		else cur=(cur*10+a[i]-'0');
	}
	va.push_back(cur);
	cur=0;
	for(int i=0;i<(int)(b.size());i++){
		if(b[i]=='.') vb.push_back(cur),cur=0;
		else cur=(cur*10+b[i]-'0');
	}
	vb.push_back(cur);
	while((int)(va.size())<(int)(vb.size())) va.push_back(0);
	while((int)(vb.size())<(int)(va.size())) vb.push_back(0);
	for(int i=0;i<(int)(va.size());i++){
		if(va[i]>vb[i]) return true;
		if(vb[i]>va[i]) return false;
	}
	return true;
}
void update(){
	system("cls");
	cout<<"Update"<<endl;
	if(system("where curl /Q")){
		cout<<"CURL wasn't found on this computer."<<endl;
		cout<<"Please install CURL to use the update function."<<endl;
		system("pause");
		return;
	}
	cout<<"Current Version:"<<ver<<endl;
	int ret=download_file("latest.txt","wdlatest.txt");
	if(ret){
		cout<<"Failed to check latest version."<<endl;
		system("pause");
		return;
	}
	string lver;
	ifstream vread("wdlatest.txt");
	vread>>lver;
	vread.close();
	system("del wdlatest.txt");
	if(later(ver,lver)){
		cout<<"Current version is latest."<<endl;
		system("pause");
		return;
	}
	else{
		cout<<"Latest version:"<<lver<<endl;
		cout<<"0)   exit"<<endl;
		cout<<"1)   download"<<endl;
		cout<<"Please choose one (index):"<<flush;
		int t=getch();
		cout<<(char)(t)<<endl;
		if(t=='0') return;
		if(t=='1'){
			cout<<"Downloading..."<<endl;
			if(havecpp){
				ret=download_release(lver,"wordle.cpp","wordle_new_update.cpp");
				if(ret){
					cout<<"Failed to download."<<endl;
					system("pause");
					return;
				}
			}
			ret=download_release(lver,"wordle.exe","wordle_new_update.exe");
			if(ret){
				cout<<"Failed to download."<<endl;
				if(havecpp)
					system("del wordle_new_update.cpp");
				system("pause");
				return;
			}
			writeconf();
			ofstream fout("wdupdater.bat");
			fout<<"@echo off"<<endl;
			fout<<"pause"<<endl;
			fout<<"copy wordle_new_update.exe "<<progname<<endl;
			if(havecpp)
				fout<<"copy wordle_new_update.cpp "<<cppname<<endl;
			fout<<"del wordle_new_update.exe"<<endl;
			if(havecpp)
				fout<<"del wordle_new_update.cpp"<<endl;
			fout<<"cls"<<endl;
			fout<<"start "<<progname<<endl;
			fout<<"exit"<<endl;
			fout.close();
			system("start wdupdater.bat");
			exit(0);
		}
	}
}
void account(){
	system("cls");
	cout<<"Change Account"<<endl;
	cout<<"Current account:"<<strv["ACCOUNT"]<<endl;
	cout<<"New account(leave blank to keep old account):"<<flush;
	string tmp;
	getline(cin,tmp);
	if(tmp!=""){
		system("pause");
		strv["ACCOUNT"]=tmp;
		if(!strv["RATING_"+strv["ACCOUNT"]].size())
			strv["RATING_"+strv["ACCOUNT"]]=encode(0,0);
		writeconf();
	}
}
int main(int argc,char** argv){
	system("title Wordle");
	ifstream test_update("wdupdater.bat");
	if(!test_update) test_update.close();
	else{
		test_update.close();
		system("del wdupdater.bat");
	}
	progname=argv[0];
	string tmpname="";
	for(int i=(int)(progname.size())-1;i>=0;i--){
		if(progname[i]=='\\') break;
		tmpname+=progname[i];
	}
	reverse(tmpname.begin(),tmpname.end());
	progname=tmpname;
	cppname=progname.substr(0,progname.size()-3)+"cpp";
	ifstream cpptest(cppname.data());
	if(!cpptest) havecpp=false;
	else havecpp=true;
	cpptest.close();
	for(int i=0;i<(int)(words.size());i++) for(int j=0;j<words[i].size();j++)
		words[i][j]-='A',words[i][j]+='a';
	system("cls");
	system("date /t > curtimewd.txt");
	ifstream tfin("curtimewd.txt");
	string tim;tfin>>tim;
	tfin.close();system("del curtimewd.txt");
	string ntim=tim.substr(0,4)+tim.substr(5,2)+tim.substr(8,2);
	tim=ntim;
	system("echo %USERPROFILE% > userrootwd.txt");
	ifstream ufin("userrootwd.txt");
	ufin>>user_root;
	ufin.close();system("del userrootwd.txt");
	config=user_root+config;
	ifstream tmpconf(config.data());
	if(!tmpconf){
		tmpconf.close();
		ofstream confgen(config.data());
		confgen<<"[TURNS]"<<endl;
		confgen<<20<<endl;
		confgen<<"<VERSION>"<<endl;
		confgen<<ver<<endl;
		confgen<<"<ACCOUNT>"<<endl;
		confgen<<"Visitor"<<endl;
		confgen.close();
	}
	else tmpconf.close();
	readconf();
	if(!vals["TURNS"]) vals["TURNS"]=20;
	if(!strv["ACCOUNT"].size()) strv["ACCOUNT"]="Visitor";
	if(!strv["RATING_"+strv["ACCOUNT"]].size()) strv["RATING_"+strv["ACCOUNT"]]=encode(0,0);
	if(!strv["VERSION"].size()) strv["VERSION"]=ver;
	writeconf();
	if(ver!=strv["VERSION"]){
		strv["VERSION"]=ver;
		cout<<"Welcome to WORDLE "<<ver<<"!"<<endl;
		writeconf();
		system("pause");
	}
	scta(back);
	srand(unsigned(time(NULL)));
	prep();
	while(1){
		system("cls");
		rat=decode(strv["RATING_"+strv["ACCOUNT"]]);
		cout<<"Hello,"<<flush;
		clr(rat.first);
		cout<<strv["ACCOUNT"]<<flush;
		scta(back);
		cout<<"!"<<endl;
		cout<<"WORDLE (C++) Version "<<ver<<" ("<<date<<") by David X"<<endl;
		cout<<"Current Rating:"<<flush;
		clr(rat.first);
		if(rat.first<1000) cout<<0;
		if(rat.first<100) cout<<0;
		if(rat.first<10) cout<<0;
		cout<<rat.first<<endl;
		scta(back);
		cout<<"Current Level:"<<flush;
		clr(rat.first);
		cout<<level(rat.first)<<endl;
		scta(back);
		cout<<"1)   guess word"<<endl;
		cout<<"2)   help"<<endl;
		cout<<"3)   settings"<<endl;
		cout<<"4)   update"<<endl;
		cout<<"5)   change account"<<endl;
		cout<<"9)   about"<<endl;
		cout<<"0)   exit"<<endl;
		cout<<"Please choose one (index):"<<flush;
		int t=getch();
		cout<<(char)(t)<<endl;
		if(t=='1') wordle();
		if(t=='2') help();
		if(t=='3') settings();
		if(t=='4') update();
		if(t=='5') account();
		if(t=='9') about();
		if(t=='0') return 0;
	}
	return 0;
}

