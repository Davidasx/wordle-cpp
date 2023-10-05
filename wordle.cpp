#include<bits/stdc++.h>
#include<conio.h>
#include<windows.h>
#include<graphics.h>
using namespace std;
string ver="3.0.0";
string date="20231005";
string config="\\wordle.conf";
string user_root;
string progname,cppname;
bool havecpp;
vector<string> answers={"ABACK","ABASE","ABATE","ABBEY","ABBOT","ABHOR","ABIDE","ABLED","ABODE","ABORT","ABOUT","ABOVE","ABUSE","ABYSS","ACORN","ACRID","ACTOR","ACUTE","ADAGE","ADAPT","ADEPT","ADMIN","ADMIT","ADOBE","ADOPT","ADORE","ADORN","ADULT","AFFIX","AFIRE","AFOOT","AFOUL","AFTER","AGAIN","AGAPE","AGATE","AGENT","AGILE","AGING","AGLOW","AGONY","AGREE","AHEAD","AIDER","AISLE","ALARM","ALBUM","ALERT","ALGAE","ALIBI","ALIEN","ALIGN","ALIKE","ALIVE","ALLAY","ALLEY","ALLOT","ALLOW","ALLOY","ALOFT","ALONE","ALONG","ALOOF","ALOUD","ALPHA","ALTAR","ALTER","AMASS","AMAZE","AMBER","AMBLE","AMEND","AMISS","AMITY","AMONG","AMPLE","AMPLY","AMUSE","ANGEL","ANGER","ANGLE","ANGRY","ANGST","ANIME","ANKLE","ANNEX","ANNOY","ANNUL","ANODE","ANTIC","ANVIL","AORTA","APART","APHID","APING","APNEA","APPLE","APPLY","APRON","APTLY","ARBOR","ARDOR","ARENA","ARGUE","ARISE","ARMOR","AROMA","AROSE","ARRAY","ARROW","ARSON","ARTSY","ASCOT","ASHEN","ASIDE","ASKEW","ASSAY","ASSET","ATOLL","ATONE","ATTIC","AUDIO","AUDIT","AUGUR","AUNTY","AVAIL","AVERT","AVIAN","AVOID","AWAIT","AWAKE","AWARD","AWARE","AWASH","AWFUL","AWOKE","AXIAL","AXIOM","AXION","AZURE","BACON","BADGE","BADLY","BAGEL","BAGGY","BAKER","BALER","BALMY","BANAL","BANJO","BARGE","BARON","BASAL","BASIC","BASIL","BASIN","BASIS","BASTE","BATCH","BATHE","BATON","BATTY","BAWDY","BAYOU","BEACH","BEADY","BEARD","BEAST","BEECH","BEEFY","BEFIT","BEGAN","BEGAT","BEGET","BEGIN","BEGUN","BEING","BELCH","BELIE","BELLE","BELLY","BELOW","BENCH","BERET","BERRY","BERTH","BESET","BETEL","BEVEL","BEZEL","BIBLE","BICEP","BIDDY","BIGOT","BILGE","BILLY","BINGE","BINGO","BIOME","BIRCH","BIRTH","BISON","BITTY","BLACK","BLADE","BLAME","BLAND","BLANK","BLARE","BLAST","BLAZE","BLEAK","BLEAT","BLEED","BLEEP","BLEND","BLESS","BLIMP","BLIND","BLINK","BLISS","BLITZ","BLOAT","BLOCK","BLOKE","BLOND","BLOOD","BLOOM","BLOWN","BLUER","BLUFF","BLUNT","BLURB","BLURT","BLUSH","BOARD","BOAST","BOBBY","BONEY","BONGO","BONUS","BOOBY","BOOST","BOOTH","BOOTY","BOOZE","BOOZY","BORAX","BORNE","BOSOM","BOSSY","BOTCH","BOUGH","BOULE","BOUND","BOWEL","BOXER","BRACE","BRAID","BRAIN","BRAKE","BRAND","BRASH","BRASS","BRAVE","BRAVO","BRAWL","BRAWN","BREAD","BREAK","BREED","BRIAR","BRIBE","BRICK","BRIDE","BRIEF","BRINE","BRING","BRINK","BRINY","BRISK","BROAD","BROIL","BROKE","BROOD","BROOK","BROOM","BROTH","BROWN","BRUNT","BRUSH","BRUTE","BUDDY","BUDGE","BUGGY","BUGLE","BUILD","BUILT","BULGE","BULKY","BULLY","BUNCH","BUNNY","BURLY","BURNT","BURST","BUSED","BUSHY","BUTCH","BUTTE","BUXOM","BUYER","BYLAW","CABAL","CABBY","CABIN","CABLE","CACAO","CACHE","CACTI","CADDY","CADET","CAGEY","CAIRN","CAMEL","CAMEO","CANAL","CANDY","CANNY","CANOE","CANON","CAPER","CAPUT","CARAT","CARGO","CAROL","CARRY","CARVE","CASTE","CATCH","CATER","CATTY","CAULK","CAUSE","CAVIL","CEASE","CEDAR","CELLO","CHAFE","CHAFF","CHAIN","CHAIR","CHALK","CHAMP","CHANT","CHAOS","CHARD","CHARM","CHART","CHASE","CHASM","CHEAP","CHEAT","CHECK","CHEEK","CHEER","CHESS","CHEST","CHICK","CHIDE","CHIEF","CHILD","CHILI","CHILL","CHIME","CHINA","CHIRP","CHOCK","CHOIR","CHOKE","CHORD","CHORE","CHOSE","CHUCK","CHUMP","CHUNK","CHURN","CHUTE","CIDER","CIGAR","CINCH","CIRCA","CIVIC","CIVIL","CLACK","CLAIM","CLAMP","CLANG","CLANK","CLASH","CLASP","CLASS","CLEAN","CLEAR","CLEAT","CLEFT","CLERK","CLICK","CLIFF","CLIMB","CLING","CLINK","CLOAK","CLOCK","CLONE","CLOSE","CLOTH","CLOUD","CLOUT","CLOVE","CLOWN","CLUCK","CLUED","CLUMP","CLUNG","COACH","COAST","COBRA","COCOA","COLON","COLOR","COMET","COMFY","COMIC","COMMA","CONCH","CONDO","CONIC","COPSE","CORAL","CORER","CORNY","COUCH","COUGH","COULD","COUNT","COUPE","COURT","COVEN","COVER","COVET","COVEY","COWER","COYLY","CRACK","CRAFT","CRAMP","CRANE","CRANK","CRASH","CRASS","CRATE","CRAVE","CRAWL","CRAZE","CRAZY","CREAK","CREAM","CREDO","CREED","CREEK","CREEP","CREME","CREPE","CREPT","CRESS","CREST","CRICK","CRIED","CRIER","CRIME","CRIMP","CRISP","CROAK","CROCK","CRONE","CRONY","CROOK","CROSS","CROUP","CROWD","CROWN","CRUDE","CRUEL","CRUMB","CRUMP","CRUSH","CRUST","CRYPT","CUBIC","CUMIN","CURIO","CURLY","CURRY","CURSE","CURVE","CURVY","CUTIE","CYBER","CYCLE","CYNIC","DADDY","DAILY","DAIRY","DAISY","DALLY","DANCE","DANDY","DATUM","DAUNT","DEALT","DEATH","DEBAR","DEBIT","DEBUG","DEBUT","DECAL","DECAY","DECOR","DECOY","DECRY","DEFER","DEIGN","DEITY","DELAY","DELTA","DELVE","DEMON","DEMUR","DENIM","DENSE","DEPOT","DEPTH","DERBY","DETER","DETOX","DEUCE","DEVIL","DIARY","DICEY","DIGIT","DILLY","DIMLY","DINER","DINGO","DINGY","DIODE","DIRGE","DIRTY","DISCO","DITCH","DITTO","DITTY","DIVER","DIZZY","DODGE","DODGY","DOGMA","DOING","DOLLY","DONOR","DONUT","DOPEY","DOUBT","DOUGH","DOWDY","DOWEL","DOWNY","DOWRY","DOZEN","DRAFT","DRAIN","DRAKE","DRAMA","DRANK","DRAPE","DRAWL","DRAWN","DREAD","DREAM","DRESS","DRIED","DRIER","DRIFT","DRILL","DRINK","DRIVE","DROIT","DROLL","DRONE","DROOL","DROOP","DROSS","DROVE","DROWN","DRUID","DRUNK","DRYER","DRYLY","DUCHY","DULLY","DUMMY","DUMPY","DUNCE","DUSKY","DUSTY","DUTCH","DUVET","DWARF","DWELL","DWELT","DYING","EAGER","EAGLE","EARLY","EARTH","EASEL","EATEN","EATER","EBONY","ECLAT","EDICT","EDIFY","EERIE","EGRET","EIGHT","EJECT","EKING","ELATE","ELBOW","ELDER","ELECT","ELEGY","ELFIN","ELIDE","ELITE","ELOPE","ELUDE","EMAIL","EMBED","EMBER","EMCEE","EMPTY","ENACT","ENDOW","ENEMA","ENEMY","ENJOY","ENNUI","ENSUE","ENTER","ENTRY","ENVOY","EPOCH","EPOXY","EQUAL","EQUIP","ERASE","ERECT","ERODE","ERROR","ERUPT","ESSAY","ESTER","ETHER","ETHIC","ETHOS","ETUDE","EVADE","EVENT","EVERY","EVICT","EVOKE","EXACT","EXALT","EXCEL","EXERT","EXILE","EXIST","EXPEL","EXTOL","EXTRA","EXULT","EYING","FABLE","FACET","FAINT","FAIRY","FAITH","FALSE","FANCY","FANNY","FARCE","FATAL","FATTY","FAULT","FAUNA","FAVOR","FEAST","FECAL","FEIGN","FELLA","FELON","FEMME","FEMUR","FENCE","FERAL","FERRY","FETAL","FETCH","FETID","FETUS","FEVER","FEWER","FIBER","FICUS","FIELD","FIEND","FIERY","FIFTH","FIFTY","FIGHT","FILER","FILET","FILLY","FILMY","FILTH","FINAL","FINCH","FINER","FIRST","FISHY","FIXER","FIZZY","FJORD","FLACK","FLAIL","FLAIR","FLAKE","FLAKY","FLAME","FLANK","FLARE","FLASH","FLASK","FLECK","FLEET","FLESH","FLICK","FLIER","FLING","FLINT","FLIRT","FLOAT","FLOCK","FLOOD","FLOOR","FLORA","FLOSS","FLOUR","FLOUT","FLOWN","FLUFF","FLUID","FLUKE","FLUME","FLUNG","FLUNK","FLUSH","FLUTE","FLYER","FOAMY","FOCAL","FOCUS","FOGGY","FOIST","FOLIO","FOLLY","FORAY","FORCE","FORGE","FORGO","FORTE","FORTH","FORTY","FORUM","FOUND","FOYER","FRAIL","FRAME","FRANK","FRAUD","FREAK","FREED","FREER","FRESH","FRIAR","FRIED","FRILL","FRISK","FRITZ","FROCK","FROND","FRONT","FROST","FROTH","FROWN","FROZE","FRUIT","FUDGE","FUGUE","FULLY","FUNGI","FUNKY","FUNNY","FUROR","FURRY","FUSSY","FUZZY","GAFFE","GAILY","GAMER","GAMMA","GAMUT","GASSY","GAUDY","GAUGE","GAUNT","GAUZE","GAVEL","GAWKY","GAYER","GAYLY","GAZER","GECKO","GEEKY","GEESE","GENIE","GENRE","GHOST","GHOUL","GIANT","GIDDY","GIPSY","GIRLY","GIRTH","GIVEN","GIVER","GLADE","GLAND","GLARE","GLASS","GLAZE","GLEAM","GLEAN","GLIDE","GLINT","GLOAT","GLOBE","GLOOM","GLORY","GLOSS","GLOVE","GLYPH","GNASH","GNOME","GODLY","GOING","GOLEM","GOLLY","GONAD","GONER","GOODY","GOOEY","GOOFY","GOOSE","GORGE","GOUGE","GOURD","GRACE","GRADE","GRAFT","GRAIL","GRAIN","GRAND","GRANT","GRAPE","GRAPH","GRASP","GRASS","GRATE","GRAVE","GRAVY","GRAZE","GREAT","GREED","GREEN","GREET","GRIEF","GRILL","GRIME","GRIMY","GRIND","GRIPE","GROAN","GROIN","GROOM","GROPE","GROSS","GROUP","GROUT","GROVE","GROWL","GROWN","GRUEL","GRUFF","GRUNT","GUARD","GUAVA","GUESS","GUEST","GUIDE","GUILD","GUILE","GUILT","GUISE","GULCH","GULLY","GUMBO","GUMMY","GUPPY","GUSTO","GUSTY","GYPSY","HABIT","HAIRY","HALVE","HANDY","HAPPY","HARDY","HAREM","HARPY","HARRY","HARSH","HASTE","HASTY","HATCH","HATER","HAUNT","HAUTE","HAVEN","HAVOC","HAZEL","HEADY","HEARD","HEART","HEATH","HEAVE","HEAVY","HEDGE","HEFTY","HEIST","HELIX","HELLO","HENCE","HERON","HILLY","HINGE","HIPPO","HIPPY","HITCH","HOARD","HOBBY","HOIST","HOLLY","HOMER","HONEY","HONOR","HORDE","HORNY","HORSE","HOTEL","HOTLY","HOUND","HOUSE","HOVEL","HOVER","HOWDY","HUMAN","HUMID","HUMOR","HUMPH","HUMUS","HUNCH","HUNKY","HURRY","HUSKY","HUSSY","HUTCH","HYDRO","HYENA","HYMEN","HYPER","ICILY","ICING","IDEAL","IDIOM","IDIOT","IDLER","IDYLL","IGLOO","ILIAC","IMAGE","IMBUE","IMPEL","IMPLY","INANE","INBOX","INCUR","INDEX","INEPT","INERT","INFER","INGOT","INLAY","INLET","INNER","INPUT","INTER","INTRO","IONIC","IRATE","IRONY","ISLET","ISSUE","ITCHY","IVORY","JAUNT","JAZZY","JELLY","JERKY","JETTY","JEWEL","JIFFY","JOINT","JOIST","JOKER","JOLLY","JOUST","JUDGE","JUICE","JUICY","JUMBO","JUMPY","JUNTA","JUNTO","JUROR","KAPPA","KARMA","KAYAK","KEBAB","KHAKI","KINKY","KIOSK","KITTY","KNACK","KNAVE","KNEAD","KNEED","KNEEL","KNELT","KNIFE","KNOCK","KNOLL","KNOWN","KOALA","KRILL","LABEL","LABOR","LADEN","LADLE","LAGER","LANCE","LANKY","LAPEL","LAPSE","LARGE","LARVA","LASSO","LATCH","LATER","LATHE","LATTE","LAUGH","LAYER","LEACH","LEAFY","LEAKY","LEANT","LEAPT","LEARN","LEASE","LEASH","LEAST","LEAVE","LEDGE","LEECH","LEERY","LEFTY","LEGAL","LEGGY","LEMON","LEMUR","LEPER","LEVEL","LEVER","LIBEL","LIEGE","LIGHT","LIKEN","LILAC","LIMBO","LIMIT","LINEN","LINER","LINGO","LIPID","LITHE","LIVER","LIVID","LLAMA","LOAMY","LOATH","LOBBY","LOCAL","LOCUS","LODGE","LOFTY","LOGIC","LOGIN","LOOPY","LOOSE","LORRY","LOSER","LOUSE","LOUSY","LOVER","LOWER","LOWLY","LOYAL","LUCID","LUCKY","LUMEN","LUMPY","LUNAR","LUNCH","LUNGE","LUPUS","LURCH","LURID","LUSTY","LYING","LYMPH","LYRIC","MACAW","MACHO","MACRO","MADAM","MADLY","MAFIA","MAGIC","MAGMA","MAIZE","MAJOR","MAKER","MAMBO","MAMMA","MAMMY","MANGA","MANGE","MANGO","MANGY","MANIA","MANIC","MANLY","MANOR","MAPLE","MARCH","MARRY","MARSH","MASON","MASSE","MATCH","MATEY","MAUVE","MAXIM","MAYBE","MAYOR","MEALY","MEANT","MEATY","MECCA","MEDAL","MEDIA","MEDIC","MELEE","MELON","MERCY","MERGE","MERIT","MERRY","METAL","METER","METRO","MICRO","MIDGE","MIDST","MIGHT","MILKY","MIMIC","MINCE","MINER","MINIM","MINOR","MINTY","MINUS","MIRTH","MISER","MISSY","MOCHA","MODAL","MODEL","MODEM","MOGUL","MOIST","MOLAR","MOLDY","MONEY","MONTH","MOODY","MOOSE","MORAL","MORON","MORPH","MOSSY","MOTEL","MOTIF","MOTOR","MOTTO","MOULT","MOUND","MOUNT","MOURN","MOUSE","MOUTH","MOVER","MOVIE","MOWER","MUCKY","MUCUS","MUDDY","MULCH","MUMMY","MUNCH","MURAL","MURKY","MUSHY","MUSIC","MUSKY","MUSTY","MYRRH","NADIR","NAIVE","NANNY","NASAL","NASTY","NATAL","NAVAL","NAVEL","NEEDY","NEIGH","NERDY","NERVE","NEVER","NEWER","NEWLY","NICER","NICHE","NIECE","NIGHT","NINJA","NINNY","NINTH","NOBLE","NOBLY","NOISE","NOISY","NOMAD","NOOSE","NORTH","NOSEY","NOTCH","NOVEL","NUDGE","NURSE","NUTTY","NYLON","NYMPH","OAKEN","OBESE","OCCUR","OCEAN","OCTAL","OCTET","ODDER","ODDLY","OFFAL","OFFER","OFTEN","OLDEN","OLDER","OLIVE","OMBRE","OMEGA","ONION","ONSET","OPERA","OPINE","OPIUM","OPTIC","ORBIT","ORDER","ORGAN","OTHER","OTTER","OUGHT","OUNCE","OUTDO","OUTER","OUTGO","OVARY","OVATE","OVERT","OVINE","OVOID","OWING","OWNER","OXIDE","OZONE","PADDY","PAGAN","PAINT","PALER","PALSY","PANEL","PANIC","PANSY","PAPAL","PAPER","PARER","PARKA","PARRY","PARSE","PARTY","PASTA","PASTE","PASTY","PATCH","PATIO","PATSY","PATTY","PAUSE","PAYEE","PAYER","PEACE","PEACH","PEARL","PECAN","PEDAL","PENAL","PENCE","PENNE","PENNY","PERCH","PERIL","PERKY","PESKY","PESTO","PETAL","PETTY","PHASE","PHONE","PHONY","PHOTO","PIANO","PICKY","PIECE","PIETY","PIGGY","PILOT","PINCH","PINEY","PINKY","PINTO","PIPER","PIQUE","PITCH","PITHY","PIVOT","PIXEL","PIXIE","PIZZA","PLACE","PLAID","PLAIN","PLAIT","PLANE","PLANK","PLANT","PLATE","PLAZA","PLEAD","PLEAT","PLIED","PLIER","PLUCK","PLUMB","PLUME","PLUMP","PLUNK","PLUSH","POESY","POINT","POISE","POKER","POLAR","POLKA","POLYP","POOCH","POPPY","PORCH","POSER","POSIT","POSSE","POUCH","POUND","POUTY","POWER","PRANK","PRAWN","PREEN","PRESS","PRICE","PRICK","PRIDE","PRIED","PRIME","PRIMO","PRINT","PRIOR","PRISM","PRIVY","PRIZE","PROBE","PRONE","PRONG","PROOF","PROSE","PROUD","PROVE","PROWL","PROXY","PRUDE","PRUNE","PSALM","PUBIC","PUDGY","PUFFY","PULPY","PULSE","PUNCH","PUPIL","PUPPY","PUREE","PURER","PURGE","PURSE","PUSHY","PUTTY","PYGMY","QUACK","QUAIL","QUAKE","QUALM","QUARK","QUART","QUASH","QUASI","QUEEN","QUEER","QUELL","QUERY","QUEST","QUEUE","QUICK","QUIET","QUILL","QUILT","QUIRK","QUITE","QUOTA","QUOTE","QUOTH","RABBI","RABID","RACER","RADAR","RADII","RADIO","RAINY","RAISE","RAJAH","RALLY","RALPH","RAMEN","RANCH","RANDY","RANGE","RAPID","RARER","RASPY","RATIO","RATTY","RAVEN","RAYON","RAZOR","REACH","REACT","READY","REALM","REARM","REBAR","REBEL","REBUS","REBUT","RECAP","RECUR","RECUT","REEDY","REFER","REFIT","REGAL","REHAB","REIGN","RELAX","RELAY","RELIC","REMIT","RENAL","RENEW","REPAY","REPEL","REPLY","RERUN","RESET","RESIN","RETCH","RETRO","RETRY","REUSE","REVEL","REVUE","RHINO","RHYME","RIDER","RIDGE","RIFLE","RIGHT","RIGID","RIGOR","RINSE","RIPEN","RIPER","RISEN","RISER","RISKY","RIVAL","RIVER","RIVET","ROACH","ROAST","ROBIN","ROBOT","ROCKY","RODEO","ROGER","ROGUE","ROOMY","ROOST","ROTOR","ROUGE","ROUGH","ROUND","ROUSE","ROUTE","ROVER","ROWDY","ROWER","ROYAL","RUDDY","RUDER","RUGBY","RULER","RUMBA","RUMOR","RUPEE","RURAL","RUSTY","SADLY","SAFER","SAINT","SALAD","SALLY","SALON","SALSA","SALTY","SALVE","SALVO","SANDY","SANER","SAPPY","SASSY","SATIN","SATYR","SAUCE","SAUCY","SAUNA","SAUTE","SAVOR","SAVOY","SAVVY","SCALD","SCALE","SCALP","SCALY","SCAMP","SCANT","SCARE","SCARF","SCARY","SCENE","SCENT","SCION","SCOFF","SCOLD","SCONE","SCOOP","SCOPE","SCORE","SCORN","SCOUR","SCOUT","SCOWL","SCRAM","SCRAP","SCREE","SCREW","SCRUB","SCRUM","SCUBA","SEDAN","SEEDY","SEGUE","SEIZE","SEMEN","SENSE","SEPIA","SERIF","SERUM","SERVE","SETUP","SEVEN","SEVER","SEWER","SHACK","SHADE","SHADY","SHAFT","SHAKE","SHAKY","SHALE","SHALL","SHALT","SHAME","SHANK","SHAPE","SHARD","SHARE","SHARK","SHARP","SHAVE","SHAWL","SHEAR","SHEEN","SHEEP","SHEER","SHEET","SHEIK","SHELF","SHELL","SHIED","SHIFT","SHINE","SHINY","SHIRE","SHIRK","SHIRT","SHOAL","SHOCK","SHONE","SHOOK","SHOOT","SHORE","SHORN","SHORT","SHOUT","SHOVE","SHOWN","SHOWY","SHREW","SHRUB","SHRUG","SHUCK","SHUNT","SHUSH","SHYLY","SIEGE","SIEVE","SIGHT","SIGMA","SILKY","SILLY","SINCE","SINEW","SINGE","SIREN","SISSY","SIXTH","SIXTY","SKATE","SKIER","SKIFF","SKILL","SKIMP","SKIRT","SKULK","SKULL","SKUNK","SLACK","SLAIN","SLANG","SLANT","SLASH","SLATE","SLEEK","SLEEP","SLEET","SLEPT","SLICE","SLICK","SLIDE","SLIME","SLIMY","SLING","SLINK","SLOOP","SLOPE","SLOSH","SLOTH","SLUMP","SLUNG","SLUNK","SLURP","SLUSH","SLYLY","SMACK","SMALL","SMART","SMASH","SMEAR","SMELL","SMELT","SMILE","SMIRK","SMITE","SMITH","SMOCK","SMOKE","SMOKY","SMOTE","SNACK","SNAIL","SNAKE","SNAKY","SNARE","SNARL","SNEAK","SNEER","SNIDE","SNIFF","SNIPE","SNOOP","SNORE","SNORT","SNOUT","SNOWY","SNUCK","SNUFF","SOAPY","SOBER","SOGGY","SOLAR","SOLID","SOLVE","SONAR","SONIC","SOOTH","SOOTY","SORRY","SOUND","SOUTH","SOWER","SPACE","SPADE","SPANK","SPARE","SPARK","SPASM","SPAWN","SPEAK","SPEAR","SPECK","SPEED","SPELL","SPELT","SPEND","SPENT","SPERM","SPICE","SPICY","SPIED","SPIEL","SPIKE","SPIKY","SPILL","SPILT","SPINE","SPINY","SPIRE","SPITE","SPLAT","SPLIT","SPOIL","SPOKE","SPOOF","SPOOK","SPOOL","SPOON","SPORE","SPORT","SPOUT","SPRAY","SPREE","SPRIG","SPUNK","SPURN","SPURT","SQUAD","SQUAT","SQUIB","STACK","STAFF","STAGE","STAID","STAIN","STAIR","STAKE","STALE","STALK","STALL","STAMP","STAND","STANK","STARE","STARK","START","STASH","STATE","STAVE","STEAD","STEAK","STEAL","STEAM","STEED","STEEL","STEEP","STEER","STEIN","STERN","STICK","STIFF","STILL","STILT","STING","STINK","STINT","STOCK","STOIC","STOKE","STOLE","STOMP","STONE","STONY","STOOD","STOOL","STOOP","STORE","STORK","STORM","STORY","STOUT","STOVE","STRAP","STRAW","STRAY","STRIP","STRUT","STUCK","STUDY","STUFF","STUMP","STUNG","STUNK","STUNT","STYLE","SUAVE","SUGAR","SUING","SUITE","SULKY","SULLY","SUMAC","SUNNY","SUPER","SURER","SURGE","SURLY","SUSHI","SWAMI","SWAMP","SWARM","SWASH","SWATH","SWEAR","SWEAT","SWEEP","SWEET","SWELL","SWEPT","SWIFT","SWILL","SWINE","SWING","SWIRL","SWISH","SWOON","SWOOP","SWORD","SWORE","SWORN","SWUNG","SYNOD","SYRUP","TABBY","TABLE","TABOO","TACIT","TACKY","TAFFY","TAINT","TAKEN","TAKER","TALLY","TALON","TAMER","TANGO","TANGY","TAPER","TAPIR","TARDY","TAROT","TASTE","TASTY","TATTY","TAUNT","TAWNY","TEACH","TEARY","TEASE","TEDDY","TEETH","TEMPO","TENET","TENOR","TENSE","TENTH","TEPEE","TEPID","TERRA","TERSE","TESTY","THANK","THEFT","THEIR","THEME","THERE","THESE","THETA","THICK","THIEF","THIGH","THING","THINK","THIRD","THONG","THORN","THOSE","THREE","THREW","THROB","THROW","THRUM","THUMB","THUMP","THYME","TIARA","TIBIA","TIDAL","TIGER","TIGHT","TILDE","TIMER","TIMID","TIPSY","TITAN","TITHE","TITLE","TOAST","TODAY","TODDY","TOKEN","TONAL","TONGA","TONIC","TOOTH","TOPAZ","TOPIC","TORCH","TORSO","TORUS","TOTAL","TOTEM","TOUCH","TOUGH","TOWEL","TOWER","TOXIC","TOXIN","TRACE","TRACK","TRACT","TRADE","TRAIL","TRAIN","TRAIT","TRAMP","TRASH","TRAWL","TREAD","TREAT","TREND","TRIAD","TRIAL","TRIBE","TRICE","TRICK","TRIED","TRIPE","TRITE","TROLL","TROOP","TROPE","TROUT","TROVE","TRUCE","TRUCK","TRUER","TRULY","TRUMP","TRUNK","TRUSS","TRUST","TRUTH","TRYST","TUBAL","TUBER","TULIP","TULLE","TUMOR","TUNIC","TURBO","TUTOR","TWANG","TWEAK","TWEED","TWEET","TWICE","TWINE","TWIRL","TWIST","TWIXT","TYING","UDDER","ULCER","ULTRA","UMBRA","UNCLE","UNCUT","UNDER","UNDID","UNDUE","UNFED","UNFIT","UNIFY","UNION","UNITE","UNITY","UNLIT","UNMET","UNSET","UNTIE","UNTIL","UNWED","UNZIP","UPPER","UPSET","URBAN","URINE","USAGE","USHER","USING","USUAL","USURP","UTILE","UTTER","VAGUE","VALET","VALID","VALOR","VALUE","VALVE","VAPID","VAPOR","VAULT","VAUNT","VEGAN","VENOM","VENUE","VERGE","VERSE","VERSO","VERVE","VICAR","VIDEO","VIGIL","VIGOR","VILLA","VINYL","VIOLA","VIPER","VIRAL","VIRUS","VISIT","VISOR","VISTA","VITAL","VIVID","VIXEN","VOCAL","VODKA","VOGUE","VOICE","VOILA","VOMIT","VOTER","VOUCH","VOWEL","VYING","WACKY","WAFER","WAGER","WAGON","WAIST","WAIVE","WALTZ","WARTY","WASTE","WATCH","WATER","WAVER","WAXEN","WEARY","WEAVE","WEDGE","WEEDY","WEIGH","WEIRD","WELCH","WELSH","WHACK","WHALE","WHARF","WHEAT","WHEEL","WHELP","WHERE","WHICH","WHIFF","WHILE","WHINE","WHINY","WHIRL","WHISK","WHITE","WHOLE","WHOOP","WHOSE","WIDEN","WIDER","WIDOW","WIDTH","WIELD","WIGHT","WILLY","WIMPY","WINCE","WINCH","WINDY","WISER","WISPY","WITCH","WITTY","WOKEN","WOMAN","WOMEN","WOODY","WOOER","WOOLY","WOOZY","WORDY","WORLD","WORRY","WORSE","WORST","WORTH","WOULD","WOUND","WOVEN","WRACK","WRATH","WREAK","WRECK","WREST","WRING","WRIST","WRITE","WRONG","WROTE","WRUNG","WRYLY","YACHT","YEARN","YEAST","YIELD","YOUNG","YOUTH","ZEBRA","ZESTY","ZONAL"};
vector<string> words;
HWND window;
int cursorx,cursory;
void clear_screen(){
	solidrectangle(0,0,1000,500);
	cursorx=cursory=0;
}
void putstring(string s){
	for(int i=0;i<(int)(s.size());i++)
		outtextxy(cursorx,cursory,s[i]),cursorx+=10;
}
void putccc(char c){
	outtextxy(cursorx,cursory,c);
	cursorx+=10;
}
void putint(int x){
	string ss;
	bool neg=false;
	if(x<0) neg=true,x=-x;
	while(x){
		ss+=(char)(x%10+'0');
		x/=10;
	}
	reverse(ss.begin(),ss.end());
	if((int)(ss.size())==0) ss="0";
	if(neg) ss="-"+x;
	putstring(ss);
}
void button(){
	putstring("  ");
	line(10,cursory-2,10,cursory+18);
	line(10,cursory+18,250,cursory+18);
	line(250,cursory+18,250,cursory-2);
	line(250,cursory-2,10,cursory-2);
}
void changeline(){
	cursory+=20;
	cursorx=0;
}
string readstring(){
	int basex=cursorx;
	string str;
	char c;
	ExMessage EM;
	int curpos=0;
	while(1){
		solidrectangle(basex,cursory,1000,cursory+20);
		cursorx=basex;
		putstring(str);
		line(basex+curpos*10,cursory,basex+curpos*10,cursory+20);
		while(1){
			EM=getmessage();
			if(EM.message==WM_CHAR){
				c=EM.ch;
				if(c==' ') c='_';
				else if(c=='\n'||c=='\r'){
					solidrectangle(basex,cursory,1000,cursory+20);
					cursorx=basex;
					putstring(str);
					changeline();
					return str;
				}
				else if(c=='\t'||c=='\b') continue; 
				str=str.substr(0,curpos)+c+str.substr(curpos);
				curpos++;
				break;
			}
			if(EM.message==WM_KEYDOWN){
				if(EM.vkcode==VK_LEFT) curpos=max(curpos-1,0);
				if(EM.vkcode==VK_RIGHT) curpos=min(curpos+1,(int)(str.size()));
				if(EM.vkcode==VK_BACK){
					if(curpos!=(int)(str.size()))
						str=str.substr(0,curpos-1)+str.substr(curpos+1);
					else
						str=str.substr(0,curpos-1);
					curpos--;
				}
				break;
			}
		}
	}
}
void pause(){
	MessageBoxA(window,LPCSTR("Continue?"),LPCSTR("Wordle"),MB_OK);
}
string user;
int back=RGB(0,0,0);
int red=RGB(255,0,0);
int green=RGB(0,255,0);
int blue=RGB(0,0,255);
int yellow=RGB(255,255,0);
int purple=RGB(255,0,255);
void scta(int clr){
	settextcolor(clr);
}
vector<pair<char,int> > hist[10010];
int cnt[26];
int col[10010];
vector<string> keyboard={"Q W E R T Y U I O P",
                         " A S D F G H J K L",
						 "  Z X C V B N M"};
int system(string s){
	return system(s.data());
}
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
	clear_screen();
	scta(back);
	putstring("Guess Word");changeline();
	putstring("Try #");putint(rounds_disp);changeline();
	for(int i=0;i<3;i++){
		for(int j=0;j<(int)(keyboard[i].size());j++){
			if(keyboard[i][j]!=' '){
				if(bad[keyboard[i][j]-'A']==0) scta(back);
				if(bad[keyboard[i][j]-'A']==1) scta(red);
				if(bad[keyboard[i][j]-'A']==2) scta(yellow);
				if(bad[keyboard[i][j]-'A']==3) scta(back);
			}
			putccc(keyboard[i][j]);
			scta(back);
		}
		changeline();
	}
	putint((int)(answers[wid].size()));
	putstring(" letters.");changeline();
	putstring(det);changeline();
	hist[rounds+1].clear();
	for(int i=1;i<=rounds;i++){
		for(int j=0;j<hist[i].size();j++){
			scta(hist[i][j].second);
			putccc(hist[i][j].first);
		}
		changeline();
	}
	scta(back);
}
void wordle(){
	clear_screen();
	int id=rand()%(int)(answers.size());
	scta(back);
	int round=0;
	memset(bad,0,sizeof(bad));
	det="";
	for(int i=0;i<(int)(answers[id].size());i++) det+="_";
	int hists=0;
	while(1){
		hists++;
		round++;
		if(round==vals["TURNS"]+1){
			printstate(hists-1,id,round-1);
			scta(red);
			putstring("GAME OVER!");changeline();
			scta(green);
			putstring("ANSWER: ");putstring(answers[id]);
			changeline();
			scta(back);
			int score=0;
			for(int i=0;i<(int)(answers[id].size());i++)
				if(det[i]!='_') score+=100;
			for(int i=0;i<3;i++)
				for(int j=0;j<(int)(keyboard[i].size());j++)
					if(keyboard[i][j]!=' '){
						if(bad[keyboard[i][j]-'A']==2)  score+=50;
					}
			putstring("Score:");putint(score);
			changeline();
			rat.second=min(rat.second+1,1000);
			rat.first=(rat.first*(rat.second-1)+score)/rat.second;
			strv["RATING_"+strv["ACCOUNT"]]=encode(rat.first,rat.second);
			writeconf();
			pause();
			return;
		}
		printstate(hists-1,id,round);
		string ssss=readstring();
		if(ssss=="help!"){
			printstate(hists-1,id,round-1);
			scta(red);
			putstring("YOU GAVE UP!");changeline();
			scta(green);
			putstring("ANSWER: ");putstring(answers[id]);
			changeline();
			scta(back);
			pause();
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
				hist[hists].push_back(make_pair(ssss[i],blue));
			round--;
			continue;
		}
    	for(int i=0;i<(int)(ssss.size());i++) if(ssss[i]<='Z')
    		ssss[i]=ssss[i]-'A'+'a';
		if((int)(ssss.size())!=(int)(answers[id].size())){
    		for(int i=0;i<(int)(ssss.size());i++)
				hist[hists].push_back(make_pair(ssss[i],purple));
			round--;
			continue;
		}
		flag=false;
		for(int i=0;i<words.size();i++) if(words[i]==ssss) flag=true;
		if(!flag){
    		for(int i=0;i<(int)(ssss.size());i++)
				hist[hists].push_back(make_pair(ssss[i],blue));
			round--;
			continue;
		}
		memset(cnt,0,sizeof(cnt));
		for(int i=0;i<(int)(answers[id].size());i++)
			cnt[answers[id][i]-'a']++;
		int GR=0;
		for(int i=0;i<(int)(answers[id].size());i++){
			if(ssss[i]==answers[id][i]){
				cnt[ssss[i]-'a']--;
				col[i]=green;
				det[i]=ssss[i];
				bad[ssss[i]-'a']=3;
				ssss[i]=ssss[i]-'a'+'A';
				GR++;
			}
		}
		for(int i=0;i<(int)(answers[id].size());i++){
			if(ssss[i]>='a'&&cnt[ssss[i]-'a']){
				cnt[ssss[i]-'a']--;
				col[i]=yellow;
				if(!bad[ssss[i]-'a']) bad[ssss[i]-'a']=2;
				ssss[i]=ssss[i]-'a'+'A';
			}
		}
		for(int i=0;i<(int)(answers[id].size());i++){
			if(ssss[i]>='a'){
				col[i]=red;
				if(!bad[ssss[i]-'a']) bad[ssss[i]-'a']=1;
				ssss[i]=ssss[i]-'a'+'A';
			}
		}
    	for(int i=0;i<(int)(answers[id].size());i++)
			hist[hists].push_back(make_pair(ssss[i]-'A'+'a',col[i]));
		if(GR==(int)(answers[id].size())){
			printstate(hists,id,round);
			putstring("Guessed correctly after ");
			putint(round);
			putstring(" tries.");changeline();
			putstring("Score:");
			clr(max(25-round,5)*100);
			putint(max(25-round,5)*100);
			changeline();
			scta(back);
			rat.second=min(rat.second+1,1000);
			rat.first=(rat.first*(rat.second-1)+max(25-round,5)*100)/rat.second;
			strv["RATING_"+strv["ACCOUNT"]]=encode(rat.first,rat.second);
			writeconf();
			scta(back);
			pause();
			return;
		}
	}
}
void about(){
	clear_screen();
	scta(back);
	putstring("About");changeline();
	putstring("WORDLE (C++) Version ");
	putstring(ver);
	putstring(" (");
	putstring(date);
	putstring(") by David X");
	changeline();
	putstring("Copyright (C) DavidX 2022-2023.All rights reserved.");
	changeline();
	pause();
}
void rules(){
	clear_screen();
	scta(back);
	putstring("Help > Rules");changeline();
	putstring("You have ");
	putint(vals["TURNS"]);
	putstring(" chances to guess a secret word.");changeline();
	putstring("If you got a letter right,and in the same position,it will turn ");
	scta(green);putstring("green");scta(back);
	putstring(".");changeline();
	putstring("If you got a letter right,but in the wrong position,it will turn ");
	scta(yellow);putstring("yellow");scta(back);
	putstring(".");changeline();
	putstring("If you got a letter wrong,it will turn ");
	scta(red);putstring("red");scta(back);
	putstring(".");changeline();
	putstring("If the word isn't in the dictionary,it will be shown in ");
	scta(blue);putstring("blue");scta(back);
	putstring(".");changeline();
	putstring("If the word is too long/short,it will be shown in ");
	scta(purple);putstring("purple");scta(back);
	putstring(".");changeline();
	putstring("When you want to give up,type \"help!\" to see the answer.");
	changeline();
	pause();
}
void rating(){
	clear_screen();
	scta(back);
	putstring("Help > Rating");changeline();
	putstring("In every round of the game,you will be given a score.");changeline();
	putstring("It depends on how quickly you guessed the word correctly.");changeline();
	putstring("Your rating is the average score of all your games.");changeline();
	putstring("Ratings are shown in different colors:");changeline();
	clr(0);putstring("0000");scta(back);putstring(" to ");
	clr(799);putstring("0799");scta(back);putstring(" : ");
	clr(799);putstring("newbie");changeline();scta(back);
	clr(800);putstring("0800");scta(back);putstring(" to ");
	clr(999);putstring("0999");scta(back);putstring(" : ");
	clr(999);putstring("pupil");changeline();scta(back);
	clr(1000);putstring("1000");scta(back);putstring(" to ");
	clr(1299);putstring("1299");scta(back);putstring(" : ");
	clr(1299);putstring("expert");changeline();scta(back);
	clr(1300);putstring("1300");scta(back);putstring(" to ");
	clr(1699);putstring("1699");scta(back);putstring(" : ");
	clr(1699);putstring("master");changeline();scta(back);
	clr(1700);putstring("1700");scta(back);putstring(" to ");
	clr(1999);putstring("1999");scta(back);putstring(" : ");
	clr(1999);putstring("grandmaster");changeline();scta(back);
	clr(2000);putstring("2000");scta(back);putstring(" to ");
	clr(2400);putstring("2400");scta(back);putstring(" : ");
	clr(2400);putstring("legendary");changeline();scta(back);
	pause();
}
void help(){
	while(1){
		clear_screen();
		putstring("Help");changeline();
		button();
		putstring("1)   rules");changeline();
		button();
		putstring("2)   rating system");changeline();
		button();
		putstring("0)   exit help");changeline();
		ExMessage EM;
		while(1){
			EM=getmessage();
			int t=0;
			if(EM.message==WM_CHAR){
				t=EM.ch-'0';
				if(t>=1&&t<=2) ;
				else if(t==0) t=3;
				else continue;
			}
			else if(EM.lbutton){
				if(EM.y>=18&&EM.y<=78&&EM.x>=10&&EM.x<=250){
					t=(EM.y+2)/20;
				}
				else continue;
			}
			else continue;
			if(t==1) rules();
			if(t==2) rating();
			if(t==3) return;
			break;
		}
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
		clear_screen();
		putstring("Settings");changeline();
		int LBASE=18;
		for(pair<string,int> conff:vals) if(desc.count(conff.first)){
			putstring(conff.first);putccc('=');
			putint(conff.second);changeline();
			LBASE+=20;
		}
		for(pair<string,string> conff:strv) if(desc.count(conff.first)){
			putstring(conff.first);putccc('=');
			putstring(conff.second);changeline();
			LBASE+=20;
		}
		button();
		putstring("1)   edit configuration");changeline();
		button();
		putstring("0)   exit settings");changeline();
		ExMessage EM;
		while(1){
			EM=getmessage();
			int t=0;
			if(EM.message==WM_CHAR){
				t=EM.ch-'0';
				if(t>=1&&t<=1) ;
				else if(t==0) t=2;
				else continue;
			}
			else if(EM.lbutton){
				if(EM.y>=LBASE&&EM.y<=LBASE+40&&EM.x>=10&&EM.x<=250){
					t=(EM.y-LBASE+20)/20;
				}
				else continue;
			}
			else continue;
			if(t==1){
				putstring("Name:");
				string S;
				S=readstring();
				if(!desc.count(S)){
					putstring("Value doesn't exist!");
					changeline();
					pause();
					break;
				}
				if(vals.count(S)){
					putstring("Description:");
					putstring(desc[S]);
					changeline();
					putstring("Old value:");
					putint(vals[S]);
					changeline();
					putstring("New value(leave blank to keep old value):");
					string tmp;
					tmp=readstring();
					if(tmp=="") break;
					int T=0;
					for(int i=0;i<(int)(tmp.size());i++) T=(T*10+tmp[i]-'0');
					vals[S]=T;
				}
				if(strv.count(S)){
					putstring("Description:");
					putstring(desc[S]);
					changeline();
					putstring("Old value:");
					putstring(strv[S]);
					changeline();
					putstring("New value(leave blank to keep old value):");
					string tmp;
					tmp=readstring();
					if(tmp=="") break;
					strv[S]=tmp;
				}
			}
			if(t==2){
				writeconf();
				return;
			}
			break;
		}
	}
}
int download_file(string pth,string out){
	string url="http://gh.api.99988866.xyz/https://raw.githubusercontent.com/Davidasx/wordle-cpp/main/"+pth;
	return system("curl \""+url+"\" --silent -o "+out);
}
int download_release(string rls,string fil,string out){
	putstring("Downloading ");
	putstring(rls);
	putstring("\\");
	putstring(fil);
	changeline();
	string url="http://gh.api.99988866.xyz/https://github.com/Davidasx/wordle-cpp/releases/download/"+rls+"/"+fil;
	return system("curl \""+url+"\" -o "+out);
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
	clear_screen();
	putstring("Update");changeline();
	if(system("where curl /Q")){
		putstring("CURL wasn't found on this computer.");
		changeline();
		putstring("Please install CURL to use the update function.");
		changeline();
		pause();
		return;
	}
	putstring("Current Version:");putstring(ver);
	changeline();
	int ret=download_file("latest.txt","wdlatest.txt");
	int ret2=download_file("assets.txt","wdassets.txt");
	if(ret||ret2){
		putstring("Failed to check latest version.");
		changeline();
		pause();
		return;
	}
	string lver;
	ifstream vread("wdlatest.txt");
	vread>>lver;
	vread.close();
	system("del wdlatest.txt");
	map<string,string> assets;
	ifstream aread("wdassets.txt");
	string file_name,update_ver;
	while(aread>>file_name>>update_ver){
		assets[file_name]=update_ver;
	}
	aread.close();
	system("del wdassets.txt");
	if(later(ver,lver)){
		putstring("Current version is latest.");changeline();
		pause();
		return;
	}
	else{
		putstring("Latest version:");putstring(lver);
		changeline();
		button();
		putstring("1)   download");changeline();
		putstring("0)   exit");changeline();
		ExMessage EM;
		while(1){
			EM=getmessage();
			int t=0;
			if(EM.message==WM_CHAR){
				t=EM.ch-'0';
				if(t>=1&&t<=1) ;
				else if(t==0) t=2;
				else continue;
			}
			else if(EM.lbutton){
				if(EM.y>=58&&EM.y<=98&&EM.x>=10&&EM.x<=250){
					t=(EM.y-38)/20;
				}
				else continue;
			}
			else continue;
			if(t==1){
				putstring("Downloading...");changeline();
				if(havecpp){
					ret=download_release(lver,"wordle.cpp","wordle_new_update.cpp");
					if(ret){
						putstring("Failed to download.");
						changeline();
						pause();
						return;
					}
				}
				ret=download_release(lver,"wordle.exe","wordle_new_update.exe");
				if(ret){
					putstring("Failed to download.");
					changeline();
					if(havecpp)
						system("del wordle_new_update.cpp");
					pause();
					return;
				}
				for(pair<string,string> P:assets){
					if(later(P.second,ver)){
						ret=download_release(P.second,P.first,P.first+"_temp");
						if(ret){
							putstring("Failed to download.");
							changeline();
							pause();
							return;
						}
					}
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
				for(pair<string,string> P:assets){
					if(later(P.second,ver)){
						fout<<"copy "<<P.first<<"_temp "<<P.first<<endl;
						fout<<"del "<<P.first<<"_temp"<<endl;
					}
				}
				fout<<"cls"<<endl;
				fout<<"start "<<progname<<endl;
				fout<<"exit"<<endl;
				fout.close();
				system("start wdupdater.bat");
				exit(0);
			}
			if(t==2) return;
			break;
		}
	}
}
void account(){
	clear_screen();
	putstring("Change Account");changeline();
	putstring("Change Account:");
	putstring(strv["ACCOUNT"]);
	changeline();
	putstring("New account(leave blank to keep old account):");
	string tmp;
	tmp=readstring();
	if(tmp!=""){
		pause();
		strv["ACCOUNT"]=tmp;
		if(!strv["RATING_"+strv["ACCOUNT"]].size())
			strv["RATING_"+strv["ACCOUNT"]]=encode(0,0);
		writeconf();
	}
}
int main(int argc,char** argv){
	window=initgraph(1000,500);
	setfillcolor(RGB(255,255,255));
	setlinecolor(RGB(0,0,0));
	settextcolor(RGB(0,0,0));
	setbkcolor(RGB(255,255,255));
	solidrectangle(0,0,1000,500);
	settextstyle(18,0,LPCTSTR("CONSOLAS"));
	ifstream reader("dictionary.txt");
	string WW;
	while(reader>>WW) words.push_back(WW);
	reader.close();
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
	for(int i=0;i<(int)(answers.size());i++) for(int j=0;j<answers[i].size();j++)
		answers[i][j]-='A',answers[i][j]+='a';
	for(int i=0;i<(int)(words.size());i++) for(int j=0;j<words[i].size();j++)
		words[i][j]-='A',words[i][j]+='a';
	clear_screen();
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
		putstring("Welcome to WORDLE ");
		putstring(ver);
		putstring("!");
		changeline();
		writeconf();
		pause();
	}
	scta(back);
	srand(unsigned(time(NULL)));
	prep();
	while(1){
		clear_screen();
		rat=decode(strv["RATING_"+strv["ACCOUNT"]]);
		putstring("Hello,");
		clr(rat.first);
		putstring(strv["ACCOUNT"]);
		scta(back);
		putstring("!");
		changeline();
		putstring("WORDLE (C++) Version ");
		putstring(ver);
		putstring(" (");
		putstring(date);
		putstring(") by David X");
		changeline();
		putstring("Current Rating:");
		clr(rat.first);
		if(rat.first<1000) putint(0);
		if(rat.first<100) putint(0);
		if(rat.first<10) putint(0);
		putint(rat.first);
		changeline();
		scta(back);
		putstring("Current Level:");
		clr(rat.first);
		putstring(level(rat.first));
		changeline();
		scta(back);
		button();
		putstring("1)   guess word");changeline();
		button();
		putstring("2)   help");changeline();
		button();
		putstring("3)   settings");changeline();
		button();
		putstring("4)   update");changeline();
		button();
		putstring("5)   change account");changeline();
		button();
		putstring("9)   about");changeline();
		button();
		putstring("0)   exit");changeline();
		ExMessage EM;
		while(1){
			EM=getmessage();
			int t=0;
			if(EM.message==WM_CHAR){
				t=EM.ch-'0';
				if(t>=1&&t<=5) ;
				else if(t==9) t=6;
				else if(t==0) t=7;
				else continue;
			}
			else if(EM.lbutton){
				if(EM.y>=78&&EM.y<=218&&EM.x>=10&&EM.x<=250){
					t=(EM.y-58)/20;
				}
				else continue;
			}
			else continue;
			if(t==1) wordle();
			if(t==2) help();
			if(t==3) settings();
			if(t==4) update();
			if(t==5) account();
			if(t==6) about();
			if(t==7) return 0;
			break;
		}
	}
	return 0;
}

