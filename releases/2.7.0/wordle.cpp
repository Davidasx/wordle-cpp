#include<bits/stdc++.h>
#include<conio.h>
#include<windows.h>
using namespace std;
string ver="2.7.0";
string date="20230529";
string config="\\wordle.conf";
string user_root;
string progname,cppname;
bool havecpp;
vector<string> answers={"aback","abase","abate","abbey","abbot","abhor","abide","abled","abode","abort","about","above","abuse","abyss","acorn","acrid","actor","acute","adage","adapt","adept","admin","admit","adobe","adopt","adore","adorn","adult","affix","afire","afoot","afoul","after","again","agape","agate","agent","agile","aging","aglow","agony","agree","ahead","aider","aisle","alarm","album","alert","algae","alibi","alien","align","alike","alive","allay","alley","allot","allow","alloy","aloft","alone","along","aloof","aloud","alpha","altar","alter","amass","amaze","amber","amble","amend","amiss","amity","among","ample","amply","amuse","angel","anger","angle","angry","angst","anime","ankle","annex","annoy","annul","anode","antic","anvil","aorta","apart","aphid","aping","apnea","apple","apply","apron","aptly","arbor","ardor","arena","argue","arise","armor","aroma","arose","array","arrow","arson","artsy","ascot","ashen","aside","askew","assay","asset","atoll","atone","attic","audio","audit","augur","aunty","avail","avert","avian","avoid","await","awake","award","aware","awash","awful","awoke","axial","axiom","axion","azure","bacon","badge","badly","bagel","baggy","baker","baler","balmy","banal","banjo","barge","baron","basal","basic","basil","basin","basis","baste","batch","bathe","baton","batty","bawdy","bayou","beach","beady","beard","beast","beech","beefy","befit","began","begat","beget","begin","begun","being","belch","belie","belle","belly","below","bench","beret","berry","berth","beset","betel","bevel","bezel","bible","bicep","biddy","bigot","bilge","billy","binge","bingo","biome","birch","birth","bison","bitty","black","blade","blame","bland","blank","blare","blast","blaze","bleak","bleat","bleed","bleep","blend","bless","blimp","blind","blink","bliss","blitz","bloat","block","bloke","blond","blood","bloom","blown","bluer","bluff","blunt","blurb","blurt","blush","board","boast","bobby","boney","bongo","bonus","booby","boost","booth","booty","booze","boozy","borax","borne","bosom","bossy","botch","bough","boule","bound","bowel","boxer","brace","braid","brain","brake","brand","brash","brass","brave","bravo","brawl","brawn","bread","break","breed","briar","bribe","brick","bride","brief","brine","bring","brink","briny","brisk","broad","broil","broke","brood","brook","broom","broth","brown","brunt","brush","brute","buddy","budge","buggy","bugle","build","built","bulge","bulky","bully","bunch","bunny","burly","burnt","burst","bused","bushy","butch","butte","buxom","buyer","bylaw","cabal","cabby","cabin","cable","cacao","cache","cacti","caddy","cadet","cagey","cairn","camel","cameo","canal","candy","canny","canoe","canon","caper","caput","carat","cargo","carol","carry","carve","caste","catch","cater","catty","caulk","cause","cavil","cease","cedar","cello","chafe","chaff","chain","chair","chalk","champ","chant","chaos","chard","charm","chart","chase","chasm","cheap","cheat","check","cheek","cheer","chess","chest","chick","chide","chief","child","chili","chill","chime","china","chirp","chock","choir","choke","chord","chore","chose","chuck","chump","chunk","churn","chute","cider","cigar","cinch","circa","civic","civil","clack","claim","clamp","clang","clank","clash","clasp","class","clean","clear","cleat","cleft","clerk","click","cliff","climb","cling","clink","cloak","clock","clone","close","cloth","cloud","clout","clove","clown","cluck","clued","clump","clung","coach","coast","cobra","cocoa","colon","color","comet","comfy","comic","comma","conch","condo","conic","copse","coral","corer","corny","couch","cough","could","count","coupe","court","coven","cover","covet","covey","cower","coyly","crack","craft","cramp","crane","crank","crash","crass","crate","crave","crawl","craze","crazy","creak","cream","credo","creed","creek","creep","creme","crepe","crept","cress","crest","crick","cried","crier","crime","crimp","crisp","croak","crock","crone","crony","crook","cross","croup","crowd","crown","crude","cruel","crumb","crump","crush","crust","crypt","cubic","cumin","curio","curly","curry","curse","curve","curvy","cutie","cyber","cycle","cynic","daddy","daily","dairy","daisy","dally","dance","dandy","datum","daunt","dealt","death","debar","debit","debug","debut","decal","decay","decor","decoy","decry","defer","deign","deity","delay","delta","delve","demon","demur","denim","dense","depot","depth","derby","deter","detox","deuce","devil","diary","dicey","digit","dilly","dimly","diner","dingo","dingy","diode","dirge","dirty","disco","ditch","ditto","ditty","diver","dizzy","dodge","dodgy","dogma","doing","dolly","donor","donut","dopey","doubt","dough","dowdy","dowel","downy","dowry","dozen","draft","drain","drake","drama","drank","drape","drawl","drawn","dread","dream","dress","dried","drier","drift","drill","drink","drive","droit","droll","drone","drool","droop","dross","drove","drown","druid","drunk","dryer","dryly","duchy","dully","dummy","dumpy","dunce","dusky","dusty","dutch","duvet","dwarf","dwell","dwelt","dying","eager","eagle","early","earth","easel","eaten","eater","ebony","eclat","edict","edify","eerie","egret","eight","eject","eking","elate","elbow","elder","elect","elegy","elfin","elide","elite","elope","elude","email","embed","ember","emcee","empty","enact","endow","enema","enemy","enjoy","ennui","ensue","enter","entry","envoy","epoch","epoxy","equal","equip","erase","erect","erode","error","erupt","essay","ester","ether","ethic","ethos","etude","evade","event","every","evict","evoke","exact","exalt","excel","exert","exile","exist","expel","extol","extra","exult","eying","fable","facet","faint","fairy","faith","false","fancy","fanny","farce","fatal","fatty","fault","fauna","favor","feast","fecal","feign","fella","felon","femme","femur","fence","feral","ferry","fetal","fetch","fetid","fetus","fever","fewer","fiber","ficus","field","fiend","fiery","fifth","fifty","fight","filer","filet","filly","filmy","filth","final","finch","finer","first","fishy","fixer","fizzy","fjord","flack","flail","flair","flake","flaky","flame","flank","flare","flash","flask","fleck","fleet","flesh","flick","flier","fling","flint","flirt","float","flock","flood","floor","flora","floss","flour","flout","flown","fluff","fluid","fluke","flume","flung","flunk","flush","flute","flyer","foamy","focal","focus","foggy","foist","folio","folly","foray","force","forge","forgo","forte","forth","forty","forum","found","foyer","frail","frame","frank","fraud","freak","freed","freer","fresh","friar","fried","frill","frisk","fritz","frock","frond","front","frost","froth","frown","froze","fruit","fudge","fugue","fully","fungi","funky","funny","furor","furry","fussy","fuzzy","gaffe","gaily","gamer","gamma","gamut","gassy","gaudy","gauge","gaunt","gauze","gavel","gawky","gayer","gayly","gazer","gecko","geeky","geese","genie","genre","ghost","ghoul","giant","giddy","gipsy","girly","girth","given","giver","glade","gland","glare","glass","glaze","gleam","glean","glide","glint","gloat","globe","gloom","glory","gloss","glove","glyph","gnash","gnome","godly","going","golem","golly","gonad","goner","goody","gooey","goofy","goose","gorge","gouge","gourd","grace","grade","graft","grail","grain","grand","grant","grape","graph","grasp","grass","grate","grave","gravy","graze","great","greed","green","greet","grief","grill","grime","grimy","grind","gripe","groan","groin","groom","grope","gross","group","grout","grove","growl","grown","gruel","gruff","grunt","guard","guava","guess","guest","guide","guild","guile","guilt","guise","gulch","gully","gumbo","gummy","guppy","gusto","gusty","gypsy","habit","hairy","halve","handy","happy","hardy","harem","harpy","harry","harsh","haste","hasty","hatch","hater","haunt","haute","haven","havoc","hazel","heady","heard","heart","heath","heave","heavy","hedge","hefty","heist","helix","hello","hence","heron","hilly","hinge","hippo","hippy","hitch","hoard","hobby","hoist","holly","homer","honey","honor","horde","horny","horse","hotel","hotly","hound","house","hovel","hover","howdy","human","humid","humor","humph","humus","hunch","hunky","hurry","husky","hussy","hutch","hydro","hyena","hymen","hyper","icily","icing","ideal","idiom","idiot","idler","idyll","igloo","iliac","image","imbue","impel","imply","inane","inbox","incur","index","inept","inert","infer","ingot","inlay","inlet","inner","input","inter","intro","ionic","irate","irony","islet","issue","itchy","ivory","jaunt","jazzy","jelly","jerky","jetty","jewel","jiffy","joint","joist","joker","jolly","joust","judge","juice","juicy","jumbo","jumpy","junta","junto","juror","kappa","karma","kayak","kebab","khaki","kinky","kiosk","kitty","knack","knave","knead","kneed","kneel","knelt","knife","knock","knoll","known","koala","krill","label","labor","laden","ladle","lager","lance","lanky","lapel","lapse","large","larva","lasso","latch","later","lathe","latte","laugh","layer","leach","leafy","leaky","leant","leapt","learn","lease","leash","least","leave","ledge","leech","leery","lefty","legal","leggy","lemon","lemur","leper","level","lever","libel","liege","light","liken","lilac","limbo","limit","linen","liner","lingo","lipid","lithe","liver","livid","llama","loamy","loath","lobby","local","locus","lodge","lofty","logic","login","loopy","loose","lorry","loser","louse","lousy","lover","lower","lowly","loyal","lucid","lucky","lumen","lumpy","lunar","lunch","lunge","lupus","lurch","lurid","lusty","lying","lymph","lyric","macaw","macho","macro","madam","madly","mafia","magic","magma","maize","major","maker","mambo","mamma","mammy","manga","mange","mango","mangy","mania","manic","manly","manor","maple","march","marry","marsh","mason","masse","match","matey","mauve","maxim","maybe","mayor","mealy","meant","meaty","mecca","medal","media","medic","melee","melon","mercy","merge","merit","merry","metal","meter","metro","micro","midge","midst","might","milky","mimic","mince","miner","minim","minor","minty","minus","mirth","miser","missy","mocha","modal","model","modem","mogul","moist","molar","moldy","money","month","moody","moose","moral","moron","morph","mossy","motel","motif","motor","motto","moult","mound","mount","mourn","mouse","mouth","mover","movie","mower","mucky","mucus","muddy","mulch","mummy","munch","mural","murky","mushy","music","musky","musty","myrrh","nadir","naive","nanny","nasal","nasty","natal","naval","navel","needy","neigh","nerdy","nerve","never","newer","newly","nicer","niche","niece","night","ninja","ninny","ninth","noble","nobly","noise","noisy","nomad","noose","north","nosey","notch","novel","nudge","nurse","nutty","nylon","nymph","oaken","obese","occur","ocean","octal","octet","odder","oddly","offal","offer","often","olden","older","olive","ombre","omega","onion","onset","opera","opine","opium","optic","orbit","order","organ","other","otter","ought","ounce","outdo","outer","outgo","ovary","ovate","overt","ovine","ovoid","owing","owner","oxide","ozone","paddy","pagan","paint","paler","palsy","panel","panic","pansy","papal","paper","parer","parka","parry","parse","party","pasta","paste","pasty","patch","patio","patsy","patty","pause","payee","payer","peace","peach","pearl","pecan","pedal","penal","pence","penne","penny","perch","peril","perky","pesky","pesto","petal","petty","phase","phone","phony","photo","piano","picky","piece","piety","piggy","pilot","pinch","piney","pinky","pinto","piper","pique","pitch","pithy","pivot","pixel","pixie","pizza","place","plaid","plain","plait","plane","plank","plant","plate","plaza","plead","pleat","plied","plier","pluck","plumb","plume","plump","plunk","plush","poesy","point","poise","poker","polar","polka","polyp","pooch","poppy","porch","poser","posit","posse","pouch","pound","pouty","power","prank","prawn","preen","press","price","prick","pride","pried","prime","primo","print","prior","prism","privy","prize","probe","prone","prong","proof","prose","proud","prove","prowl","proxy","prude","prune","psalm","pubic","pudgy","puffy","pulpy","pulse","punch","pupil","puppy","puree","purer","purge","purse","pushy","putty","pygmy","quack","quail","quake","qualm","quark","quart","quash","quasi","queen","queer","quell","query","quest","queue","quick","quiet","quill","quilt","quirk","quite","quota","quote","quoth","rabbi","rabid","racer","radar","radii","radio","rainy","raise","rajah","rally","ralph","ramen","ranch","randy","range","rapid","rarer","raspy","ratio","ratty","raven","rayon","razor","reach","react","ready","realm","rearm","rebar","rebel","rebus","rebut","recap","recur","recut","reedy","refer","refit","regal","rehab","reign","relax","relay","relic","remit","renal","renew","repay","repel","reply","rerun","reset","resin","retch","retro","retry","reuse","revel","revue","rhino","rhyme","rider","ridge","rifle","right","rigid","rigor","rinse","ripen","riper","risen","riser","risky","rival","river","rivet","roach","roast","robin","robot","rocky","rodeo","roger","rogue","roomy","roost","rotor","rouge","rough","round","rouse","route","rover","rowdy","rower","royal","ruddy","ruder","rugby","ruler","rumba","rumor","rupee","rural","rusty","sadly","safer","saint","salad","sally","salon","salsa","salty","salve","salvo","sandy","saner","sappy","sassy","satin","satyr","sauce","saucy","sauna","saute","savor","savoy","savvy","scald","scale","scalp","scaly","scamp","scant","scare","scarf","scary","scene","scent","scion","scoff","scold","scone","scoop","scope","score","scorn","scour","scout","scowl","scram","scrap","scree","screw","scrub","scrum","scuba","sedan","seedy","segue","seize","semen","sense","sepia","serif","serum","serve","setup","seven","sever","sewer","shack","shade","shady","shaft","shake","shaky","shale","shall","shalt","shame","shank","shape","shard","share","shark","sharp","shave","shawl","shear","sheen","sheep","sheer","sheet","sheik","shelf","shell","shied","shift","shine","shiny","shire","shirk","shirt","shoal","shock","shone","shook","shoot","shore","shorn","short","shout","shove","shown","showy","shrew","shrub","shrug","shuck","shunt","shush","shyly","siege","sieve","sight","sigma","silky","silly","since","sinew","singe","siren","sissy","sixth","sixty","skate","skier","skiff","skill","skimp","skirt","skulk","skull","skunk","slack","slain","slang","slant","slash","slate","sleek","sleep","sleet","slept","slice","slick","slide","slime","slimy","sling","slink","sloop","slope","slosh","sloth","slump","slung","slunk","slurp","slush","slyly","smack","small","smart","smash","smear","smell","smelt","smile","smirk","smite","smith","smock","smoke","smoky","smote","snack","snail","snake","snaky","snare","snarl","sneak","sneer","snide","sniff","snipe","snoop","snore","snort","snout","snowy","snuck","snuff","soapy","sober","soggy","solar","solid","solve","sonar","sonic","sooth","sooty","sorry","sound","south","sower","space","spade","spank","spare","spark","spasm","spawn","speak","spear","speck","speed","spell","spelt","spend","spent","sperm","spice","spicy","spied","spiel","spike","spiky","spill","spilt","spine","spiny","spire","spite","splat","split","spoil","spoke","spoof","spook","spool","spoon","spore","sport","spout","spray","spree","sprig","spunk","spurn","spurt","squad","squat","squib","stack","staff","stage","staid","stain","stair","stake","stale","stalk","stall","stamp","stand","stank","stare","stark","start","stash","state","stave","stead","steak","steal","steam","steed","steel","steep","steer","stein","stern","stick","stiff","still","stilt","sting","stink","stint","stock","stoic","stoke","stole","stomp","stone","stony","stood","stool","stoop","store","stork","storm","story","stout","stove","strap","straw","stray","strip","strut","stuck","study","stuff","stump","stung","stunk","stunt","style","suave","sugar","suing","suite","sulky","sully","sumac","sunny","super","surer","surge","surly","sushi","swami","swamp","swarm","swash","swath","swear","sweat","sweep","sweet","swell","swept","swift","swill","swine","swing","swirl","swish","swoon","swoop","sword","swore","sworn","swung","synod","syrup","tabby","table","taboo","tacit","tacky","taffy","taint","taken","taker","tally","talon","tamer","tango","tangy","taper","tapir","tardy","tarot","taste","tasty","tatty","taunt","tawny","teach","teary","tease","teddy","teeth","tempo","tenet","tenor","tense","tenth","tepee","tepid","terra","terse","testy","thank","theft","their","theme","there","these","theta","thick","thief","thigh","thing","think","third","thong","thorn","those","three","threw","throb","throw","thrum","thumb","thump","thyme","tiara","tibia","tidal","tiger","tight","tilde","timer","timid","tipsy","titan","tithe","title","toast","today","toddy","token","tonal","tonga","tonic","tooth","topaz","topic","torch","torso","torus","total","totem","touch","tough","towel","tower","toxic","toxin","trace","track","tract","trade","trail","train","trait","tramp","trash","trawl","tread","treat","trend","triad","trial","tribe","trice","trick","tried","tripe","trite","troll","troop","trope","trout","trove","truce","truck","truer","truly","trump","trunk","truss","trust","truth","tryst","tubal","tuber","tulip","tulle","tumor","tunic","turbo","tutor","twang","tweak","tweed","tweet","twice","twine","twirl","twist","twixt","tying","udder","ulcer","ultra","umbra","uncle","uncut","under","undid","undue","unfed","unfit","unify","union","unite","unity","unlit","unmet","unset","untie","until","unwed","unzip","upper","upset","urban","urine","usage","usher","using","usual","usurp","utile","utter","vague","valet","valid","valor","value","valve","vapid","vapor","vault","vaunt","vegan","venom","venue","verge","verse","verso","verve","vicar","video","vigil","vigor","villa","vinyl","viola","viper","viral","virus","visit","visor","vista","vital","vivid","vixen","vocal","vodka","vogue","voice","voila","vomit","voter","vouch","vowel","vying","wacky","wafer","wager","wagon","waist","waive","waltz","warty","waste","watch","water","waver","waxen","weary","weave","wedge","weedy","weigh","weird","welch","welsh","whack","whale","wharf","wheat","wheel","whelp","where","which","whiff","while","whine","whiny","whirl","whisk","white","whole","whoop","whose","widen","wider","widow","width","wield","wight","willy","wimpy","wince","winch","windy","wiser","wispy","witch","witty","woken","woman","women","woody","wooer","wooly","woozy","wordy","world","worry","worse","worst","worth","would","wound","woven","wrack","wrath","wreak","wreck","wrest","wring","wrist","write","wrong","wrote","wrung","wryly","yacht","yearn","yeast","yield","young","youth","zebra","zesty","zonal"};
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
	cout<<(int)(answers[wid].size())<<" letters."<<endl;
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
	int id=rand()%(int)(answers.size());
	scta(back);
	int round=0;
	memset(bad,0,sizeof(bad));
	det="";
	for(int i=0;i<(int)(answers[id].size());i++) det+="_";
	while(1){
		round++;
		if(round==vals["TURNS"]+1){
			printstate(round-1,id,round-1);
			scta(red);
			cout<<"GAME OVER!"<<endl;
			scta(green);
			cout<<"ANSWER: "<<answers[id]<<endl;
			scta(back);
			int score=0;
			for(int i=0;i<(int)(answers[id].size());i++)
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
			cout<<"ANSWER: "<<answers[id]<<endl;
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
		if((int)(ssss.size())!=(int)(answers[id].size())){
    		for(int i=0;i<(int)(ssss.size());i++)
				hist[round].push_back(make_pair(ssss[i],purple));
			continue;
		}
		flag=false;
		for(int i=0;i<answers.size();i++) if(answers[i]==ssss) flag=true;
		if(!flag){
    		for(int i=0;i<(int)(ssss.size());i++)
				hist[round].push_back(make_pair(ssss[i],blue));
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
			hist[round].push_back(make_pair(ssss[i]-'A'+'a',col[i]));
		if(GR==(int)(answers[id].size())){
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
	for(int i=0;i<(int)(answers.size());i++) for(int j=0;j<answers[i].size();j++)
		answers[i][j]-='A',answers[i][j]+='a';
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

