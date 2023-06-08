#include<bits/stdc++.h>
#include<conio.h>
#include<windows.h>
using namespace std;
string ver="1.4.0";
string type="¦Â";
string date="20220412";
string expire="20220425";
vector<string> words;
string user;
string base="\\.wordle\\data.wd";
vector<string> readbase(string place){
	cout<<"Reading wordle database, please wait..."<<flush;
	ifstream fin(place.data());
	vector<string> tmp;
	string word;
	while(fin>>word) tmp.push_back(word);
	fin.close();
	cout<<"OK."<<endl;
	return tmp;
}
void writebase(string place){
	cout<<"Writing wordle database, please wait..."<<flush;
	cout<<place<<endl;
	ofstream fout(place.data());
	for(int i=0;i<(int)(words.size());i++) fout<<words[i]<<endl;
	fout.close();
	cout<<"OK."<<endl;
}
void process(){
	ifstream tfin(base.data());
	if(!tfin){
		system("mkdir %USERPROFILE%\\.wordle");
		system("echo hello > %USERPROFILE%\\.wordle\\data.wd");
	}
}
void recover(){
	string path="data.wd";
	vector<string> tmp=readbase(path);
	for(int i=0;i<tmp.size();i++){
		bool ok=true;
		for(int j=0;j<words.size();j++) if(words[j]==tmp[i]){
			ok=false;
			break;
		}
		if(ok)
			words.push_back(tmp[i]);
	}
	writebase(base);
}
void addword(){
	string word;
	retry:;
	cout<<"Please enter the word:"<<flush;
	cin>>word;
	for(int i=0;i<words.size();i++) if(words[i]==word){
		cout<<"Word already exists! Try again."<<endl;
		goto retry;
	}
	words.push_back(word);
	writebase(base);
}
void imbase(){
	cout<<"File:"<<flush;
	string path;
	getline(cin,path);
	vector<string> tmp=readbase(path);
	for(int i=0;i<tmp.size();i++){
		bool ok=true;
		for(int j=0;j<words.size();j++) if(words[j]==tmp[i]){
			ok=false;
			break;
		}
		if(ok)
			words.push_back(tmp[i]);
	}
	writebase(base);
}
void exbase(){
	cout<<"File:"<<flush;
	string path;
	getline(cin,path);
	writebase(path);
}
void quicksave(){
	writebase("data.wd");
}
int back=FOREGROUND_INTENSITY|FOREGROUND_RED|FOREGROUND_BLUE|FOREGROUND_GREEN;
int red=FOREGROUND_INTENSITY|FOREGROUND_RED;
int green=FOREGROUND_INTENSITY|FOREGROUND_GREEN;
int blue=FOREGROUND_INTENSITY|FOREGROUND_BLUE;
int yellow=FOREGROUND_INTENSITY|FOREGROUND_GREEN|FOREGROUND_RED;
void scta(int clr){
	SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),clr);
}
vector<pair<char,int> > hist[10010];
int cnt[26];
int col[10010];
void wordle(){
	int id=rand()%(int)(words.size());
	scta(back);
	int round=0;
	while(1){
		round++;
		if(round==10000){
			scta(red);
			cout<<"GAME OVER!"<<endl;
			scta(green);
			cout<<"ANSWER: "<<words[id]<<endl;
			scta(back);
			system("pause");
			return;
		}
		system("cls");
		scta(back);
		printf("Try #%4d\n",round);
		cout<<(int)(words[id].size())<<" letters."<<endl;
		for(int i=0;i<(int)(words[id].size());i++) cout<<"_";
		cout<<endl;
		hist[round].clear();
		for(int i=1;i<round;i++){
			for(int j=0;j<hist[i].size();j++){
				scta(hist[i][j].second);
				cout<<hist[i][j].first;
			}
			cout<<endl;
		}
		scta(back);
		string ssss;
		cin>>ssss;
		if((int)(ssss.size())!=(int)(words[id].size())){
    		for(int i=0;i<(int)(ssss.size());i++)
				hist[round].push_back(make_pair(ssss[i],blue));
			continue;
		}
		memset(cnt,0,sizeof(cnt));
		for(int i=0;i<(int)(words[id].size());i++) cnt[words[id][i]-'a']++;
		int GR=0;
		for(int i=0;i<(int)(words[id].size());i++){
			if(ssss[i]==words[id][i]){
				cnt[ssss[i]-'a']--;
				col[i]=green;
				ssss[i]=ssss[i]-'a'+'A';
				GR++;
			}
		}
		for(int i=0;i<(int)(words[id].size());i++){
			if(ssss[i]>='a'&&cnt[ssss[i]-'a']){
				cnt[ssss[i]-'a']--;
				col[i]=yellow;
				ssss[i]=ssss[i]-'a'+'A';
			}
		}
		for(int i=0;i<(int)(words[id].size());i++){
			if(ssss[i]>='a'){
				col[i]=red;
				ssss[i]=ssss[i]-'a'+'A';
			}
		}
    	for(int i=0;i<(int)(words[id].size());i++)
			hist[round].push_back(make_pair(ssss[i]-'A'+'a',col[i]));
		if(GR==(int)(words[id].size())){
			system("cls");
			printf("Try #%4d\n",round);
			cout<<(int)(words[id].size())<<" letters."<<endl;
			for(int i=0;i<(int)(words[id].size());i++) cout<<"_";
			cout<<endl;
			for(int i=1;i<=round;i++){
				for(int j=0;j<hist[i].size();j++){
					scta(hist[i][j].second);
					cout<<hist[i][j].first;
				}
				cout<<endl;
			}
			scta(back);
			printf("Guessed correctly after %d tries.\n",round);
			scta(back);
			system("pause");
			return;
		}
	}
}
int main(){
	system("cls");
	system("date /t > curtimewd.txt");
	ifstream tfin("curtimewd.txt");
	string tim;tfin>>tim;
	tfin.close();system("del curtimewd.txt");
	string ntim=tim.substr(0,4)+tim.substr(5,2)+tim.substr(8,2);
	tim=ntim;
	if(ntim>expire){
		scta(red);
		cout<<"WARNING:This version of wordle is no longer supported."<<endl;
		cout<<"        It's suggested to download a newer version and replace this one."<<endl;
		scta(back);
		system("pause");
		system("cls");
	}
	scta(back);
	srand(unsigned(time(NULL)));
	system("echo %USERPROFILE% > userxxxx.txt");
	ifstream fin("userxxxx.txt");
	fin>>user;
	fin.close();
	system("del userxxxx.txt");
	base=user+base;
	process();
	words=readbase(base);
	ifstream test("data.wd");
	if(test){
		cout<<"Found wordle database \"data.wd\". Would you like to import it?[Y/N]"<<endl;
		int X=getch();
		if(X=='Y'||X=='y')
			recover();
		test.close();
		system("cls");
	}
	while(1){
		system("cls");
		cout<<"WORDLE (C++) Version "<<ver<<" "<<type<<" ("<<date<<") by David X"<<endl;
		cout<<"1)   add new word"<<endl;
		cout<<"2)   import database"<<endl;
		cout<<"3)   export database"<<endl;
		cout<<"4)   play wordle"<<endl;
		cout<<"5)   quick save"<<endl;
		cout<<"0)   exit"<<endl;
		cout<<"Please choose one (index):"<<flush;
		int t=getch();
		cout<<(char)(t)<<endl;
		if(t=='0') return 0;
		if(t=='1') addword();
		if(t=='2') imbase();
		if(t=='3') exbase();
		if(t=='4') wordle();
		if(t=='5') quicksave();
	}
	return 0;
}

