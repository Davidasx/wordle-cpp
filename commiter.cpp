#include<bits/stdc++.h>
using namespace std;
int main(){
	string randlab="";
	srand(unsigned(time(NULL)));
	rand();
	for(int i=0;i<10;i++){
		int X=rand()%36;
		if(X<10) randlab+=(char)('0'+X);
		else randlab+=(char)('a'+X-10);
	}
	system("git add .");
	system(("git commit -m \""+randlab+"\"").data());
	system("git branch -M main");
	system("git push -u origin main");
	return 0;
}


