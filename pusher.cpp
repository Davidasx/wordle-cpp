#include<bits/stdc++.h>
using namespace std;
unsigned int system(string s){
	return system(s.data());
}
int main(int argc,char** argv){
	ifstream fin("..\\wordle.cpp");
	string ver,tmp;
	getline(fin,tmp);
	getline(fin,tmp);
	getline(fin,tmp);
	getline(fin,tmp);
	getline(fin,tmp);
	fin.close();
	for(int i=12;i<(int)(tmp.size());i++){
		if(tmp[i]=='\"') break;
		ver+=tmp[i];
	}
	system("echo "+ver+" > latest.txt");
	system("mkdir releases\\"+ver);
	system("copy ..\\wordle.cpp wordle.cpp");
	system("copy ..\\wordle.cpp releases\\"+ver+"\\wordle.cpp");
	system("g++ ..\\wordle.cpp -o ..\\wordle.exe");
	system("copy ..\\wordle.exe wordle.exe");
	system("copy ..\\wordle.exe releases\\"+ver+"\\wordle.exe");
	system("commiter");
	system("pause");
	return 0;
}

