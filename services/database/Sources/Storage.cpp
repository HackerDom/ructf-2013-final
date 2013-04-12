#include <fstream>
#include <sstream>
#include <cryptopp/sosemanuk.h>
#include <cryptopp/gzip.h>
#include "Storage.h"
#include "Errors.h"

using namespace CryptoPP;

const string Storage::signature = "\x09\x09";
string Storage::dbpath = ".";
pthread_mutex_t Storage::mutex;
int Storage::step = 0;
unordered_map<string, Storage::DbInfo *> Storage::bases;

void Storage::Initialize()
{
	pthread_mutexattr_t mutexattr;
	pthread_mutexattr_init(&mutexattr);
	pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&mutex, &mutexattr);
}

void Storage::SafeAddDb(const string &name, Database *db)
{
	bases[name] = new DbInfo(db, ++step);
	if (bases.size() < MAX_BASES)
		return;
	unordered_map<string, DbInfo *>::iterator victim = bases.begin();
	for (auto i = bases.begin(); i != bases.end(); i++)
		if (i->second->lastAccess < victim->second->lastAccess)
			victim = i;
	cout << "Garbaging " << victim->first << endl;
	SaveDatabase(victim->first);
	delete victim->second;
	bases.erase(victim);
}

void Storage::AddDatabase(const string &name, Database *db)
{
	pthread_mutex_lock(&mutex);

	SafeAddDb(name, db);

	pthread_mutex_unlock(&mutex);
}

Database *Storage::CreateDatabase(const string &name, const string &id)
{
	pthread_mutex_lock(&mutex);

	if (FindDatabase(name))
	{
		pthread_mutex_unlock(&mutex);
		return NULL;
	}
	JSONNode tables;
	tables.set_name("tables");
	JSONNode json;
	json.push_back(tables);
	auto db = new Database(id, json);
	SafeAddDb(name, db);
	SaveDatabase(name);
	
	pthread_mutex_unlock(&mutex);
	return db;
}

Database *Storage::FindDatabase(const string &name, bool capture)
{
	pthread_mutex_lock(&mutex);

	if (bases.find(name) == bases.end())
	{
		auto db = LoadDatabase(name);
		bases[name] = new DbInfo(db, step);
	}
	bases[name]->lastAccess = ++step;
	if (capture)
		pthread_mutex_lock(&bases[name]->mutex);

	pthread_mutex_unlock(&mutex);
	return bases[name]->value;
}

void Storage::ReleaseDatabase(const string &name)
{
	pthread_mutex_lock(&mutex);
	
	if (bases.find(name) != bases.end())
		pthread_mutex_unlock(&bases[name]->mutex);

	pthread_mutex_unlock(&mutex);
}

bool Storage::DropDatabase(const string &name)
{
	pthread_mutex_lock(&mutex);

	if (bases.find(name) != bases.end())
	{
		delete bases[name];
		bases.erase(name);
	}
	stringstream dbfile;
	dbfile << dbpath << "/" << name;
	bool success = remove(dbfile.str().c_str()) == 0;
		
	pthread_mutex_unlock(&mutex);
	return success;
}

string MakeRandomString(int length) 
{
	string s;
	s.resize(length);
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    for (int i = 0; i < length; i++)
        s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];

	return s;
}

void Storage::SaveDatabase(const string &name)
{
	auto db = bases[name]->value;

	stringstream dbfile;
	dbfile << dbpath << "/" << name;
	ofstream out(dbfile.str(), ios::binary);
	
	string key = MakeRandomString(KEY_LENGTH);
	string iv = MakeRandomString(Sosemanuk::IV_LENGTH);
	out << signature << db->GetId() << key << iv;
	string plaintext = db->GetJson()->write_formatted();
	string zipped;
	Gzip zipper(new StringSink(zipped), Gzip::MAX_DEFLATE_LEVEL);
	zipper.Put((byte *)plaintext.c_str(), plaintext.length());
	zipper.MessageEnd();

	byte *ciphertext = new byte[zipped.length()];
    Sosemanuk::Encryption enc((byte *)key.c_str(), KEY_LENGTH, (byte *)iv.c_str());
    enc.ProcessData(ciphertext, (byte *)zipped.c_str(), zipped.length());
	int length = zipped.length();
	out.write((char *)&length, sizeof(length));
	out.write((char *)ciphertext, zipped.length());
	out.close();
	delete [] ciphertext;
}

Database *Storage::LoadDatabase(const string &name)
{
	stringstream dbfile;
	dbfile << dbpath << "/" << name;
	//cout << dbfile.str() << endl;
	ifstream in(dbfile.str(), ios::binary);
	if (!in)
		return NULL;
	
	string sig(2, 0);
	in.read((char *)sig.c_str(), 2);
	if (sig != signature)
		return NULL;
	
	string id(UID_LENGTH, 0);
	in.read((char *)id.c_str(), UID_LENGTH);
	
	//cout << id << endl;
	
	string key(KEY_LENGTH, 0);
	in.read((char *)key.c_str(), KEY_LENGTH);
	//cout << key << endl;

	string iv(Sosemanuk::IV_LENGTH, 0);
	in.read((char *)iv.c_str(), Sosemanuk::IV_LENGTH);
	//cout << iv << endl;

	int length;
	in.read((char *)&length, sizeof(length));

	//cout << length << endl;

	byte *ciphertext = new byte[length];
	in.read((char *)ciphertext, length);
	in.close();

	//cout << ciphertext << endl;

	string zipped(length, 0);
	Sosemanuk::Decryption dec((byte *)key.c_str(), KEY_LENGTH, (byte *)iv.c_str());
    dec.ProcessData((byte *)zipped.c_str(), (byte *)ciphertext, length);
	delete [] ciphertext;
	//cout << zipped << endl;
	string plaintext;
    StringSource(zipped, true, new Gunzip(new StringSink(plaintext)));
    //cout << plaintext << endl;

	return new Database(id, libjson::parse(plaintext));
}

void Storage::UnloadAll() //TODO: test if destructor works
{
	pthread_mutex_lock(&mutex);

	for (auto i = bases.begin(); i != bases.end(); i++)
		delete i->second;
	bases.clear();
	
	pthread_mutex_unlock(&mutex);
}

void Storage::Flush()
{
	pthread_mutex_lock(&mutex);

	for (auto i = bases.begin(); i != bases.end(); i++)
		SaveDatabase(i->first);
	
	pthread_mutex_unlock(&mutex);
}