int main() {
print_string("hello world!");
string g = "hi";
print_string(g);

string h;
h = "hello chang";

print_string(h);
string file = "hello.txt";
int fd = bopen(file , 66 , 384);
print_string(file);
print_int(fd);
string buf =  malloc(10); 
int hi = bread(fd, buf, 10);
print_int(hi);
print_string(buf); 
bclose(fd);
free(buf); 
return 4;
}
