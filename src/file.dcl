int main() {
string file = "helloworld.txt";
int fd = bopen(file , 66 , 384);
bwrite(fd, "helloworld", 10);
bclose(fd);
fd = bopen(file , 66 , 384);
string buf =  malloc(10); 
int hi = bread(fd, buf, 10);
print_int(hi);
print_string(buf); 
bclose(fd);
free(buf); 
return 0;
}
