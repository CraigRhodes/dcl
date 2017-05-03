int main() {
int fd = bopen("test1.dcl", 66, 384);
bwrite(fd, "hello from test2.dcl",20);
return 0; 
}
