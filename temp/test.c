/* Program bitmap.c reads uncompressed .bmp files.
   Copyright 2005, David R. Brooks.
   Written with Microsoft QuickC.
   This program can be used without restriction for any educational
   or other non-commercial purpose, but please acknowledge the source.
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
void bw_count(unsigned char pixel, unsigned long *blk, unsigned long *wht);
void main() {
	FILE *in;
	unsigned char ch[50],p_ch;
	char filename[20],yes_no;
	unsigned int k,i,bits_per_pixel,offset;
	unsigned long ht,wt,n_bytes;
	unsigned long blk,wht,sum_blk,sum_wht;
/* File name no more than 8 characters, not including extension. */
	printf("Give file name without its .bmp extension: " );
	scanf("%s",filename);
	strcat(filename,".bmp"); /* Add .bmp file extension to name. */
	in=fopen(filename,"r");
	if (in == NULL) {
	  printf("Can't find file. Abort.\n");
	  exit(1);
	}
/* Read 14-byte file header. */
	i=0;
	printf("File header: \n");
	while (i<14) {
	  fscanf(in,"%c",&ch[i]);
	  printf("%i ",ch[i]);
	  i++;
	}
	printf("\n");
	printf("file size = %li\n",
	  (long)ch[4]*65536+(long)ch[3]*256+(long)ch[2]);
	offset=(int)ch[11]*256+(int)ch[10];
	printf("offset to image = %i\n",offset);

/* Read 40-byte image information header. */
	i=0;
	printf("Image header: \n");
	while (i<40) {
	  fscanf(in,"%c",&ch[i]);
	   printf("%i ",ch[i]);
	  i++;
	} printf("\n");
	wt=(long)ch[6]*65536+(long)ch[5]*256+(long)ch[4];
	ht=(long)ch[10]*65536+(long)ch[9]*256+(long)ch[8];
	printf("image width, height, size: %li %li %li\n",wt,ht,wt*ht);
	printf("image size from header: %li\n",
	  (long)ch[22]*65536+(long)ch[21]*256+(long)ch[20]);
	bits_per_pixel=(int)ch[14];
	printf("bits per pixel = %i\n",bits_per_pixel);
	/* printf("number of colors = %i\n",(int)ch[33]*256+(int)ch[2]); */
	fclose(in);
	printf("Read image now? (y or n) ");
	fflush(stdin);
	scanf("%c",&yes_no);
	if (yes_no == 'y') {
/* Open file as binary to read image. */
	  in=fopen(filename,"rb");
	  fseek(in,offset,SEEK_SET); /* Go to first image byte. */
	  printf("Enter -1 to read entire file, or # of bytes: ");
	  scanf("%li",&n_bytes);
	  if (n_bytes != -1) {
/* Read specified number of bytes. */
	    for (i=1; i<=n_bytes; i++) {
	      fread(&p_ch,1,1,in);
	      printf("%i ",p_ch);
	    }
	  }
	  else {
/* Read entire file, one row at a time. */
	    blk=0; wht=0; sum_blk=0; sum_wht=0; /* Initialize for B&W pixels. */
	    for (i=1; i<=ht; i++) {
	      printf("\nRow # %4i\n",i);
	      for (k=1; k<=wt/8; k++) {
		fread(&p_ch,1,1,in);
		printf("%i ",p_ch);
		if (bits_per_pixel == 1) { /* Count black and white pixels. */
		  bw_count(p_ch,&blk,&wht);
		  sum_blk+=blk; sum_wht+=wht;
		}
	      }
	      fread(&p_ch,1,1,in); /* Read end-of-row byte. */
	    }
	    printf("\n"); /* End of row. */
	  }
	  printf("\n... done\n");
	  fclose(in);
	} /* End of read file loop. */
/* Display black and white pixel count. */
	if ((bits_per_pixel == 1) && (n_bytes == -1))
	  printf("black white count: %li %li\n",sum_blk,sum_wht);
	  printf("Percent black and white: %.1lf %.1lf\n",
		100.*sum_blk/(sum_blk+sum_wht),100.*sum_wht/(sum_blk+sum_wht));
	  fflush(stdin);
	  printf("Press any digit or character to quit... ");scanf("%c",&p_ch);
}
void bw_count(unsigned char p, unsigned long *blk, unsigned long *wht) {
/* For a black and white image (8 pixels per byte, either 0 or 1),
   this function counts black and white pixels per byte.
*/
  int i;
  unsigned long n=1,b=0,w=0;
  b=0; w=0;
  for (i=0; i<=7; i++) {
/*    printf("%li %li\n",n,p&n); */
    if ((long)(p&n) == n) w++; else b++; /* Apply bitwise AND operator. */
    n*=2;
  }
  *blk=b; *wht=w;
}
