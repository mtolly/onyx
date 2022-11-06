#include <stdio.h>
#include <string.h>

// DKDSP 0.4
// convert a mono, devkit-standard gamecube DSP into stereo,
// Donkey Konga/Star Fox Assault format

// 0.1 - round up to next interleave segment
// 0.2 - correct file length calculation (bytes, not nibbles in DK header)
//     - optionally read filler DK header info from existing file
// 0.3 - write second channel header properly
// 0.4 - stereo option

unsigned long dkdsp_read32(unsigned char * inword) {
    return ((unsigned long)(inword[0])<<24)|
           ((unsigned long)(inword[1])<<16)|
           ((unsigned long)(inword[2])<<8)|
           inword[3];
}

void dkdsp_usage(char * name) {
    printf("usage: %s MONO.DSP [-s MONO2.DSP] STEREO.DSP [DKHEADER.DSP]\n",name);
}
void dkdsp_openerror(char * name) {
    printf("error opening %s\n",name);
}

int dkdsp_main(int argc, char * * argv) {
    unsigned long c,i;
    FILE * infile, * infile2=NULL, * infile3, * outfile;

    // default header filler (from 29zelda.dsp)
    char inhead[0x60],outhead[0xe0]={
    0x43, 0x73, 0x74, 0x72, 0x00, 0x66, 0x08, 0x00, 0x1F, 0x57, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xCC, 0x5E, 0x00, 0x44, 0xAC, 0x40, 0x02, 0x00, 0x00, 0x00, 0x00,
    };
    unsigned char buffer[0x800*16];

    printf("DKDSP 0.4 - GC DSP mono to stereo converter\nby hcs\n\n");

    if (argc <3 || argc > 6) {dkdsp_usage(argv[0]); return 1;}

    i=0;

    if (!(infile=fopen(argv[1],"rb"))) {dkdsp_openerror(argv[1]); return 1;}
    if (argv[2][0]=='-' && argv[2][1]=='s') {
        if (!(infile2=fopen(argv[3],"rb"))) {dkdsp_openerror(argv[3]); return 1;}
        i+=2;
    }
    if (!(outfile=fopen(argv[2+i],"wb"))) {dkdsp_openerror(argv[2+i]); return 1;}

    if (argc == 4+i) {
        printf("reading filler...");
        if(!(infile3=fopen(argv[3+i],"rb"))) {dkdsp_openerror(argv[3]); return 1;}
        fread(outhead,sizeof(outhead),1,infile3);
        fclose(infile3);
        printf("done\n");
    }

    printf("copying header...");

    fread(inhead,sizeof(inhead),1,infile);
    
    memcpy(outhead+0x20,inhead,sizeof(inhead)); // channel 1

    if (infile2) fread(inhead,sizeof(inhead),1,infile2); // 2nd header
    memcpy(outhead+0x80,inhead,sizeof(inhead)); // channel 2

    fwrite(outhead,sizeof(outhead),1,outfile);

    printf("done\ninterleaving mono->stereo...");

    for (c=0; c < (dkdsp_read32(outhead+0x24)/2)/0x800+1; c++) {
        fread(buffer,0x800,1,infile);
        fwrite(buffer,0x800,1,outfile);
        if (infile2) fread(buffer,0x800,1,infile2);
        fwrite(buffer,0x800,1,outfile);
    }

    printf("done\n");

    fclose(infile);
    if (infile2) fclose(infile2);
    fclose(outfile);

    return 0;
}
