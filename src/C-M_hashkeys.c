#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
/* ============================================================================================================================== */
long C_djb2 (unsigned char *str) {
uint64_t djb2_64 = 5381;
int c;

   fprintf(stdout,"*djb2 C*              hashing string=");
   while (c = *str++){
      djb2_64 = ((djb2_64 << 5) + djb2_64) + c; /* equivalent to djb2_64 * 33 + c */
      /*
      fprintf(stdout,"%c",c);
      */
   }
   /*
   fprintf(stdout," hash=%lu hex hash=%32.32lx\n",djb2_64,djb2_64);
   fflush(stdout);
   */
   return (int64_t)djb2_64;
}
/* ============================================================================================================================== */
uint32_t rc_crc32 (uint32_t crc, const char *buf, size_t len) {
   static uint32_t table[256];
   static int have_table = 0;
   uint32_t rem;
   uint8_t octet;
   int i, j;
   const char *p, *q;

   /* This check is not thread safe; there is no mutex. */
   if (have_table == 0) {
      /* Calculate CRC table. */
      for (i = 0; i < 256; i++) {
         rem = i;               /* remainder from polynomial division */
         for (j = 0; j < 8; j++) {
            if (rem & 1) {
               rem >>= 1;
               rem ^= 0xedb88320;
            } else
               rem >>= 1;
         }
         table[i] = rem;
      }
      have_table = 1;
   }

   crc = ~crc;
   q = buf + len;
   for (p = buf; p < q; p++) {
      octet = *p;               /* Cast to unsigned octet. */
      crc = (crc >> 8) ^ table[(crc & 0xff) ^ octet];
   }
   return ~crc;
}
#ifdef rc_crc32
int main() {
        const char *s = "The quick brown fox jumps over the lazy dog";
        printf("%" PRIX32 "\n", rc_crc32(0, s, strlen(s)));

        return 0;
}
#endif
/* ============================================================================================================================== */
/*
   this algorithm was created for sdbm (a public-domain reimplementation
   of ndbm) database library. it was found to do well in scrambling bits,
   causing better distribution of the keys and fewer splits. it also happens
   to be a good general hashing function with good distribution. the actual
   function is hash(i) = hash(i - 1) * 65599 + str[i]; what is included
   below is the faster version used in gawk. [there is even a faster,
   duff-device version] the magic constant 65599 was picked out of thin
   air while experimenting with different constants, and turns out to be a
   prime. this is one of the algorithms used in berkeley db (see sleepycat)
   and elsewhere.
*/

static unsigned long sdbm (unsigned char *str) {
   uint64_t hash = 0; /* unsigned long hash = 0; */
   int c;

   while (c = *str++){
      hash = c + (hash << 6) + (hash << 16) - hash;
   }

   return hash;
}
/* ============================================================================================================================== */
