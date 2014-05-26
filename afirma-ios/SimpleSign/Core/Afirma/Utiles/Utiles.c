//
//  Utiles.c
//  CadesASN1Simplificado
//
//

#include <stdio.h>

static int write_out(const void *buffer, size_t size, void *app_key){
    FILE *out_fp = app_key;
    size_t wrote;
    wrote = fwrite(buffer, 1, size, out_fp);
    
    return (wrote == size) ? 0 : -1;
}