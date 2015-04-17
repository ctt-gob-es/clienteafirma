//
//  DesCipher.h
//  TestObc
//
//  Created by Tomas Garcia-Meras on 6/7/14.
//  Copyright (c) 2014 Gobierno de España. All rights reserved.

#include <CommonCrypto/CommonCryptor.h>

@interface DesCypher:NSObject
+ (NSString*) cypherData:(NSData*) data sk:(NSData*) sk;
+ (NSData*) decypherData:(NSString*) prefixedBase64Data sk:(NSData*) sk;
@end