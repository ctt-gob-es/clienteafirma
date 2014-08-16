//
//  DesCipher.h
//  TestObc
//
//  Created by Tomas Garcia-Meras on 6/7/14.
//  Copyright (c) 2014 Gobierno de España. All rights reserved.

#include <CommonCrypto/CommonCryptor.h>

@interface DesCypher:NSObject
+ (NSData*) padData:(NSData*) data;
+ (NSData*) doDes:(NSData*) data sk:(NSData*) sk opMode:(CCOperation) opMode;
+ (NSString*) cypherData:(NSData*) data sk:(NSData*) sk;
+ (NSData*) decypher:(NSData*) data sk:(NSData*) sk;
+ (int) getBlockSize;
@end