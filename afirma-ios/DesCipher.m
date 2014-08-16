//
//  DesCipher.m
//  TestObc
//
//  Created by Tomas Garcia-Meras on 6/7/14.
//  Copyright (c) 2014 Gobierno de España. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonCryptor.h>
#import "DesCypher.h"
#import "Base64.h"

@implementation DesCypher : NSObject

const int BLOCK_SIZE = 8;
NSString *PADDING_CHAR_SEPARATOR = @".";

+ (NSData*) padData:(NSData*) data
{
    if ([data length] % BLOCK_SIZE == 0)
    {
        return data;
    }
    NSMutableData *ret = [NSMutableData dataWithCapacity:[data length]];
    [ret setData:data];
    [ret increaseLengthBy:(((data.length / BLOCK_SIZE + 1) * BLOCK_SIZE) - data.length)];
    return ret;
}

+ (NSData*) doDes:(NSData*) data sk:(NSData*) sk opMode:(CCOperation) opMode
{
    NSMutableData* encData = [NSMutableData dataWithLength:[data length]+BLOCK_SIZE];
    size_t dataMoved = 0;
    NSData *paddedData = [DesCypher padData:data];
    CCCryptorStatus ccStatus = CCCrypt(
                                       opMode,               // Operation
                                       kCCAlgorithmDES,      // Algorithm
                                       kCCOptionECBMode,     // Options
                                       [sk bytes],           // Key
                                       kCCKeySizeDES,        // KeyLength
                                       NULL,                 // IV
                                       [paddedData bytes],   // DataIn
                                       [paddedData length],  // DataInLength
                                       encData.mutableBytes, // DataOut
                                       encData.length,       // DataOutAvailable
                                       &dataMoved            // DataOutMoved
                                       );
    if (ccStatus == kCCSuccess)
    {
        NSLog(@"Cifrado/descifrado DES realizado con exito para una salida de %zu octetos", dataMoved);
        encData.length = dataMoved;
        return encData;
    }
    NSLog(@"Fallo en el cifrado DES");
    return nil;
}

+ (NSData*) cypher:(NSData*) data sk:(NSData*) sk
{
    return [DesCypher doDes:data sk:sk opMode:kCCEncrypt];
}

+ (NSData*) decypher:(NSData*) data sk:(NSData*) sk
{
    return [DesCypher doDes:data sk:sk opMode:kCCDecrypt];
}

+ (int) getBlockSize
{
    return BLOCK_SIZE;
}

+ (NSString*) getNeededPaddingAsString:(NSData*) data
{
    int pad = ([self getBlockSize] - data.length % [self getBlockSize]) % [self getBlockSize];
    return [NSString stringWithFormat:@"%d", pad];
}

+ (NSString*) cypherData:(NSData*) data sk:(NSData*) sk
{
    NSString* base64EncodedData = [Base64 encode:[self cypher:data sk:sk] urlSafe:true];
    return [[[self getNeededPaddingAsString:data] stringByAppendingString:PADDING_CHAR_SEPARATOR] stringByAppendingString:base64EncodedData];
}

@end
