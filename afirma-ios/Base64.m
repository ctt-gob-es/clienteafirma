//
//  Base64.m
//
//  Created by Tomas Garcia-Meras on 8/7/14.
//  Copyright (c) 2014 Gobierno de España. All rights reserved.

#import <Foundation/Foundation.h>
#import "Base64.h"

@implementation Base64:NSObject

+(NSData*) decode:(NSString*) str urlSafe:(Boolean) urlSafe;
{
    if (urlSafe)
    {
        return [Base64 decode:[Base64 urlSafeDecode:str]];
    }
    return [Base64 decode:str];
}

+(NSData*) decode:(NSString*) str
{
    // Para hacer la decodificación a prueba de errores se deshace un posible "URL Encoding" entes de decodificar
    // el Base64
    return [[NSData alloc] initWithBase64EncodedString:[str stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding] options:NSDataBase64DecodingIgnoreUnknownCharacters];
}

+(NSString*) encode:(NSData*) source urlSafe:(Boolean) urlSafe;
{
    if (urlSafe)
    {
        return [[Base64 urlSafeEncode:[Base64 encode:source]] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    }
    return [Base64 encode:source];
}

+(NSString*) encode:(NSData*) source
{
    return [[source base64EncodedStringWithOptions:0] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
}

+ (NSString*) urlSafeEncode: (NSString*) string {
    return [[string stringByReplacingOccurrencesOfString:@"+" withString:@"-"] stringByReplacingOccurrencesOfString:@"/" withString:@"_"];
}

+ (NSString*) urlSafeDecode: (NSString*) string {
    return [[string stringByReplacingOccurrencesOfString:@"-" withString:@"+"] stringByReplacingOccurrencesOfString:@"_" withString:@"/"];
}

@end
