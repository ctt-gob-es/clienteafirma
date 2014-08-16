//
//  Base64.h
//
//  Created by Tomas Garcia-Meras on 8/7/14.
//  Copyright (c) 2014 Gobierno de España. All rights reserved.

@interface Base64:NSObject
+(NSData*) decode:(NSString*) str urlSafe:(Boolean) urlSafe;
+(NSString*) encode:(NSData*) source;
+(NSString*) encode:(NSData*) source urlSafe:(Boolean) urlSafe;
+(NSString*) urlSafeEncode: (NSString*) string;
@end