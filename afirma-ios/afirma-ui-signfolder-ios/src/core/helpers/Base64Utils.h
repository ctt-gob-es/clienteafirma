//
//  Base64Utils.h
//  PortaFirmasUniv
//
//  Created by David on 31/01/14.
//  Copyright (c) 2014 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Base64Utils : NSObject

+ (NSString *) base64EncodeString: (NSString *) strData;
+ (NSString *) base64EncodeData: (NSData *) objData;
+ (NSData *) base64DecodeString: (NSString *) strBase64;

@end
