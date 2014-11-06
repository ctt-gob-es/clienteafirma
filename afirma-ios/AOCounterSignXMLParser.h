//
//  AOXMLParser.h
//  SignSample02
//
//  Created by Javier on 20/10/14.
//  Copyright (c) 2014 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "AOCounterSignPreItems.h"

@interface AOCounterSignXMLParser : NSObject<NSXMLParserDelegate>

-(AOCounterSignPreItems *)parseXML:(NSData *) data;

@property (nonatomic, strong) NSXMLParser *xmlParser;

@property (nonatomic, strong) NSString *foundValue;

@property (nonatomic, strong) NSString *dValue;

@property (nonatomic, strong) NSString *ddValue;

@property (nonatomic, strong) NSString *currentElement;

@property (nonatomic, assign) BOOL error;

@property (nonatomic, strong) AOCounterSignPreItems *counterSignPreItems;


@end
