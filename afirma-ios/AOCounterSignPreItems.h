//
//  AOXMLCountersignParsed.h
//  SignSample02
//
//  Created by Javier on 20/10/14.
//  Copyright (c) 2014 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface AOCounterSignPreItems : NSObject

-(NSString *)generateXML;

@property (nonatomic, assign) NSString *date;

@property (nonatomic, strong) NSMutableDictionary *elements;

@end
