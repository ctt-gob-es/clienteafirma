//
//  AOXMLCountersignParsed.m
//  SignSample02
//
//  Created by Javier on 20/10/14.
//  Copyright (c) 2014 Atos. All rights reserved.
//

#import "AOCounterSignPreItems.h"

@implementation AOCounterSignPreItems

- (id)init {
    self = [super init];
    self.elements = [[NSMutableDictionary alloc] init];
    return self;
}

-(NSString *)generateXML{
    NSString *res = [[NSString alloc] init];
    
    res = [[[[[[[res
           stringByAppendingString:@"<xml>\n"]
            stringByAppendingString:@" <cs>\n"]
            stringByAppendingString:@"  "]
            stringByAppendingString:self.date]
            stringByAppendingString:@"\n"]
            stringByAppendingString:@" </cs>\n"]
           stringByAppendingString:@" <css>\n"];
    
    for (NSString* key in self.elements) {
        NSString *value = [self.elements objectForKey:key];
        
        res = [[[[[[[[[[[[res
            stringByAppendingString:@"  <dcs>\n"]
            stringByAppendingString:@"  <d>\n"]
            stringByAppendingString:@"    "]
            stringByAppendingString:value]
            stringByAppendingString:@"\n"]
            stringByAppendingString:@"  </d>\n"]
            stringByAppendingString:@"  <dd>\n"]
            stringByAppendingString:@"   "]
            stringByAppendingString:key]
            stringByAppendingString:@"\n"]
            stringByAppendingString:@"  </dd>\n"]
         stringByAppendingString:@"  </dcs>\n"];
        
    }
    res = [[res
        stringByAppendingString:@" </css>\n"]
     stringByAppendingString:@"</xml>"];
    
    return res;
}

@end
