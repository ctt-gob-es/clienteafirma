//
//  NSDateFormatter+Utils.m
//  bluekiwi
//
//  Created by David Arrufat on 17/02/14.
//  Copyright (c) 2014 Tempos 21. All rights reserved.
//

#import "NSDateFormatter+Utils.h"

@implementation NSDateFormatter (Utils)

- (id) initWithCurrentLocale
{
    static NSLocale* locale = nil;
    self = [self init];
    if (locale == nil) {
        NSString *currentLanguage = [[NSBundle mainBundle] preferredLocalizations][0];
        locale = [[NSLocale alloc] initWithLocaleIdentifier:currentLanguage];
    }
    [self setLocale:locale];
    return self;
}

@end
