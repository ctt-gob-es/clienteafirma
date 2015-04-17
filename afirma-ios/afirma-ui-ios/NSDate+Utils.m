//
//  NSDate+Utils.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 12/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "NSDate+Utils.h"
#import "NSDateFormatter+Utils.h"

@implementation NSDate (Utils)

- (NSString *)stringWithFormat:(NSString *)dateFormat
{
    NSDateFormatter *df = [[NSDateFormatter alloc] initWithCurrentLocale];

    [df setDateFormat:dateFormat];

    return [df stringFromDate:self];
}

- (BOOL)isGreaterThan:(NSDate *)otherDate
{
    return [self timeIntervalSinceDate:otherDate] > 0;
}

- (BOOL)isMinorThan:(NSDate *)otherDate
{
    return [self timeIntervalSinceDate:otherDate] < 0;
}

@end
