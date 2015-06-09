//
//  DateHelper.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 12/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface DateHelper : NSObject

+ (NSString *)getStringFromDate:(NSDate *)date;
+ (NSString *)getStringFromDate:(NSDate *)date withFormat:(NSString *)format;
+ (NSDate *)getDateFromString:(NSString *)stringDate;
+ (NSDate *)getDateFromString:(NSString *)stringDate withFormat:(NSString *)format;
+ (NSDate *)getGreaterDate:(NSArray *)datesArray;

@end
