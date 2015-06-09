//
//  NSDate+Utils.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 12/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NSDate (Utils)

- (NSString *)stringWithFormat:(NSString *)dateFormat;
- (BOOL)isGreaterThan:(NSDate *)otherDate;
- (BOOL)isMinorThan:(NSDate *)otherDate;

@end
