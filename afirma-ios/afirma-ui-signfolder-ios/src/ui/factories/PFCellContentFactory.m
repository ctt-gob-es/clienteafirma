//
//  PFCellContentFactory.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 05/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "PFCellContentFactory.h"

@interface PFCellContentFactory ()

@end

@implementation PFCellContentFactory

+ (CALayer *)iconLayerForPriority:(NSString *)priorityStr withSize:(CGFloat)iconSize
{
    NSInteger priority = [priorityStr integerValue];

    switch (priority) {
        case 2:

            return [QuartzUtils circleWithColor:COLOR_PRIORITY_YELLOW andRect:CGRectMake(0, 0, iconSize, iconSize)];

        case 3:

            return [QuartzUtils circleWithColor:COLOR_PRIORITY_ORANGE andRect:CGRectMake(0, 0, iconSize, iconSize)];

        case 4:

            return [QuartzUtils circleWithColor:COLOR_PRIORITY_RED andRect:CGRectMake(0, 0, iconSize, iconSize)];

        default:

            return nil;
    }
}

@end
