//
//  PFCellContentFactory.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 05/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface PFCellContentFactory : NSObject

+ (CALayer *)iconLayerForPriority:(NSString *)priorityStr withSize:(CGFloat)iconSize;

@end
