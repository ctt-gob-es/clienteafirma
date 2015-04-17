//
//  QuartzUtils.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 6/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface QuartzUtils : NSObject

#pragma mark - Draw circles
+ (CALayer *)circleWithColor:(UIColor *)color andRect:(CGRect)rect;

#pragma mark - Tint images
+ (UIImage *)getImageWithName:(NSString *)imageName andTintColor:(UIColor *)color;

#pragma mark - Shadows
+ (void)drawShadowInView:(UIView *)view;

@end
