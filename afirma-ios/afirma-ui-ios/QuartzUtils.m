//
//  QuartzUtils.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 6/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "QuartzUtils.h"

@implementation QuartzUtils

#pragma mark - Draw circles

+ (CALayer *)circleWithColor:(UIColor *)color andRect:(CGRect)rect
{
    CALayer *circleLayer = [CALayer layer];

    circleLayer.backgroundColor = color.CGColor;
    [circleLayer setFrame:rect];
    [circleLayer setCornerRadius:rect.size.height / 2];

    return circleLayer;
}

#pragma mark - Tint images

+ (UIImage *)getImageWithName:(NSString *)imageName andTintColor:(UIColor *)color
{
    UIImage *result = nil;

    if (imageName && color) {
        UIImage *image = [UIImage imageNamed:imageName];

        if (image) {
            CGRect rect = CGRectMake(0, 0, image.size.width, image.size.height);
            UIGraphicsBeginImageContextWithOptions(rect.size, NO, image.scale);
            CGContextRef c = UIGraphicsGetCurrentContext();

            if (c) {
                [image drawInRect:rect];
                CGContextSetFillColorWithColor(c, [color CGColor]);
                CGContextSetBlendMode(c, kCGBlendModeSourceAtop);
                CGContextFillRect(c, rect);
                result = UIGraphicsGetImageFromCurrentImageContext();
            } else {
                NSLog(@"No context?");
            }
            UIGraphicsEndImageContext();
        }
    }

    return result;
}

#pragma mark - Shadows

+ (void)drawShadowInView:(UIView *)view
{
    [view.layer setShadowColor:[UIColor blackColor].CGColor];
    [view.layer setShadowOffset:CGSizeMake(-1, 0)];
    [view.layer setShadowOpacity:0.5f];
    [view.layer setShadowRadius:0.5f];
    [view.layer setMasksToBounds:NO];
    [view.layer setShadowPath:[UIBezierPath bezierPathWithRect:view.bounds].CGPath];
}

@end
