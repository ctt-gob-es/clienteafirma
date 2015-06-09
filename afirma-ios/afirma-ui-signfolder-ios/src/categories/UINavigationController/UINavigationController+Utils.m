//
//  UINavigationController+Utils.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 25/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "UINavigationController+Utils.h"

@implementation UINavigationController (Utils)

- (UIViewController *)previousViewController
{
    return self.viewControllers && self.viewControllers.count > 1 ? self.viewControllers[self.viewControllers.count-2] : nil;
}

- (UIViewController *)rootViewController
{
    return self.viewControllers && self.viewControllers.count > 0 ? self.viewControllers[0] : nil;
}

@end
