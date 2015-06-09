//
//  AppDelegate.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 29/10/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "CertificateUtils.h"

@interface AppDelegate : UIResponder <UIApplicationDelegate>

@property (strong, nonatomic) UIWindow *window;
@property (strong, nonatomic) NSString *certificateName;
@property (strong, nonatomic) NSDictionary *appConfig;
//@property (strong, nonatomic) CertificateUtils *certificate;
@end
