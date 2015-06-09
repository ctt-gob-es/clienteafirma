//
//  DefaultConstants.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 16/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#pragma mark - Device info
#define isiPad                           UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad
#define isiPhone                         !isiPad

#pragma mark - iOS info
#define IOS_NEWER_OR_EQUAL_TO_8          ([[UIDevice currentDevice] systemVersion].floatValue >= 8.0)
#define IOS_NEWER_OR_EQUAL_TO_7          ([[UIDevice currentDevice] systemVersion].floatValue >= 7.0)

#pragma mark - Screen info
#define SCREEN_HEIGHT                    (IOS_NEWER_OR_EQUAL_TO_8 ? ([UIScreen mainScreen].bounds.size.height) :                                                                                           SCREEN_HEIGHT_IOS_7)
#define SCREEN_HEIGHT_IOS_7              ((UIInterfaceOrientationIsLandscape([UIApplication sharedApplication].statusBarOrientation)) ? [[UIScreen mainScreen] bounds].size.width : [[UIScreen mainScreen] bounds].size.height)
#define SCREEN_WIDTH (IOS_NEWER_OR_EQUAL_TO_8  ? ([UIScreen mainScreen].bounds.size.width) :                                                                                                               SCREEN_WIDTH_IOS_7)
#define SCREEN_WIDTH_IOS_7 ((UIInterfaceOrientationIsLandscape([UIApplication sharedApplication].statusBarOrientation)) ? [[UIScreen mainScreen] bounds].size.height : [[UIScreen mainScreen]              bounds].size.width)

#pragma mark - Bars info
#define STATUSBAR_ORIENTATION            [UIApplication sharedApplication].statusBarOrientation
#define NAV_BAR_HEIGHT                                  44
#define STATUS_BAR_HEIGHT                               20
