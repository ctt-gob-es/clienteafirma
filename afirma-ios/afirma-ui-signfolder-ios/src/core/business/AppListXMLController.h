//
//  AppListXMLController.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 13/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "XMLController.h"
#import "WSDataController.h"

@interface AppListXMLController : XMLController <NSXMLParserDelegate, WSDelegate>

+ (AppListXMLController *)sharedInstance;
- (void)requestAppsList;
- (NSArray *)appsArray;

@end
