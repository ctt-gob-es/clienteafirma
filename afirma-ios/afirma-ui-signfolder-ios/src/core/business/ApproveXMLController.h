//
//  AproveXMLController.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 6/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "XMLController.h"

@interface ApproveXMLController : XMLController <NSXMLParserDelegate>

@property (nonatomic, retain) NSMutableArray *dataSource;

+ (NSString *)buildRequestWithRequestArray:(NSArray *)requestsArray;

@end
