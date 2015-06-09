//
//  ServerListTVC.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 17/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "PFBaseTVC.h"

@protocol ServerListTVCDelegate <NSObject>

@required
- (void)serverListDidSelectServer:(NSDictionary *)serverInfo;

@end

@interface ServerListTVC : PFBaseTVC <UIAlertViewDelegate>

@property (nonatomic, weak) id<ServerListTVCDelegate> delegate;

@end
