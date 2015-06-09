//
//  RequestCellNoUI.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 26/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "PFRequest.h"

@interface RequestCellNoUI : UITableViewCell

@property (nonatomic, strong) IBOutlet UILabel *title;
@property (nonatomic, strong) IBOutlet UILabel *detail;
@property (nonatomic, strong) IBOutlet UILabel *inputDate;
@property (nonatomic, strong) IBOutlet UIImageView *iconRequestType;

- (void)setPFRequest:(PFRequest *)request;

@end