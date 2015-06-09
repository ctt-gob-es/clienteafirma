//
//  RequestCellNoUI.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 26/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "RequestCellNoUI.h"

@implementation RequestCellNoUI

- (void)setPFRequest:(PFRequest *)request
{
    [_title setText:request.snder];
    [_detail setText:request.subj];
    [_inputDate setText:request.date];
    [self setupRequestTypeIcon:request.type];
    [self setBackgroundColor:request.isNew ? ThemeColorWithAlpha(0.08):[UIColor clearColor]];
}

- (void)setupRequestTypeIcon:(PFRequestType)type
{
    NSString *iconImageName = type == PFRequestTypeSign ? @"icn_firma" : @"icn_check";

    [_iconRequestType setImage:[QuartzUtils getImageWithName:iconImageName andTintColor:THEME_COLOR]];
}

@end
