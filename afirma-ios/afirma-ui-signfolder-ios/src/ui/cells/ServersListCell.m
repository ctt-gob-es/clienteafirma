//
//  ServersListCell.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 17/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "ServersListCell.h"

@interface ServersListCell ()

@property (nonatomic, strong) IBOutlet UILabel *aliasLabel;
@property (nonatomic, strong) IBOutlet UILabel *urlLabel;

@end

@implementation ServersListCell

- (void)setServerInfo:(NSDictionary *)serverInfo
{
    if (serverInfo) {
        [_aliasLabel setText:[serverInfo.allKeys containsObject:kPFUserDefaultsKeyAlias] ? serverInfo[kPFUserDefaultsKeyAlias] : @""];
        [_urlLabel setText:[serverInfo.allKeys containsObject:kPFUserDefaultsKeyURL] ? serverInfo[kPFUserDefaultsKeyURL] : @""];
    }
}

@end
