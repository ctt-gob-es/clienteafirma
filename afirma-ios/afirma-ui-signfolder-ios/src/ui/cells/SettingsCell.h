//
//  SettingsCell.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 16/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>

typedef NS_ENUM (NSInteger, SettingsCellType)
{
    SettingsCellTypeServerURL,
    SettingsCellTypeCertificate
};

@interface SettingsCell : UITableViewCell

- (void)setupForType:(SettingsCellType)type;

@end
