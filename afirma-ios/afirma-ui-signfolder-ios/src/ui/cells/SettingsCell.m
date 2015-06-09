//
//  SettingsCell.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 16/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "SettingsCell.h"

#define KEYS_ARRAY @[kPFUserDefaultsKeyCurrentServer, kPFUserDefaultsKeyCurrentCertificate]

static NSString *const kSettingsCellUndefinedTitle = @"Sin especificar";

@interface SettingsCell ()

@property (nonatomic, strong) IBOutlet UILabel *titleLabel;

@end

@implementation SettingsCell

- (void)awakeFromNib {
    // Initialization code
}

- (void)setupForType:(SettingsCellType)type
{
    NSDictionary *typeDict = (NSDictionary *)[[NSUserDefaults standardUserDefaults] objectForKey:KEYS_ARRAY[type]];
    if (typeDict && [typeDict.allKeys containsObject:kPFUserDefaultsKeyAlias]) {
        [_titleLabel setText:typeDict[kPFUserDefaultsKeyAlias]];
        [_titleLabel setTextColor:[UIColor blackColor]];
    } else {
        [self setupForUndefinedValue];
    }
}

- (void)setupForUndefinedValue
{
    [_titleLabel setText:kSettingsCellUndefinedTitle];
    [_titleLabel setTextColor:[UIColor grayColor]];
}

@end
