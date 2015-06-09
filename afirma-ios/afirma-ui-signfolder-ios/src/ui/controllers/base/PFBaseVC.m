//
//  PFBaseVC.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 17/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "PFBaseVC.h"

@interface PFBaseVC ()

@end

@implementation PFBaseVC

#pragma mark - Life Cycle

- (void)viewDidLoad {
    [super viewDidLoad];
    [self addWatermark];
}

#pragma mark - User Interface

- (void)addWatermark
{
    UIImageView *watermarkIV = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"logo_transp"]];
    
    [watermarkIV setFrame:self.view.bounds];
    [watermarkIV setAutoresizingMask:UIViewAutoresizingFlexibleHeight | UIViewAutoresizingFlexibleWidth ];
    [watermarkIV setContentMode:UIViewContentModeCenter];
    [self.view addSubview:watermarkIV];
    [self.view sendSubviewToBack:watermarkIV];
}

@end
