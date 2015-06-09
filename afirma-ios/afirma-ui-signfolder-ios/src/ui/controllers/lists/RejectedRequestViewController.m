//
//  RejectedRequestViewController.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana Sánchez on 06/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "RejectedRequestViewController.h"

@interface RejectedRequestViewController ()

@end

@implementation RejectedRequestViewController

#pragma mark - Init Methods

- (id)initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];

    if (self) {
        self.dataStatus = kBaseListVCDataStatusRejected;
    }

    return self;
}

#pragma mark - Life Cycle

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
    [self.parentViewController setHidesBottomBarWhenPushed:TRUE];
    [self.navigationController setToolbarHidden:YES];
}

- (void)viewWillDisappear:(BOOL)animated
{
    [super viewWillDisappear:animated];
    [self.parentViewController setHidesBottomBarWhenPushed:TRUE];
    [self.navigationController setToolbarHidden:YES];
}

#pragma mark - User Interaction

- (void)goBack:(id)sender
{
    [self.parentViewController.navigationController popViewControllerAnimated:TRUE];
}

- (IBAction)didTapOnBackButton:(id)sender
{
    [self.navigationController dismissViewControllerAnimated:YES completion:nil];
}

#pragma mark - Navigation Methods

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    T21LogDebug(@"BaseListTVC::prepareForSegueWithIdentifier=%@", [segue identifier]);

    if ([[segue identifier] isEqualToString:@"segueDetail"]) {
        [self prepareForDetailSegue:segue enablingSigning:NO];
    }
}

@end
