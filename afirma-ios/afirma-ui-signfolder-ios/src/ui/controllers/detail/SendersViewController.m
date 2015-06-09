//
//  SendersViewController.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana Sánchez on 16/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "SendersViewController.h"

@interface SendersViewController ()

@end

@implementation SendersViewController
@synthesize dataSource = _dataSource;

- (void)viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
    self.navigationController.toolbarHidden = YES;

}

- (void)viewDidLoad
{
    [super viewDidLoad];
    [self.tableView setTableFooterView:[[UIView alloc] initWithFrame:CGRectZero]];

}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{

    // Return the number of sections.
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    // Return the number of rows in the section.
    T21LogDebug(@"SendersViewController::numberOfRowsInSection=%ld. rows=%lu", (long)section, (unsigned long)[_dataSource count]);

    return [_dataSource count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    T21LogDebug(@"SenderViewController::cellForRowAtIndexPath row=%ld", (long)[indexPath row]);

    static NSString *CellIdentifier = @"SendersCell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];

    if (cell == nil) {
        T21LogDebug(@"SendersViewController::cell is nill");
    }

    // Configure the cell...
    NSString *sender = [_dataSource objectAtIndex:[indexPath row]];
    cell.textLabel.text = sender;
    sender = nil;

    return cell;
}

@end
