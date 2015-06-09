//
//  SendersViewController.m
//  PortaFirmas_@Firma
//
//  Created by Antonio Fiñana Sánchez on 19/10/12.
//  Copyright (c) 2012 Luis Lopez. All rights reserved.
//

#import "ReceiversViewController.h"
#import "RequestListXMLController.h"
#import "Detail.h"
#import "SignLine.h"

@interface ReceiversViewController ()

@end

@implementation ReceiversViewController
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

- (void)viewDidUnload
{

    [super viewDidUnload];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{

    // Return the number of sections.
    return [_dataSource count];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    T21LogDebug(@"SendersViewController::numberOfRowsInSection::%@.", [_dataSource description]);

    // Return the number of rows in the section.
    SignLine *signLine = [_dataSource objectAtIndex:section];

    if (signLine.receivers) {
        return [signLine.receivers count];
    } else {
        return 0;
    }
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"ReceiversCell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];

    SignLine *signLine = [_dataSource objectAtIndex:[indexPath section]];

    // Configure the cell...
    cell.textLabel.text = [signLine.receivers objectAtIndex:[indexPath row]];

    return cell;
}

- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section
{

    return [[NSString alloc] initWithFormat:@"Linea de firma %ld", (long)section];
}

#pragma mark - Table view delegate
@end
