//
//  ServerListTVC.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 17/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "ServerListTVC.h"
#import "ServersListCell.h"

static NSString *const kServerListTVCCellIdentifier = @"ServersListCell";

@interface ServerListTVC ()
{
    NSMutableArray *_serversArray;
}

@property (strong, nonatomic) IBOutlet UIBarButtonItem *editBarButtonItem;

@end

@implementation ServerListTVC

#pragma mark - Life Cycle

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    [self.tableView setTableFooterView:[[UIView alloc] initWithFrame:CGRectZero]];
    [self.navigationItem setRightBarButtonItems:@[self.navigationItem.rightBarButtonItem, _editBarButtonItem] animated:YES];
}

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
    
    _serversArray = [[[[NSUserDefaults standardUserDefaults] arrayForKey:kPFUserDefaultsKeyServersArray] sortedArrayUsingComparator:^NSComparisonResult(NSDictionary *serverInfo1, NSDictionary *serverInfo2) {
        return [serverInfo1[kPFUserDefaultsKeyAlias] compare:serverInfo2[kPFUserDefaultsKeyAlias] options:NSCaseInsensitiveSearch];
    }] mutableCopy];
    
    [self.tableView reloadData];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
    _serversArray = nil;
}

#pragma mark - User Interaction

- (IBAction)didClickEditButton:(id)sender
{
    if ([_serversArray count] > 0) {
        [self editTable:!self.editing];
    }
}

#pragma mark - Navigation

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([segue.identifier isEqualToString:@"showAddServerVC"]) {
        if (self.isEditing) {
            [self editTable:NO];
        }
    }
}

#pragma mark - UITableViewDataSource

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return _serversArray ? _serversArray.count : 0;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    ServersListCell *cell = [self.tableView dequeueReusableCellWithIdentifier:kServerListTVCCellIdentifier];
    
    if (!cell) {
        T21LogError(@"ServersListVC::cellForRowAtIndexPath - Cell is nil");
        return nil;
    }
    
    [cell setServerInfo:_serversArray[indexPath.row]];
    
    return cell;
}

- (void)editTable:(BOOL)edit
{
    [self setEditing:edit animated:NO];
    [self.tableView reloadData];
    
    [_editBarButtonItem setTitle:edit ? @"Hecho" : @"Editar"];
}

- (UITableViewCellEditingStyle)tableView:(UITableView *)aTableView editingStyleForRowAtIndexPath:(NSIndexPath *)indexPath
{
    return UITableViewCellEditingStyleDelete;
}

- (NSString *)tableView:(UITableView *)tableView titleForDeleteConfirmationButtonForRowAtIndexPath:(NSIndexPath *)indexPath
{
    return @"Eliminar";
}

- (void)tableView:(UITableView *)aTableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
{
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        [self.tableView beginUpdates];
        [_serversArray removeObjectAtIndex:indexPath.row];
        [[NSUserDefaults standardUserDefaults] setObject:_serversArray forKey:kPFUserDefaultsKeyServersArray];
        [[NSUserDefaults standardUserDefaults] synchronize];
        [self.tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationAutomatic];
        [self.tableView endUpdates];
    }
    
}

#pragma mark - UITableViewDelegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    NSDictionary *serverInfo = _serversArray[indexPath.row];
    [[[UIAlertView alloc] initWithTitle:@""
                                message:[NSString stringWithFormat:@"Se va a seleccionar el servidor %@ con url %@.", serverInfo[kPFUserDefaultsKeyAlias],serverInfo[kPFUserDefaultsKeyURL]]
                               delegate:self
                      cancelButtonTitle:@"Cancelar"
                      otherButtonTitles:@"Aceptar", nil] show];
}

#pragma mark - UIAlertViewDelegate

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex
{
    if (buttonIndex != kPFAlertViewCancelButtonIndex) {
        NSDictionary *serverInfo = _serversArray[[self.tableView indexPathForSelectedRow].row];
        [[NSUserDefaults standardUserDefaults] setObject:serverInfo forKey:kPFUserDefaultsKeyCurrentServer];
        [[NSUserDefaults standardUserDefaults] synchronize];
        if (_delegate) {
            [_delegate serverListDidSelectServer:serverInfo];
        }
        
        [self.navigationController popViewControllerAnimated:YES];
    }
}

@end
