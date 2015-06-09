//
//  AddServerVC.m
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 17/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "AddServerVC.h"

@interface AddServerVC ()

@property (nonatomic, weak) IBOutlet UIBarButtonItem *saveBarButtonItem;
@property (nonatomic, weak) IBOutlet UITextField *aliasTextField;
@property (nonatomic, weak) IBOutlet UITextField *urlTextField;

@end

@implementation AddServerVC

#pragma mark - Life Cycle

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    [self updateSaveButton];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - User Interface

- (void)updateSaveButton
{
    NSString *alias = [_aliasTextField.text stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    NSString *url = [_urlTextField.text stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    
    [_saveBarButtonItem setEnabled:alias && alias.length > 0 && url && url.length > 0];
}

#pragma mark - User Interaction

- (IBAction)didClickCancelButton:(id)sender
{
    [self dismissViewControllerAnimated:YES completion:nil];
}

- (IBAction)didClickSaveButton:(id)sender
{
    NSString *alias = [_aliasTextField.text stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    NSString *url = [_urlTextField.text stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    
    NSUserDefaults *standardUserDefaults = [NSUserDefaults standardUserDefaults];
    NSArray *defaultsKeys = standardUserDefaults.dictionaryRepresentation.allKeys;
    NSMutableArray *serversArray = [[defaultsKeys containsObject:kPFUserDefaultsKeyServersArray] ? [standardUserDefaults arrayForKey:kPFUserDefaultsKeyServersArray] : @[] mutableCopy];
    
    [serversArray addObject:@{kPFUserDefaultsKeyAlias:alias, kPFUserDefaultsKeyURL:url}];
    [[NSUserDefaults standardUserDefaults] setObject:serversArray forKey:kPFUserDefaultsKeyServersArray];
    [[NSUserDefaults standardUserDefaults] synchronize];
    
    [self dismissViewControllerAnimated:YES completion:nil];
}

- (IBAction)textFieldDidChange:(id)sender
{
    [self updateSaveButton];
}

#pragma mark - UITextFieldDelegate

- (BOOL)textFieldShouldReturn:(UITextField *)textField
{
    [textField resignFirstResponder];
    
    return YES;
}

@end
