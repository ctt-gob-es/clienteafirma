//
//  AORegisterCertificateVC.m
//  SignSample02
//
//  Created by Rocio Tovar on 25/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import "AORegisterCertificateVC.h"
#import "CertificateUtils.h"
#import "ColorChart.h"

@interface AORegisterCertificateVC ()
{
    NSString *_password;
    NSString *_message;
}

@end

@implementation AORegisterCertificateVC

#pragma mark - Life Cycle

- (void)viewDidLoad {
    [super viewDidLoad];
    
    [self.navigationController.navigationBar setTintColor:THEME_COLOR];
    [_selectedCertificateLabel setText:_selectedCertificate];
    [_passwordTextField becomeFirstResponder];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - User Interaction

- (IBAction)didClickRegister:(id)sender
{
    _password = _passwordTextField.text;
    
    if (!_password || [_password isEqualToString:@""]) {
        _messageLabel.text = @"Por favor, introduce la contraseña del certificado";
    } else {
        [self registerWithCertificate];
    }
}

- (IBAction)didClickCancel:(id)sender
{
    [self.navigationController popViewControllerAnimated:YES];
}

#pragma mark - Certificates Methods

- (void)registerWithCertificate
{
    OSStatus status = noErr;
    
#if TARGET_IPHONE_SIMULATOR
    // Load certificate from bundle
    status = [[CertificateUtils sharedWrapper] loadCertKeyChainWithName:_selectedCertificate password:_password fromDocument:NO];
#else
    // Load certificate from Documents directory
    status = [[CertificateUtils sharedWrapper] loadCertKeyChainWithName:_selectedCertificate password:_password fromDocument:YES];
#endif
    
    if (status != noErr) {
        switch (status) {
            case errSecItemNotFound:
                _message = @"No se ha encontrado el certificado";
                break;
            case errSecAuthFailed:
                _message = @"Contraseña incorrecta";
                break;
            case errSecDuplicateItem:
                _message = @"El certificado ya estaba cargado";
                break;
            default:
                _message = [NSString stringWithFormat:@"Se ha producido un error(%d)", (int)status];
                break;
        }
    } else {
        _message = @"El certificado se ha cargado correctamente";
        
        if (_delegate) {
            [_delegate certificateAdded];
        }
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Certificado cargado"
                                                        message:@"El certificado se ha cargado correctamente en su aplicación."
                                                       delegate:nil
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
        [alert show];
    }
    
    _messageLabel.text = _message;
    
    return;
}

#pragma mark - UITextFieldDelegate

- (BOOL)textFieldShouldReturn:(UITextField *)textField
{
    [textField resignFirstResponder];
    
    return YES;
}

@end
