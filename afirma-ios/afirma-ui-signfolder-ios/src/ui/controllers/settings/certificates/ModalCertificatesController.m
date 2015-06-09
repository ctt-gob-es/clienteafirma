//
//  ModalCertificatesController.m
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana on 10/12/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "ModalCertificatesController.h"
#import "CertificateUtils.h"

@interface ModalCertificatesController ()

@end

@implementation ModalCertificatesController
@synthesize selectedCertificate = _selectedCertificate;
@synthesize registrarBtn;

#pragma mark - User Interaction

- (IBAction)clickImport:(id)sender
{
    T21LogDebug(@"ClickImport");
    _password = _passwordText.text;

    if (!_password || [_password isEqualToString:@""]) {
        __messageView.text = @"Por favor, introduce la contraseña del certificado";
    } else {
        [self registerWithCertificate];
    }
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    [_passwordText becomeFirstResponder];
}

- (void)viewDidUnload
{
    [self setPasswordText:nil];
    [self set_messageView:nil];
    [super viewDidUnload];
}

- (void)registerWithCertificate
{
    OSStatus status = noErr;

#if TARGET_IPHONE_SIMULATOR
    // Load certificate from bundle
    status = [[CertificateUtils sharedWrapper] loadCertKeyChainWithName:_selectedCertificate password:_password fromDocument:NO];
#else
    // Load certificate from Documents directory
    status = [[CertificateUtils sharedWrapper] loadCertKeyChainWithName:_selectedCertificate password:_password fromDocument:YES];
    [SVProgressHUD dismiss];
#endif

    if (status != noErr) {
        switch (status) {
            case errSecItemNotFound:
                _infoLabel = @"No se ha encontrado el certificado";
                break;
            case errSecAuthFailed:
                _infoLabel = @"Contraseña incorrecta";
                break;
            case errSecDuplicateItem:
                _infoLabel = @"El certificado ya estaba cargado";
                break;
            default:
                _infoLabel = [NSString stringWithFormat:@"Se ha producido un error(%d)", (int)status];
                break;
        }
    } else {
        _infoLabel = @"El certificado se ha cargado correctamente";

        T21LogDebug(@"registerWithCertificateName::Certificate is loaded");
        // Volvemos a la pantalla anterior
//        [self.navigationController popViewControllerAnimated:YES];
        if (_delegate) {
            [_delegate certificateAdded];
        }

        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Certificado cargado" message:@"El certificado se ha cargado correctamente en su aplicación." delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil];
        [alert show];
    }

    __messageView.text = _infoLabel;

    return;
}

- (void)alertView:(UIAlertView *)alertView willDismissWithButtonIndex:(NSInteger)buttonIndex
{
    // Boton OK presionado - certificado cargado correctamente
    if (buttonIndex == 0) {
        T21LogDebug(@"registerWithCertificateName::Certificado cargado");
        T21LogDebug(@"registerWithCertificateName::Volvemos a la vista anterior...");
    }

    [_passwordText resignFirstResponder];
    [_passwordText removeFromSuperview];
    //  [_passwordText = nil];

}

/* Boton volver*/
- (IBAction)clickCancel:(id)sender
{
    T21LogDebug(@"registerWithCertificateName::Volvemos a la vista anterior...");
    [self.navigationController popViewControllerAnimated:YES];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    UIInterfaceOrientation des = self.interfaceOrientation;

    if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad) { // iPad
        if (des == UIInterfaceOrientationPortrait || des == UIInterfaceOrientationPortraitUpsideDown) { // ipad-portrait

        } else { // ipad -landscape

        }
    } else { // iphone
        UIInterfaceOrientation des = self.interfaceOrientation;

        if (des == UIInterfaceOrientationPortrait || des == UIInterfaceOrientationPortraitUpsideDown) { // iphone portrait

        } else { // iphone -landscape

        }
    }

    return YES;
}

@end
