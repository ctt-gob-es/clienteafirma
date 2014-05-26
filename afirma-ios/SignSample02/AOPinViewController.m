//
//  AOPinViewController.m
//  SignSample02
//
//

#import "AOPinViewController.h"
#import "CADESSignUtils.h"
#import "AOSignViewController.h"

@interface AOPinViewController ()

@end

@implementation AOPinViewController
@synthesize nombreCertInUse = _nombreCertInUse;
@synthesize nombreCert = _nombreCert;
@synthesize pinTextField = _pinTextField;
@synthesize base64UrlSafeCertificateData = _base64UrlSafeCertificateData;
@synthesize certificateName = _certificateName;
@synthesize parameters;

SecKeyRef privateKeyPkcs12 = NULL;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
	[[self navigationController] setNavigationBarHidden:YES animated:YES];
    //etiqueta con el nombre del almacen seleccionado
    NSString *etiqueta =  NSLocalizedString(@"introducir_contrasenia",nil);
    if(self.nombreCertInUse!=NULL){
        etiqueta = [etiqueta stringByAppendingString:@"'"];
        etiqueta = [etiqueta stringByAppendingString:self.nombreCertInUse];
        etiqueta = [etiqueta stringByAppendingString:@"'"];
        
       
    }
    self.nombreCert.text = etiqueta;
    
    self.screenName = @"IOS AOPinViewController - certificate pin window";
}

// Evento de teclear el el campo de texto del PIN
-(IBAction)pinButtonPressed:(id)sender {
    self.pinTextField = sender;
}

//cuando se pulsa el botón del centro
-(void)onGoingToBackGround:(NSNotification*) notification {
    @try {
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
    }
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([[segue identifier] isEqualToString:@"pinEntered"]) {
        
        // Get destination view
        AOSignViewController *vc = [segue destinationViewController];        
        
        // Set the selected button in the new view
        [vc setParameters:self.parameters];
        [vc setCertificateName:self.certificateName];
        [vc setBase64UrlSafeCertificateData:self.base64UrlSafeCertificateData];
        [vc setPrivateKey:&privateKeyPkcs12];
    }
}

//evento que se lanza
-(BOOL)textFieldShouldReturn:(UITextField*)theTextField {
    [self.pinTextField resignFirstResponder];
    if ([self checkPin:self.pinTextField.text]) {
        [self performSegueWithIdentifier:@"pinEntered" sender:self];
    }
    return YES;
}

//Se invoca cuando se realiza cualquier accion
- (BOOL)shouldPerformSegueWithIdentifier:(NSString *)identifier sender:(id)sender {

    //antes de pasar a la pantalla de firma, comprobamos que se introducido correctamente el pin.
    if([identifier isEqualToString:@"pinEntered"])
        return [self checkPin:self.pinTextField.text];
    else
        return YES;
}


-(BOOL) checkPin:(NSString*)pin {
    OSStatus status = [self openPkcs12Store:pin];
    
    if (status == 0) {
        [self.pinTextField setText:nil];
        return YES;
    }
    if (status == -25293) {
                
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_contrasenia",nil) delegate:self cancelButtonTitle: NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
        UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(75, 6, 40, 40)];
        
        NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"warning_mini.png"]];
        UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
        [imageView setImage:bkgImg];
        [bkgImg release];
        [path release];
        
        [alert addSubview:imageView];
        [imageView release];
        
        [alert show];
        [alert release];
        
        [self.pinTextField setText:nil];
        
    }
    else {
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message:NSLocalizedString(@"error_carga_almacen",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
        UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(75, 6, 40, 40)];
        
        NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"warning_mini.png"]];
        UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
        [imageView setImage:bkgImg];
        [bkgImg release];
        [path release];
        
        [alert addSubview:imageView];
        [imageView release];
        
        [alert show];
        [alert release];
        
        [self.pinTextField setText:nil];
    }    
    return NO;
}


-(OSStatus) openPkcs12Store:(NSString*)pin {
    
    // Cargamos el PKCS#12 desde como un recurso
    NSString *thePath = NULL;
#if TARGET_IPHONE_SIMULATOR
    
    // Cargamos el PKCS#12 desde como un recurso
    thePath = [[NSBundle mainBundle] pathForResource:@"ANF_PF_Activo" ofType:@"p12"];
    
#else
    
    // Cargamos el PKCS#12 con el nombre de la celda seleccionada.
    thePath = [self loadFileFromDocumentsFolder:self.nombreCertInUse];
    
#endif
    
    NSData *PKCS12Data = [[NSData alloc] initWithContentsOfFile:thePath];
    CFDataRef inPKCS12Data = (CFDataRef) CFBridgingRetain(PKCS12Data);
    
    if (inPKCS12Data == NULL) {
        return 255;
    }
    
    OSStatus status = noErr;
	SecIdentityRef myIdentity;
	SecTrustRef myTrust;
    
	status = [CADESSignUtils extractIdentityAndTrust:inPKCS12Data :pin :&myIdentity :&myTrust];
	
	if (status != 0) {
        return status;
	}
    
    SecCertificateRef myReturnedCertificate = NULL;
    status = SecIdentityCopyCertificate (myIdentity, &myReturnedCertificate);
    
    if (status != 0){
        return status;
    }
    
    self.base64UrlSafeCertificateData = [CADESSignUtils encodeBase64:(NSData*) CFBridgingRelease(SecCertificateCopyData(myReturnedCertificate))];
    //NSLog(@"Certificado en Base64: %@", base64UrlSafeCertificateData);
    
    status = SecIdentityCopyPrivateKey(myIdentity, &privateKeyPkcs12);
    
    if (status != 0){
        return status;
    }
    
    CFStringRef certSummary = SecCertificateCopySubjectSummary(myReturnedCertificate);
    NSString* summaryString = [[NSString alloc] initWithString:(NSString*)certSummary];
    
    self.certificateName = summaryString;
    
    return status;
    
}

//Carga carga el almacen de certificados del directorio iTunes.
-(NSString*)loadFileFromDocumentsFolder:(NSString *) filename {
    // Se obtiene el almacen de la carpeta de documentos.
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    return [documentsDirectory stringByAppendingPathComponent:filename];
    
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (void)dealloc {
    [_nombreCert release];
    [_nombreCert release];
    [_pinTextField release];
    [_pinButton release];
    [super dealloc];
}
- (void)viewDidUnload {
    [self setNombreCert:nil];
    [self setNombreCert:nil];
    [self setPinTextField:nil];
    [self setPinButton:nil];
    [super viewDidUnload];
}
@end
