//
//  AOPinDropBoxCloudViewController.m
//  SignSample02
//
//

#import "AOPinDropBoxCloudViewController.h"
#import <DropboxSDK/DropboxSDK.h>
#import "CADESSignUtils.h"

@interface AOPinDropBoxCloudViewController ()

@end

@implementation AOPinDropBoxCloudViewController

@synthesize paramNameCertDropBox;
@synthesize paramUrlCertDropBox;
@synthesize nameCertDropBoxCloud;
@synthesize pinCertDropBoxCloud;

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
	// Do any additional setup after loading the view.
    
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(onGoingToBackGround:) name:UIApplicationDidEnterBackgroundNotification object:nil];
    
    [[self navigationController] setNavigationBarHidden:YES animated:YES];
    //etiqueta con el nombre del almacen seleccionado
    NSString *etiqueta =  NSLocalizedString(@"introducir_contrasenia",nil);
    if(self.paramNameCertDropBox!=NULL){
        etiqueta = [etiqueta stringByAppendingString:@"'"];
        etiqueta = [etiqueta stringByAppendingString:self.paramNameCertDropBox];
        etiqueta = [etiqueta stringByAppendingString:@"'"];
    }
    self.nameCertDropBoxCloud.text = etiqueta;
    
    self.screenName = @"IOS AOPinDropBoxCloudViewController - Dropbox cloud pin request";

}

-(IBAction)pinButtonPressed:(id)sender{
    //self.pinCertDropBoxCloud = sender;
    [self checkData ];
}

//evento que se lanza
-(BOOL)textFieldShouldReturn:(UITextField*)theTextField {
    [self.pinCertDropBoxCloud resignFirstResponder];
    [self checkData ];
    return YES;
}

-(BOOL) checkData{
    if ([self checkPin:self.pinCertDropBoxCloud.text]) {
        
        NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
        NSString *documentsDirectory = [paths objectAtIndex:0];
        
        NSFileManager *manager = [NSFileManager defaultManager];
        
        NSError *error;
        BOOL success=false;
        
        NSString *destination = [documentsDirectory stringByAppendingString:@"/"];
        destination = [destination stringByAppendingString:paramNameCertDropBox];
        
        NSLog(@"Destino: %@", destination);
        
        success = [manager copyItemAtPath:paramUrlCertDropBox toPath:destination error:&error];
        
        if (!success){
            NSLog(@"Error. No se ha podido copiar el fichero: %@", error.localizedFailureReason);
        }
        
        [self performSegueWithIdentifier:@"toStoreManager" sender:self];
    }
    return YES;
}

#pragma mark AlertView Delegate
-(void) alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex
{
    if (buttonIndex == alertView.cancelButtonIndex)
    {
       [self performSegueWithIdentifier:@"toStoreManager" sender:self];
    }
}


-(BOOL) checkPin:(NSString*)pin {
    OSStatus status = [self openPkcs12Store:pin];
    
    if (status == 0) {
        [self.pinCertDropBoxCloud setText:nil];
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
        
        [self.pinCertDropBoxCloud setText:nil];
        
    }
    else {
        
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"error",nil) message: NSLocalizedString(@"error_carga_almacen",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:nil];
        
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
         NSLog(@"Error al cargar el almacen de claves: %ld", status);
        [self.pinCertDropBoxCloud setText:nil];
    }
    return NO;
}


-(OSStatus) openPkcs12Store:(NSString*)pin {
    
    NSData *PKCS12Data = [[NSData alloc] initWithContentsOfFile:self.paramUrlCertDropBox];
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
    
    CFStringRef certSummary = SecCertificateCopySubjectSummary(myReturnedCertificate);
    NSString* summaryString = [[NSString alloc] initWithString:(NSString*)certSummary];
    
    NSLog(@"Leido: %@", summaryString);
    
    return status;
    
}

//cuando se pulsa el botón del centro
-(void)onGoingToBackGround:(NSNotification*) notification {
    //Destruimos la sesión
    [[DBSession sharedSession] unlinkAll];
    @try {
        [self performSegueWithIdentifier:@"toFirstScreen" sender:self];
    }
    @catch (NSException *e) {
        // Se ignora
    }
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)dealloc {
    [nameCertDropBoxCloud release];
    [pinCertDropBoxCloud release];
    [_buttonCertDropBoxCloud release];
    [super dealloc];
}
- (void)viewDidUnload {
    [self setNameCertDropBoxCloud:nil];
    [self setPinCertDropBoxCloud:nil];
    [self setButtonCertDropBoxCloud:nil];
    [super viewDidUnload];
}
@end
