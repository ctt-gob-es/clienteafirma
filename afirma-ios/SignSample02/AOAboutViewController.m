//
//  AOAboutViewController.m
//  SignSample02
//
//

#import "AOAboutViewController.h"
#import "AOCertificateSelectionViewController.h"
#import <DropboxSDK/DropboxSDK.h>
#import "AODropBoxCloudViewController.h"
#import "AOUrlDropbox.h"
#import "CADESConstants.h"
#import "AODropboxSession.h"

@interface AOAboutViewController () <DBSessionDelegate, DBNetworkRequestDelegate>

@end

@implementation AOAboutViewController

NSString *startUrlIncoming= NULL;
NSString *urlDropbox = NULL;

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
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(onReadUrl:) name:@"urlReaded" object:nil];
	// Do any additional setup after loading the view.
    
    self.screenName = @"IOS AOAboutViewController - Main window";
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

-(void) onReadUrl:(NSNotification*) notification {

    NSString *url = [notification object];
    //NSLog(@"URL notificada: %@", url);
   
    NSString *base= @"db-";
    NSString *urlManage = [base stringByAppendingString:URL_STORE];
    NSString *urlSign = [base stringByAppendingString:URL_SIGN];
    
    //ES LA URL DE ALMACENAMIENTO DE ALMACENES db-n3qvxtrnqshhs3p://
    if([url hasPrefix:urlManage]){
        urlDropbox = url;
        
        AOUrlDropbox *obj=[AOUrlDropbox getInstance];
        obj.str= urlDropbox;
        
        if (![[DBSession sharedSession] isLinked]) {
                        
            if ([urlDropbox rangeOfString:@"cancel"].location != NSNotFound) {
                @try {
                    [self performSegueWithIdentifier:@"toStoreManager" sender:self];
                }
                @catch (NSException *e) {
                    // Se ignora
                    NSLog(@"no se ha podido lanzar el segue %@",e);
                }
            }
            else{
                @try {
                    [self performSegueWithIdentifier:@"toDropBoxCloud" sender:self];
                }
                @catch (NSException *e) {
                    // Se ignora
                    NSLog(@"no se ha podido lanzar el segue %@",e);
                }
            }
                        
            //[[DBSession sharedSession] linkFromController:self];
        } else {
            /*
            [[DBSession sharedSession] unlinkAll];
            [[[[UIAlertView alloc]
               initWithTitle:@"Account Unlinked!" message:@"Your dropbox account has been unlinked"
               delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil]
              autorelease]
             show];
             */
            
            //Nos desplazamos a la pantalla de gestion de almacenes
            @try {
                [self performSegueWithIdentifier:@"toStoreManager" sender:self];
            }
            @catch (NSException *e) {
                // Se ignora
                NSLog(@"no se ha podido lanzar el segue %@",e);
            }
        }
        
        
    }
    
    else if ([url hasPrefix:urlSign]){
        urlDropbox = url;
        
        AOUrlDropbox *obj=[AOUrlDropbox getInstance];
        obj.str= urlDropbox;
        
        if (![[DBSession sharedSession] isLinked]) {
            
            if (!([urlDropbox rangeOfString:@"cancel"].location != NSNotFound)) {
                
                //Nos desplazamos a la pantalla de seleccion de gestion de almacenes
                @try {
                    [self performSegueWithIdentifier:@"toDropBoxSignScreen" sender:self];
                }
                @catch (NSException *e) {
                    // Se ignora
                    NSLog(@"no se ha podido lanzar el segue %@",e);
                }
            }
            
        } 
        
    }
    
    // ES LA URL DE AFIRMA://
    else{
        //pasar al segue
        startUrlIncoming = url;
        
        
        //Nos desplazamos a la pantalla de seleccion de pkcs12.
        @try {
            [self performSegueWithIdentifier:@"toSelectionScreen" sender:self];
        }
        @catch (NSException *e) {
            // Se ignora
            NSLog(@"no se ha podido lanzar el segue %@",e);
        }
    }
    
    
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
        
    if ([[segue identifier] isEqualToString:@"toSelectionScreen"]) {
        
        // Get destination view
        AOCertificateSelectionViewController *vc = [segue destinationViewController];
               
        
        // Set the selected button in the new view
        [vc setStartUrl:startUrlIncoming];
    }
}

- (void)tabBar:(UITabBar *)tabBar didSelectItem:(UITabBarItem *)item {
  
    if(item.tag == 0){
        //Nos desplazamos a la pantalla de seleccion de gestion de almacenes
        @try {
            [self performSegueWithIdentifier:@"toHelpScreen" sender:self];
        }
        @catch (NSException *e) {
            // Se ignora
            NSLog(@"no se ha podido lanzar el segue %@",e);
        }
    }
    else if(item.tag == 1){
        AODropboxSession *sess = [AODropboxSession getInstance];
        if(![sess.session isEqualToString:URL_SIGN]){
            [[DBSession sharedSession] unlinkAll];
            sess.session=URL_SIGN;
        }
        [self createSignDropBoxConection];

    }
    else if (item.tag == 2){        
        //Nos desplazamos a la pantalla de seleccion de ayuda
        @try {
            AODropboxSession *sess = [AODropboxSession getInstance];
            if(![sess.session isEqualToString:URL_STORE]){
                [[DBSession sharedSession] unlinkAll];
                sess.session=URL_STORE;
            }
            [self performSegueWithIdentifier:@"toStoreManager" sender:self];
        }
        @catch (NSException *e) {
            // Se ignora
            NSLog(@"no se ha podido lanzar el segue %@",e);
        }
    }
}

- (void) createSignDropBoxConection{
    
        NSDictionary *launchOptions=nil;
        
        // Set these variables before launching the app
        NSString* appKey = URL_SIGN;
        NSString* appSecret = PASS_SIGN;
        NSString *root = kDBRootAppFolder; // Should be set to either kDBRootAppFolder or kDBRootDropbox
        // You can determine if you have App folder access or Full Dropbox along with your consumer key/secret
        // from https://dropbox.com/developers/apps
        
        // Look below where the DBSession is created to understand how to use DBSession in your app
        
        DBSession* session =
        [[DBSession alloc] initWithAppKey:appKey appSecret:appSecret root:root];
        session.delegate = self; // DBSessionDelegate methods allow you to handle re-authenticating
        [DBSession setSharedSession:session];
        [session release];
        
        [DBRequest setNetworkRequestDelegate:self];
        // Add the navigation controller's view to the window and display.
        //[window addSubview:navigationController.view];
        //[window makeKeyAndVisible];
        
        NSURL *launchURL = [launchOptions objectForKey:UIApplicationLaunchOptionsURLKey];
        NSInteger majorVersion =
        [[[[[UIDevice currentDevice] systemVersion] componentsSeparatedByString:@"."] objectAtIndex:0] integerValue];
        if (launchURL && majorVersion < 4) {
            // Pre-iOS 4.0 won't call application:handleOpenURL; this code is only needed if you support
            // iOS versions 3.2 or below
            //[self application:application handleOpenURL:launchURL];
        }
        
        if (![[DBSession sharedSession] isLinked]) {
            [[DBSession sharedSession] linkFromController:self];
        }

    if ([[DBSession sharedSession] isLinked]) {
        
        //Nos desplazamos a la pantalla de gestion de almacenes
        @try {
            [self performSegueWithIdentifier:@"toDropBoxSignScreen" sender:self];
        }
        @catch (NSException *e) {
            // Se ignora
            NSLog(@"no se ha podido lanzar el segue %@",e);
        }
    }
}



#pragma mark -
#pragma mark DBSessionDelegate methods

- (void)sessionDidReceiveAuthorizationFailure:(DBSession*)session userId:(NSString *)userId {
	relinkUserId = [userId retain];
    
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:NSLocalizedString(@"atencion",nil) message:NSLocalizedString(@"sesion_finalizada",nil) delegate:self cancelButtonTitle:NSLocalizedString(@"cerrar",nil) otherButtonTitles:NSLocalizedString(@"conectar",nil), nil];
    
    UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(50, 6, 40, 40)];
    
    NSString *path = [[NSString alloc] initWithString:[[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"info_mini.png"]];
    UIImage *bkgImg = [[UIImage alloc] initWithContentsOfFile:path];
    [imageView setImage:bkgImg];
    [bkgImg release];
    [path release];
    
    [alert addSubview:imageView];
    [imageView release];
    
    [alert show];
    [alert release];
    
	
}


#pragma mark -
#pragma mark UIAlertViewDelegate methods

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)index {
	if (index != alertView.cancelButtonIndex) {
		[[DBSession sharedSession] linkUserId:relinkUserId fromController:self];
	}
	[relinkUserId release];
	relinkUserId = nil;
}


#pragma mark -
#pragma mark DBNetworkRequestDelegate methods

static int outstandingRequests;

- (void)networkRequestStarted {
	outstandingRequests++;
	if (outstandingRequests == 1) {
		[[UIApplication sharedApplication] setNetworkActivityIndicatorVisible:YES];
	}
}

- (void)networkRequestStopped {
	outstandingRequests--;
	if (outstandingRequests == 0) {
		[[UIApplication sharedApplication] setNetworkActivityIndicatorVisible:NO];
	}
}


- (void)dealloc {
    [_manageStoreButton release];
    [tabBar release];
    [super dealloc];
}
- (void)viewDidUnload {
    [self setManageStoreButton:nil];
    [tabBar release];
    tabBar = nil;
    [super viewDidUnload];
}
@end
