//
//  AOAboutViewController.m
//  SignSample02
//
//

#import "AOAboutViewController.h"
#import "AOCertificateSelectionViewController.h"
#import "CADESConstants.h"

@interface AOAboutViewController ()

@end

@implementation AOAboutViewController

NSString *startUrlIncoming= NULL;

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
        //Nos desplazamos a la pantalla de ayuda
        @try {
            [self performSegueWithIdentifier:@"toHelpScreen" sender:self];
        }
        @catch (NSException *e) {
            // Se ignora
            NSLog(@"no se ha podido lanzar el segue %@",e);
        }
    }
    else
    {
        //Nos desplazamos a la pantalla de seleccion de almacenes
        @try {
            [self performSegueWithIdentifier:@"toStoreManager" sender:self];
        }
        @catch (NSException *e) {
            // Se ignora
            NSLog(@"no se ha podido lanzar el segue %@",e);
        }
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
