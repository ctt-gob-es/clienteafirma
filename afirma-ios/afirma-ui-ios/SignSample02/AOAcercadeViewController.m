//
//  AOAcercadeViewController.m
//  SignSample02
//
//

#import "AOAcercadeViewController.h"
#import <QuartzCore/QuartzCore.h>

@interface AOAcercadeViewController ()

@end

@implementation AOAcercadeViewController

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
    
    self.screenName = @"IOS AOAboutViewController - Help Screen";
    
}

-(IBAction)buttonPressed:(id)sender {
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString: NSLocalizedString(@"url_forja",nil)]];
}

- (IBAction)didClickBackButton:(id)sender
{
    [self.navigationController popViewControllerAnimated:YES];
}

//cuando se pulsa el botón del centro
-(void)onGoingToBackGround:(NSNotification*) notification {
    [self.navigationController popToRootViewControllerAnimated:YES];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)dealloc {
    [_masInfoButton release];
    [super dealloc];
}
- (void)viewDidUnload {
    [self setMasInfoButton:nil];
    [super viewDidUnload];
}
@end
