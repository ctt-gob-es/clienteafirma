//
//  AlertProgressBar.m
//  SignSample02
//
//

#import "AlertProgressBar.h"

@implementation AlertProgressBar

@synthesize av;
@synthesize spinner;

- (void) createProgressBar:(UIView*)view{
    
    av = [[UIAlertView alloc] initWithTitle: NSLocalizedString(@"processing",nil) message:@"" delegate:nil cancelButtonTitle:nil otherButtonTitles:nil];
    
    spinner = [[UIActivityIndicatorView alloc] initWithActivityIndicatorStyle:UIActivityIndicatorViewStyleWhiteLarge];
    
    spinner.center = CGPointMake(140, 70);
    
    spinner.hidesWhenStopped = YES;
    
    [spinner startAnimating];
    [av addSubview:spinner];
    [av show];

}

-(void) destroy {
    [spinner stopAnimating];
    [av dismissWithClickedButtonIndex:0 animated:true];
}

@end
