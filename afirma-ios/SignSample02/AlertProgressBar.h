//
//  AlertProgressBar.h
//  SignSample02
//
//

#import <Foundation/Foundation.h>

@interface AlertProgressBar : NSObject

@property (nonatomic, retain) UIAlertView *av;
@property (nonatomic, retain) UIActivityIndicatorView *spinner;

-(void) createProgressBar:(UIView*)view;
-(void) destroy;

@end
