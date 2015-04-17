//
//  AOAboutViewController.h
//  SignSample02
//
//

#import <UIKit/UIKit.h>
#import "GAITrackedViewController.h"

@interface AOAboutViewController : GAITrackedViewController <UITabBarControllerDelegate> {
    
    IBOutlet UITabBar *tabBar;
    NSString *relinkUserId;
}

@property (retain, nonatomic) IBOutlet UIButton *manageStoreButton;



@end
