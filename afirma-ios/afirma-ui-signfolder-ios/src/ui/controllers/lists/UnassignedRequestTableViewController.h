//
//  RequestTableViewController.h
//  PortaFirmas_@Firma
//
//  Created by Antonio Fiñana on 22/10/12.
//  Copyright (c) 2012 Tempos21. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "RequestSignerController.h"
#import "BaseListTVC.h"

@interface UnassignedRequestTableViewController : BaseListTVC <RequestSignerEvent, UIAlertViewDelegate>
{
    RequestSignerController *requestSignerController;
    NSMutableArray *selectedRows;
    NSDictionary *appConfig;
}

@property (weak, nonatomic) IBOutlet UIBarButtonItem *signBarButton;
@property (weak, nonatomic) IBOutlet UIBarButtonItem *rejectBarButton;
@property (weak, nonatomic) IBOutlet UIBarButtonItem *selectButtonItem;

- (IBAction)rejectAction:(id)sender;
- (IBAction)cancelAction:(id)sender;

@end
