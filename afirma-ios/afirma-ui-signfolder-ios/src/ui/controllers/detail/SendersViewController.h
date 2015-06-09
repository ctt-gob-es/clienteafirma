//
//  SendersViewController.h
//  PortaFirmasUniv
//
//  Created by Antonio Fiñana Sánchez on 16/11/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface SendersViewController : UITableViewController
{
    NSMutableArray* _dataSource;
}
@property (strong, nonatomic) NSMutableArray* dataSource;
@end
