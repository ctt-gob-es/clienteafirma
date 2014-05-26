//
//  AOViewController.h
//  SignSample02
//

#import <UIKit/UIKit.h>

@interface AOViewController : UIViewController <UITableViewDelegate>{
    
    //Outlet para el boton de firma
    IBOutlet UIButton *signButton;
    
    // Firma completada
    BOOL signDone;
    
    // Boton de aceptacion del PIN
    IBOutlet UIButton *pinButton;
    
    
    IBOutlet UIButton *selectionButton;
    
    IBOutlet UITableView *tblView;
    
}

@property (nonatomic, assign) BOOL signDone;


@property (nonatomic, retain) UIButton *signButton;
-(IBAction)buttonPressed:(id)sender;

@property (nonatomic, retain) UIButton *pinButton;
-(IBAction)pinButtonPressed:(id)sender;

@property (nonatomic, retain) UIButton *selectionButton;

@property(nonatomic, retain) UITextField *pinTextField;

@property(nonatomic, retain) NSString *datosInUse;

@property (retain, nonatomic) IBOutlet UILabel *nombreCert;

@property (retain, nonatomic) IBOutlet UITableView *tblView;

@property (retain, nonatomic) IBOutlet UITextView *nombreAlmacen;


@end
