//
//  KeyboardObserver.h
//  bluekiwi
//
//  Created by David Arrufat on 27/01/14.
//  Copyright (c) 2014 Tempos 21. All rights reserved.
//

#pragma mark - Keyboard info
#define isKeyboardShowed                 [[KeyboardObserver getInstance] isShowed]
#define isKeyboardHidden                 [[KeyboardObserver getInstance] isHidden]
#define isKeyboardHiding                 [[KeyboardObserver getInstance] isHiding]
#define KEYBOARD_HEIGHT                  ((SCREEN_HEIGHT)-(KEYBOARD_Y))
#define KEYBOARD_Y                       (IOS_NEWER_OR_EQUAL_TO_8 ? [[KeyboardObserver getInstance] getKeyboardFrame].origin.y : KEYBOARD_Y_IOS_7)
// TODO: delete it when the minimum iOS version will be 8
#define KEYBOARD_Y_IOS_7                 (isiPhone ? [[KeyboardObserver getInstance] getKeyboardFrame].origin.y : (KEYBOARD_Y_IOS_7_LANDSCAPE))
#define KEYBOARD_Y_IOS_7_LANDSCAPE       (STATUSBAR_ORIENTATION == UIInterfaceOrientationLandscapeLeft ? KEYBOARD_Y_IOS_7_LANDSCAPE_LEFT : KEYBOARD_Y_IOS_7_LANDSCAPE_RIGHT)
#define KEYBOARD_Y_IOS_7_LANDSCAPE_LEFT  ([[KeyboardObserver getInstance] getKeyboardFrame].origin.x)
#define KEYBOARD_Y_IOS_7_LANDSCAPE_RIGHT ((SCREEN_HEIGHT)-abs([[KeyboardObserver getInstance] getKeyboardFrame].origin.x + [[KeyboardObserver getInstance] getKeyboardFrame].size.width))

@protocol KeyboardObserverDelegate <NSObject>

- (void)handleKeyboardChange;

@end

typedef enum
{
    KeyboardStatusShowing,
    KeyboardStatusShowed,
    KeyboardStatusHiding,
    KeyboardStatusHidden
} KeyboardStatus;

@class KeyboardObserver;

@interface KeyboardObserver : NSObject

+ (KeyboardObserver *)getInstance;

- (void)addObserver:(NSObject *)observer;
- (void)removeObserver:(NSObject *)observer;

- (BOOL)isShowed;
- (BOOL)isHiding;
- (BOOL)isHidden;

- (CGRect)getKeyboardFrame;

@end
