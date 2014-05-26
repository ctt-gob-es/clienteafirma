using System;
using System.Collections.Generic;
using System.Runtime.InteropServices.WindowsRuntime;
using System.Threading;
using System.Threading.Tasks;
using Windows.Foundation;
using Windows.System;
using Windows.UI;
using Windows.UI.Core;
using Windows.UI.Popups;
using Windows.UI.ViewManagement;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;

namespace VisuallyLocated.UI.Popups
{
    public class CustomDialog
    {
        private Popup _popup;
        private TaskCompletionSource<IUICommand> _taskCompletionSource;
        private StackPanel _buttonPanel;

        /// <summary>
        /// Initializes a new instance of the CustomDialog class to display an untitled
        /// dialog that can be used to show your user custom content.
        /// </summary>
        /// <param name="content">The content displayed to the user.</param>
        public CustomDialog(object content)
        {
            Title = string.Empty;
            Commands = new List<IUICommand>();
            CancelCommandIndex = Int32.MaxValue;
            DefaultCommandIndex = Int32.MaxValue;
            HeaderBrush = new SolidColorBrush(Colors.White);
            Content = content;
        }

        /// <summary>
        /// Initializes a new instance of the CustomDialog class to display a titled
        /// dialog that can be used to show your user custom content.
        /// </summary>
        /// <param name="content">The content displayed to the user.</param>
        /// <param name="title">The title you want displayed on the dialog.</param>
        public CustomDialog(object content, string title)
            : this(content)
        {
            if (string.IsNullOrEmpty(title) == false)
            {
                Title = title;
            }
        }

        /// <Summary>
        /// Gets or sets the index of the command you want to use as the default. This
        /// is the command that fires by default when users press the ENTER key.
        /// </Summary>
        /// <Returns>The index of the default command.</Returns>
        public int DefaultCommandIndex { get; set; }

        /// <Summary>
        /// Gets or sets the index of the command you want to use as the cancel command.
        /// This is the command that fires when users press the ESC key.
        /// </Summary>
        /// <Returns>The index of the cancel command.</Returns>
        public int CancelCommandIndex { get; set; }

        /// <Summary>
        /// Gets the set of commands that appear in the command bar of the message dialog.
        /// </Summary>
        /// <Returns>The commands.</Returns>
        public IList<IUICommand> Commands { get; private set; }

        /// <Summary>
        /// Gets or sets the content to be displayed to the user.
        /// </Summary>
        /// <Returns>The message to be displayed to the user.</Returns>
        public object Content { get; set; }

        /// <Summary>
        /// Gets or sets the title to display on the dialog, if any.
        /// </Summary>
        /// <Returns>
        /// The title you want to display on the dialog. If the title is not set, this
        /// will return an empty string.
        /// </Returns>
        public string Title { get; set; }

        public Brush HeaderBrush { get; set; }

        /// <Summary>
        /// Begins an asynchronous operation showing a dialog.
        /// </Summary>
        /// <Returns>     
        /// An object that represents the asynchronous operation. For more on the async
        /// pattern, see Asynchronous programming in the Windows Runtime.
        /// </Returns>
        public IAsyncOperation<IUICommand> ShowAsync()
        {
            _popup = new Popup { Child = CreateDialog() };
            if (_popup.Child != null)
            {
                SubscribeEvents();
                _popup.IsOpen = true;
            }
            return AsyncInfo.Run(WaitForInput);
        }

        private Task<IUICommand> WaitForInput(CancellationToken token)
        {
            _taskCompletionSource = new TaskCompletionSource<IUICommand>();

            token.Register(OnCanceled);

            return _taskCompletionSource.Task;
        }

        private UIElement CreateDialog()
        {
            var content = Window.Current.Content as FrameworkElement;
            if (content == null)
            {
                // The dialog is being shown before content has been created for the window
                Window.Current.Activated += OnWindowActivated;
                return null;
            }

            Style subHeaderTextStyle = Application.Current.Resources["LightSubheaderTextStyle"] as Style;
            Style buttonStyle = Application.Current.Resources["LightButtonStyle"] as Style;

            double width = Window.Current.Bounds.Width;
            double height = Window.Current.Bounds.Height;
            var root = new Grid { Width = width, Height = height };
            var overlay = new Grid { Background = new SolidColorBrush(Colors.Black), Opacity = 0.2D };
            root.Children.Add(overlay);

            var dialogPanel = new Grid { VerticalAlignment = VerticalAlignment.Center };
            dialogPanel.RowDefinitions.Add(new RowDefinition { Height = new GridLength(1, GridUnitType.Auto) });
            dialogPanel.RowDefinitions.Add(new RowDefinition { Height = new GridLength(1, GridUnitType.Star) });
            dialogPanel.RowDefinitions.Add(new RowDefinition { Height = new GridLength(1, GridUnitType.Auto) });
            dialogPanel.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(1, GridUnitType.Star) });
            dialogPanel.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(2, GridUnitType.Star) });
            dialogPanel.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(1, GridUnitType.Star) });

            var titleBorder = new Border { Background = HeaderBrush, Height = 70D };
            Grid.SetColumnSpan(titleBorder, 3);
            dialogPanel.Children.Add(titleBorder);

            var titleTextBlock = new TextBlock();
            titleTextBlock.Text = Title;
            titleTextBlock.Style = subHeaderTextStyle;
            titleTextBlock.Margin = new Thickness(0, 0, 0, 20);
            titleTextBlock.VerticalAlignment = VerticalAlignment.Bottom;
            Grid.SetColumn(titleTextBlock, 1);
            dialogPanel.Children.Add(titleTextBlock);

            Border contentBorder = new Border { Background = new SolidColorBrush(Colors.White) };
            Grid.SetRow(contentBorder, 1);
            Grid.SetRowSpan(contentBorder, 2);
            Grid.SetColumnSpan(contentBorder, 3);
            dialogPanel.Children.Add(contentBorder);

            var contentPresenter = new ContentPresenter();
            contentPresenter.Content = Content;
            Grid.SetColumn(contentPresenter, 1);
            Grid.SetRow(contentPresenter, 1);
            dialogPanel.Children.Add(contentPresenter);

            _buttonPanel = new StackPanel { HorizontalAlignment = HorizontalAlignment.Right, Orientation = Orientation.Horizontal };
            Grid.SetRow(_buttonPanel, 2);
            Grid.SetColumn(_buttonPanel, 1);
            dialogPanel.Children.Add(_buttonPanel);

            if (Commands.Count == 0)
            {
                Button button = new Button();
                button.Style = buttonStyle;
                button.Content = "Close";
                button.MinWidth = 92;
                button.Margin = new Thickness(20, 20, 0, 20);
                button.Focus(Windows.UI.Xaml.FocusState.Programmatic);
                button.Click += (okSender, okArgs) => CloseDialog(null);
                _buttonPanel.Children.Add(button);
            }
            else
            {
                bool flag = true;
                foreach (var command in Commands)
                {
                    IUICommand currentCommand = command;
                    Button button = new Button();
                    button.Style = buttonStyle;
                    button.Content = command.Label;
                    button.Margin = new Thickness(20, 20, 0, 20);
                    button.MinWidth = 92;
                    if (flag)
                    {
                        button.Focus(Windows.UI.Xaml.FocusState.Programmatic);
                        flag = false;
                    }
                    button.Click += (okSender, okArgs) => CloseDialog(currentCommand);
                    _buttonPanel.Children.Add(button);
                }
            }

            root.Children.Add(dialogPanel);
            return root;
        }

        // adjust for different view states
        private void OnWindowSizeChanged(object sender, WindowSizeChangedEventArgs e)
        {
            if (_popup.IsOpen == false) return;

            var child = _popup.Child as FrameworkElement;
            if (child == null) return;

            child.Width = e.Size.Width;
            child.Height = e.Size.Height;
        }

        // Adjust the name/password textboxes for the virtual keyuboard
        private void OnInputShowing(InputPane sender, InputPaneVisibilityEventArgs args)
        {
            var child = _popup.Child as FrameworkElement;
            if (child == null) return;

            var transform = _buttonPanel.TransformToVisual(child);
            var topLeft = transform.TransformPoint(new Point(0, 0));

            // Need to be able to view the entire textblock (plus a little more)
            var buffer = 20;
            if ((topLeft.Y - buffer) > sender.OccludedRect.Top)
            {
                var margin = topLeft.Y - sender.OccludedRect.Top;
                margin -= buffer;
                child.Margin = new Thickness(0, -margin, 0, 0);
            }
        }

        private void OnInputHiding(InputPane sender, InputPaneVisibilityEventArgs args)
        {
            var child = _popup.Child as FrameworkElement;
            if (child == null) return;

            child.Margin = new Thickness(0);
        }

        private void OnKeyDown(object sender, KeyRoutedEventArgs e)
        {
            // Only respond to Esc if there is a cancel index
            if ((e.Key == VirtualKey.Escape) &&
                (CancelCommandIndex >= 0) && (CancelCommandIndex < Commands.Count))
            {
                OnCanceled();
            }

            // Only respond to Enter if there is a cancel index
            if ((e.Key == VirtualKey.Enter) &&
                (DefaultCommandIndex >= 0) && (DefaultCommandIndex < Commands.Count))
            {
                CloseDialog(Commands[DefaultCommandIndex]);
            }
        }

        private void OnWindowActivated(object sender, WindowActivatedEventArgs e)
        {
            Window.Current.Activated -= OnWindowActivated;
            SubscribeEvents();
            _popup.Child = CreateDialog();
            _popup.IsOpen = true;
        }

        private void OnCanceled()
        {
            UnsubscribeEvents();

            IUICommand command = null;
            if (CancelCommandIndex < Commands.Count)
            {
                command = Commands[CancelCommandIndex];
            }
            CloseDialog(command);
        }

        private void CloseDialog(IUICommand command)
        {
            UnsubscribeEvents();

            if (command != null)
            {
                command.Invoked(command);
            }
            _popup.IsOpen = false;
            _taskCompletionSource.SetResult(command);
        }

        private void SubscribeEvents()
        {
            Window.Current.SizeChanged += OnWindowSizeChanged;
            Window.Current.Content.KeyDown += OnKeyDown;

            var input = InputPane.GetForCurrentView();
            input.Showing += OnInputShowing;
            input.Hiding += OnInputHiding;
        }

        private void UnsubscribeEvents()
        {
            Window.Current.SizeChanged -= OnWindowSizeChanged;
            Window.Current.Content.KeyDown -= OnKeyDown;

            var input = InputPane.GetForCurrentView();
            input.Showing -= OnInputShowing;
            input.Hiding -= OnInputHiding;
        }
    }
}
