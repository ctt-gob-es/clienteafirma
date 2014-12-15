package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToolBar;

final class UtilToolBar extends JToolBar {

	private static final long serialVersionUID = 4971993407723910690L;

	public UtilToolBar(final CertificateSelectionDialog selectionDialog, final Component parent) {

		setName(CertificateSelectionDialogMessages.getString("UtilToolBar.0")); //$NON-NLS-1$

		final JButton refresh = new JButton(
			new ImageIcon(
				UtilToolBar.class.getResource("/resources/toolbar/ic_autorenew_black_18dp.png"), //$NON-NLS-1$
				CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
			)
		);
		refresh.getAccessibleContext().setAccessibleDescription(
			CertificateSelectionDialogMessages.getString("UtilToolBar.1") //$NON-NLS-1$
		);
		refresh.setToolTipText(CertificateSelectionDialogMessages.getString("UtilToolBar.1")); //$NON-NLS-1$
		refresh.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					UtilActions.doRefresh(selectionDialog, parent);
				}
			}
		);
		add(refresh);

		final JButton open = new JButton(
			new ImageIcon(
				UtilToolBar.class.getResource("/resources/toolbar/ic_open_in_browser_black_18dp.png"), //$NON-NLS-1$
				CertificateSelectionDialogMessages.getString("UtilToolBar.2") //$NON-NLS-1$
			)
		);
		open.getAccessibleContext().setAccessibleDescription(
			CertificateSelectionDialogMessages.getString("UtilToolBar.2") //$NON-NLS-1$
		);
		open.setToolTipText(CertificateSelectionDialogMessages.getString("UtilToolBar.2")); //$NON-NLS-1$
		open.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					UtilActions.doOpen(selectionDialog, parent);
				}
			}
		);
		add(open);

		final JButton help = new JButton(
			new ImageIcon(
				UtilToolBar.class.getResource("/resources/toolbar/ic_help_black_18dp.png"), //$NON-NLS-1$
				CertificateSelectionDialogMessages.getString("UtilToolBar.3") //$NON-NLS-1$
			)
		);
		help.getAccessibleContext().setAccessibleDescription(
			CertificateSelectionDialogMessages.getString("UtilToolBar.3") //$NON-NLS-1$
		);
		help.setToolTipText(CertificateSelectionDialogMessages.getString("UtilToolBar.3")); //$NON-NLS-1$
		help.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					UtilActions.doHelp();
				}
			}
		);
		add(help);

	}

}
