package org.skyve.wildcat.tools.kickstart;

import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.net.URL;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.RootPaneContainer;
import javax.swing.SwingUtilities;

/**
 * GUI utility static class.
 */
public final class UIUtil {
	/**
	 * A no-op MouseAdapter used to stop mouse input.
	 */
	private static final MouseAdapter noopMouseAdapter = new MouseAdapter() {
		// nothing to see here
	}; 

	/**
	 * Prevent instantiation
	 */
	private UIUtil() {
	}

	/**
	 * Popup a message dialog.
	 * @param message	The message to display.
	 */
	private static void popupMessage(String message) {
		JOptionPane.showMessageDialog(null, message, "Problem", JOptionPane.ERROR_MESSAGE); 
	}
	
	/**
	 * A <code>Runnable</code> that calls {@link #popupMessage}
	 */
	private static final class PopupRunnable implements Runnable {
		/**
		 * The message to display.
		 */
		String message;
		
		/**
		 * Constructor.
		 * @param message	The message to display.
		 */
		private PopupRunnable(String message) {
			this.message = message;
		}
		
		/**
		 * Popup the message.
		 */
		@Override
		@SuppressWarnings("synthetic-access")
		public void run() {
			popupMessage(message); 
		}
	}

	/**
	 * Popup a message in a dialog on the event-dispatch thread.
	 * @param message	The mesasge to popup.
	 */
	@SuppressWarnings("synthetic-access")
	public static void popup(String message) {
		if (SwingUtilities.isEventDispatchThread()) { // we are in the event-dispatch thread
			popupMessage(message); 
		}
		else { // not the event-dispatch thread
			SwingUtilities.invokeLater(new PopupRunnable(message));
		}
	}
	
	/**
	 * Popup information related to a <code>Throwable</code>.
	 * @param t	The throwable to display.
	 */
	public static void popup(Throwable t) {
		t.printStackTrace();

		String message = t.getMessage();

		if (t.getCause() != null) {
			if ((message != null) && (message.length() == 0)) {
				message += " caused by ";
			}
			message += t.getCause().getMessage();
		}

		popup(message);
	}
	
	/**
	 * Sets the wait (hourglass) mouse cursor for an application containing <code>component</code>.
	 * @param component	The component to block.
	 */
	public static void startWaitCursor(JComponent component) {
		RootPaneContainer root = (RootPaneContainer) component.getTopLevelAncestor();
		root.getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		root.getGlassPane().addMouseListener(noopMouseAdapter);
		root.getGlassPane().setVisible(true);
	}

	/**
	 * Unsets the wait (hourglass) mouse cursor for an application containing <code>component</code>.
	 * @param component	The component of interest.
	 */
	public static void stopWaitCursor(JComponent component) {
		RootPaneContainer root = (RootPaneContainer) component.getTopLevelAncestor();
		root.getGlassPane().setCursor(Cursor.getDefaultCursor());
		root.getGlassPane().removeMouseListener(noopMouseAdapter);
		root.getGlassPane().setVisible(false);
	}

	/**
	 * Locates and creates an Icon from a file name.
	 * @param name 	The name of the icon file.
	 * @return	The icon.
	 */
	public static ImageIcon getIcon(String name) {
		ImageIcon result = null;
		
		String imagePath = "images/" + name;

		URL url = new UIUtil().getClass().getResource(imagePath);
		if (url != null)  {
			result = new ImageIcon(url);
		}
		
		return result;
	}
}
