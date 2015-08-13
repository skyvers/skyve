package org.skyve.wildcat.tools.kickstart;

import java.awt.Dimension;
import java.awt.Toolkit;

import javax.swing.JFrame;

public class KickStart extends JFrame {
	private static final long serialVersionUID = 4752040208957605063L;

	public KickStart() {
		super("KickStart");
		init();
		pack();
		setVisible(true);
	}
	
	private void init() {
		Dimension size = new Dimension(800, 480);
		setPreferredSize(size);

		add(new KickStartPanel());
		
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		setLocation(screenSize.width / 2 - size.width / 2, 
						screenSize.height / 2 - size.height / 2);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
	}
	
	
	@SuppressWarnings("unused")
	public static void main(String[] args) {
		new KickStart();
	}
}
