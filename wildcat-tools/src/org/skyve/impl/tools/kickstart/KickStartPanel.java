package org.skyve.impl.tools.kickstart;

import java.awt.BorderLayout;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.SpringLayout;

import org.skyve.util.Util;

class KickStartPanel extends JPanel {
	private static final long serialVersionUID = 6266130993737889638L;

	private JTextField customer = new JTextField("demo");
	private JTextField user = new JTextField("admin");
	private JPasswordField password = new JPasswordField();
	private JTextField email = new JTextField("admin@demo.com");
	private JTextField mobile = new JTextField();
	private JComboBox<String> dbDialect = new JComboBox<>(new String[] {"org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect",
																			"org.skyve.impl.persistence.hibernate.dialect.SQLServer2005SpatialDialect",
																			"org.skyve.impl.persistence.hibernate.dialect.SQLServer2008SpatialDialect",
																			"org.skyve.impl.persistence.hibernate.dialect.MySQL5InnoDBSpatialDialect"});
	private JComboBox<String> dbDriver = new JComboBox<>(new String[] {"org.h2.jdbcx.JdbcDataSource",
																		"com.microsoft.sqlserver.jdbc.SQLServerDriver",
																		"com.mysql.jdbc.Driver"});
	private JComboBox<String> dbUrl = new JComboBox<>(new String[] {"jdbc:h2:file:~/<db-name>;IGNORECASE=TRUE",
																		"jdbc:sqlserver://localhost;databaseName=<db-name>;sendStringParametersAsUnicode=false;",
																		"jdbc:mysql://localhost:3306/<db-name>?useCursorFetch=true&defaultFetchSize=100"});
	private JTextField dbUserName = new JTextField("sa");
	private JTextField dbPassword = new JTextField("sa");
	
	public KickStartPanel() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

		dbDialect.setEditable(true);
		dbDriver.setEditable(true);
		dbUrl.setEditable(true);
		
		JPanel userPanel = new JPanel(new SpringLayout());
		userPanel.setBorder(BorderFactory.createTitledBorder("Customer/User"));
		userPanel.add(new JLabel("Customer Name:*"));
		userPanel.add(customer);
		userPanel.add(new JLabel("Skyve admin user name:*"));
		userPanel.add(user);
		userPanel.add(new JLabel("Skyve admin password:*"));
		userPanel.add(password);
		userPanel.add(new JLabel("Skyve admin user email:*"));
		userPanel.add(email);
		userPanel.add(new JLabel("Skyve admin user mobile:"));
		userPanel.add(mobile);
		SpringUtilities.makeCompactGrid(userPanel, 5, 2, 5, 5, 5, 5);

		add(userPanel);

		JPanel dbPanel = new JPanel(new SpringLayout());
		dbPanel.setBorder(BorderFactory.createTitledBorder("Database"));
		dbPanel.add(new JLabel("DB Dialect:*"));
		dbPanel.add(dbDialect);
		dbPanel.add(new JLabel("DB Driver:*"));
		dbPanel.add(dbDriver);
		dbPanel.add(new JLabel("DB URL:*"));
		dbPanel.add(dbUrl);
		dbPanel.add(new JLabel("DB Username:*"));
		dbPanel.add(dbUserName);
		dbPanel.add(new JLabel("DB Password:*"));
		dbPanel.add(dbPassword);
		SpringUtilities.makeCompactGrid(dbPanel, 5, 2, 5, 5, 5, 5);

		add(dbPanel);
		
		JPanel buttonPanel = new JPanel(new BorderLayout());
		JButton createButton = new JButton(new CreateAction(this));
		buttonPanel.add(createButton, BorderLayout.EAST);
		add(buttonPanel);
	}
	
	String getCustomer() {
		return Util.processStringValue(customer.getText());
	}

	String getUser() {
		return Util.processStringValue(user.getText());
	}
	
	char[] getPassword() {
		return password.getPassword();
	}
	
	String getEmail() {
		return Util.processStringValue(email.getText());
	}

	String getMobile() {
		return Util.processStringValue(mobile.getText());
	}

	String getDBDialect() {
		return Util.processStringValue((String) dbDialect.getSelectedItem());
	}
	
	String getDBDriver() {
		return Util.processStringValue((String) dbDriver.getSelectedItem());
	}
	
	String getDBUrl() {
		return Util.processStringValue((String) dbUrl.getSelectedItem());
	}
	
	String getDBUserName() {
		return Util.processStringValue(dbUserName.getText());
	}
	
	String getDBPassword() {
		return Util.processStringValue(dbPassword.getText());
	}
}
