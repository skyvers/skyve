package org.skyve.impl.tools.kickstart;

import geodb.GeoDB;

import java.awt.event.ActionEvent;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.Connection;
import java.sql.Statement;
import java.util.Date;

import javax.swing.AbstractAction;

import org.apache.commons.codec.binary.Base64;
import java.util.UUID;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateElasticSearchPersistence;
import org.skyve.impl.util.UtilImpl;

class CreateAction extends AbstractAction {
	private static final long serialVersionUID = -7217121655136714836L;

	private KickStartPanel panel;
	
	private static final String[] ROLES = new String[] {"admin.BasicUser",
															"admin.ContactViewer",
															"admin.SecurityAdministrator",
															"admin.JobMaintainer",
															"whosin.Manager",
															"whosinIntegrate.Manager"};
	
	CreateAction(KickStartPanel panel) {
		super("Create");
		this.panel = panel;
	}

	@Override
	@SuppressWarnings("resource") // this is the connection which is handled by Persistence
	public void actionPerformed(ActionEvent evt) {
		try {
			UIUtil.startWaitCursor(panel);

			AbstractPersistence.IMPLEMENTATION_CLASS = HibernateElasticSearchPersistence.class;
//			UtilImpl.CONTENT_DIRECTORY = contentDirectory;
			UtilImpl.DIALECT = panel.getDBDialect();
			UtilImpl.STANDALONE_DATABASE_JDBC_DRIVER = panel.getDBDriver();
			UtilImpl.STANDALONE_DATABASE_CONNECTION_URL = panel.getDBUrl();
			UtilImpl.STANDALONE_DATABASE_USERNAME = panel.getDBUserName();
			UtilImpl.STANDALONE_DATABASE_PASSWORD = panel.getDBPassword();
			UtilImpl.DDL_SYNC = true;
//			UtilImpl.APPS_JAR_DIRECTORY = "/C:/_/skyve/skyve-ee/javaee/skyve.ear/apps.jar/";
			
			AbstractRepository.set(new LocalDesignRepository());
			SuperUser user = new SuperUser();
			user.setCustomerName(panel.getCustomer());
			user.setName(panel.getUser());
			
			HibernateElasticSearchPersistence persistence = (HibernateElasticSearchPersistence) AbstractPersistence.get();
			persistence.setUser(user);

			Connection connection = null;
			try {
				// Don't close this connection
				connection = persistence.getConnection();
				try (Statement statement = connection.createStatement()) {
					if ("org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect".equals(UtilImpl.DIALECT)) {
						GeoDB.InitGeoDB(connection);
					}
System.out.println(createSql());
					statement.executeUpdate(createSql());
				}
				UIUtil.popup("Database has been created");
			}
			finally {
				if (connection != null) {
					connection.commit();
				}
				persistence.commit(true);
			}
		}
		catch (Throwable t) {
			UIUtil.popup(t);
		}
		finally {
			UIUtil.stopWaitCursor(panel);
		}
	}

	private String createSql() throws NoSuchAlgorithmException {
		StringBuilder result = new StringBuilder(1024);
		
		String contactId = UUID.randomUUID().toString();
		String userId = UUID.randomUUID().toString();
		String groupId = UUID.randomUUID().toString();
		String optimisticLock = new OptimisticLock(panel.getUser(), new Date()).toString();
		
		// Contact
		result.append("insert into ADM_Contact (bizId,bizVersion,bizLock,bizCustomer,bizUserId,bizKey,name,contactType,email1,mobile) values ('");
		result.append(contactId).append("',0,'").append(optimisticLock);
		result.append("','").append(panel.getCustomer()).append("','").append(userId);
		result.append("','").append(panel.getUser()).append("','").append(panel.getUser()).append("','Person',");
		String value = panel.getEmail();
		if (value == null) {
			result.append("null");
		}
		else {
			result.append('\'').append(value).append('\'');
		}
		result.append(',');
		value = panel.getMobile();
		if (value == null) {
			result.append("null");
		}
		else {
			result.append('\'').append(value).append('\'');
		}
		result.append(");\n");

		// Group
		result.append("insert into ADM_SecurityGroup (bizId,bizVersion,bizLock,bizCustomer,bizUserId,bizKey,name,description) values ('");
		result.append(groupId).append("',0,'").append(optimisticLock);
		result.append("','").append(panel.getCustomer()).append("','").append(userId);
		result.append("','Everything','Everything','The kitchen sink');\n");

		// Roles
		for (String role : ROLES) {
			result.append("insert into ADM_SecurityGroupRole (bizId,bizVersion,bizLock,bizCustomer,bizUserId,bizKey,roleName,parent_id) values ('");
			result.append(UUID.randomUUID().toString()).append("',0,'").append(optimisticLock);
			result.append("','").append(panel.getCustomer()).append("','").append(userId);
			result.append("','").append(role).append("','").append(role).append("','").append(groupId).append("');\n");
		}

		MessageDigest md = MessageDigest.getInstance("SHA1");
		Base64 base64Codec = new Base64();
		String hashedPassword = new String(base64Codec.encode(md.digest(new String(panel.getPassword()).getBytes())));

		// User
		result.append("insert into ADM_SecurityUser (bizId,bizVersion,bizLock,bizCustomer,bizUserId,bizKey,userName,password,contact_id) values ('");
		result.append(userId).append("',0,'").append(optimisticLock);
		result.append("','").append(panel.getCustomer()).append("','").append(userId);
		result.append("','").append(panel.getUser()).append("','").append(panel.getUser());
		result.append("','").append(hashedPassword).append("','").append(contactId).append("');\n");

		// User Group
		result.append("insert into ADM_SecurityUser_groups (owner_id,element_id) values ('");
		result.append(userId).append("','").append(groupId).append("');\n");

		return result.toString();
	}
}
